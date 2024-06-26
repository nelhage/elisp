;;; helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-adaptive" "../../../.emacs.d/elpa/helm-20211125.534/helm-adaptive.el"
;;;;;;  "1363e3d700fd1f83b431305d1aee68fe")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-adaptive.el

(defvar helm-adaptive-mode nil "\
Non-nil if Helm-Adaptive mode is enabled.
See the `helm-adaptive-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.")

(custom-autoload 'helm-adaptive-mode "helm-adaptive" nil)

(autoload 'helm-adaptive-mode "helm-adaptive" "\
Toggle adaptive sorting in all sources.

If called interactively, enable Helm-Adaptive mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'helm-reset-adaptive-history "helm-adaptive" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted
`helm-adaptive-history-file'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-adaptive"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-adaptive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-adaptive.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-adaptive" '("helm-adapt")))

;;;***

;;;***

;;;### (autoloads nil "helm-bookmark" "../../../.emacs.d/elpa/helm-20211125.534/helm-bookmark.el"
;;;;;;  "aa171c1b0c5d69ac1f09f56c30bfd41f")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-bookmark.el

(autoload 'helm-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks." t nil)

(autoload 'helm-filtered-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded only
if external addressbook-bookmark package is installed." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-bookmark"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-bookmark.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-bookmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-bookmark" '("bmkext-jump-" "bookmark" "helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-buffers" "../../../.emacs.d/elpa/helm-20211125.534/helm-buffers.el"
;;;;;;  "412c1c0107dbb8609507f38c47310aa1")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-buffers.el

(autoload 'helm-buffers-list "helm-buffers" "\
Preconfigured `helm' to list buffers." t nil)

(autoload 'helm-mini "helm-buffers" "\
Preconfigured `helm' displaying `helm-mini-default-sources'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-buffers"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-buffers.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-buffers.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-buffers" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-color" "../../../.emacs.d/elpa/helm-20211125.534/helm-color.el"
;;;;;;  "338489c88e0f08d3b60956452e73ccf6")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-color.el

(autoload 'helm-colors "helm-color" "\
Preconfigured `helm' for color." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-color" "../../../.emacs.d/elpa/helm-20211125.534/helm-color.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-color.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-color" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-comint" "../../../.emacs.d/elpa/helm-20211125.534/helm-comint.el"
;;;;;;  "042b3e412efe74808ac08d1ef73cecc4")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-comint.el

(autoload 'helm-comint-prompts "helm-comint" "\
Pre-configured `helm' to browse the prompts of the current comint buffer." t nil)

(autoload 'helm-comint-prompts-all "helm-comint" "\
Pre-configured `helm' to browse the prompts of all comint sessions." t nil)

(autoload 'helm-comint-input-ring "helm-comint" "\
Preconfigured `helm' that provide completion of `comint' history." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-comint"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-comint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-comint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-comint" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-command" "../../../.emacs.d/elpa/helm-20211125.534/helm-command.el"
;;;;;;  "bd9d97571f18694e5f437c5fa37d96db")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-command.el

(autoload 'helm-M-x "helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x'
`execute-extended-command'.

Unlike regular `M-x' Emacs vanilla `execute-extended-command'
command, the prefix args if needed, can be passed AFTER starting
`helm-M-x'.  When a prefix arg is passed BEFORE starting
`helm-M-x', the first `C-u' while in `helm-M-x' session will
disable it.

You can get help on each command by persistent action.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-command"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-command.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-command.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-command" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-dabbrev" "../../../.emacs.d/elpa/helm-20211125.534/helm-dabbrev.el"
;;;;;;  "672d192e7be5e098685d1df5482f55fc")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-dabbrev.el

(autoload 'helm-dabbrev "helm-dabbrev" "\
Preconfigured helm for dynamic abbreviations." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-dabbrev"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-dabbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-dabbrev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-dabbrev" '("helm-dabbrev-")))

;;;***

;;;***

;;;### (autoloads nil "helm-elisp" "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp.el"
;;;;;;  "c3bc63bc0130a52f8f05ac51f24ecf88")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm-elisp" "\
Preconfigured Helm for Lisp symbol completion at point." t nil)

(autoload 'helm-complete-file-name-at-point "helm-elisp" "\
Preconfigured Helm to complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "helm-elisp" nil t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "\
Preconfigured Helm to complete Lisp symbol or filename at point.
Filename completion happens if string start after or between a
double quote." t nil)

(autoload 'helm-apropos "helm-elisp" "\
Preconfigured Helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as
a string, i.e. the `symbol-name' of any existing symbol.

\(fn DEFAULT)" t nil)

(autoload 'helm-manage-advice "helm-elisp" "\
Preconfigured `helm' to disable/enable function advices." t nil)

(autoload 'helm-locate-library "helm-elisp" "\
Preconfigured helm to locate elisp libraries." t nil)

(autoload 'helm-timers "helm-elisp" "\
Preconfigured `helm' for timers." t nil)

(autoload 'helm-complex-command-history "helm-elisp" "\
Preconfigured `helm' for complex command history." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-elisp" "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp" '("helm-" "with-helm-show-completion")))

;;;***

;;;***

;;;### (autoloads nil "helm-elisp-package" "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp-package.el"
;;;;;;  "d84cfb4ae795b65b2bda40f4b6c9c232")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-elisp-package.el

(autoload 'helm-list-elisp-packages "helm-elisp-package" "\
Preconfigured `helm' for listing and handling Emacs packages.

\(fn ARG)" t nil)

(autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "\
Preconfigured Helm for Emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on
remote.  Called with a prefix ARG always fetch packages on
remote.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-elisp-package"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp-package.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-elisp-package.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-elisp-package" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-epa" "../../../.emacs.d/elpa/helm-20211125.534/helm-epa.el"
;;;;;;  "ea0f27716cb624d57819a227ae6ca540")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-epa.el

(defvar helm-epa-mode nil "\
Non-nil if Helm-Epa mode is enabled.
See the `helm-epa-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-epa-mode'.")

(custom-autoload 'helm-epa-mode "helm-epa" nil)

(autoload 'helm-epa-mode "helm-epa" "\
Enable helm completion on gpg keys in epa functions.

If called interactively, enable Helm-Epa mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'helm-epa-list-keys "helm-epa" "\
List all gpg keys.
This is the helm interface for `epa-list-keys'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-epa" "../../../.emacs.d/elpa/helm-20211125.534/helm-epa.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-epa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-epa" '("helm-epa")))

;;;***

;;;***

;;;### (autoloads nil "helm-eshell" "../../../.emacs.d/elpa/helm-20211125.534/helm-eshell.el"
;;;;;;  "a70114a9a31e74934a4f55f7fcd4244a")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-eshell.el

(autoload 'helm-esh-pcomplete "helm-eshell" "\
Preconfigured `helm' to provide Helm completion in Eshell." t nil)

(autoload 'helm-eshell-history "helm-eshell" "\
Preconfigured Helm for Eshell history." t nil)

(autoload 'helm-eshell-prompts "helm-eshell" "\
Pre-configured `helm' to browse the prompts of the current Eshell." t nil)

(autoload 'helm-eshell-prompts-all "helm-eshell" "\
Pre-configured `helm' to browse the prompts of all Eshell sessions." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-eshell"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-eshell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-eshell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eshell" '("helm-e")))

;;;***

;;;***

;;;### (autoloads nil "helm-eval" "../../../.emacs.d/elpa/helm-20211125.534/helm-eval.el"
;;;;;;  "2a6e6a4bccd7cb3409f062b6367e3c5d")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-eval.el

(autoload 'helm-eval-expression "helm-eval" "\
Preconfigured `helm' for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm-eval" "\
Preconfigured `helm' for `helm-source-evaluation-result' with `eldoc' support." t nil)

(autoload 'helm-calcul-expression "helm-eval" "\
Preconfigured `helm' for `helm-source-calculation-result'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-eval" "../../../.emacs.d/elpa/helm-20211125.534/helm-eval.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-eval" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-external" "../../../.emacs.d/elpa/helm-20211125.534/helm-external.el"
;;;;;;  "087d7ba52c3359a25e0d45c7c05e621e")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-external.el

(autoload 'helm-run-external-command "helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running try to run `helm-raise-command' if
defined otherwise exit with error. You can set your own list of
commands with `helm-external-commands-list'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-external"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-external.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-external.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-external" '("helm-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "helm-fd" "../../../.emacs.d/elpa/helm-20211125.534/helm-fd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-fd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-fd" '("helm-fd-")))

;;;***

;;;### (autoloads nil "helm-files" "../../../.emacs.d/elpa/helm-20211125.534/helm-files.el"
;;;;;;  "85ccfddf17b10d970137b0e31353d23f")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-files.el

(autoload 'helm-projects-history "helm-files" "\


\(fn ARG)" t nil)

(autoload 'helm-browse-project "helm-files" "\
Preconfigured helm to browse projects.
Browse files and see status of project with its VCS.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files' if current
directory is not under control of one of those VCS.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled
directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>.

\(fn ARG)" t nil)

(autoload 'helm-find-files "helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on
files.

\(fn ARG)" t nil)

(autoload 'helm-delete-tramp-connection "helm-files" "\
Allow deleting tramp connection or marked tramp connections at once.

This replace `tramp-cleanup-connection' which is partially broken
in Emacs < to 25.1.50.1 (See Emacs bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=24432).

It allows additionally to delete more than one connection at
once." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-files" "../../../.emacs.d/elpa/helm-20211125.534/helm-files.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-files.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-files" '("eshell-command-aliases-list" "helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-find" "../../../.emacs.d/elpa/helm-20211125.534/helm-find.el"
;;;;;;  "f6e0764ab8fcdf5d4ae35fe25b341c03")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-find.el

(autoload 'helm-find "helm-find" "\
Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-find" "../../../.emacs.d/elpa/helm-20211125.534/helm-find.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-find.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-find" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-font" "../../../.emacs.d/elpa/helm-20211125.534/helm-font.el"
;;;;;;  "1e4226869baa8dbc79f5582418f50f90")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-font.el

(autoload 'helm-select-xfont "helm-font" "\
Preconfigured `helm' to select Xfont." t nil)

(autoload 'helm-ucs "helm-font" "\
Preconfigured `helm' for `ucs-names'.

Called with a prefix arg force reloading cache.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-font" "../../../.emacs.d/elpa/helm-20211125.534/helm-font.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-font.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-font" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-for-files" "../../../.emacs.d/elpa/helm-20211125.534/helm-for-files.el"
;;;;;;  "75e7a8b0d51d8e68407c45269d7834ec")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-for-files.el

(autoload 'helm-for-files "helm-for-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'." t nil)

(autoload 'helm-multi-files "helm-for-files" "\
Preconfigured helm like `helm-for-files' but running locate only on demand.

Allow toggling back and forth from locate to others sources with
`helm-multi-files-toggle-locate-binding' key.
This avoids launching locate needlessly when what you are
searching for is already found." t nil)

(autoload 'helm-recentf "helm-for-files" "\
Preconfigured `helm' for `recentf'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-for-files"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-for-files.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-for-files.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-for-files" '("helm-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "helm-global-bindings"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-global-bindings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-global-bindings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-global-bindings" '("helm-command-")))

;;;***

;;;### (autoloads nil "helm-grep" "../../../.emacs.d/elpa/helm-20211125.534/helm-grep.el"
;;;;;;  "416ae9a9dac09e06fdeec36e870621e1")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-grep.el

(autoload 'helm-goto-precedent-file "helm-grep" "\
Go to previous file in Helm grep/etags buffers." t nil)

(autoload 'helm-goto-next-file "helm-grep" "\
Go to previous file in Helm grep/etags buffers." t nil)

(autoload 'helm-do-grep-ag "helm-grep" "\
Preconfigured `helm' for grepping with AG in `default-directory'.
With prefix arg prompt for type if available with your AG
version.

\(fn ARG)" t nil)

(autoload 'helm-grep-do-git-grep "helm-grep" "\
Preconfigured `helm' for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-grep" "../../../.emacs.d/elpa/helm-20211125.534/helm-grep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-grep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-grep" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-help" "../../../.emacs.d/elpa/helm-20211125.534/helm-help.el"
;;;;;;  "6b022b4eec88509b038241333002f0aa")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-help.el

(autoload 'helm-documentation "helm-help" "\
Preconfigured `helm' for Helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all documented sources." t nil)

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend \\[helm-customize-group]:Conf")

;;;### (autoloads "actual autoloads are elsewhere" "helm-help" "../../../.emacs.d/elpa/helm-20211125.534/helm-help.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-help.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-help" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-id-utils" "../../../.emacs.d/elpa/helm-20211125.534/helm-id-utils.el"
;;;;;;  "3f171406578d2290e5ae2d1836e98805")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-id-utils.el

(autoload 'helm-gid "helm-id-utils" "\
Preconfigured `helm' for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above
`default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc..
See <https://www.gnu.org/software/idutils/>." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-id-utils"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-id-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-id-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-id-utils" '("helm-gid-")))

;;;***

;;;***

;;;### (autoloads nil "helm-imenu" "../../../.emacs.d/elpa/helm-20211125.534/helm-imenu.el"
;;;;;;  "59f01d6592f812dea1d462f7a7b2f5fa")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-imenu.el

(autoload 'helm-imenu "helm-imenu" "\
Preconfigured `helm' for `imenu'." t nil)

(autoload 'helm-imenu-in-all-buffers "helm-imenu" "\
Preconfigured `helm' for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived
i.e. `derived-mode-p' or it have an association in
`helm-imenu-all-buffer-assoc'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-imenu" "../../../.emacs.d/elpa/helm-20211125.534/helm-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-imenu" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-info" "../../../.emacs.d/elpa/helm-20211125.534/helm-info.el"
;;;;;;  "27df2681407bf1e396d4d49e76744a03")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-info.el

(autoload 'helm-info "helm-info" "\
Preconfigured `helm' for searching Info files' indices.

With a prefix argument \\[universal-argument], set REFRESH to
non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.  If
`helm-default-info-index-list' has not been customized, the new
Info files are made available.

\(fn &optional REFRESH)" t nil)

(autoload 'helm-info-at-point "helm-info" "\
Preconfigured `helm' for searching info at point." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-info" "../../../.emacs.d/elpa/helm-20211125.534/helm-info.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-info" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-locate" "../../../.emacs.d/elpa/helm-20211125.534/helm-locate.el"
;;;;;;  "0f12dd157845486cb6dd4a4f6b1de052")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-locate.el

(autoload 'helm-projects-find-files "helm-locate" "\
Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

\(fn UPDATE)" t nil)

(autoload 'helm-locate "helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-locate"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-locate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-locate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-locate" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-man" "../../../.emacs.d/elpa/helm-20211125.534/helm-man.el"
;;;;;;  "bd8a1d55e10e535e5c3e26b32d463018")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-man.el

(autoload 'helm-man-woman "helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-man" "../../../.emacs.d/elpa/helm-20211125.534/helm-man.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-man" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-misc" "../../../.emacs.d/elpa/helm-20211125.534/helm-misc.el"
;;;;;;  "a2bae8bcb5f003ca6591a221e76d079e")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-misc.el

(defvar helm-minibuffer-history-mode nil "\
Non-nil if Helm-Minibuffer-History mode is enabled.
See the `helm-minibuffer-history-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-minibuffer-history-mode'.")

(custom-autoload 'helm-minibuffer-history-mode "helm-misc" nil)

(autoload 'helm-minibuffer-history-mode "helm-misc" "\
Bind `helm-minibuffer-history-key' in al minibuffer maps.
This mode is enabled by `helm-mode', so there is no need to enable it directly.

If called interactively, enable Helm-Minibuffer-History mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'helm-world-time "helm-misc" "\
Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs." t nil)

(autoload 'helm-insert-latex-math "helm-misc" "\
Preconfigured helm for latex math symbols completion." t nil)

(autoload 'helm-ratpoison-commands "helm-misc" "\
Preconfigured `helm' to execute ratpoison commands." t nil)

(autoload 'helm-stumpwm-commands "helm-misc" "\
Preconfigured helm for stumpwm commands." t nil)

(autoload 'helm-minibuffer-history "helm-misc" "\
Preconfigured `helm' for `minibuffer-history'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-misc" "../../../.emacs.d/elpa/helm-20211125.534/helm-misc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-misc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-misc" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-mode" "../../../.emacs.d/elpa/helm-20211125.534/helm-mode.el"
;;;;;;  "0375331424c3b7bed04f4f541e7bcb98")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-mode.el

(autoload 'helm-comp-read "helm-mode" "\
Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, alist, vector, obarray or hash-table.
  For alists and hash-tables their car are use as real value of
  candidate unless ALISTP is non-nil.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A symbol where each result will be saved.
  If not specified as a symbol an error will popup.
  When specified, all elements of HISTORY are displayed in
  a special source before or after COLLECTION according to REVERSE-HISTORY.
  The main difference with INPUT-HISTORY is that the result of the
  completion is saved whereas in INPUT-HISTORY it is the minibuffer
  contents which is saved when you exit.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.
  NOTE: As mentionned above this has nothing to do with
  `minibuffer-history-variable', therefore if you want to save this
  history persistently, you will have to add this variable to the
  relevant variable of your favorite tool for persistent emacs session
  i.e. psession, desktop etc...

- RAW-HISTORY: When non-nil do not remove backslashs if some in
  HISTORY candidates.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.  You can navigate in this history with
  `M-p' and `M-n'.
  Don't use the same symbol for INPUT-HISTORY and HISTORY.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP:
  When non-nil (default) pass the value of (DISPLAY . REAL)
  candidate in COLLECTION to action when COLLECTION is an alist or a
  hash-table, otherwise DISPLAY is always returned as result on exit,
  which is the default when using `completing-read'.
  See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example.

\(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (BUFFER \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (REQUIRES-PATTERN 0) (HISTORY nil SHISTORY) RAW-HISTORY INPUT-HISTORY (CASE-FOLD helm-comp-read-case-fold-search) (PERSISTENT-ACTION nil) (PERSISTENT-HELP \"DoNothing\") (MODE-LINE helm-comp-read-mode-line) HELP-MESSAGE (KEYMAP helm-comp-read-map) (NAME \"Helm Completions\") HEADER-NAME CANDIDATES-IN-BUFFER MATCH-PART MATCH-DYNAMIC EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (VOLATILE t) SORT FC-TRANSFORMER HIST-FC-TRANSFORMER (MARKED-CANDIDATES helm-comp-read-use-marked) NOMARK (ALISTP t) (CANDIDATE-NUMBER-LIMIT helm-candidate-number-limit) MULTILINE ALLOW-NEST COERCE (GROUP \\='helm))" nil nil)

(autoload 'helm-read-file-name "helm-mode" "\
Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name, default to `default-directory' or $HOME.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

\(fn PROMPT &key (NAME \"Read File Name\") INITIAL-INPUT (BUFFER \"*Helm file completions*\") TEST NORET (CASE-FOLD helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH (FUZZY t) DEFAULT MARKED-CANDIDATES (CANDIDATE-NUMBER-LIMIT helm-ff-candidate-number-limit) NOMARK (ALISTP t) (PERSISTENT-ACTION-IF \\='helm-find-files-persistent-action-if) (PERSISTENT-HELP \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (MODE-LINE helm-read-file-name-mode-line-string))" nil nil)

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the `helm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm-mode" nil)

(autoload 'helm-mode "helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-mode" "../../../.emacs.d/elpa/helm-20211125.534/helm-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-mode" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-net" "../../../.emacs.d/elpa/helm-20211125.534/helm-net.el"
;;;;;;  "9d34ff549bdbe48e0498bee4110a1fb9")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-net.el

(autoload 'helm-browse-url-firefox "helm-net" "\
Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g., Windows).

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-browse-url-opera "helm-net" "\
Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g., Windows).

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-browse-url-chromium "helm-net" "\
Browse URL with Google Chrome browser.

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-browse-url-uzbl "helm-net" "\
Browse URL with uzbl browser.

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-browse-url-conkeror "helm-net" "\
Browse URL with conkeror browser.

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-browse-url-nyxt "helm-net" "\
Browse URL with nyxt browser.

\(fn URL &optional IGNORE)" t nil)

(autoload 'helm-surfraw "helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm-net" "\
Preconfigured `helm' for Google search with Google suggest." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-net" "../../../.emacs.d/elpa/helm-20211125.534/helm-net.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-net.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-net" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-occur" "../../../.emacs.d/elpa/helm-20211125.534/helm-occur.el"
;;;;;;  "c808ddea0a9144b3dcf7371a56989146")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-occur.el

(autoload 'helm-occur "helm-occur" "\
Preconfigured helm for searching lines matching pattern in `current-buffer'.

When `helm-source-occur' is member of
`helm-sources-using-default-as-input' which is the default,
symbol at point is searched at startup.

When a region is marked search only in this region by narrowing.

To search in multiples buffers start from one of the commands listing
buffers (i.e. a helm command using `helm-source-buffers-list' like
`helm-mini') and use the multi occur buffers action.

This is the helm implementation that collect lines matching pattern
like vanilla Emacs `occur' but have nothing to do with it, the search
engine beeing completely different and also much faster." t nil)

(autoload 'helm-occur-visible-buffers "helm-occur" "\
Run helm-occur on all visible buffers in frame." t nil)

(autoload 'helm-occur-from-isearch "helm-occur" "\
Invoke `helm-occur' from isearch.

To use this bind it to a key in `isearch-mode-map'." t nil)

(autoload 'helm-multi-occur-from-isearch "helm-occur" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

To use this bind it to a key in `isearch-mode-map'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-occur" "../../../.emacs.d/elpa/helm-20211125.534/helm-occur.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-occur.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-occur" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-regexp" "../../../.emacs.d/elpa/helm-20211125.534/helm-regexp.el"
;;;;;;  "1fb3a708704825c185205ee211e8f12e")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-regexp.el

(autoload 'helm-regexp "helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-regexp"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-regexp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-regexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-regexp" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-ring" "../../../.emacs.d/elpa/helm-20211125.534/helm-ring.el"
;;;;;;  "b6e1fdcb9890d56597812ffbc3f4f0ea")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-ring.el

(autoload 'helm-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'." t nil)

(autoload 'helm-global-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'." t nil)

(autoload 'helm-all-mark-rings "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'." t nil)

(autoload 'helm-register "helm-ring" "\
Preconfigured `helm' for Emacs registers." t nil)

(autoload 'helm-show-kill-ring "helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line." t nil)

(autoload 'helm-execute-kmacro "helm-ring" "\
Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-ring" "../../../.emacs.d/elpa/helm-20211125.534/helm-ring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-ring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-ring" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-semantic" "../../../.emacs.d/elpa/helm-20211125.534/helm-semantic.el"
;;;;;;  "32ca01b98b6e1e5dd271d73c69e7ae56")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-semantic.el

(autoload 'helm-semantic "helm-semantic" "\
Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current.

\(fn ARG)" t nil)

(autoload 'helm-semantic-or-imenu "helm-semantic" "\
Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-semantic"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-semantic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-semantic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-semantic" '("helm-s")))

;;;***

;;;***

;;;### (autoloads nil "helm-shell" "../../../.emacs.d/elpa/helm-20211125.534/helm-shell.el"
;;;;;;  "279dd4ed33083ac38457d1a65660a91e")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-shell.el

(defalias 'helm-shell-prompts 'helm-comint-prompts)

(defalias 'helm-shell-prompts-all 'helm-comint-prompts-all)

;;;***

;;;### (autoloads nil "helm-sys" "../../../.emacs.d/elpa/helm-20211125.534/helm-sys.el"
;;;;;;  "d19b983708251b0955926383892d60ee")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-sys.el

(defvar helm-top-poll-mode nil "\
Non-nil if Helm-Top-Poll mode is enabled.
See the `helm-top-poll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-top-poll-mode'.")

(custom-autoload 'helm-top-poll-mode "helm-sys" nil)

(autoload 'helm-top-poll-mode "helm-sys" "\
Refresh automatically helm top buffer once enabled.

If called interactively, enable Helm-Top-Poll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'helm-top "helm-sys" "\
Preconfigured `helm' for top command." t nil)

(autoload 'helm-list-emacs-process "helm-sys" "\
Preconfigured `helm' for Emacs process." t nil)

(autoload 'helm-xrandr-set "helm-sys" "\
Preconfigured helm for xrandr." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-sys" "../../../.emacs.d/elpa/helm-20211125.534/helm-sys.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-sys.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-sys" '("helm-")))

;;;***

;;;***

;;;### (autoloads nil "helm-tags" "../../../.emacs.d/elpa/helm-20211125.534/helm-tags.el"
;;;;;;  "2d391d9475abb962928062f62c21a043")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-tags.el

(autoload 'helm-etags-select "helm-tags" "\
Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

\(fn REINIT)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-tags" "../../../.emacs.d/elpa/helm-20211125.534/helm-tags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-tags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-tags" '("helm-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "helm-types" "../../../.emacs.d/elpa/helm-20211125.534/helm-types.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-types.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-types" '("helm-")))

;;;***

;;;### (autoloads nil "helm-utils" "../../../.emacs.d/elpa/helm-20211125.534/helm-utils.el"
;;;;;;  "1436bb6b3c61dd5694e92cf66fc9a3c3")
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-utils.el

(defvar helm-popup-tip-mode nil "\
Non-nil if Helm-Popup-Tip mode is enabled.
See the `helm-popup-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.")

(custom-autoload 'helm-popup-tip-mode "helm-utils" nil)

(autoload 'helm-popup-tip-mode "helm-utils" "\
Show help-echo informations in a popup tip at end of line.

If called interactively, enable Helm-Popup-Tip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "helm-utils" "../../../.emacs.d/elpa/helm-20211125.534/helm-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-utils" '("helm-" "with-helm-display-marked-candidates")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "helm-x-files"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-x-files.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/helm-20211125.534/helm-x-files.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-x-files" '("helm-")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/helm-20211125.534/helm-adaptive.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-bookmark.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-buffers.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-color.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-comint.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-command.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-config.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-dabbrev.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-easymenu.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp-package.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-elisp.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-epa.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-eshell.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-eval.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-external.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-fd.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-files.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-find.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-font.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-for-files.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-global-bindings.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-grep.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-help.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-id-utils.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-imenu.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-info.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-locate.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-man.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-misc.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-mode.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-net.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-occur.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-pkg.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-regexp.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-ring.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-semantic.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-shell.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-sys.el" "../../../.emacs.d/elpa/helm-20211125.534/helm-tags.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-types.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-utils.el"
;;;;;;  "../../../.emacs.d/elpa/helm-20211125.534/helm-x-files.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-autoloads.el ends here

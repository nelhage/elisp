;;; git-commit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit"
;;;;;;  "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit.el

(defvar global-git-commit-mode t "\
Non-nil if Global Git-Commit mode is enabled.
See the `global-git-commit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.")

(custom-autoload 'global-git-commit-mode "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit" nil)

(autoload 'global-git-commit-mode "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit" "\
Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

If called interactively, enable Global Git-Commit mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defconst git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")

(autoload 'git-commit-setup-check-buffer "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit" nil nil nil)

(autoload 'git-commit-setup "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit" nil nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit" '("git-commit-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/git-commit-20200828.1753/git-commit-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-commit-autoloads.el ends here

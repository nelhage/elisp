;;; git-link-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-link" "../../../.emacs.d/elpa/git-link-20230216.300/git-link.el"
;;;;;;  "d4ca87678388fd29cdf00a77dcef2c03")
;;; Generated autoloads from ../../../.emacs.d/elpa/git-link-20230216.300/git-link.el

(autoload 'git-link "git-link" "\
Create a URL representing the current buffer's location in its
GitHub/Bitbucket/GitLab/... repository at the current line number
or active region. The URL will be added to the kill ring.  If
`git-link-open-in-browser' is non-nil also call `browse-url'.

With a prefix argument of - generate a link without line number(s).
Also see `git-link-use-single-line-number'.

With a single prefix argument prompt for the remote's name.
Defaults to \"origin\".

With a double prefix argument invert the value of
`git-link-use-commit'.

\(fn REMOTE START END)" t nil)

(autoload 'git-link-commit "git-link" "\
Create a URL representing the commit for the hash under point
in the current buffer's GitHub/Bitbucket/GitLab/...
repository. The URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\".

\(fn REMOTE)" t nil)

(autoload 'git-link-homepage "git-link" "\
Create a URL representing the homepage of the current
buffer's GitHub/Bitbucket/GitLab/... repository. The
URL will be added to the kill ring.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\".

\(fn REMOTE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "git-link" "../../../.emacs.d/elpa/git-link-20230216.300/git-link.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/git-link-20230216.300/git-link.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-link" '("git-link-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/git-link-20230216.300/git-link-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/git-link-20230216.300/git-link.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-link-autoloads.el ends here

;;; ghub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "buck" "../../../.emacs.d/elpa/ghub-20181031.2158/buck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/buck.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buck" '("buck-default-host")))

;;;***

;;;### (autoloads nil "ghub" "../../../.emacs.d/elpa/ghub-20181031.2158/ghub.el"
;;;;;;  "8f655e95bdcc8cfba259217c21dd045d")
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/ghub.el

(autoload 'ghub-create-token "ghub" "\
Create, store and return a new token.

HOST is the Github instance, usually \"api.github.com\".
USERNAME is the name of a user on that instance.
PACKAGE is the package that will use the token.
SCOPES are the scopes the token is given access to.

\(fn HOST USERNAME PACKAGE SCOPES)" t nil)

(autoload 'ghub-token-scopes "ghub" "\
Return and echo the scopes of the specified token.
This is intended for debugging purposes only.  The user
has to provide several values including their password.

\(fn HOST USERNAME PACKAGE)" t nil)

(autoload 'ghub-clear-caches "ghub" "\
Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ghub" "../../../.emacs.d/elpa/ghub-20181031.2158/ghub.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/ghub.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghub" '("auth-source-netrc-parse-next-interesting@save-match-data" "ghub-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ghub-graphql"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/ghub-graphql.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/ghub-graphql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghub-graphql" '("ghub-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "glab" "../../../.emacs.d/elpa/ghub-20181031.2158/glab.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/glab.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "glab" '("glab-default-host")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "gogs" "../../../.emacs.d/elpa/ghub-20181031.2158/gogs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/gogs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gogs" '("gogs-default-host")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "gtea" "../../../.emacs.d/elpa/ghub-20181031.2158/gtea.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ghub-20181031.2158/gtea.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gtea" '("gtea-default-host")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/ghub-20181031.2158/buck.el"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/ghub-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/ghub-graphql.el"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/ghub-pkg.el" "../../../.emacs.d/elpa/ghub-20181031.2158/ghub.el"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/glab.el" "../../../.emacs.d/elpa/ghub-20181031.2158/gogs.el"
;;;;;;  "../../../.emacs.d/elpa/ghub-20181031.2158/gtea.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ghub-autoloads.el ends here

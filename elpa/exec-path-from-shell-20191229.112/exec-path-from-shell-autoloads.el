;;; exec-path-from-shell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "exec-path-from-shell" "../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell.el"
;;;;;;  "ab9c9b5a08ecdd86a02100c1843eb201")
;;; Generated autoloads from ../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell.el

(autoload 'exec-path-from-shell-copy-envs "exec-path-from-shell" "\
Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then `exec-path' and
`eshell-path-env' are also set appropriately.  The result is an alist,
as described by `exec-path-from-shell-getenvs'.

\(fn NAMES)" nil nil)

(autoload 'exec-path-from-shell-copy-env "exec-path-from-shell" "\
Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then `exec-path' and
`eshell-path-env' are also set appropriately.  Return the value
of the environment variable.

\(fn NAME)" t nil)

(autoload 'exec-path-from-shell-initialize "exec-path-from-shell" "\
Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-shell-variables' are set from the corresponding
values used in the user's shell.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "exec-path-from-shell"
;;;;;;  "../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exec-path-from-shell" '("exec-path-from-shell-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/exec-path-from-shell-20191229.112/exec-path-from-shell.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; exec-path-from-shell-autoloads.el ends here

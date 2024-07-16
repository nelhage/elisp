;;; inheritenv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv"
;;;;;;  "../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv.el

(autoload 'inheritenv-apply "../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv" "\
Apply FUNC such that the environment it sees will match the current value.
This is useful if FUNC creates a temp buffer, because that will
not inherit any buffer-local values of variables `exec-path' and
`process-environment'.

This function is designed for convenient use as an \"around\" advice.

ARGS is as for ORIG.

\(fn FUNC &rest ARGS)" nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv" '("inheritenv"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/inheritenv-20230804.651/inheritenv-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inheritenv-autoloads.el ends here

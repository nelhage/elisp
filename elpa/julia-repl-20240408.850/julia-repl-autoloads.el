;;; julia-repl-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from julia-repl.el

(autoload 'julia-repl "julia-repl" "\
Raise the Julia REPL inferior buffer, creating one if it does not exist.

This is the standard entry point for using this package." t)
(autoload 'julia-repl-mode "julia-repl" "\
Minor mode for interacting with a Julia REPL running inside a term.

\\{julia-repl-mode-map}

This is a minor mode.  If called interactively, toggle the `Julia
-Repl mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `julia-repl-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "julia-repl" '("julia-repl-"))

;;; End of scraped data

(provide 'julia-repl-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; julia-repl-autoloads.el ends here
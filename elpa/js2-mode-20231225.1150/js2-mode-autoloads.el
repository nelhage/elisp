;;; js2-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras"
;;;;;;  "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras" nil nil nil)

(autoload 'js2-imenu-extras-mode "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

This is a minor mode.  If called interactively, toggle the
`Js2-Imenu-Extras mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `js2-imenu-extras-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-imenu-extras" '("js2-imenu-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode"
;;;;;;  "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode" "\
Toggle highlight of unused variables.

This is a minor mode.  If called interactively, toggle the
`Js2-Highlight-Unused-Variables mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `js2-highlight-unused-variables-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

This is a minor mode.  If called interactively, toggle the `Js2
minor mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `js2-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

(autoload 'js2-jsx-mode "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode" "\
Major mode for editing JSX code in Emacs 26 and earlier.

To edit JSX code in Emacs 27, use `js-mode' as your major mode
with `js2-minor-mode' enabled.

To customize the indentation for this mode, set the SGML offset
variables (`sgml-basic-offset' et al) locally, like so:

  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset js2-basic-offset))
  (add-hook \\='js2-jsx-mode-hook #\\='set-jsx-indentation)

\(fn)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode" '("js2-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-old-indent"
;;;;;;  "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-old-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-old-indent.el

(register-definition-prefixes "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-old-indent" '("js2-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/js2-mode-20231225.1150/js2-mode-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js2-mode-autoloads.el ends here

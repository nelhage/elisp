;;; adaptive-wrap-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap"
;;;;;;  "../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap.el

(autoload 'adaptive-wrap-prefix-mode "../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap" "\
Wrap the buffer text with adaptive filling.

This is a minor mode.  If called interactively, toggle the
`Adaptive-Wrap-Prefix mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `adaptive-wrap-prefix-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap" '("adaptive-wrap-" "lookup-key"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/adaptive-wrap-0.8/adaptive-wrap-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; adaptive-wrap-autoloads.el ends here

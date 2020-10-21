;;; auto-complete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete"
;;;;;;  "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete.el

(autoload 'auto-complete "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete" "\
Start auto-completion at current point.

\(fn &optional SOURCES)" t nil)

(autoload 'auto-complete-mode "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete" "\
AutoComplete mode

If called interactively, enable Auto-Complete mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-auto-complete-mode 'globalized-minor-mode t)

(defvar global-auto-complete-mode nil "\
Non-nil if Global Auto-Complete mode is enabled.
See the `global-auto-complete-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-auto-complete-mode'.")

(custom-autoload 'global-auto-complete-mode "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete" nil)

(autoload 'global-auto-complete-mode "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete" "\
Toggle Auto-Complete mode in all buffers.
With prefix ARG, enable Global Auto-Complete mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Complete mode is enabled in all buffers where
`auto-complete-mode-maybe' would do it.
See `auto-complete-mode' for more information on Auto-Complete mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete" '("ac-" "auto-complete-mode"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-config"
;;;;;;  "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-config.el

(autoload 'ac-config-default "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-config" "\
No documentation." nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-config" '("ac-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/auto-complete-20201011.1341/auto-complete-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-complete-autoloads.el ends here

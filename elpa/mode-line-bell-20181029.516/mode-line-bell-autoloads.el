;;; mode-line-bell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell"
;;;;;;  "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell.el

(autoload 'mode-line-bell-flash "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell" "\
Flash the mode line momentarily." nil nil)

(defvar mode-line-bell-mode nil "\
Non-nil if Mode-Line-Bell mode is enabled.
See the `mode-line-bell-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mode-line-bell-mode'.")

(custom-autoload 'mode-line-bell-mode "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell" nil)

(autoload 'mode-line-bell-mode "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell" "\
Flash the mode line instead of ringing the bell.

If called interactively, enable Mode-Line-Bell mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell" '("mode-line-bell-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/mode-line-bell-20181029.516/mode-line-bell-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-line-bell-autoloads.el ends here

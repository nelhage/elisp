;;; guru-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "guru-mode" "guru-mode.el" (22857 27249 767516
;;;;;;  739000))
;;; Generated autoloads from guru-mode.el

(autoload 'guru-mode "guru-mode" "\
A minor mode that teaches you to use Emacs effectively.

\(fn &optional ARG)" t nil)

(defvar guru-global-mode nil "\
Non-nil if Guru-Global mode is enabled.
See the command `guru-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `guru-global-mode'.")

(custom-autoload 'guru-global-mode "guru-mode" nil)

(autoload 'guru-global-mode "guru-mode" "\
Toggle Guru mode in all buffers.
With prefix ARG, enable Guru-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Guru mode is enabled in all buffers where
`guru-mode' would do it.
See `guru-mode' for more information on Guru mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; guru-mode-autoloads.el ends here

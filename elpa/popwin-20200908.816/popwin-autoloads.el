;;; popwin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/popwin-20200908.816/popwin"
;;;;;;  "../../../.emacs.d/elpa/popwin-20200908.816/popwin.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/popwin-20200908.816/popwin.el

(autoload 'popwin:popup-buffer "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. If TAIL is
non-nil, the popup window will show the last contents. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER.

\(fn BUFFER &key (WIDTH popwin:popup-window-width) (HEIGHT popwin:popup-window-height) (POSITION popwin:popup-window-position) NOSELECT DEDICATED STICK TAIL)" t nil)

(autoload 'popwin:display-buffer "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Display BUFFER-OR-NAME, if possible, in a popup window, or as usual.
This function can be used as a value of
`display-buffer-function'.

\(fn BUFFER-OR-NAME &optional NOT-THIS-WINDOW)" t nil)

(autoload 'popwin:pop-to-buffer "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Same as `pop-to-buffer' except that this function will use `popwin:display-buffer-1' instead of `display-buffer'.  BUFFER,
OTHER-WINDOW amd NORECORD are the same arguments.

\(fn BUFFER &optional OTHER-WINDOW NORECORD)" t nil)

(autoload 'popwin:universal-display "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Call the following command interactively with letting `popwin:special-display-config' be `popwin:universal-display-config'.
This will be useful when displaying buffers in popup windows temporarily." t nil)

(autoload 'popwin:one-window "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Delete other window than the popup window. C-g restores the original window configuration." t nil)

(autoload 'popwin:popup-buffer-tail "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Same as `popwin:popup-buffer' except that the buffer will be `recenter'ed at the bottom.

\(fn &rest SAME-AS-POPWIN:POPUP-BUFFER)" t nil)

(autoload 'popwin:find-file "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Edit file FILENAME with popup window by `popwin:popup-buffer'.

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'popwin:find-file-tail "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Edit file FILENAME with popup window by `popwin:popup-buffer-tail'.

\(fn FILE &optional WILDCARD)" t nil)

(autoload 'popwin:messages "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Display *Messages* buffer in a popup window." t nil)

(defvar popwin-mode nil "\
Non-nil if Popwin mode is enabled.
See the `popwin-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `popwin-mode'.")

(custom-autoload 'popwin-mode "../../../.emacs.d/elpa/popwin-20200908.816/popwin" nil)

(autoload 'popwin-mode "../../../.emacs.d/elpa/popwin-20200908.816/popwin" "\
Minor mode for `popwin-mode'.

If called interactively, enable Popwin mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/popwin-20200908.816/popwin" '("popwin:"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/popwin-20200908.816/popwin-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; popwin-autoloads.el ends here

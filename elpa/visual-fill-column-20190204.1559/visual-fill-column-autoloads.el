;;; visual-fill-column-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "visual-fill-column" "../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column.el"
;;;;;;  "34b7f66993cc26ed718c725649215db5")
;;; Generated autoloads from ../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column.el

(autoload 'visual-fill-column-mode "visual-fill-column" "\
Wrap lines according to `fill-column' in `visual-line-mode'.

\(fn &optional ARG)" t nil)

(defvar global-visual-fill-column-mode nil "\
Non-nil if Global Visual-Fill-Column mode is enabled.
See the `global-visual-fill-column-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-visual-fill-column-mode'.")

(custom-autoload 'global-visual-fill-column-mode "visual-fill-column" nil)

(autoload 'global-visual-fill-column-mode "visual-fill-column" "\
Toggle Visual-Fill-Column mode in all buffers.
With prefix ARG, enable Global Visual-Fill-Column mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Visual-Fill-Column mode is enabled in all buffers where
`turn-on-visual-fill-column-mode' would do it.
See `visual-fill-column-mode' for more information on Visual-Fill-Column mode.

\(fn &optional ARG)" t nil)

(autoload 'visual-fill-column-split-window-sensibly "visual-fill-column" "\
Split WINDOW sensibly, unsetting its margins first.
This function unsets the window margins and calls
`split-window-sensibly'.

By default, `split-window-sensibly' does not split a window
vertically if it has wide margins, even if there is enough space
for a vertical split.  This function can be used as the value of
`split-window-preferred-function' to enable vertically splitting
windows with wide margins.

\(fn &optional WINDOW)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "visual-fill-column"
;;;;;;  "../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "visual-fill-column" '("visual-fill-column-" "turn-on-visual-fill-column-mode")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/visual-fill-column-20190204.1559/visual-fill-column.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; visual-fill-column-autoloads.el ends here

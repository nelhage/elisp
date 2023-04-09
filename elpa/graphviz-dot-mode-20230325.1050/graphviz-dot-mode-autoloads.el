;;; graphviz-dot-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "graphviz-dot-mode" "../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode.el"
;;;;;;  "528865965fd335ae1f16f36992e868c3")
;;; Generated autoloads from ../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode.el

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "\
Major mode for the dot language.

Functionallity specific to this mode:

  `indent-for-tab-command'    \\[indent-for-tab-command]
        Indents a single line.
  `graphviz-dot-preview'      \\[graphviz-dot-preview]
        Previews graph in a buffer.
  `graphviz-dot-view'         \\[graphviz-dot-view]
        Views graph in an external viewer.
  `graphviz-dot-indent-line'  \\[graphviz-dot-indent-line]
        Indents current line of code.

Variables specific to this mode:

  `graphviz-dot-dot-program'                   (default `dot')
       Program used to compile the graphs.
  `graphviz-dot-preview-extension'             (default `png')
       File type to use for output.
  `graphviz-dot-view-command'                  (default `dotty %s')
       Command to run when `graphviz-dot-view' is executed.
  `graphviz-dot-view-edit-command'             (default nil)
       If the user should be asked to edit the view command.
  `graphviz-dot-save-before-view'              (default t)
       Automatically save current buffer berore `graphviz-dot-view'.

\(fn)" t nil)

(autoload 'graphviz-dot-preview "graphviz-dot-mode" "\
Compile the graph between BEGIN and END and preview it in an other buffer.
BEGIN (resp. END) is a number defaulting to `point-min' (resp. `point-max')
representing the current buffer's point where the graph definition starts
\(resp. stops).

\(fn &optional BEGIN END)" t nil)

(autoload 'graphviz-turn-on-live-preview "graphviz-dot-mode" "\
Turn on live preview.
This will update the preview on every save." t nil)

(autoload 'graphviz-turn-off-live-preview "graphviz-dot-mode" "\
Turn off live preview.
Saving the file will no longer also update the preview." t nil)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

;;;### (autoloads "actual autoloads are elsewhere" "graphviz-dot-mode"
;;;;;;  "../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "graphviz-dot-mode" '("dot-menu" "graphviz-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/graphviz-dot-mode-20230325.1050/graphviz-dot-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; graphviz-dot-mode-autoloads.el ends here

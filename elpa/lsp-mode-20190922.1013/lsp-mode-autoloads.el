;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-mode" "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-mode.el"
;;;;;;  "2046084bf4792e3a48c15ec57e955b18")
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-mode.el

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be openned in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

(autoload 'lsp-deferred "lsp-mode" "\
Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-clients.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-clojure.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-css.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-dart.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-elm.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-erlang.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-fsharp.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-go.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-haxe.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-html.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-intelephense.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-metals.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-mode.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-pyls.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-rust.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-solargraph.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-vetur.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp-xml.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190922.1013/lsp.el") (23944
;;;;;;  21076 160680 291000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-mode-autoloads.el ends here

;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "lsp-clients"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clients.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clients.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clients" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-clojure"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clojure" '("lsp-cl")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-css" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-css.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-css" '("lsp-c")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-dart" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-dart.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-dart.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dart" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-elm" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-elm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-elm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elm" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-erlang" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-erlang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-erlang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-fsharp" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-fsharp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-fsharp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-go" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-go.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-go.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-go" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-html" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-html.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-html" '("lsp-html-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-intelephense"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-intelephense.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-intelephense.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-intelephense" '("lsp-intelephense-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-metals" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-metals.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-metals.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-metals" '("lsp-metals-")))

;;;***

;;;### (autoloads nil "lsp-mode" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode.el"
;;;;;;  "750bcb1ac53cdd71ddf3af8d383b1e76")
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode.el

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

;;;### (autoloads "actual autoloads are elsewhere" "lsp-mode" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("log--notification-performance" "lsp-" "make-lsp-client" "with-lsp-workspace" "when-lsp-workspace" "seq-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-pyls" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-pyls.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-pyls.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pyls" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-rust" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-rust.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-rust.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rust" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-solargraph"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-solargraph.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-solargraph.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-vetur" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-vetur.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-vetur.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vetur" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-xml" "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-xml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-xml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-xml" '("lsp-xml-")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clients.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-clojure.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-css.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-dart.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-elm.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-erlang.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-fsharp.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-go.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-html.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-intelephense.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-metals.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-mode.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-pyls.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-rust.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-solargraph.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-vetur.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp-xml.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20190723.2001/lsp.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here

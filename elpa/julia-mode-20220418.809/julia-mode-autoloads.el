;;; julia-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "julia-mode" "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode.el"
;;;;;;  "806105e4f5072bdd6fa20c8bb70916f5")
;;; Generated autoloads from ../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode.el

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(autoload 'julia-mode "julia-mode" "\
Major mode for editing julia code.

\(fn)" t nil)

(autoload 'inferior-julia "julia-mode" "\
Run an inferior instance of julia inside Emacs." t nil)

(defalias 'run-julia #'inferior-julia "\
Run an inferior instance of julia inside Emacs.")

;;;### (autoloads "actual autoloads are elsewhere" "julia-mode" "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-mode" '("inferior-julia-" "julia-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "julia-mode-latexsubs"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode-latexsubs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode-latexsubs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "julia-mode-latexsubs" '("julia-mode-latexsubs")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode-latexsubs.el"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-20220418.809/julia-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-mode-autoloads.el ends here

;;; julia-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/julia-mode-0.4/julia-mode.el

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(autoload 'julia-mode "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode" "\
Major mode for editing julia code.

\(fn)" t nil)

(autoload 'inferior-julia "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode" "\
Run an inferior instance of `julia' inside Emacs." t nil)

(defalias 'run-julia #'inferior-julia "\
Run an inferior instance of `julia' inside Emacs.")

(register-definition-prefixes "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode" '("inferior-julia-" "julia-" "latexsub"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/julia-mode-0.4/julia-latexsubs.el"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/julia-mode-0.4/julia-mode-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; julia-mode-autoloads.el ends here

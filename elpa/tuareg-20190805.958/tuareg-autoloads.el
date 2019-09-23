;;; tuareg-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ocamldebug" "../../../.emacs.d/elpa/tuareg-20190805.958/ocamldebug.el"
;;;;;;  "7a031fe80e3d14ee6139cc0bb4d7cb37")
;;; Generated autoloads from ../../../.emacs.d/elpa/tuareg-20190805.958/ocamldebug.el

(autoload 'ocamldebug "ocamldebug" "\
Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for ocamldebug.  If you wish to change this, use
the ocamldebug commands `cd DIR' and `directory'.

\(fn PGM-PATH)" t nil)

(defalias 'camldebug 'ocamldebug)

;;;***

;;;### (autoloads nil "tuareg" "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg.el"
;;;;;;  "fa896117bfeea3e0ce48bd2ee54a9660")
;;; Generated autoloads from ../../../.emacs.d/elpa/tuareg-20190805.958/tuareg.el
(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.eliomi?\\'" . tuareg-mode))
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
               ".annot" ".cmt" ".cmti"))
 (add-to-list 'completion-ignored-extensions ext))

(autoload 'tuareg-mode "tuareg" "\
Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a REPL (aka toplevel) is
included.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the REPL:
\\{tuareg-interactive-mode-map}

\(fn)" t nil)

(autoload 'tuareg-run-ocaml "tuareg" "\
Run an OCaml REPL process.  I/O via buffer `*OCaml*'.

\(fn)" t nil)

(defalias 'run-ocaml 'tuareg-run-ocaml)

(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))

(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

;;;***

;;;### (autoloads nil "tuareg-jbuild" "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-jbuild.el"
;;;;;;  "1e889f998c8b6ca5c00442189f9d7a23")
;;; Generated autoloads from ../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-jbuild.el

(autoload 'tuareg-jbuild-mode "tuareg-jbuild" "\
Major mode to edit jbuild files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)jbuild\\(?:\\.inc\\)?\\'" . tuareg-jbuild-mode))

;;;***

;;;### (autoloads nil "tuareg-menhir" "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-menhir.el"
;;;;;;  "f1865266e69c6df20cf39723155a4e55")
;;; Generated autoloads from ../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-menhir.el

(add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-menhir-mode))

(autoload 'tuareg-menhir-mode "tuareg-menhir" "\
Major mode to edit Menhir (and Ocamlyacc) files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "tuareg-opam" "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-opam.el"
;;;;;;  "cc9fc1d6b0bd2b8ec4a951a157866014")
;;; Generated autoloads from ../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-opam.el

(autoload 'tuareg-opam-mode "tuareg-opam" "\
Major mode to edit opam files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("[./]opam_?\\'" . tuareg-opam-mode))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/tuareg-20190805.958/dot-emacs.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/ocamldebug.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-jbuild.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-menhir.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-opam.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg-site-file.el"
;;;;;;  "../../../.emacs.d/elpa/tuareg-20190805.958/tuareg.el") (23944
;;;;;;  21056 387680 291000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tuareg-autoloads.el ends here

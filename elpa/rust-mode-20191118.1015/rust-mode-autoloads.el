;;; rust-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rust-mode" "../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode.el"
;;;;;;  "6d4d63078f2b2b2f19c983d8472a70db")
;;; Generated autoloads from ../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(autoload 'rust-dbg-wrap-or-unwrap "rust-mode" "\
Either remove or add the dbg! macro.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "rust-mode" "../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-mode" '("rust" "cargo-compilation-regexps")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/rust-mode-20191118.1015/rust-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rust-mode-autoloads.el ends here

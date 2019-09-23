;;; typescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "typescript-mode" "../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode.el"
;;;;;;  "fe822ac81a6ab56981a90c9f6bc1be96")
;;; Generated autoloads from ../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode.el
(put 'typescript-indent-level 'safe-local-variable #'integerp)

(autoload 'typescript-mode "typescript-mode" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode-test-utilities.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190710.2011/typescript-mode.el")
;;;;;;  (23937 29247 394431 422000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; typescript-mode-autoloads.el ends here

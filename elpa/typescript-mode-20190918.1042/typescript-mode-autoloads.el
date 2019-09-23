;;; typescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "typescript-mode" "../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode.el"
;;;;;;  "86da2d93feb25989a2d51598ac684025")
;;; Generated autoloads from ../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode.el
(put 'typescript-indent-level 'safe-local-variable #'integerp)

(autoload 'typescript-mode "typescript-mode" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode-test-utilities.el"
;;;;;;  "../../../.emacs.d/elpa/typescript-mode-20190918.1042/typescript-mode.el")
;;;;;;  (23944 21055 97680 291000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; typescript-mode-autoloads.el ends here

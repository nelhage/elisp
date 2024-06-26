;;; coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "coffee-mode" "../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode.el"
;;;;;;  "2093cbeaab23e324ccdfb568efe92222")
;;; Generated autoloads from ../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.cson\\'" . coffee-mode))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;;### (autoloads "actual autoloads are elsewhere" "coffee-mode"
;;;;;;  "../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "coffee-mode" '("coffee-" "iced-coffee-cs-keywords" "js2coffee-command")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/coffee-mode-20200315.1133/coffee-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; coffee-mode-autoloads.el ends here

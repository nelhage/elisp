;;; yaml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yaml-mode" "../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode.el"
;;;;;;  "634f17bfa016743c22ea7c3188c21d89")
;;; Generated autoloads from ../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode.el

(let ((loads (get 'yaml 'custom-loads))) (if (member '"yaml-mode" loads) nil (put 'yaml 'custom-loads (cons '"yaml-mode" loads))))

(autoload 'yaml-mode "yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))

(add-to-list 'magic-mode-alist '("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode))

;;;### (autoloads "actual autoloads are elsewhere" "yaml-mode" "../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-mode" '("yaml-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/yaml-mode-20210808.1122/yaml-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yaml-mode-autoloads.el ends here

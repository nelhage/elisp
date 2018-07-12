;;; scala-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "ob-scala" "../../../.emacs.d/elpa/scala-mode-20170802.432/ob-scala.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/ob-scala.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-scala" '("org-babel-")))

;;;***

;;;### (autoloads nil "scala-mode" "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode.el"
;;;;;;  "8685b8fadf5f3bc6c94f078230dda468")
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode.el

(autoload 'scala-mode:set-scala-syntax-mode "scala-mode" "\
Sets the syntax-table and other related variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table.

\(fn)" nil nil)

(autoload 'scala-mode:goto-start-of-code "scala-mode" "\
Go to the start of the real code in the file: object, class or trait.

\(fn)" t nil)

(autoload 'scala-mode "scala-mode" "\
Major mode for editing scala code.

When started, runs `scala-mode-hook'.

\\{scala-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode" "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode" '("scala-mode:")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-fontlock"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-fontlock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-fontlock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-fontlock" '("scala-font-lock:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-imenu"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-imenu" '("scala-imenu:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-indent"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-indent" '("scala-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-lib"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-lib.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-lib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-lib" '("scala-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-map"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-map.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-map.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-map" '("scala-mode-map")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-paragraph"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-paragraph.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-paragraph.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-paragraph" '("scala-paragraph:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "scala-mode-syntax"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-syntax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scala-mode-syntax" '("scala-syntax:")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/scala-mode-20170802.432/ob-scala.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-fontlock.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-imenu.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-indent.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-lib.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-map.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-paragraph.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-prettify-symbols.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode-syntax.el"
;;;;;;  "../../../.emacs.d/elpa/scala-mode-20170802.432/scala-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scala-mode-autoloads.el ends here

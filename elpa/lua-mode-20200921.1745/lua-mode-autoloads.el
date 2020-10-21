;;; lua-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/lua-mode-20200921.1745/init-tryout"
;;;;;;  "../../../.emacs.d/elpa/lua-mode-20200921.1745/init-tryout.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lua-mode-20200921.1745/init-tryout.el

(register-definition-prefixes "../../../.emacs.d/elpa/lua-mode-20200921.1745/init-tryout" '("add-trace-for"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode"
;;;;;;  "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode.el

(autoload 'lua-mode "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode" "\
Major mode for editing Lua code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defalias 'run-lua #'lua-start-process)

(autoload 'lua-start-process "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode" "\
Start a Lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer.

\(fn &optional NAME PROGRAM STARTFILE &rest SWITCHES)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode" '("lua-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lua-mode-20200921.1745/lua-mode-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lua-mode-autoloads.el ends here

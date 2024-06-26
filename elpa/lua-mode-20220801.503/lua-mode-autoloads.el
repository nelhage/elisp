;;; lua-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lua-mode" "../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode.el"
;;;;;;  "a5b19a9e96bbd1e37ea7c5e0442a55eb")
;;; Generated autoloads from ../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode.el

(autoload 'lua-mode "lua-mode" "\
Major mode for editing Lua code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defalias 'run-lua #'lua-start-process)

(autoload 'lua-start-process "lua-mode" "\
Start a Lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer.

\(fn &optional NAME PROGRAM STARTFILE &rest SWITCHES)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "lua-mode" "../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lua-mode" '("lua-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lua-mode-20220801.503/lua-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lua-mode-autoloads.el ends here

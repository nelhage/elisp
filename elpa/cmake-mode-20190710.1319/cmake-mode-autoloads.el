;;; cmake-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cmake-mode" "../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode.el"
;;;;;;  "59e6d967cc6de5430b3d2997caa4429d")
;;; Generated autoloads from ../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode.el

(autoload 'cmake-mode "cmake-mode" "\
Major mode for editing CMake source files.

\(fn)" t nil)

(autoload 'cmake-command-run "cmake-mode" "\
Runs the command cmake with the arguments specified.  The
optional argument topic will be appended to the argument list.

\(fn TYPE &optional TOPIC BUFFER)" t nil)

(autoload 'cmake-help-list-commands "cmake-mode" "\
Prints out a list of the cmake commands.

\(fn)" t nil)

(autoload 'cmake-help-command "cmake-mode" "\
Prints out the help message for the command the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-module "cmake-mode" "\
Prints out the help message for the module the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-variable "cmake-mode" "\
Prints out the help message for the variable the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help-property "cmake-mode" "\
Prints out the help message for the property the cursor is on.

\(fn)" t nil)

(autoload 'cmake-help "cmake-mode" "\
Queries for any of the four available help topics and prints out the appropriate page.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;;### (autoloads "actual autoloads are elsewhere" "cmake-mode" "../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cmake-mode" '("cmake-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/cmake-mode-20190710.1319/cmake-mode.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cmake-mode-autoloads.el ends here

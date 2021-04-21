;;; python-black-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-black" "../../../.emacs.d/elpa/python-black-20200324.930/python-black.el"
;;;;;;  "ada722d5d8b36a492db779967f7a688e")
;;; Generated autoloads from ../../../.emacs.d/elpa/python-black-20200324.930/python-black.el
 (autoload 'python-black-buffer "python-black" nil t)
 (autoload 'python-black-region "python-black" nil t)
 (autoload 'python-black-on-save-mode "python-black" nil t)

(autoload 'python-black-statement "python-black" "\
Reformats the current statement.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails.

\(fn &optional DISPLAY-ERRORS)" t nil)

(autoload 'python-black-partial-dwim "python-black" "\
Reformats the active region or the current statement.

This runs ‘python-black-region’ or ‘python-black-statement’ depending
on whether the region is currently active.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails.

\(fn &optional DISPLAY-ERRORS)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "python-black"
;;;;;;  "../../../.emacs.d/elpa/python-black-20200324.930/python-black.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/python-black-20200324.930/python-black.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-black" '("python-black-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/python-black-20200324.930/python-black-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/python-black-20200324.930/python-black.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-black-autoloads.el ends here

;;; rubocop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rubocop" "../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop.el"
;;;;;;  "35217654136445c0902443321c08151b")
;;; Generated autoloads from ../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop.el

(autoload 'rubocop-check-project "rubocop" "\
Run check on current project." t nil)

(autoload 'rubocop-autocorrect-project "rubocop" "\
Run autocorrect on current project." t nil)

(autoload 'rubocop-format-project "rubocop" "\
Run format on current project." t nil)

(autoload 'rubocop-check-directory "rubocop" "\
Run check on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-autocorrect-directory "rubocop" "\
Run autocorrect on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-check-current-file "rubocop" "\
Run check on current file." t nil)

(autoload 'rubocop-autocorrect-current-file "rubocop" "\
Run autocorrect on current file." t nil)

(autoload 'rubocop-format-current-file "rubocop" "\
Run format on current file." t nil)

(autoload 'rubocop-mode "rubocop" "\
Minor mode to interface with RuboCop.

If called interactively, enable Rubocop mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "rubocop" "../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rubocop" '("rubocop-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/rubocop-20210309.1241/rubocop.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rubocop-autoloads.el ends here

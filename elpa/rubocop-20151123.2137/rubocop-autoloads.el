;;; rubocop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "rubocop" "rubocop.el" (22191 44338 0 0))
;;; Generated autoloads from rubocop.el

(autoload 'rubocop-check-project "rubocop" "\
Run on current project.

\(fn)" t nil)

(autoload 'rubocop-autocorrect-project "rubocop" "\
Run on current project.

\(fn)" t nil)

(autoload 'rubocop-check-directory "rubocop" "\
Run on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-autocorrect-directory "rubocop" "\
Run on DIRECTORY if present.
Alternatively prompt user for directory.

\(fn &optional DIRECTORY)" t nil)

(autoload 'rubocop-check-current-file "rubocop" "\
Run on current file.

\(fn)" t nil)

(autoload 'rubocop-autocorrect-current-file "rubocop" "\
Run on current file.

\(fn)" t nil)

(autoload 'rubocop-mode "rubocop" "\
Minor mode to interface with RuboCop.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rubocop-autoloads.el ends here

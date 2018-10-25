;;; ruby-electric-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ruby-electric" "../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric.el"
;;;;;;  "486d937aabb2075eea074ca69bf77b5f")
;;; Generated autoloads from ../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric.el

(autoload 'ruby-electric-mode "ruby-electric" "\
Toggle Ruby Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When Ruby Electric mode is enabled, an indented 'end' is
heuristicaly inserted whenever typing a word like 'module',
'class', 'def', 'if', 'unless', 'case', 'until', 'for', 'begin',
'do' followed by a space.  Single, double and back quotes as well
as braces are paired auto-magically.  Expansion does not occur
inside comments and strings. Note that you must have Font Lock
enabled.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ruby-electric"
;;;;;;  "../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ruby-electric" '("ruby-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/ruby-electric-20170810.1130/ruby-electric.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ruby-electric-autoloads.el ends here

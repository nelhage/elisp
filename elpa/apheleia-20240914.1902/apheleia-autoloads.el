;;; apheleia-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from apheleia.el

(autoload 'apheleia-format-buffer "apheleia" "\
Run code formatter asynchronously on current buffer, preserving point.

FORMATTER is a symbol appearing as a key in
`apheleia-formatters', or a list of them to run multiple
formatters in a chain. If called interactively, run the currently
configured formatters (see `apheleia-formatter' and
`apheleia-mode-alist'), or prompt from `apheleia-formatters' if
there is none configured for the current buffer. With a prefix
argument, prompt always.

After the formatters finish running, the diff utility is invoked to
determine what changes it made. That diff is then used to apply the
formatter's changes to the current buffer without moving point or
changing the scroll position in any window displaying the buffer. If
the buffer has been modified since the formatter started running,
however, the operation is aborted.

If the formatter actually finishes running and the buffer is
successfully updated (even if the formatter has not made any
changes), SUCCESS-CALLBACK, if provided, is invoked with no
arguments.

If provided, CALLBACK is invoked unconditionally (unless there is
a synchronous nonlocal exit) with a plist. Callback function must
accept unknown keywords. At present only `:error' is included,
this is either an error or nil.

(fn FORMATTER &optional SUCCESS-CALLBACK &key CALLBACK)" t)
(autoload 'apheleia-format-after-save "apheleia" "\
Run code formatter for current buffer if any configured, then save.")
(define-minor-mode apheleia-mode "Minor mode for reformatting code on save without moving point.
It is customized by means of the variables `apheleia-mode-alist'
and `apheleia-formatters'." :lighter apheleia-mode-lighter (if apheleia-mode (add-hook 'after-save-hook #'apheleia-format-after-save nil 'local) (remove-hook 'after-save-hook #'apheleia-format-after-save 'local)))
(defvar-local apheleia-inhibit nil "\
Do not enable `apheleia-mode' automatically if non-nil.
This is designed for use in .dir-locals.el.

See also `apheleia-inhibit-functions'.")
(put 'apheleia-inhibit 'safe-local-variable #'booleanp)
(defun apheleia-mode-maybe nil "\
Enable `apheleia-mode' if allowed by user configuration.
This checks `apheleia-inhibit-functions' and `apheleia-inhibit'
to see if it is allowed." (unless (or apheleia-inhibit (run-hook-with-args-until-success 'apheleia-inhibit-functions)) (apheleia-mode)))
(define-globalized-minor-mode apheleia-global-mode apheleia-mode apheleia-mode-maybe :group 'apheleia)
(put 'apheleia-mode 'safe-local-variable #'booleanp)
(register-definition-prefixes "apheleia" '("apheleia-"))


;;; Generated autoloads from apheleia-dp.el

(register-definition-prefixes "apheleia-dp" '("apheleia--align-point"))


;;; Generated autoloads from apheleia-formatter-context.el

(register-definition-prefixes "apheleia-formatter-context" '("apheleia-formatter--context"))


;;; Generated autoloads from apheleia-formatters.el

(defvar apheleia-mode-predicates '(apheleia-mhtml-mode-predicate) "\
List of predicates that check for sneaky major modes.
Sometimes a major mode will set `major-mode' to something other
than itself, making it hard to correctly detect what major mode
is active. In such cases you can add a predicate to this list to
handle it. Predicates take no arguments, are run in the current
buffer, and should return the name of a mode if one is detected.
If all the predicates return nil, or if there aren't any in the
list, then only the value of `major-mode' is used to determine
the major mode. The detected major mode affects the selection
from `apheleia-mode-alist'.")
(custom-autoload 'apheleia-mode-predicates "apheleia-formatters" t)
(defvar-local apheleia-formatter nil "\
Name of formatter to use in current buffer, a symbol or nil.
If non-nil, then `apheleia-formatters' should have a matching
entry. This overrides `apheleia-mode-alist'.

The value can also be a list of symbols to apply multiple
formatters in sequence.")
(register-definition-prefixes "apheleia-formatters" '("apheleia-"))


;;; Generated autoloads from apheleia-log.el

(autoload 'apheleia-goto-error "apheleia-log" "\
Go to the most recently reported formatter error message." t)
(register-definition-prefixes "apheleia-log" '("apheleia-"))


;;; Generated autoloads from apheleia-rcs.el

(register-definition-prefixes "apheleia-rcs" '("apheleia-"))


;;; Generated autoloads from apheleia-utils.el

(register-definition-prefixes "apheleia-utils" '("apheleia-formatters-"))

;;; End of scraped data

(provide 'apheleia-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; apheleia-autoloads.el ends here
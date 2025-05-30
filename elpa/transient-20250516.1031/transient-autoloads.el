;;; transient-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from transient.el

(autoload 'transient-insert-suffix "transient" "\
Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.  When the conflict appears to be a false-positive,
  non-nil KEEP-OTHER may be ignored, which can be prevented
  by using `always'.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)")
(function-put 'transient-insert-suffix 'lisp-indent-function 'defun)
(autoload 'transient-append-suffix "transient" "\
Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.  When the conflict appears to be a false-positive,
  non-nil KEEP-OTHER may be ignored, which can be prevented
  by using `always'.
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX &optional KEEP-OTHER)")
(function-put 'transient-append-suffix 'lisp-indent-function 'defun)
(autoload 'transient-replace-suffix "transient" "\
Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)")
(function-put 'transient-replace-suffix 'lisp-indent-function 'defun)
(autoload 'transient-inline-group "transient" "\
Inline the included GROUP into PREFIX.
Replace the symbol GROUP with its expanded layout in the
layout of PREFIX.

(fn PREFIX GROUP)")
(function-put 'transient-inline-group 'lisp-indent-function 'defun)
(autoload 'transient-remove-suffix "transient" "\
Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)")
(function-put 'transient-remove-suffix 'lisp-indent-function 'defun)
(register-definition-prefixes "transient" '("find-function-advised-original" "transient"))

;;; End of scraped data

(provide 'transient-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; transient-autoloads.el ends here

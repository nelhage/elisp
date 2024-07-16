;;; company-coq-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq"
;;;;;;  "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-coq-20221130.536/company-coq.el

(autoload 'company-coq-tutorial "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq" "\
Open the company-coq tutorial, creating a new buffer if needed." t nil)

(autoload 'company-coq-describe-feature "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq" "\
Describe company-coq feature FEATURE.

\(fn FEATURE)" t nil)

(autoload 'company-coq-mode "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq" "\
Toggle company-coq-mode on or off.

Company-Coq is a collection of Proof-General extensions.  See
https://github.com/cpitclaudel/company-coq/ for a detailed
description, including screenshots and documentation.  First time
users may want to use \\[company-coq-tutorial] to open the
tutorial.

With a prefix argument ARG, enable %s if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

\\{company-coq-map}

\(fn &optional ARG)" t nil)

(autoload 'company-coq-initialize "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq" "\
Deprecated: Use `company-coq-mode' instead." t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq" '("company-coq-" "toggle-company-coq-debug"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-abbrev"
;;;;;;  "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-abbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-abbrev.el

(register-definition-prefixes "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-abbrev" '("company-coq--refman-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-tg"
;;;;;;  "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-tg.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-tg.el

(register-definition-prefixes "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-tg" '("company-coq-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-utils"
;;;;;;  "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-utils.el

(autoload 'company-coq-cite "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-utils" "\
Insert BibTeX entries for Coq, PG, and company-coq." t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-utils" '("company-coq--"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/company-coq-20221130.536/company-coq-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-coq-autoloads.el ends here

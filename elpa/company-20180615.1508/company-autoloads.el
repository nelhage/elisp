;;; company-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company" "../../../.emacs.d/elpa/company-20180615.1508/company.el"
;;;;;;  "848bb681764b7ebc4f45659bda1bc384")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company.el

(autoload 'company-mode "company" "\
\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

\(fn &optional ARG)" t nil)

(defvar global-company-mode nil "\
Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.")

(custom-autoload 'global-company-mode "company" nil)

(autoload 'global-company-mode "company" "\
Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

\(fn &optional ARG)" t nil)

(autoload 'company-manual-begin "company" "\


\(fn)" t nil)

(autoload 'company-complete "company" "\
Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company" "../../../.emacs.d/elpa/company-20180615.1508/company.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company" '("company-")))

;;;***

;;;***

;;;### (autoloads nil "company-abbrev" "../../../.emacs.d/elpa/company-20180615.1508/company-abbrev.el"
;;;;;;  "5c53bd58b979c8d570d3e6f0f4e35579")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-abbrev.el

(autoload 'company-abbrev "company-abbrev" "\
`company-mode' completion backend for abbrev.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-abbrev"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-abbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-abbrev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert")))

;;;***

;;;***

;;;### (autoloads nil "company-bbdb" "../../../.emacs.d/elpa/company-20180615.1508/company-bbdb.el"
;;;;;;  "fec5aa48cc472cfddf2eeb9c66ecbd7a")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-bbdb.el

(autoload 'company-bbdb "company-bbdb" "\
`company-mode' completion backend for BBDB.

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-bbdb"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-bbdb.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-bbdb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-bbdb" '("company-bbdb-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "company-capf"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-capf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-capf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-capf" '("company-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "company-clang"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-clang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-clang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-clang" '("company-clang")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "company-cmake"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-cmake.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-cmake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-cmake" '("company-cmake")))

;;;***

;;;### (autoloads nil "company-css" "../../../.emacs.d/elpa/company-20180615.1508/company-css.el"
;;;;;;  "08908e649d5189c7a96e7e95c2bfdbff")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-css.el

(autoload 'company-css "company-css" "\
`company-mode' completion backend for `css-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-css"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-css.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-css" '("company-css-")))

;;;***

;;;***

;;;### (autoloads nil "company-dabbrev" "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev.el"
;;;;;;  "789a195f95ddb9581c6b48f67a1e2d6c")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev.el

(autoload 'company-dabbrev "company-dabbrev" "\
dabbrev-like `company-mode' completion backend.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-dabbrev"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-")))

;;;***

;;;***

;;;### (autoloads nil "company-dabbrev-code" "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev-code.el"
;;;;;;  "48580f10fbb5c8b3aadae36f968ae8f5")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev-code.el

(autoload 'company-dabbrev-code "company-dabbrev-code" "\
dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-dabbrev-code"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev-code.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev-code.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "company-eclim"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-eclim.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-eclim.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-eclim" '("company-eclim")))

;;;***

;;;### (autoloads nil "company-elisp" "../../../.emacs.d/elpa/company-20180615.1508/company-elisp.el"
;;;;;;  "37dc6d9a70d7c716c823e52868eb1293")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-elisp.el

(autoload 'company-elisp "company-elisp" "\
`company-mode' completion backend for Emacs Lisp.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-elisp"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-elisp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-elisp" '("company-elisp-")))

;;;***

;;;***

;;;### (autoloads nil "company-etags" "../../../.emacs.d/elpa/company-20180615.1508/company-etags.el"
;;;;;;  "8594d62ca3f76d6bca1c70d3adc9dccb")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-etags.el

(autoload 'company-etags "company-etags" "\
`company-mode' completion backend for etags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-etags"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-etags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-etags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-etags" '("company-etags-")))

;;;***

;;;***

;;;### (autoloads nil "company-files" "../../../.emacs.d/elpa/company-20180615.1508/company-files.el"
;;;;;;  "52837a9678f74622a57d51960714b299")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-files.el

(autoload 'company-files "company-files" "\
`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-files"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-files.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-files.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-files" '("company-file")))

;;;***

;;;***

;;;### (autoloads nil "company-gtags" "../../../.emacs.d/elpa/company-20180615.1508/company-gtags.el"
;;;;;;  "cf0fb8d3371d0d127d07578233d8ee6f")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-gtags.el

(autoload 'company-gtags "company-gtags" "\
`company-mode' completion backend for GNU Global.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-gtags"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-gtags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-gtags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-gtags" '("company-gtags-")))

;;;***

;;;***

;;;### (autoloads nil "company-ispell" "../../../.emacs.d/elpa/company-20180615.1508/company-ispell.el"
;;;;;;  "f44bf84e35b80953b17c66cb18ce4a75")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-ispell.el

(autoload 'company-ispell "company-ispell" "\
`company-mode' completion backend using Ispell.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-ispell"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-ispell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-ispell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ispell" '("company-ispell-")))

;;;***

;;;***

;;;### (autoloads nil "company-keywords" "../../../.emacs.d/elpa/company-20180615.1508/company-keywords.el"
;;;;;;  "421aa9b4f116c381d2bba745f2cb2936")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-keywords.el

(autoload 'company-keywords "company-keywords" "\
`company-mode' backend for programming language keywords.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-keywords"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-keywords.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-keywords.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-keywords" '("company-keywords-")))

;;;***

;;;***

;;;### (autoloads nil "company-nxml" "../../../.emacs.d/elpa/company-20180615.1508/company-nxml.el"
;;;;;;  "296292d9e829c7cd40fcf0566f085ed8")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-nxml.el

(autoload 'company-nxml "company-nxml" "\
`company-mode' completion backend for `nxml-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-nxml"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-nxml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-nxml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nxml" '("company-nxml-")))

;;;***

;;;***

;;;### (autoloads nil "company-oddmuse" "../../../.emacs.d/elpa/company-20180615.1508/company-oddmuse.el"
;;;;;;  "4d585ed74b0f1654770bb4486c5554da")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-oddmuse.el

(autoload 'company-oddmuse "company-oddmuse" "\
`company-mode' completion backend for `oddmuse-mode'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-oddmuse"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-oddmuse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-oddmuse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-")))

;;;***

;;;***

;;;### (autoloads nil "company-semantic" "../../../.emacs.d/elpa/company-20180615.1508/company-semantic.el"
;;;;;;  "905ad8450f74fcdff3855d8544a47bfe")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-semantic.el

(autoload 'company-semantic "company-semantic" "\
`company-mode' completion backend using CEDET Semantic.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-semantic"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-semantic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-semantic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-semantic" '("company-semantic-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "company-template"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-template.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-template.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-template" '("company-template-")))

;;;***

;;;### (autoloads nil "company-tempo" "../../../.emacs.d/elpa/company-20180615.1508/company-tempo.el"
;;;;;;  "ef7bf243940ad89d1f44ecffcfa3949c")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-tempo.el

(autoload 'company-tempo "company-tempo" "\
`company-mode' completion backend for tempo.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-tempo"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-tempo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-tempo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tempo" '("company-tempo-")))

;;;***

;;;***

;;;### (autoloads nil "company-tng" "../../../.emacs.d/elpa/company-20180615.1508/company-tng.el"
;;;;;;  "3d84d0bdb9b3587296421740bd565ba8")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-tng.el

(autoload 'company-tng-frontend "company-tng" "\
When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

\(fn COMMAND)" nil nil)

(autoload 'company-tng-configure-default "company-tng" "\
Applies the default configuration to enable company-tng.

\(fn)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-tng"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-tng.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-tng.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tng" '("company-tng--")))

;;;***

;;;***

;;;### (autoloads nil "company-xcode" "../../../.emacs.d/elpa/company-20180615.1508/company-xcode.el"
;;;;;;  "0106fec3041ec311c0850ad16a5d099a")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-xcode.el

(autoload 'company-xcode "company-xcode" "\
`company-mode' completion backend for Xcode projects.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-xcode"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-xcode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-xcode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-xcode" '("company-xcode-")))

;;;***

;;;***

;;;### (autoloads nil "company-yasnippet" "../../../.emacs.d/elpa/company-20180615.1508/company-yasnippet.el"
;;;;;;  "b0537a76e047045c033f5f1756cc97d0")
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-yasnippet.el

(autoload 'company-yasnippet "company-yasnippet" "\
`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push '(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet)

\(fn COMMAND &optional ARG &rest IGNORE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "company-yasnippet"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-yasnippet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/company-20180615.1508/company-yasnippet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet--")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/company-20180615.1508/company-abbrev.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-bbdb.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-capf.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-clang.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-cmake.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-css.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev-code.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-dabbrev.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-eclim.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-elisp.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-etags.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-files.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-gtags.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-ispell.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-keywords.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-nxml.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-oddmuse.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-semantic.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-template.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-tempo.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-tng.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-xcode.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company-yasnippet.el"
;;;;;;  "../../../.emacs.d/elpa/company-20180615.1508/company.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-autoloads.el ends here

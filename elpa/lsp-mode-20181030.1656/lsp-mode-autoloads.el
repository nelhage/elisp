;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "lsp-common" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-common" '("with-lsp-workspace" "when-lsp-workspace" "lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-imenu" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-imenu" '("lsp-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-io" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-io.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-io.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-io" '("lsp-")))

;;;***

;;;### (autoloads nil "lsp-methods" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-methods.el"
;;;;;;  "e74a1845a496d50be704eb0887ec8e5f")
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-methods.el

(let ((loads (get 'lsp-mode 'custom-loads))) (if (member '"lsp-methods" loads) nil (put 'lsp-mode 'custom-loads (cons '"lsp-methods" loads))))

(let ((loads (get 'lsp-faces 'custom-loads))) (if (member '"lsp-methods" loads) nil (put 'lsp-faces 'custom-loads (cons '"lsp-methods" loads))))

(defvar lsp-document-sync-method nil "\
How to sync the document with the language server.")

(custom-autoload 'lsp-document-sync-method "lsp-methods" t)

(defvar lsp-project-blacklist nil "\
A list of project directory regexps for which LSP shouldn't be initialized.
LSP should be initialized if the given project root matches one pattern in the
whitelist, or does not match any pattern in the blacklist.")

(custom-autoload 'lsp-project-blacklist "lsp-methods" t)

(defvar lsp-enable-eldoc t "\
Enable `eldoc-mode' integration.")

(custom-autoload 'lsp-enable-eldoc "lsp-methods" t)

(defvar lsp-eldoc-render-all t "\
Define whether all of the returned by document/onHover will be displayed.

If `lsp-markup-display-all' is set to nil `eldoc' will show only
the symbol information.")

(custom-autoload 'lsp-eldoc-render-all "lsp-methods" t)

(defvar lsp-highlight-symbol-at-point t "\
Highlight the symbol under the point.")

(custom-autoload 'lsp-highlight-symbol-at-point "lsp-methods" t)

(defvar lsp-enable-codeaction t "\
Enable code action processing.")

(custom-autoload 'lsp-enable-codeaction "lsp-methods" t)

(defvar lsp-enable-completion-at-point t "\
Enable `completion-at-point' integration.")

(custom-autoload 'lsp-enable-completion-at-point "lsp-methods" t)

(defvar lsp-enable-xref t "\
Enable xref integration.")

(custom-autoload 'lsp-enable-xref "lsp-methods" t)

(defvar lsp-enable-indentation t "\
Indent regions using the file formatting functionality provided by the language server.")

(custom-autoload 'lsp-enable-indentation "lsp-methods" t)

(defvar lsp-before-save-edits t "\
If non-nil, `lsp-mode' will apply edits suggested by the language server
before saving a document.")

(custom-autoload 'lsp-before-save-edits "lsp-methods" t)

(defvar lsp-hover-text-function 'lsp--text-document-hover-string "\
The LSP method to use to display text on hover.")

(custom-autoload 'lsp-hover-text-function "lsp-methods" t)

(defface lsp-face-highlight-textual '((t :inherit highlight)) "\
Face used for textual occurances of symbols." :group (quote lsp-faces))

(defface lsp-face-highlight-read '((t :inherit highlight :underline t)) "\
Face used for highlighting symbols being read." :group (quote lsp-faces))

(defface lsp-face-highlight-write '((t :inherit highlight :italic t)) "\
Face used for highlighting symbols being written to." :group (quote lsp-faces))

;;;### (autoloads "actual autoloads are elsewhere" "lsp-methods"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-methods.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-methods.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-methods" '("lsp-")))

;;;***

;;;***

;;;### (autoloads nil "lsp-mode" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode.el"
;;;;;;  "b4caee83f2366d26f9db92a9bb825312")
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode.el

(autoload 'lsp-mode "lsp-mode" "\


\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "lsp-mode" "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("lsp-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "lsp-notifications"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-notifications.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-notifications.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-notifications" '("lsp-")))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-common.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-flycheck.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-imenu.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-io.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-methods.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-mode.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20181030.1656/lsp-notifications.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here
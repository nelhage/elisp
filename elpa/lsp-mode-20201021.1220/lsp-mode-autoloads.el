;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ada"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ada.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ada.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ada" '("lsp-ada-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-angular"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-angular.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-angular.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-angular" '("lsp-client"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-bash"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-bash.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-bash.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-bash" '("lsp-bash-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clangd"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clangd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clangd.el

(autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clangd" "\
Explain a clang-tidy ERROR by scraping documentation from llvm.org.

\(fn ERROR)" nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clangd" '("lsp-c"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clojure"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clojure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clojure.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-clojure" '("lsp-clojure-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion.el

(define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1")

(autoload 'lsp-completion-at-point "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion" "\
Get lsp completions." nil nil)

(autoload 'lsp-completion--enable "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion" "\
Enable LSP completion support." nil nil)

(autoload 'lsp-completion-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion" "\
Toggle LSP completion support.

If called interactively, enable Lsp-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable))))

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-completion" '("lsp-completion-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-crystal"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-crystal.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-crystal.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-crystal" '("lsp-clients-crystal-executable"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-csharp"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-csharp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-csharp.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-csharp" '("lsp-csharp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-css"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-css.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-css.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-css" '("lsp-css-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics.el

(define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1")

(define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1")

(autoload 'lsp-diagnostics--enable "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics" "\
Enable LSP checker support." nil nil)

(autoload 'lsp-diagnostics-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics" "\
Toggle LSP diagnostics integration.

If called interactively, enable Lsp-Diagnostics mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable))))

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-diagnostics" '("lsp-diagnostics-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-dockerfile"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-dockerfile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-dockerfile.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-dockerfile" '("lsp-dockerfile-language-server-command"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elixir"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elixir.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elixir.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elixir" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elm"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elm.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-elm" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-erlang"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-erlang.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-erlang.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-erlang" '("lsp-erlang-server-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-eslint"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-eslint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-eslint.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-eslint" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fortran"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fortran.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fortran.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fortran" '("lsp-clients-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fsharp"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fsharp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fsharp.el

(autoload 'lsp-fsharp--workspace-load "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fsharp" "\
Load all of the provided PROJECTS.

\(fn PROJECTS)" nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-fsharp" '("lsp-fsharp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-gdscript"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-gdscript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-gdscript.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-gdscript" '("lsp-gdscript-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-go"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-go.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-go.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-go" '("lsp-go-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-groovy"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-groovy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-groovy.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-groovy" '("lsp-groovy-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-hack"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-hack.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-hack.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-hack" '("lsp-clients-hack-command"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-haxe"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-haxe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-haxe.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-haxe" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline.el

(autoload 'lsp-headerline-breadcrumb-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline" "\
Toggle breadcrumb on headerline.

If called interactively, enable Lsp-Headerline-Breadcrumb mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-breadcrumb-go-to-symbol "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline" "\
Go to the symbol on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(autoload 'lsp-breadcrumb-narrow-to-symbol "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline" "\
Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

\(fn SYMBOL-POSITION)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-headerline" '("lsp-headerline-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-html"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-html.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-html.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-html" '("lsp-html-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-javascript"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-javascript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-javascript.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-javascript" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-json"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-json.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-json.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-json" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-kotlin"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-kotlin.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-kotlin.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-kotlin" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens.el

(autoload 'lsp-lens-show "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens" "\
Display lenses in the buffer." t nil)

(autoload 'lsp-lens-hide "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens" "\
Delete all lenses." t nil)

(autoload 'lsp-lens-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens" "\
Toggle code-lens overlays.

If called interactively, enable Lsp-Lens mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-avy-lens "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens" "\
Click lsp lens using `avy' package." t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lens" '("lsp-lens-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lua"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lua.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lua.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-lua" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode.el
(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)
(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(autoload 'lsp "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start. 

\(fn &optional ARG)" t nil)

(autoload 'lsp-deferred "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode" "\
Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode" '("lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline.el

(define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1")

(autoload 'lsp-modeline-code-actions-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline" "\
Toggle code actions on modeline.

If called interactively, enable Lsp-Modeline-Code-Actions mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1")

(autoload 'lsp-modeline-diagnostics-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline" "\
Toggle diagnostics modeline.

If called interactively, enable Lsp-Modeline-Diagnostics mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'lsp-modeline-workspace-status-mode "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline" "\
Toggle workspace status on modeline.

If called interactively, enable Lsp-Modeline-Workspace-Status
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-modeline" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-nix"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-nix.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-nix.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-nix" '("lsp-nix-server-path"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ocaml"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ocaml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ocaml.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-ocaml" '("lsp-ocaml-l"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-perl"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-perl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-perl.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-perl" '("lsp-perl-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-php"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-php.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-php.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-php" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-prolog"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-prolog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-prolog.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-prolog" '("lsp-prolog-server-command"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-protocol"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-protocol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-protocol.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-protocol" '("dash-expand:&RangeToPoint" "lsp"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-purescript"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-purescript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-purescript.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-purescript" '("lsp-purescript-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pwsh"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pwsh.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pwsh.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pwsh" '("lsp-pwsh-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pyls"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pyls.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pyls.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-pyls" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-r"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-r.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-r.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-r" '("lsp-clients-r-server-command"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-racket"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-racket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-racket.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-racket" '("lsp-racket-lang"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rf"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rf.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rust"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rust.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rust.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-rust" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-solargraph"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-solargraph.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-solargraph.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-solargraph" '("lsp-solargraph-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-sqls"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-sqls.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-sqls.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-sqls" '("lsp-sqls-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-terraform"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-terraform.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-terraform.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-terraform" '("lsp-terraform-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-tex"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-tex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-tex.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-tex" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-verilog"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-verilog.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-verilog.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-verilog" '("lsp-clients-verilog-executable"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vetur"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vetur.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vetur.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vetur" '("lsp-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vhdl"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vhdl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vhdl.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vimscript"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vimscript.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vimscript.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-vimscript" '("lsp-clients-vim-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-xml"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-xml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-xml.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-xml" '("lsp-xml-"))

;;;***

;;;### (autoloads nil "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-yaml"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-yaml.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-yaml.el

(register-definition-prefixes "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-yaml" '("lsp-yaml-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-cmake.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-dhall.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-mode-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-nim.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp-svelte.el"
;;;;;;  "../../../.emacs.d/elpa/lsp-mode-20201021.1220/lsp.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-mode-autoloads.el ends here

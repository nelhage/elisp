;;; inf-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby"
;;;;;;  "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby.el

(defvar ruby-source-modes '(ruby-mode enh-ruby-mode) "\
Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `ruby-load-file'.
Used by these commands to determine defaults.")

(autoload 'inf-ruby-setup-keybindings "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Hook up `inf-ruby-minor-mode' to each of `ruby-source-modes'." nil nil)

(autoload 'inf-ruby-minor-mode "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Minor mode for interacting with the inferior process buffer.

If called interactively, enable Inf-Ruby minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{inf-ruby-minor-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'inf-ruby "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use.

If there is a Ruby process running in an existing buffer, switch
to that buffer. Otherwise create a new buffer.

\(fn &optional IMPL)" t nil)

(autoload 'run-ruby "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run an inferior Ruby process, input and output in a buffer.

If there is a process already running in a corresponding buffer,
switch to that buffer. Otherwise create a new buffer.

The consecutive buffer names will be:
`*NAME*', `*NAME*<2>', `*NAME*<3>' and so on.

COMMAND defaults to the default entry in
`inf-ruby-implementations'. NAME defaults to \"ruby\".

Runs the hooks `comint-mode-hook' and `inf-ruby-mode-hook'.

Type \\[describe-mode] in the process buffer for the list of commands.

\(fn &optional COMMAND NAME)" t nil)

(autoload 'inf-ruby-switch-setup "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `inf-ruby-switch-from-compilation' to `С-x C-q'." nil nil)

(autoload 'inf-ruby-console-auto "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run the appropriate Ruby console command.
The command and the directory to run it from are detected
automatically." t nil)

(autoload 'inf-ruby-console-zeus "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run Rails console in DIR using Zeus.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-rails "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run Rails console in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-gem "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction.

\(fn DIR)" t nil)

(autoload 'inf-ruby-auto-enter "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Switch to `inf-ruby-mode' if the breakpoint pattern matches the current line." nil nil)

(autoload 'inf-ruby-auto-exit "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Return to the previous compilation mode if INPUT is a debugger exit command.

\(fn INPUT)" nil nil)

(autoload 'inf-ruby-console-script "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run custom bin/console, console or console.rb in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-default "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\
Run Pry, or bundle console, in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-file-contents-match "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" "\


\(fn FILE REGEXP &optional MATCH-GROUP)" nil nil)
 (dolist (mode ruby-source-modes) (add-hook (intern (format "%s-hook" mode)) 'inf-ruby-minor-mode))

(register-definition-prefixes "../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby" '("inf-ruby-" "ruby-" "run-ruby-"))

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/inf-ruby-20200826.1458/inf-ruby-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inf-ruby-autoloads.el ends here

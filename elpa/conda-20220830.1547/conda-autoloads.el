;;; conda-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "conda" "../../../.emacs.d/elpa/conda-20220830.1547/conda.el"
;;;;;;  "bf4ca020e90a20b312e3a42c2bdd3ca2")
;;; Generated autoloads from ../../../.emacs.d/elpa/conda-20220830.1547/conda.el

(autoload 'conda-env-deactivate "conda" "\
Deactivate the current conda env." t nil)

(autoload 'conda-env-activate "conda" "\
Switch to environment NAME, prompting if called interactively.

\(fn &optional NAME)" t nil)

(autoload 'conda-env-activate-path "conda" "\
Switch to environment PATH, prompting if called interactively.

\(fn &optional PATH)" t nil)

(autoload 'conda-env-list "conda" "\
List all available conda environments in a temp buffer." t nil)

(autoload 'conda-with-env-shell-command "conda" "\
With environment NAME active, execute the shell string COMMAND.

\(fn NAME COMMAND)" nil nil)

(autoload 'conda-env-shell-init "conda" "\
Activate the current env in a newly opened shell PROCESS.

\(fn PROCESS)" nil nil)

(autoload 'conda-env-eshell-prompt "conda" "\
An Eshell prompt function to insert the active Conda environment." nil nil)

(autoload 'conda-env-initialize-interactive-shells "conda" "\
Configure interactive shells for use with conda.el." nil nil)

(autoload 'conda-env-initialize-eshell "conda" "\
Configure eshell for use with conda.el." nil nil)

(autoload 'conda-env-activate-for-buffer "conda" "\
Activate the conda environment implied by the current buffer.

This can be set by a buffer-local or project-local variable (e.g. a
`.dir-locals.el` that defines `conda-project-env-path`), or inferred from an
`environment.yml` or similar at the project level." t nil)

(defvar conda-env-autoactivate-mode nil "\
Non-nil if Conda-Env-Autoactivate mode is enabled.
See the `conda-env-autoactivate-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `conda-env-autoactivate-mode'.")

(custom-autoload 'conda-env-autoactivate-mode "conda" nil)

(autoload 'conda-env-autoactivate-mode "conda" "\
Toggle conda-env-autoactivate mode.

If called interactively, enable Conda-Env-Autoactivate mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

This mode automatically tries to activate a conda environment for the current
buffer.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "conda" "../../../.emacs.d/elpa/conda-20220830.1547/conda.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/conda-20220830.1547/conda.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "conda" '("conda-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/conda-20220830.1547/conda-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/conda-20220830.1547/conda.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; conda-autoloads.el ends here

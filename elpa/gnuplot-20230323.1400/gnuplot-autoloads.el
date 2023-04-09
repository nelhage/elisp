;;; gnuplot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gnuplot" "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot.el"
;;;;;;  "b7b4aeb2591e8b9299e6e754729b0f1c")
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot.el

(autoload 'gnuplot-mode "gnuplot" "\
Major mode for editing and executing GNUPLOT scripts.
This was written with version 4.6 of gnuplot in mind, but should
work with newer and older versions.

Report bugs at https://github.com/emacs-gnuplot/gnuplot/issues

                            ------O------

Gnuplot-mode includes two different systems for keyword
completion and documentation lookup: a newer one,
`gnuplot-context-sensitive-mode' (enabled by default), and a
older one which extracts keywords from gnuplot's Info file.  Both
systems allow looking up documentation in the Info file.  The
older system also depends having the info file properly installed
to make a list of keywords.

The info file should be installed by default with the Gnuplot
distribution, or is available at the `gnuplot-mode' web page:
https://github.com/emacs-gnuplot/gnuplot/

With the new context-sensitive mode active, `gnuplot-mode' can also
provide function/`eldoc-mode' syntax hints as you type.  This requires a
separate file of strings, `gnuplot-eldoc.el', which is also
provided by recent Gnuplot distributions.

                            ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
\(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 3.  The GUI does not know how to read from continuation lines.
 4.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 5.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

                            ------O------

 Key bindings:
 \\{gnuplot-mode-map}" t nil)

(autoload 'gnuplot-make-buffer "gnuplot" "\
Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (autoload 'gnuplot-make-buffer \"gnuplot\"
               \"open a buffer in gnuplot mode\" t)
     (global-set-key [(f10)] 'gnuplot-make-buffer)" t nil)

(autoload 'run-gnuplot "gnuplot" "\
Run an inferior Gnuplot process." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "gnuplot" "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnuplot" '("gnuplot-")))

;;;***

;;;***

;;;### (autoloads nil "gnuplot-context" "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-context.el"
;;;;;;  "b3e5ea0b60027a0d6d3fe9388854fe61")
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-context.el

(autoload 'gnuplot-context-sensitive-mode "gnuplot-context" "\
Use context-sensitive completion and help in `gnuplot-mode'.

If called interactively, enable Gnuplot-Context-Sensitive mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

When context-sensitive mode is enabled, `gnuplot-mode' tries to
provide more useful completions and help suggestions for built-in
keywords and functions by parsing each command as you type.  It
attempts to take into account Gnuplot's many abbreviated
keywords.  For example, with point at the end of a line reading
\"plot 'datafile' w \", typing \\[completion-at-point] will pop
up a list of plotting styles.

Key bindings:

\\[completion-at-point] will complete the keyword at point based
on its context in the command. To make keyword completion work on
pressing TAB, set `tab-always-indent' to `complete', or customize
`gnuplot-tab-completion' to make this automatic in gnuplot-mode
buffers.

\\[gnuplot-info-at-point] will try to find the most relevant
Gnuplot info node for the construction at point, prompting for a
node name if nothing is found.

\\[gnuplot-help-function] will pop up a brief summary of the
syntax at point in the minibuffer. To have one-line syntax
summaries appear in the echo area as you type, toggle
`eldoc-mode' or customize `gnuplot-eldoc-mode'.

To choose whether to use this mode by default in Gnuplot buffers,
customize the variable
`gnuplot-use-context-sensitive-completion'.

Note: help strings for eldoc-mode and \\[gnuplot-help-function]
need to be provided in an Emacs-readable form by the Gnuplot
distribution. See gnuplot-context.el for details.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "gnuplot-context"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-context.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-context.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnuplot-context" '("gnuplot-")))

;;;***

;;;***

;;;### (autoloads nil "gnuplot-gui" "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-gui.el"
;;;;;;  "2b595deee3000a51cd21cb2df03dc744")
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-gui.el

(autoload 'gnuplot-gui-swap-simple-complete "gnuplot-gui" nil t nil)

(autoload 'gnuplot-gui-mouse-set "gnuplot-gui" "\
Use the mouse to begin setting options using a GUI interface.
EVENT is a mouse event.  Bound to \\[gnuplot-gui-mouse-set]
Note that \"plot\", \"splot\", \"fit\", and \"cntrparam\" are not
currently supported.

\(fn EVENT)" t nil)

(autoload 'gnuplot-gui-set-options-and-insert "gnuplot-gui" "\
Insert arguments using a GUI interface.
Determine contents of current line and set up the appropriate GUI
frame.  Bound to \\[gnuplot-gui-set-options-and-insert]
Note that \"cntrparam\" is not currently supported." t nil)

(autoload 'gnuplot-gui-toggle-popup "gnuplot-gui" nil t nil)

;;;### (autoloads "actual autoloads are elsewhere" "gnuplot-gui"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-gui.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-gui.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gnuplot-gui" '("gnuplot-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-context.el"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-gui.el"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/gnuplot-20230323.1400/gnuplot.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gnuplot-autoloads.el ends here

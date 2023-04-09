;;; ein-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads "actual autoloads are elsewhere" "ein-cell" "../../../.emacs.d/elpa/ein-20220911.1319/ein-cell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-cell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-cell" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-classes"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-classes.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-classes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-classes" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-contents-api"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-contents-api.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-contents-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-contents-api" '("*ein:content-hierarchy*" "ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-core" "../../../.emacs.d/elpa/ein-20220911.1319/ein-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-core" '("*ein:" "ein:")))

;;;***

;;;### (autoloads nil "ein-dev" "../../../.emacs.d/elpa/ein-20220911.1319/ein-dev.el"
;;;;;;  "934b5cae9848b1776388fc34b4e22bad")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-dev.el

(autoload 'ein:dev-start-debug "ein-dev" "\
Start logging a bunch of stuff." t nil)

(autoload 'ein:dev-stop-debug "ein-dev" "\
Inverse of `ein:dev-start-debug'.
Impossible to maintain because it needs to match start." t nil)

(autoload 'ein:dev-bug-report-template "ein-dev" "\
Open a buffer with bug report template." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-dev" "../../../.emacs.d/elpa/ein-20220911.1319/ein-dev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-dev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-dev" '("ein:dev-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-events" "../../../.emacs.d/elpa/ein-20220911.1319/ein-events.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-events.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-events" '("ein:events-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-file" "../../../.emacs.d/elpa/ein-20220911.1319/ein-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-file" '("*ein:file-buffername-template*" "ein:")))

;;;***

;;;### (autoloads nil "ein-gat" "../../../.emacs.d/elpa/ein-20220911.1319/ein-gat.el"
;;;;;;  "c09a099a2378e502df8023434d322aa3")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-gat.el

(autoload 'ein:gat-create "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-local-batch "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-local "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-remote-batch "ein-gat" "\


\(fn &optional REFRESH)" t nil)

(autoload 'ein:gat-run-remote "ein-gat" "\


\(fn &optional REFRESH)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-gat" "../../../.emacs.d/elpa/ein-20220911.1319/ein-gat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-gat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-gat" '("ein:gat-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-ipdb" "../../../.emacs.d/elpa/ein-20220911.1319/ein-ipdb.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-ipdb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-ipdb" '("*ein:ipdb-sessions*" "ein:ipdb-")))

;;;***

;;;### (autoloads nil "ein-ipynb-mode" "../../../.emacs.d/elpa/ein-20220911.1319/ein-ipynb-mode.el"
;;;;;;  "7796335a9017ee849dec240958d4c5f5")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-ipynb-mode.el

(autoload 'ein:ipynb-mode "ein-ipynb-mode" "\
A simple mode for ipynb file.

\\{ein:ipynb-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . ein:ipynb-mode))

;;;***

;;;### (autoloads nil "ein-jupyter" "../../../.emacs.d/elpa/ein-20220911.1319/ein-jupyter.el"
;;;;;;  "22eeba27172760afe648118c1e142852")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-jupyter.el

(autoload 'ein:jupyter-crib-token "ein-jupyter" "\
Shell out to jupyter for its credentials knowledge.  Return list
of (PASSWORD TOKEN).

\(fn URL-OR-PORT)" nil nil)

(autoload 'ein:jupyter-crib-running-servers "ein-jupyter" "\
Shell out to jupyter for running servers." nil nil)

(autoload 'ein:jupyter-server-start "ein-jupyter" "\
Start SERVER-COMMAND with `--notebook-dir' NOTEBOOK-DIRECTORY.

Login after connection established unless NO-LOGIN-P is set.
LOGIN-CALLBACK takes two arguments, the buffer created by
`ein:notebooklist-open--finish', and the url-or-port argument
of `ein:notebooklist-open*'.

With \\[universal-argument] prefix arg, prompt the user for the
server command.

\(fn SERVER-COMMAND NOTEBOOK-DIRECTORY &optional NO-LOGIN-P LOGIN-CALLBACK PORT)" t nil)

(defalias 'ein:run 'ein:jupyter-server-start)

(defalias 'ein:stop 'ein:jupyter-server-stop)

(autoload 'ein:jupyter-server-stop "ein-jupyter" "\


\(fn &optional ASK-P URL-OR-PORT)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-jupyter"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-jupyter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-jupyter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-jupyter" '("*ein:jupyter-server-" "ein:")))

;;;***

;;;***

;;;### (autoloads nil "ein-kernel" "../../../.emacs.d/elpa/ein-20220911.1319/ein-kernel.el"
;;;;;;  "8eb1e5be447d9bb3563787b87161f1a9")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-kernel.el

(defalias 'ein:kernel-url-or-port 'ein:$kernel-url-or-port)

(defalias 'ein:kernel-id 'ein:$kernel-kernel-id)

;;;### (autoloads "actual autoloads are elsewhere" "ein-kernel" "../../../.emacs.d/elpa/ein-20220911.1319/ein-kernel.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-kernel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kernel" '("ein:")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-kernelinfo"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-kernelinfo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-kernelinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kernelinfo" '("ein:kernelinfo")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-kill-ring"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-kill-ring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-kill-ring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-kill-ring" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-log" "../../../.emacs.d/elpa/ein-20220911.1319/ein-log.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-log.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-log" '("ein:")))

;;;***

;;;### (autoloads nil "ein-markdown-mode" "../../../.emacs.d/elpa/ein-20220911.1319/ein-markdown-mode.el"
;;;;;;  "bb5a99f45c6b0aec0c341ffb6c92a093")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-markdown-mode.el

(autoload 'ein:markdown-mode "ein-markdown-mode" "\
Major mode for editing ein:markdown files.

\(fn)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-markdown-mode"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-markdown-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-markdown-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-markdown-mode" '("defun-markdown-" "ein:markdown")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-node" "../../../.emacs.d/elpa/ein-20220911.1319/ein-node.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-node.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-node" '("ein:node-")))

;;;***

;;;### (autoloads nil "ein-notebook" "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebook.el"
;;;;;;  "6c014ac32630951b00523a2b1a9c708f")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-notebook.el

(defalias 'ein:notebook-name 'ein:$notebook-notebook-name)

(autoload 'ein:notebook-jump-to-opened-notebook "ein-notebook" "\
List all opened notebook buffers and switch to one that the user selects.

\(fn NOTEBOOK)" t nil)

(autoload 'ein:notebook-open "ein-notebook" "\
Returns notebook at URL-OR-PORT/PATH.

Note that notebook sends for its contents and won't have them right away.

After the notebook is opened, CALLBACK is called as::

  (funcall CALLBACK notebook created)

where `created' indicates a new notebook or an existing one.

\(fn URL-OR-PORT PATH &optional KERNELSPEC CALLBACK ERRBACK NO-POP)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-notebook"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebook.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-notebook.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notebook" '("*ein:notebook--pending-query*" "ein:")))

;;;***

;;;***

;;;### (autoloads nil "ein-notebooklist" "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebooklist.el"
;;;;;;  "beb842dd36e055a50b43d8c71e0534c0")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-notebooklist.el

(autoload 'ein:notebooklist-reload "ein-notebooklist" "\
Reload current Notebook list.

\(fn &optional NBLIST RESYNC CALLBACK)" t nil)

(autoload 'ein:notebooklist-new-notebook "ein-notebooklist" "\


\(fn URL-OR-PORT KERNELSPEC &optional CALLBACK NO-POP RETRY EXPLICIT-PATH)" t nil)

(autoload 'ein:notebooklist-new-notebook-with-name "ein-notebooklist" "\
Upon notebook-open, rename the notebook, then funcall CALLBACK.

\(fn URL-OR-PORT KERNELSPEC NAME &optional CALLBACK NO-POP)" t nil)

(autoload 'ein:notebooklist-list-paths "ein-notebooklist" "\
Return all files of CONTENT-TYPE for all sessions

\(fn &optional CONTENT-TYPE)" nil nil)

(autoload 'ein:notebooklist-load "ein-notebooklist" "\
Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:notebooklist-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:notebooklist-load)

\(fn &optional URL-OR-PORT)" nil nil)

(autoload 'ein:notebooklist-open "ein-notebooklist" "\
This is now an alias for `ein:notebooklist-login'.

\(fn URL-OR-PORT CALLBACK)" t nil)

(defalias 'ein:login 'ein:notebooklist-login)

(autoload 'ein:notebooklist-login "ein-notebooklist" "\
Deal with security before main entry of ein:notebooklist-open*.
CALLBACK takes two arguments, the buffer created by
ein:notebooklist-open--success and the url-or-port argument of
ein:notebooklist-open*.

\(fn URL-OR-PORT CALLBACK &optional COOKIE-NAME COOKIE-CONTENT TOKEN)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-notebooklist"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebooklist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-notebooklist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notebooklist" '("ein:" "generate-breadcrumbs" "render-")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-notification"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-notification.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-notification.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-notification" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-output-area"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-output-area.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-output-area.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-output-area" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-pager" "../../../.emacs.d/elpa/ein-20220911.1319/ein-pager.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-pager.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-pager" '("ein:pager-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-process"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-process.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-process.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-process" '("ein:process-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-python-send"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-python-send.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-python-send.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-python-send" '("ein:python-send-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-pytools"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-pytools.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-pytools.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-pytools" '("ein:pytools-jump-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-query" "../../../.emacs.d/elpa/ein-20220911.1319/ein-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-query" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-scratchsheet"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-scratchsheet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-scratchsheet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-scratchsheet" '("ein:scratchsheet")))

;;;***

;;;### (autoloads nil "ein-shared-output" "../../../.emacs.d/elpa/ein-20220911.1319/ein-shared-output.el"
;;;;;;  "95796fb9fdd08f35746c8e61023be37e")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-shared-output.el

(autoload 'ein:shared-output-pop-to-buffer "ein-shared-output" "\
Open shared output buffer." t nil)

(autoload 'ein:shared-output-show-code-cell-at-point "ein-shared-output" "\
Show code cell at point in shared-output buffer.
It is useful when the output of the cell at point is truncated.
See also `ein:cell-max-num-outputs'." t nil)

(autoload 'ein:shared-output-eval-string "ein-shared-output" "\
Entry to `ein:cell-execute-internal' from the shared output cell.

\(fn KERNEL CODE &rest ARGS)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-shared-output"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-shared-output.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-shared-output.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-shared-output" '("*ein:shared-output*" "ein:")))

;;;***

;;;***

;;;### (autoloads nil "ein-traceback" "../../../.emacs.d/elpa/ein-20220911.1319/ein-traceback.el"
;;;;;;  "fbb0c6d5834133b6417869db156be7fa")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-traceback.el

(autoload 'ein:tb-show "ein-traceback" "\
Show full traceback in traceback viewer." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "ein-traceback"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-traceback.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-traceback.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-traceback" '("ein:t")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-utils" "../../../.emacs.d/elpa/ein-20220911.1319/ein-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-utils" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-websocket"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-websocket.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-websocket.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-websocket" '("ein:")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ein-worksheet"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-worksheet.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ein-worksheet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ein-worksheet" '("ein:" "hof-add")))

;;;***

;;;### (autoloads nil "ob-ein" "../../../.emacs.d/elpa/ein-20220911.1319/ob-ein.el"
;;;;;;  "14969e6809f01381f3789e6a34227e55")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ob-ein.el

(when (featurep 'org) (let* ((orig (get 'org-babel-load-languages 'custom-type)) (orig-cdr (cdr orig)) (choices (plist-get orig-cdr :key-type))) (push '(const :tag "Ein" ein) (nthcdr 1 choices)) (put 'org-babel-load-languages 'custom-type (cons (car orig) (plist-put orig-cdr :key-type choices)))))

;;;### (autoloads "actual autoloads are elsewhere" "ob-ein" "../../../.emacs.d/elpa/ein-20220911.1319/ob-ein.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/ob-ein.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ein" '("*ob-ein-sentinel*" "ob-ein-")))

;;;***

;;;***

;;;### (autoloads nil "poly-ein" "../../../.emacs.d/elpa/ein-20220911.1319/poly-ein.el"
;;;;;;  "9f1bb407e1d4adf5164e9182beff0f2e")
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/poly-ein.el
 (autoload 'poly-ein-mode "poly-ein")

;;;### (autoloads "actual autoloads are elsewhere" "poly-ein" "../../../.emacs.d/elpa/ein-20220911.1319/poly-ein.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../.emacs.d/elpa/ein-20220911.1319/poly-ein.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "poly-ein" '("pm-" "poly-ein-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../.emacs.d/elpa/ein-20220911.1319/ein-autoloads.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-cell.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-classes.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-completer.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-contents-api.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-core.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-dev.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-events.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-file.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-gat.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-ipdb.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-ipynb-mode.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-jupyter.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-kernel.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-kernelinfo.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-kill-ring.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-log.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-markdown-mode.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-node.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebook.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-notebooklist.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-notification.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-output-area.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-pager.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-pkg.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-process.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-python-send.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-pytools.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-query.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-scratchsheet.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-shared-output.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-traceback.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-utils.el" "../../../.emacs.d/elpa/ein-20220911.1319/ein-websocket.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein-worksheet.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/ein.el" "../../../.emacs.d/elpa/ein-20220911.1319/ob-ein.el"
;;;;;;  "../../../.emacs.d/elpa/ein-20220911.1319/poly-ein.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ein-autoloads.el ends here

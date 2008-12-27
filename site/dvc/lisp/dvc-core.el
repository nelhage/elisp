;;; dvc-core.el --- Core functions for distributed version control

;; Copyright (C) 2005-2008 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions From:
;;         Matthieu Moy <Matthieu.Moy@imag.fr>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the low-level functions used by the DVC interfaces
;; to distributed revison control systems.


;;; History:

;; This file holds general useful functions, previously only used for tla.

;;; Code:

(require 'dvc-defs)
(require 'dvc-register)
(eval-and-compile (require 'dvc-utils))
(require 'dvc-buffers)
(eval-when-compile (require 'cl))
(eval-when-compile (require 'dired))
(eval-and-compile (require 'dvc-lisp))

(defvar dvc-sh-executable "sh" "The shell that is used for dvc interaction.")

;; --------------------------------------------------------------------------------
;; Various constants
;; --------------------------------------------------------------------------------

(defconst dvc-mark (dvc-face-add "*" 'dvc-mark) "Fontified string used for marking.")
(defconst dvc-exclude (dvc-face-add "E" 'dvc-mark) "Fontified string used for excluded files.")

;; --------------------------------------------------------------------------------
;; Internal variables
;; --------------------------------------------------------------------------------

(defvar dvc-memorized-log-header nil)
(defvar dvc-memorized-log-message nil)
(defvar dvc-memorized-version nil)
(defvar dvc-memorized-patch-sender nil)

;; --------------------------------------------------------------------------------
;; Various helper functions
;; --------------------------------------------------------------------------------

;; list-buffers-directory is used by uniquify to get the directory for
;; the buffer when buffer-file-name is nil, as it is for many dvc
;; buffers (dvc-diff-mode, etc). It needs to survive
;; kill-all-local-variables, so we declare it permanent local.
(make-variable-buffer-local 'list-buffers-directory)
(put 'list-buffers-directory 'permanent-local t)

(defun dvc-find-tree-root-file-first (file-or-dir &optional location)
  "Find FILE-OR-DIR upward in the file system from LOCATION.
Finding is continued upward to \"/\" until FILE-OR-DIR can be found.
Once FILE-OR-DIR is found, the finding is broken off.
A directory which holds FILE-OR-DIR is returned. If no such directory
`nil' is returned. `default-directory' is used instead if LOCATION is not
given,

The resulting directory is guaranteed to end in a \"/\" character.

This function may be useful to find \{arch\} and/or _darcs directories."
  (let ((pwd (or location default-directory))
        (pwd-stack nil)
        new-pwd)
    (while (not (or (string= pwd "/")
                    (member pwd pwd-stack)
                    (file-exists-p (concat (file-name-as-directory pwd)
                                           file-or-dir))))
      (setq pwd-stack (cons pwd pwd-stack))
      (setq new-pwd
            (dvc-expand-file-name (concat (file-name-as-directory pwd) "..")))

      ;; detect MS-Windows roots (c:/, d:/, ...)
      (setq pwd (if (string= new-pwd pwd) "/" new-pwd)))

    (unless (string= pwd "/")
      (setq pwd (replace-regexp-in-string "\\([^:]\\)/*$" "\\1" pwd))
      (setq pwd (file-name-as-directory pwd))
      (if (memq system-type '(ms-dos windows-nt))
          (expand-file-name pwd)
        pwd))))

(defun dvc-tree-root-helper (file-or-dir interactivep msg
                                         &optional location no-error)
  "Find FILE-OR-DIR upward in the file system from LOCATION.

Calls `dvc-find-tree-root-file-first', shows a message when
called interactively, and manages no-error.

If LOCATION is nil, the tree root is returned, and it is
guaranteed to end in a \"/\" character.

MSG must be of the form \"%S is not a ...-managed tree\"."
  (let ((location (dvc-uniquify-file-name location)))
    (let ((pwd (dvc-find-tree-root-file-first
                file-or-dir location)))
      (when (and interactivep pwd)
        (dvc-trace "%s" pwd))
      (or pwd
          (if no-error
              nil
            (error msg
                   (or location default-directory)))))))

(defun dvc-find-tree-root-file-last (file-or-dir &optional location)
  "Like `dvc-find-tree-root-file-upward' but recursively if FILE-OR-DIR is found.
Finding is started from LOCATION but is stoped when FILE-OR-DIR cannot be found.
Fiddled is continued upward while FILE-OR-DIR can be found.
The last found directory which holds FILE-OR-DIR is returned. `nil' is returned
if finding failed.
`default-directory' is used instead if LOCATION is not given,

This function may be useful to find CVS or .svn directories"
  (let ((pwd (or location default-directory))
        old-pwd)
    (while (and pwd (not (string= pwd "/")))
      (if (file-exists-p (concat (file-name-as-directory pwd)
                                 file-or-dir))
          (setq old-pwd pwd
                pwd (expand-file-name (concat (file-name-as-directory pwd)
                                              "..")))
        (setq pwd nil)))
    (when old-pwd
      (expand-file-name
       (replace-regexp-in-string "/+$" "/" old-pwd)))))

(defmacro dvc-make-bymouse-function (function)
  "Create a new function by adding mouse interface to FUNCTION.
The new function is named FUNCTION-by-mouse; and takes one argument,
a mouse click event.
Thew new function moves the point to the place where mouse is clicked
then invoke FUNCTION."
  `(defun ,(intern (concat (symbol-name function) "-by-mouse")) (event)
     ,(concat "`" (symbol-name function) "'" " with mouse interface.")
     (interactive "e")
     (mouse-set-point event)
     (,function)))

;; Adapted from `dired-delete-file' in Emacs 22
(defun dvc-delete-recursively (file)
  "Delete FILE or directory recursively."
  (let (files)
    (if (not (eq t (car (file-attributes file))))
        (delete-file file)
      (when (setq files
                  (directory-files
                   file t "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
        (while files
          (dvc-delete-recursively (car files))
          (setq files (cdr files))))
      (delete-directory file))))

;; --------------------------------------------------------------------------------
;; File selection helpers
;; --------------------------------------------------------------------------------

(defvar dvc-get-file-info-at-point-function nil
  "Function used to get the file at point, anywhere.")

(defun dvc-get-file-info-at-point ()
  "Gets the filename at point, according to mode.
Calls the function `dvc-get-file-info-at-point-function' if defined.
When in dired mode, return the file where point is.
Otherwise return the buffer file name."
  (cond (dvc-get-file-info-at-point-function
         (funcall dvc-get-file-info-at-point-function))
        ((eq major-mode 'dired-mode)
         (dired-get-filename))
        (t (buffer-file-name))))

;;;###autoload
(defun dvc-current-file-list (&optional selection-mode)
  "Return a list of currently active files.
When in dired mode, return the marked files or the file under point.
In a DVC mode, return `dvc-buffer-marked-file-list' if non-nil;
otherwise the result depends on SELECTION-MODE:
* When 'nil-if-none-marked, return nil.
* When 'all-if-none-marked, return all files.
* Otherwise return result of calling `dvc-get-file-info-at-point'."
  (cond
   ((eq major-mode 'dired-mode)
    (dired-get-marked-files))

   ((dvc-derived-mode-p 'dvc-diff-mode)
    (or (remove nil dvc-buffer-marked-file-list)
        (cond
         ((eq selection-mode 'nil-if-none-marked)
          nil)

         ((eq selection-mode 'all-if-none-marked)
          (dvc-fileinfo-all-files))

         (t (list (dvc-get-file-info-at-point))))))

   ((eq major-mode 'dvc-bookmark-mode)
    (cond
     ((eq selection-mode 'nil-if-none-marked)
      nil)

     (t
      (error "selection-mode %s not implemented for dvc bookmark buffer" selection-mode))))

   ;; If other modes are added here, dvc-log-edit must be updated to
   ;; support them as well.

   (t
    ;; Some other mode. We assume it has no notion of "marked files",
    ;; so there are none marked. The only file name available is
    ;; buffer-file-name, so we could just return that. But some DVC
    ;; mode might set dvc-get-file-info-at-point-function without
    ;; updating this function, so support that.
    (if (eq selection-mode 'nil-if-none-marked)
        nil
      (list (dvc-get-file-info-at-point))))))

(defun dvc-confirm-read-file-name (prompt &optional mustmatch file-name default-filename)
  "A wrapper around `read-file-name' that provides some useful defaults."
  (unless file-name
    (setq file-name (dvc-get-file-info-at-point)))
  (read-file-name prompt
                  (file-name-directory (or file-name ""))
                  default-filename
                  mustmatch
                  (file-name-nondirectory (or file-name ""))))

(defun dvc-confirm-read-file-name-list (prompt &optional files single-prompt mustmatch)
  (or
   (if dvc-test-mode files)
   (let ((num-files (length files)))
     (if (= num-files 1)
         (let ((confirmed-file-name
                (dvc-confirm-read-file-name single-prompt mustmatch (car files))))
           ;; I don't think `dvc-confirm-read-file-name' can return nil.
           (assert confirmed-file-name)
           (list confirmed-file-name))
       (and (y-or-n-p (format prompt num-files))
            files)))))

(defcustom dvc-confirm-file-op-method 'y-or-n-p
  "Function to use for confirming file-based DVC operations.
Some valid options are:
y-or-n-p: Prompt for 'y' or 'n' keystroke.
yes-or-no-p: Prompt for \"yes\" or \"no\" string.
dvc-always-true: Do not display a prompt."
  :type 'function
  :group 'dvc)

(defun dvc-always-true (&rest ignore)
  "Do nothing and return t.
This function accepts any number of arguments, but ignores them."
  (interactive)
  t)

(defun dvc-confirm-file-op (operation files confirm)
  "Confirm OPERATION (a string, used in prompt) on FILE (list of strings).
If CONFIRM is nil, just return FILES (no prompt).
Returns FILES, or nil if not confirmed.

If you want to adjust the function called to confirm the
operation, then customize the `dvc-confirm-file-op-method' function."
  (or
   ;; Allow bypassing confirmation with `dvc-test-mode'. See
   ;; tests/xmtn-tests.el dvc-status-add.
   (if dvc-test-mode files)
   ;; Abstracted from pcvs.el cvs-do-removal
   (if (not confirm)
       files
     (let ((nfiles (length files)))
       (if (funcall (or (and (functionp dvc-confirm-file-op-method)
                             dvc-confirm-file-op-method)
                        'y-or-n-p)
                    (if (= 1 nfiles)
                        (format "%s file: \"%s\" ? "
                                operation
                                (car files))
                      (format "%s %d files? "
                              operation
                              nfiles)))
           files
         nil)))))

(defun dvc-dvc-files-to-commit ()
  ;;todo: set the correct modifier, one of dvc-modified, dvc-added, dvc-move, now use only nil
  ;; FIXME: this is only used by dvc-log-insert-commit-file-list; should just merge this code there.
  (let ((files
         (with-current-buffer dvc-partner-buffer (dvc-current-file-list 'all-if-none-marked))))
    (mapcar (lambda (arg) (cons nil arg)) files)))

(defun dvc-find-file-at-point ()
  "Opens the file at point.
The filename is obtained with `dvc-get-file-info-at-point'."
  (interactive)
  (let* ((file (dvc-get-file-info-at-point)))
    (cond
     ((not file)
      (error "No file at point"))
     (t
      (find-file file)))))

(dvc-make-bymouse-function dvc-find-file-at-point)

(defun dvc-find-file-other-window ()
  "Visit the current file in the other window.
The filename is obtained with `dvc-get-file-info-at-point'."
  (interactive)
  (let ((file (dvc-get-file-info-at-point)))
    (if file
        (progn
          (find-file-other-window file))
      (error "No file at point"))))

(defun dvc-view-file ()
  "Visit the current file in `view-mode'.
The filename is obtained with `dvc-get-file-info-at-point'."
  (interactive)
  (let ((file (dvc-get-file-info-at-point)))
    (if file
        (view-file-other-window file)
      (error "No file at point"))))

(defun dvc-dired-jump ()
  "Jump to a dired buffer, containing the file at point."
  (interactive)
  (let ((file-full-path (expand-file-name (or (dvc-get-file-info-at-point) ""))))
    (let ((default-directory (file-name-directory file-full-path)))
      (dvc-funcall-if-exists dired-jump))
    (dired-goto-file file-full-path)))

(defun dvc-purge-files (&rest files)
  "Delete FILES from the harddisk. No backup is created for these FILES.
These function bypasses the used revision control system."
  (interactive (dvc-current-file-list))
  (let ((multiprompt (format "Are you sure to purge %%d files? "))
        (singleprompt (format "Purge file: ")))
    (when (dvc-confirm-read-file-name-list multiprompt files singleprompt nil)
      (mapcar #'delete-file files)
      (message "Purged %S" files))))

(defun dvc-current-executable ()
  "Return the name of the binary associated with the current dvc backend.
This uses `dvc-current-active-dvc'.

\"DVC\" is returned if `dvc-current-active-dvc' returns nil."
  (let ((dvc (dvc-current-active-dvc)))
    (if (not dvc)
        "DVC"
      (dvc-variable dvc "executable"))))

;; partner buffer stuff
(defvar dvc-partner-buffer nil
  "DVC Partner buffer; stores diff buffer for log-edit, etc.
Local to each buffer, not killed by kill-all-local-variables.")
(make-variable-buffer-local 'dvc-partner-buffer)
(put 'dvc-partner-buffer 'permanent-local t)

(defun dvc-buffer-pop-to-partner-buffer ()
  "Pop to dvc-partner-buffer, if available."
  (interactive)
  (if (and (boundp 'dvc-partner-buffer) dvc-partner-buffer)
      (if (buffer-live-p dvc-partner-buffer)
          (pop-to-buffer dvc-partner-buffer)
        (message "Partner buffer has been killed"))
    (message "No partner buffer set for this buffer.")))


(defmacro dvc-with-keywords (keywords plist &rest body)
  "Execute a body of code with keywords bound.
Each keyword listed in KEYWORDS is bound to its value from PLIST, then
BODY is evaluated."
  (declare (indent 1) (debug (sexp sexp body)))
  (flet ((keyword-to-symbol (keyword)
                            (intern (substring (symbol-name keyword) 1))))
    (let ((keyword (make-symbol "keyword"))
          (default (make-symbol "default")))
      `(let ,(mapcar (lambda (keyword-entry)
                       (keyword-to-symbol (if (consp keyword-entry)
                                              (car keyword-entry)
                                            keyword-entry)))
                     keywords)
         (dolist (keyword-entry ',keywords)
           (let ((,keyword (if (consp keyword-entry)
                               (car keyword-entry)
                             keyword-entry))
                 (,default (if (consp keyword-entry)
                               (cadr keyword-entry)
                             nil)))
             (set (intern (substring (symbol-name ,keyword) 1))
                  (or (cadr (member ,keyword ,plist))
                      ,default))))
         ,@body))))


;; ----------------------------------------------------------------------------
;; Process management
;; ----------------------------------------------------------------------------

;; Candidates for process handlers
(defun dvc-default-error-function (output error status arguments)
  "Default function called when a DVC process ends with a non-zero status.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (if (> (with-current-buffer error (point-max)) 1)
      (dvc-show-error-buffer error)
    (if (> (with-current-buffer output (point-max)) 1)
        (dvc-show-error-buffer output)
      (error "`%s %s' failed with code %d and no output!"
             (dvc-current-executable)
             (mapconcat 'identity arguments " ")
             status)))
  (error "`%s %s' failed with code %d"
         (dvc-current-executable)
         (mapconcat 'identity arguments " ")
         status))

(defvar dvc-default-killed-function-noerror 0
  "The number of killed processes we will ignore until throwing an error.
If the value is 0, `dvc-default-killed-function' will throw an error.
See `dvc-default-killed-function'.")

(defun dvc-default-killed-function (output error status arguments)
  "Default function called when a DVC process is killed.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (if (> dvc-default-killed-function-noerror 0)
      (setq dvc-default-killed-function-noerror
            (- dvc-default-killed-function-noerror 1))
    (dvc-switch-to-buffer error)
    (error "`%s %s' process killed !"
           (dvc-current-executable)
           (mapconcat 'identity arguments " "))))

(defun dvc-null-handler (output error status arguments)
  "Handle a finished process without doing anything.
Candidate as an argument for one of the keywords :finished, :error or :killed
in `dvc-run-dvc-sync' or `dvc-run-dvc-async'.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  nil)

(defun dvc-status-handler (output error status arguments)
  "Return an integer value that reflects the process status.
Candidate as an argument for one of the keywords :finished, :error or :killed
in `dvc-run-dvc-sync' or `dvc-run-dvc-async'.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (cond ((numberp status) status)
        ((string-match "^exited abnormally with code \\(.*\\)" status)
         (string-to-number (match-string 1)))
        (t (error status))))

(defun dvc-output-buffer-handler (output error status arguments)
  "Return the output of a finished process, stripping any trailing newline.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (dvc-buffer-content output))

(defun dvc-output-buffer-handler-withnewline (output error status arguments)
  "Same as dvc-output-buffer-handler, but keep potential final newline."
  (with-current-buffer output (buffer-string)))

(defun dvc-output-and-error-buffer-handler (output error status arguments)
  "Return the output of a finished process, stripping any trailing newline.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (concat (dvc-buffer-content output)
          (dvc-buffer-content error)))

(defun dvc-output-buffer-split-handler (output error status arguments)
  "Return the output of a finished process as a list of lines.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (split-string (dvc-buffer-content output) "\n"))

(defun dvc-default-finish-function (output error status arguments)
  "Default function called when a DVC process terminates.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called with."
  (let ((has-output))
    (with-current-buffer output
      (dvc-process-buffer-mode)
      (setq has-output (> (point-max) 1)))
    (when has-output
      (dvc-switch-to-buffer output))
    (when (or dvc-debug has-output)
      (message "Process `%s %s' finished"
               (dvc-current-executable)
               (mapconcat 'identity arguments " ")))
    status))

(defun dvc-finish-function-without-buffer-switch (output error status arguments)
  "Similar to `dvc-default-finish-function' but no buffer switch.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
STATUS indicates the return status of the program.
ARGUMENTS is a list of the arguments that the process was called
  with."
  (with-current-buffer output
    (dvc-trace "Process `%s %s' finished"
               (dvc-current-executable)
               (mapconcat 'identity arguments " "))
    status))

(defvar dvc-process-running nil
  "List of DVC processes running.
A value of nil indicates no processes are running.

The list is a list of pairs (process event) where EVENT is the event
corresponding to the beginning of the execution of process.  It can be
used to get more info about the process.")

(defun dvc-build-dvc-command (dvc list-args)
  "Build a shell command to run DVC with args LIST-ARGS.
DVC can be one of 'baz, 'xhg, ..."
  (let ((executable (executable-find (dvc-variable dvc "executable"))))
    ;; 'executable-find' allows leading ~
    (if (not executable)
        (error "executable for %s not found" (symbol-name dvc)))
    (mapconcat 'shell-quote-argument
               (cons executable
                     (remq nil list-args))
               " ")))

(defcustom dvc-password-prompt-regexp
  "[Pp]ass\\(word\\|phrase\\).*:\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process."
  :type 'regexp
  :group 'dvc)

(defun dvc-process-filter (proc string &optional no-insert)
  "Filter PROC's STRING.
Prompt for password with `read-passwd' if the output of PROC matches
`dvc-password-prompt-regexp'.

If NO-INSERT is non-nil, do not insert the string.

In all cases, a new string is returned after normalizing newlines."
  (with-current-buffer (process-buffer proc)
    (setq string (replace-regexp-in-string "\015" "\n" string))
    (unless no-insert
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point)))
    (when (string-match dvc-password-prompt-regexp string)
      (string-match "^\\([^\n]+\\)\n*\\'" string)
      (let ((passwd (read-passwd (match-string 1 string))))
        (process-send-string proc (concat passwd "\n"))))
    string))

(defun dvc-prepare-environment (env)
  "By default, do not touch the environment"
  env)

(defun dvc-default-global-argument ()
  "By default, no global argument."
  nil)

(defun dvc-run-dvc-async (dvc arguments &rest keys)
  "Run a process asynchronously.
Current directory for the process is the current `default-directory'.
ARGUMENTS is a list of arguments.  nil values in this list are removed.
KEYS is a list of keywords and values.  Possible keywords are:

 :finished ....... Function run when the process finishes.  If none
                   specified, `dvc-default-finish-function' is run.

 :killed ......... Function run when the process is killed.  If none
                   specified, `dvc-default-killed-function' is run.

 :error .......... Function run when the process exits with a non 0
                   status.  If none specified,
                   `dvc-default-error-function' is run.

All these functions take 4 arguments : output, error, status, and
arguments.

   - \"output\" is the output buffer
   - \"error\" is the buffer where standard error is redirected
   - \"status\" is the numeric exit-status or the signal number
   - \"arguments\" is the list of arguments, as a list of strings,
              like '(\"changes\" \"--diffs\")

   `dvc-null-handler' can be used here if there's nothing to do.

 :filter           Function to call every time we receive output from
                   the process.  It should take arguments proc and string.
                   The string will have been run through
                   `dvc-process-filter' to deal with password prompts and
                   newlines.

 :output-buffer .. Buffer where the output of the process should be
                   redirected.  If none specified, a new one is
                   created, and will be entered in
                   `dvc-dead-process-buffer-queue' to be killed
                   later.

 :error-buffer ... Buffer where the standard error of the process
                   should be redirected.

 :related-buffer . Defaults to `current-buffer'.  This is the buffer
                   where the result of the process will be used.  If
                   this buffer is killed before the end of the
                   execution, the user is prompted if he wants to kill
                   the process."
  (dvc-with-keywords
      (:finished :killed :error :filter
                 :output-buffer :error-buffer :related-buffer)
    keys
    (let* ((output-buf (or (and output-buffer
                                (get-buffer-create output-buffer))
                           (dvc-new-process-buffer nil dvc)))
           (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                           (dvc-new-error-buffer nil dvc)))
           (error-file (dvc-make-temp-name "dvc-errors"))
           (global-arg (funcall (dvc-function dvc "default-global-argument")))
           (command (dvc-build-dvc-command
                     dvc (append global-arg arguments)))
           ;; Make the `default-directory' unique. The trailing slash
           ;; may be necessary in some cases.
           (default-directory (dvc-uniquify-file-name default-directory))
           (process
            (let ((process-environment
                   (funcall (dvc-function dvc "prepare-environment")
                            process-environment)))
              (with-current-buffer output-buf
                ;; process filter will need to know which dvc to run
                ;; if there is a choice
                (setq dvc-buffer-current-active-dvc dvc))

              ;; `start-process' sends both stderr and stdout to
              ;; `output-buf'. But we want to keep stderr separate. So
              ;; we use a shell to redirect stderr before Emacs sees
              ;; it. Note that this means we require "sh" even on
              ;; MS Windows.
              (start-process
               (dvc-variable dvc "executable") output-buf
               dvc-sh-executable "-c"
               (format "%s 2> %s"
                       command error-file))))
           (process-event
            (list process
                  (dvc-log-event output-buf
                                 error-buf
                                 command
                                 default-directory "started"))))
      (with-current-buffer (or related-buffer (current-buffer))
        (dvc-trace "Running process `%s' in `%s'" command default-directory)
        (add-to-list 'dvc-process-running process-event)
        (set-process-filter
         process
         (if (not filter)
             'dvc-process-filter
           (dvc-capturing-lambda (proc string)
             (funcall (capture filter)
                      proc
                      (dvc-process-filter proc string t)))))
        (set-process-sentinel
         process
         (dvc-capturing-lambda (process event)
           (let ((default-directory (capture default-directory)))
             (dvc-log-event (capture output-buf) (capture error-buf)
                            (capture command)
                            (capture default-directory)
                            (dvc-strip-final-newline event))
             (setq dvc-process-running
                   (delq (capture process-event) dvc-process-running))
             (when (file-exists-p (capture error-file))
               (with-current-buffer (capture error-buf)
                 (insert-file-contents (capture error-file)))
               (delete-file (capture error-file)))
             (let ((state (process-status process))
                   (status (process-exit-status process))
                   (dvc-temp-current-active-dvc (capture dvc)))
               (unwind-protect
                   (cond ((and (eq state 'exit) (= status 0))
                          (funcall (or (capture finished)
                                       'dvc-default-finish-function)
                                   (capture output-buf) (capture error-buf)
                                   status (capture arguments)))
                         ((eq state 'signal)
                          (funcall (or (capture killed)
                                       'dvc-default-killed-function)
                                   (capture output-buf) (capture error-buf)
                                   status (capture arguments)))
                         ((eq state 'exit) ;; status != 0
                          (funcall (or (capture error)
                                       'dvc-default-error-function)
                                   (capture output-buf) (capture error-buf)
                                   status (capture arguments)))))
               ;; Schedule any buffers we created for killing
               (unless (capture output-buffer)
                 (dvc-kill-process-buffer (capture output-buf)))
               (unless (capture error-buffer)
                 (dvc-kill-process-buffer (capture error-buf)))))))
        process))))

(defun dvc-run-dvc-sync (dvc arguments &rest keys)
  "Run DVC synchronously.
See `dvc-run-dvc-async' for details on possible ARGUMENTS and KEYS."
  (dvc-with-keywords
      (:finished :killed :error :output-buffer :error-buffer :related-buffer)
    keys
    (let* ((output-buf (or (and output-buffer
                                (get-buffer-create output-buffer))
                           (dvc-new-process-buffer t dvc)))
           (error-buf  (or (and error-buffer (get-buffer-create error-buffer))
                           (dvc-new-error-buffer t dvc)))
           (global-arg (funcall (dvc-function dvc "default-global-argument")))
           (command (dvc-build-dvc-command
                     dvc (append global-arg arguments)))
           (arguments (remq nil arguments))
           (error-file (dvc-make-temp-name "arch-errors"))
           ;; Make the `default-directory' unique. The trailing slash
           ;; may be necessary in some cases.
           (default-directory (dvc-uniquify-file-name default-directory)))
      (with-current-buffer (or related-buffer (current-buffer))
        (dvc-log-event output-buf error-buf command default-directory
                       "started")
        (let ((status (let ((process-environment
                             (funcall (dvc-function dvc "prepare-environment")
                                      process-environment)))
                        (call-process dvc-sh-executable nil output-buf nil "-c"
                                      (format "%s 2> %s"
                                              command
                                              error-file)))))
          (when (file-exists-p error-file)
            (with-current-buffer error-buf
              (insert-file-contents error-file))
            (delete-file error-file))
          (unwind-protect
              (let ((dvc-temp-current-active-dvc dvc))
                (cond ((stringp status)
                       (when (string= status "Terminated")
                         (funcall (or killed 'dvc-default-killed-function)
                                  output-buf error-buf status arguments)))
                      ((numberp status)
                       (if (zerop status)
                           (funcall (or finished 'dvc-default-finish-function)
                                    output-buf error-buf status arguments)
                         (funcall (or error 'dvc-default-error-function)
                                  output-buf error-buf status arguments)))
                      (t (message "Unknown status - %s" status))))
            ;; Schedule any buffers we created for killing
            (unless output-buffer (dvc-kill-process-buffer output-buf))
            (unless error-buffer (dvc-kill-process-buffer error-buf))))))))

(defun dvc-processes-related-to-buffer (buffer)
  "Returns a list of DVC process whose related buffer is BUFFER."
  (let ((accu nil))
    (dolist (entry dvc-process-running)
      (when (eq (dvc-event-related-buffer (cadr entry)) buffer)
        (push (car entry) accu)))
    (setq accu (nreverse accu))
    accu))

(defun dvc-kill-process-maybe (buffer)
  "Prompts and possibly kill process whose related buffer is BUFFER."
  ;; FIXME: It would be reasonable to run this here, to give any
  ;;  process one last chance to run. But somehow this screws up
  ;;  package-maint-clean-some-elc. (accept-process-output)
  (let* ((processes (dvc-processes-related-to-buffer buffer))
         (l (length processes)))
    (when (and processes
               (y-or-n-p (format "%s process%s running in buffer %s.  Kill %s? "
                                 l (if (= l 1) "" "es")
                                 (buffer-name buffer)
                                 (if (= l 1) "it" "them"))))
      (dolist (process processes)
        (when (eq (process-status process) 'run)
          (incf dvc-default-killed-function-noerror)
          (kill-process process)))))
  ;; make sure it worked
  (let ((processes (dvc-processes-related-to-buffer buffer)))
    (when processes
      (error "Process still running in buffer %s" buffer))))

(add-hook 'kill-buffer-hook 'dvc-kill-buffer-function)

(defun dvc-kill-buffer-function ()
  "Function run when a buffer is killed."
  (dvc-buffers-tree-remove (current-buffer))
  (dvc-kill-process-maybe (current-buffer)))

(defun dvc-run-dvc-display-as-info (dvc arg-list &optional show-error-buffer info-string asynchron)
  "Call either `dvc-run-dvc-async' or `dvc-run-dvc-sync' and display the result in an info buffer.
When INFO-STRING is given, insert it at the buffer beginning."
  (let ((buffer (dvc-get-buffer-create dvc 'info)))
    (funcall (if asynchron 'dvc-run-dvc-async 'dvc-run-dvc-sync) dvc arg-list
             :finished
             (dvc-capturing-lambda (output error status arguments)
               (progn
                 (with-current-buffer (capture buffer)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (dvc-info-buffer-mode)
                     (when (capture info-string)
                       (insert (capture info-string)))
                     (insert-buffer-substring output)
                     (when (capture show-error-buffer)
                       (insert-buffer-substring error))
                     (toggle-read-only 1)))
                 (dvc-switch-to-buffer (capture buffer)))))))

(defvar dvc-info-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in a dvc info buffer.")

(define-derived-mode dvc-info-buffer-mode fundamental-mode
  "DVC info mode"
  "Major mode for dvc info buffers"
  (dvc-install-buffer-menu)
  (toggle-read-only 1))


(defvar dvc-log-cookie nil)

(defstruct (dvc-event) output-buffer error-buffer related-buffer
  command tree event time)

(defsubst dvc-log-printer-print-buffer (buffer function)
  "Helper function for `dvc-log-printer'.
Print a buffer filed for BUFFER during printing a log event.
The printed name of BUFFER is mouse sensitive.  If the user
clicks it, FUNCTION is invoked."
  (let ((alive-p (buffer-live-p buffer))
        map)
    (dvc-face-add
     (or
      ;; pp-to-string is very costly.
      ;; Handle the typical case with hard-coding.
      (unless alive-p "#<killed buffer>")
      ;; Normal case.
      (buffer-name buffer)
      ;; Extra case.
      (pp-to-string buffer))
     'dvc-buffer
     (when alive-p
       (setq map (make-sparse-keymap))
       (define-key map [mouse-2] function)
       map)
     nil
     "Show the buffer")))

(defun dvc-log-recently-p (elem limit-minute)
  "Check ELEM recorded a recent event or not.
Return nil If ELEM recorded an event older than LIMIT-MINUTE.
Else return t."
  (let* ((recorded (dvc-event-time elem))
         (cur      (current-time))
         (diff-minute (/ (+ (* 65536 (- (nth 0 cur)
                                        (nth 0 recorded)))
                            (- (nth 1 cur)
                               (nth 1 recorded)))
                         60)))
    (if (> limit-minute diff-minute)
        t
      nil)))

(defun dvc-log-printer (elem)
  "Arch event printer which prints ELEM."
  (let ((event (dvc-event-event elem))
        (p (point)))
    (insert
     "Command: " (dvc-event-command elem)
     "\nDirectory: " (dvc-face-add (or (dvc-event-tree elem) "(nil)")
                                   'dvc-local-directory)
     "\nDate: " (format-time-string "%c" (dvc-event-time elem))
     "\nRelated Buffer: " (dvc-log-printer-print-buffer
                           (dvc-event-related-buffer elem)
                           'dvc-switch-to-related-buffer-by-mouse)
     "\nOutput Buffer: "  (dvc-log-printer-print-buffer
                           (dvc-event-output-buffer elem)
                           'dvc-switch-to-output-buffer-by-mouse)
     "\nError Buffer: "   (dvc-log-printer-print-buffer
                           (dvc-event-error-buffer elem)
                           'dvc-switch-to-error-buffer-by-mouse)
     (if (not (string= event "started"))
         (concat "\nEvent: " event)
       "")
     "\n")
    ;; Reflect the point to `default-directory'.
    ;; NOTE: XEmacs doesn't have `point-entered' special text property.
    (put-text-property
     p (point)
     'point-entered (lambda (old new)
                      (setq default-directory
                            (dvc-event-tree
                             (ewoc-data
                              (ewoc-locate dvc-log-cookie))))))))

(defmacro dvc-switch-to-buffer-macro (function accessor)
  "Define a FUNCTION for switching to the buffer associated with some event.
ACCESSOR is a function for retrieving the appropriate buffer from a
`dvc-event' structure."
  `(defun ,function ()
     "In a log buffer, pops to the output or error buffer corresponding to the
process at point"
     (interactive)
     (let ((buffer (,accessor
                    (ewoc-data (ewoc-locate dvc-log-cookie)))))
       (cond ((buffer-live-p buffer)
              (dvc-switch-to-buffer buffer)
              (unless (member buffer
                              (mapcar (lambda (p)
                                        (process-buffer (car p)))
                                      dvc-process-running))
                (dvc-process-buffer-mode)))
             (t (error "Buffer has been killed"))))))

(dvc-switch-to-buffer-macro dvc-switch-to-output-buffer
                            dvc-event-output-buffer)

(dvc-switch-to-buffer-macro dvc-switch-to-error-buffer
                            dvc-event-error-buffer)

(dvc-switch-to-buffer-macro dvc-switch-to-related-buffer
                            dvc-event-related-buffer)

(dvc-make-bymouse-function dvc-switch-to-output-buffer)
(dvc-make-bymouse-function dvc-switch-to-error-buffer)
(dvc-make-bymouse-function dvc-switch-to-related-buffer)

(defun dvc-log-event (output error command tree event)
  "Log an event in the `dvc-log-buffer' buffer.
OUTPUT is the buffer containing process standard output.
ERROR is the buffer containing process error output.
COMMAND is the command that was executed.
TREE is the process's working directory.
EVENT is the event that occurred.
Returns that event."
  (unless (and dvc-log-cookie
               (buffer-live-p (ewoc-buffer dvc-log-cookie)))
    (with-current-buffer (get-buffer-create dvc-log-buffer)
      (setq dvc-log-cookie
            (ewoc-create (dvc-ewoc-create-api-select
                          #'dvc-log-printer)))
      (dvc-log-buffer-mode)))
  (let ((related-buffer (current-buffer)))
    (with-current-buffer (ewoc-buffer dvc-log-cookie)
      (let ((elem (make-dvc-event :output-buffer output
                                  :error-buffer error
                                  :related-buffer related-buffer
                                  :command command
                                  :tree tree
                                  :event event
                                  :time (current-time)))
            buffer-read-only)
        (ewoc-enter-last dvc-log-cookie elem)
        ;; If an event is too old (30 minutes after it has been
        ;; recorded), throw it away.
        (ewoc-filter dvc-log-cookie 'dvc-log-recently-p 30)
        (ewoc-refresh dvc-log-cookie)
        elem))))

(defun dvc-log-next ()
  "Move to the next log entry."
  (interactive)
  (let ((next (ewoc-next dvc-log-cookie
                         (ewoc-locate dvc-log-cookie))))
    (when next (goto-char (ewoc-location next)))))

(defun dvc-log-prev ()
  "Move to the previous log entry."
  (interactive)
  (let ((prev (ewoc-prev dvc-log-cookie
                         (ewoc-locate dvc-log-cookie))))
    (when prev (goto-char (ewoc-location prev)))))

;;
;; Log buffer mode section
;;
(defvar dvc-log-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [?o] 'dvc-switch-to-output-buffer)
    (define-key map "\C-m" 'dvc-switch-to-output-buffer)
    (define-key map [?e] 'dvc-switch-to-error-buffer)
    (define-key map [?r] 'dvc-switch-to-related-buffer)
    (define-key map [?n] 'dvc-log-next)
    (define-key map [?p] 'dvc-log-prev)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in DVC's log buffer.")

(define-derived-mode dvc-log-buffer-mode fundamental-mode "DVC Log"
  "Major mode for DVC's internal log buffer. You can open this buffer
with `dvc-open-internal-log-buffer'."
  (toggle-read-only 1))

(defun dvc-open-internal-log-buffer ()
  "Switch to the DVC's internal log buffer.
This buffer contains a list of all the DVC commands previously executed.
The buffer uses the mode `dvc-log-buffer-mode'"
  (interactive)
  (let ((buffer-name (buffer-name)))
    (dvc-switch-to-buffer dvc-log-buffer)
    (goto-char (point-max))
    (when (re-search-backward (concat " Buffer: "
                                      (regexp-quote buffer-name)
                                      "$")
                              nil t)
      (dvc-flash-line))))

(defun dvc-clear-log-buffer ()
  "Kill the log buffer."
  (when (bufferp (get-buffer dvc-log-buffer))
    (kill-buffer dvc-log-buffer)))

(defun dvc-get-process-output ()
  "Return the content of the last process buffer.
Strips the final newline if there is one."
  (dvc-buffer-content dvc-last-process-buffer))

(defun dvc-get-error-output ()
  "Return the content of the last error buffer.
Strips the final newline if there is one."
  (dvc-buffer-content dvc-last-error-buffer))


;; TODO: per backend cound.
(add-to-list 'minor-mode-alist
             '(dvc-process-running
               (:eval (if (equal (length dvc-process-running) 1)
                          " DVC running"
                        (concat " DVC running("
                                (int-to-string (length dvc-process-running))
                                ")")))))

(defun dvc-log-edit-file-name ()
  "Return a suitable file name to edit the commit message"
  ;; FIXME: replace this with define-dvc-unified-command
  (dvc-call "dvc-log-edit-file-name-func"))

(defun dvc-dvc-log-edit-file-name-func ()
  (concat (file-name-as-directory (dvc-tree-root))
          (dvc-variable (dvc-current-active-dvc)
                        "log-edit-file-name")))

;;
;; Revision manipulation
;;

;; revision grammar is specified in ../docs/DVC-API

;; accessors
(defun dvc-revision-get-dvc (revision-id)
  (car revision-id))

(defun dvc-revision-get-type (revision-id)
  (car (nth 1 revision-id)))

(defun dvc-revision-get-data (revision-id)
  (cdr (nth 1 revision-id)))

(defun dvc-revision-to-string (revision-id &optional prev-format orig-str)
  "Return a string representation for REVISION-ID.

If PREV-FORMAT is specified, it is the format string to use for
entries that are before the given revision ID.  The format string
should take two parameters.  The first is the revision ID, and
the second is a number which indicates how many generations back
to travel.

If ORIG-STR is specified, it is the string that indicates the
current revision of the working tree."
  (let* ((type (dvc-revision-get-type revision-id))
         (data (dvc-revision-get-data revision-id)))
    ;;(dvc-trace "dvc-revision-to-string: type: %s, data: %s, orig-str: %s" type data orig-str)
    (case type
      (revision (dvc-name-construct (nth 0 data)))
      (local-tree (car data))
      (last-revision (or orig-str "original"))
      (previous-revision
       (format (or prev-format "%s:-%s")
               (dvc-revision-to-string
                (list (dvc-revision-get-dvc revision-id) (nth 0 data)))
               (int-to-string (nth 1 data))))
      (t "UNKNOWN"))))

(defun dvc-revision-get-buffer (file revision-id)
  "Return an empty buffer suitable for viewing FILE in REVISION-ID.

The name of the buffer is chosen according to FILE and REVISION-ID.

REVISION-ID may have the values described in docs/DVC-API."
  (let* ((type (dvc-revision-get-type revision-id))
         (name (concat
                (file-name-nondirectory file)
                "(" (dvc-revision-to-string revision-id) ")")))
    ;; replace / by | to work around uniquify
    (setq name (replace-regexp-in-string "\\/" "|" name))
    (let ((buffer (generate-new-buffer name)))
      (with-current-buffer buffer
        (let ((buffer-file-name file))
          (set-auto-mode t)))
      (dvc-buffers-tree-add (dvc-revision-get-dvc revision-id) type file buffer)
      buffer)))


(defun dvc-revision-get-file-in-buffer (file revision-id)
  "Return a buffer with the content of FILE at REVISION-ID.

REVISION-ID is as specified in docs/DVC-API."
  (dvc-trace "dvc-revision-get-file-in-buffer. revision-id=%S" revision-id)
  (let* ((type (dvc-revision-get-type revision-id))
         (inhibit-read-only t)
         ;; find-file-noselect will call dvc-current-active-dvc in a
         ;; hook; specify dvc for dvc-call
         (dvc-temp-current-active-dvc (dvc-revision-get-dvc revision-id))
         (buffer (unless (eq type 'local-tree) (dvc-revision-get-buffer file revision-id))))
    (case type
      (local-tree (find-file-noselect file))

      (revision
       (with-current-buffer buffer
         (dvc-call "revision-get-file-revision"
                   file (dvc-revision-get-data revision-id))
         (set-buffer-modified-p nil)
         (toggle-read-only 1)
         buffer))

      (previous-revision
       (with-current-buffer buffer
         (let* ((dvc (dvc-revision-get-dvc revision-id))
                (data (nth 0 (dvc-revision-get-data revision-id)))
                (rev-id (list dvc data)))
           (dvc-call "revision-get-previous-revision" file rev-id))
         (set-buffer-modified-p nil)
         (toggle-read-only 1)
         buffer))

      (last-revision
       (with-current-buffer buffer
         (dvc-call "revision-get-last-revision"
                   file (dvc-revision-get-data revision-id))
         (set-buffer-modified-p nil)
         (toggle-read-only 1)
         buffer))

      (t (error "TODO: dvc-revision-get-file-in-buffer type %S" type)))))

(defun dvc-dvc-revision-nth-ancestor (revision n)
  "Default function to get the n-th ancestor of REVISION."
  (let ((count n)
        (res revision))
    (while (> count 0)
      (setq res (dvc-revision-direct-ancestor res)
            count (- count 1)))
    res))

;;
;; DVC command version
;;
(defun dvc-dvc-command-version ()
  "Fallback for `dvc-command-vesion'. Returns just `nil'.
This function is called only if the current backend doesn't
implement `command-version' function."
  nil)

(provide 'dvc-core)
;;; dvc-core.el ends here

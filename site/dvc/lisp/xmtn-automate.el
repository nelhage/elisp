;;; xmtn-automate.el --- Interface to monotone's "automate" functionality

;; Copyright (C) 2008 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; This library provides access to monotone's "automate" interface
;; from Emacs Lisp.
;;
;; I found monotone's automate stdio mode (see
;; http://www.venge.net/monotone/docs/Automation.html for details)
;; rather intriguing, so I tried to make full use of it.  I don't know
;; whether it is really significantly more efficient than spawning a
;; new subprocess for each command.  But, in theory, feeding multiple
;; commands to one process allows that process to do all kinds of
;; smart caching, so it could make very large differences, even
;; differences in orders of magnitude.  I don't know whether monotone
;; currently does any caching, but at least this means we have an
;; excuse for not doing any caching in Emacs.  (If it becomes clear
;; that caching would be a good idea, it can be implemented in
;; monotone instead of Emacs; this way, other front-ends to monotone
;; can also benefit from it.)
;;
;; To allow xmtn-automate to track how long an automate stdio process
;; needs to be kept around, we introduce the concept of a session.  To
;; the programmer using this library, a session is an opaque object
;; that is needed to run automate commands.  Each session is
;; associated with a monotone workspace ("root") that the commands
;; will operate on.  (Using xmtn-auomate to run commands with no
;; workspace is not currently part of the design.)  A session can be
;; obtained using `xmtn-automate-with-session' and has dynamic extent.
;; Note that `xmtn-automate-with-session' doesn't necessarily start a
;; fresh monotone process; xmtn-automate may reuse existing session
;; objects and processes, or launch the process only when the first
;; command is sent to the session.  There is also no guarantee about
;; how long xmtn-automate will keep the process running after
;; `xmtn-automate-with-session' exits.  (The function
;; `xmtn-automate-terminate-processes-in-root' can be used to tell
;; xmtn-automate to terminate all processes in a given root as soon as
;; possible, and wait until they terminate.  I imagine this could be
;; necessary to free locks, but whether mtn automate stdio does any
;; locking doesn't seem to be specified in monotone's manual.)  To put
;; it another way, the mapping between `xmtn-automate-with-session'
;; forms and monotone processes is not necessarily one-to-one.
;;
;; `xmtn-automate-with-session' forms can safely be nested.
;;
;; Once you have a session object, you can use
;; `xmtn-automate-with-command' forms to send commands to monotone.
;; Each such form gets you a so-called command-handle.  Again, this is
;; an opaque object with dynamic extent.  You can use this handle to
;; check the error code of the command and obtain its output.  Your
;; Emacs Lisp code can also do other computation while the monotone
;; command runs.  Allowing this kind of parallelism and incremental
;; processing of command output is the main reason for introducing
;; command handles.
;;
;; The following operations are defined on command handles.
;;
;;   * xmtn-automate-command-error-code (command-handle) --> 0, 1 or 2
;;
;;     Returns the error code of the command.  See monotone
;;     documentation.  This operation blocks until the monotone process
;;     has sent the error code.
;;
;;   * xmtn-automate-command-wait-until-finished (command-handle) -->
;;     nil
;;
;;     Blocks until the command has finished (successfully or not).
;;     After this operation returns, `xmtn-automate-command-finished-p'
;;     will return true for this command.
;;
;;   * xmtn-automate-command-buffer (command-handle) --> buffer
;;
;;     Returns the so-called command buffer associated with the command
;;     handle.  This is a buffer with the output that the command has
;;     generated so far.  The buffer contents will be updated as new
;;     output arrives.  The buffer has the same extent as the command
;;     handle.  This operation does not block.
;;
;;   * xmtn-automate-command-write-marker-position (command-handle)
;;     --> position
;;
;;     The position in the output buffer after the last character of
;;     output the command has generated so far.  This is also where new
;;     output will be inserted.  This operation does not block.
;;
;;   * xmtn-automate-command-finished-p (command-handle) --> boolean
;;
;;     Returns nil if the command is still running, non-nil if it has
;;     finished (successfully or not).  If this function returns non-nil,
;;     the full output of the command is available in the command buffer.
;;     This operation does not block.
;;
;;   * xmtn-automate-command-accept-output (command-handle) -->
;;     output-received-p
;;
;;     Allows Emacs to process more output from the command (and
;;     possibly from other processes).  Blocks until more output has
;;     been received from the command or the command has finished.
;;     Returns non-nil if more output has been received.
;;
;; The intention behind this protocol is to allow Emacs Lisp code to
;; process command output incrementally as it arrives instead of
;; waiting until it is complete.  However, for xmtn-basic-io, the
;; bookkeeping overhead for this kind of pipelining was excessive --
;; byte-compiled Emacs Lisp is rather slow.  But I didn't try very
;; hard to tune it, either.  So I'm not sure whether incremental
;; processing is useful.
;;
;; In the output buffer, the "chunking" (the <command number>:<err
;; code>:<last?>:<size>:<output> thing) that monotone automate stdio does
;; has already been decoded and removed.  However, no other processing or
;; parsing has been done.  The output buffer contains raw 8-bit data.
;;
;; Different automate commands generate data in different formats: For
;; example, get_manifest generates basic_io; select generates a list
;; of lines with one ID each, graph generates a list of lines with one
;; or more IDs each; inventory and the packet_* commands generate
;; different custom line-based formats; and get_file generates binary
;; output.  Parsing these formats is not part of xmtn-automate.
;;
;; You shouldn't manually kill the output buffer; xmtn-automate will take
;; care of it when the `xmtn-automate-with-command' form exits.
;;
;; Example:
;;
;; (xmtn-automate-with-session (session "/path/to/workspace")
;;   ;; The variable `session' now holds a session object associated
;;   ;; with the workspace.
;;   (xmtn-automate-with-command (handle session '("get_base_revision_id"))
;;     ;; The variable `handle' now holds a command handle.
;;     ;; Check that the command was successful (not described above);
;;     ;; generate a default error message otherwise and abort.
;;     (xmtn-automate-command-check-for-and-report-error handle)
;;     ;; Wait until the entire output of the command has arrived.
;;     (xmtn-automate-command-wait-until-finished handle)
;;     ;; Process output (in command buffer).
;;     (message "Base revision id is %s"
;;              (with-current-buffer (xmtn-automate-command-buffer handle)
;;                (buffer-substring (point-min)
;;                                  ;; Ignore final newline.
;;                                  (1- (point-max)))))))
;;
;; There are some utility functions built on top of this general
;; interface that help express common uses more concisely; for
;; example,
;;
;; (message "Base revision id is %s"
;;          (xmtn-automate-simple-command-output-line
;;           "/path/to/workspace" '("get_base_revision_id")))
;;
;; does the same thing as the above code.
;;
;; If multiple "simple" automate commands are run in succession on the
;; same workspace, it's a good idea to wrap an
;; `xmtn-automate-with-session' form around them so xmtn knows that it
;; should reuse the same process.
;;
;; (xmtn-automate-with-session (nil "/path/to/workspace")
;;   (message "Base revision id is %s, current revision is %s"
;;            (xmtn-automate-simple-command-output-line
;;             "/path/to/workspace" '("get_base_revision_id"))
;;            (xmtn-automate-simple-command-output-line
;;             "/path/to/workspace" '("get_current_revision_id")))
;;
;; Here, the session object is not explicitly passed to the functions
;; that actually feed commands to monotone.  But, since the containing
;; session is still open after the first command, xmtn knows that it
;; should keep the process alive, and it is smart enough to reuse the
;; process for the second command.
;;
;; The fact that `xmtn-automate-with-command' always forces commands
;; to either happen in sequence or properly nested can be a
;; limitation.  For example, it's not possible to write a
;; (non-recursive) loop that runs N automate commands and processes
;; their output, always launching the (k+1)th automate command ahead
;; of time to run in parallel with the kth iteration.  (Some of the
;; revlist and cert-parsing code really wants to do this, I think.)
;; (But maybe writing this recursively wouldn't be all that bad...  It
;; is asymptotically less (stack-!)space-efficient but makes it
;; impossible to get the cleanup wrong.)  Providing the two halves of
;; `xmtn-automate-with-command' as two functions
;; `xmtn-automate-open-command' and `xmtn-automate-close-command' that
;; always need to be called in pairs would be more flexible.  (Common
;; Lisp also has with-open-file but also open and close.)

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'parse-time)                 ;for parse-integer
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-compat))

(defun xmtn-automate-command-error-code (command)
  (let ((process (xmtn-automate--session-process
                  (xmtn-automate--command-handle-session command))))
    (while (null (xmtn-automate--command-handle-error-code command))
      (xmtn--assert-for-effect
       (accept-process-output process))))
  (xmtn-automate--command-handle-error-code command))

(defun xmtn-automate-command-buffer (command)
  (xmtn-automate--command-handle-buffer command))

(defun xmtn-automate-command-write-marker-position (command)
  (marker-position (xmtn-automate--command-handle-write-marker command)))

(defun xmtn-automate-command-accept-output (command)
  (let ((previous-write-marker-position
         (marker-position (xmtn-automate--command-handle-write-marker
                           command))))
    (while (and (= (marker-position (xmtn-automate--command-handle-write-marker
                                     command))
                   previous-write-marker-position)
                (not (xmtn-automate--command-handle-finished-p command)))
      (xmtn--assert-for-effect
       (accept-process-output
        (xmtn-automate--session-process
         (xmtn-automate--command-handle-session command)))))
    (> (marker-position (xmtn-automate--command-handle-write-marker
                         command))
       previous-write-marker-position)))

(defun xmtn-automate-command-finished-p (command)
  (xmtn-automate--command-handle-finished-p command))

(defun xmtn-automate-command-wait-until-finished (handle)
  (while (not (xmtn-automate-command-finished-p handle))
    (xmtn--assert-for-effect (or (xmtn-automate-command-accept-output handle)
                                 (xmtn-automate-command-finished-p handle))))
  nil)

(defvar xmtn-automate--*sessions* '())

(defmacro* xmtn-automate-with-session ((session-var-or-null root-form &key)
                                       &body body)
  (declare (indent 1) (debug (sexp body)))
  ;; I would prefer to factor out a function
  ;; `xmtn-automate--call-with-session' here, but that would make
  ;; profiler output unreadable, since every function would only
  ;; appear to call `xmtn-automate--call-with-session', and that
  ;; function would appear to do all computation.
  ;;
  ;; mtn automate stdio requires a valid database, so we require a
  ;; root directory here.
  (let ((session (gensym))
        (session-var (or session-var-or-null (gensym)))
        (root (gensym))
        (key (gensym))
        (thunk (gensym)))
    `(let* ((,root (file-name-as-directory ,root-form))
            (,key (file-truename ,root))
            (,session (cdr (assoc ,key xmtn-automate--*sessions*)))
            (,thunk (lambda ()
                      (let ((,session-var ,session))
                        ,@body))))
       (if ,session
           (funcall ,thunk)
         (unwind-protect
             (progn
               (setq ,session (xmtn-automate--make-session ,root ,key))
               (let ((xmtn-automate--*sessions*
                      (acons ,key ,session xmtn-automate--*sessions*)))
                 (funcall ,thunk)))
           (when ,session (xmtn-automate--close-session ,session)))))))

(defmacro* xmtn-automate-with-command ((handle-var session-form command-form
                                                   &key ((:may-kill-p
                                                          may-kill-p-form)))
                                       &body body)
  "Send COMMAND_FORM (a list of strings, or cons of lists of
strings) to session SESSION_FORM (current if nil). If car
COMMAND_FORM is a list, car COMMAND_FORM is options, cdr is command."
  (declare (indent 1) (debug (sexp body)))
  (let ((session (gensym))
        (command (gensym))
        (may-kill-p (gensym))
        (handle (gensym)))
    `(let ((,session ,session-form)
           (,command ,command-form)
           (,may-kill-p ,may-kill-p-form)
           (,handle nil))
       (unwind-protect
           (progn
             (setq ,handle (xmtn-automate--new-command ,session
                                                       ,command
                                                       ,may-kill-p))
             (xmtn--assert-optional (xmtn-automate--command-handle-p ,handle))
             (let ((,handle-var ,handle))
               ,@body))
         (when ,handle
           (xmtn-automate--cleanup-command ,handle))))))

(defun xmtn-automate--command-output-as-string-ignoring-exit-code (handle)
  (xmtn-automate-command-wait-until-finished handle)
  (with-current-buffer (xmtn-automate-command-buffer handle)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun xmtn-automate-command-check-for-and-report-error (handle)
  (unless (eql (xmtn-automate-command-error-code handle) 0)
    (error "mtn automate command (arguments %S) reported an error (code %s):\n%s"
           (xmtn-automate--command-handle-arguments handle)
           (xmtn-automate-command-error-code handle)
           (xmtn-automate--command-output-as-string-ignoring-exit-code handle)))
  nil)

(defun xmtn-automate-simple-command-output-string (root command)
  (xmtn-automate-with-session (session root)
    (xmtn-automate-with-command (handle session command)
      (xmtn-automate-command-check-for-and-report-error handle)
      (xmtn-automate--command-output-as-string-ignoring-exit-code handle))))

(defun xmtn-automate-simple-command-output-insert-into-buffer
  (root buffer command)
  "Send COMMAND (a list of strings, or cons of lists of strings)
to current session. If car COMMAND is a list, car COMMAND is
options, cdr is command. Insert result into BUFFER."
  (xmtn-automate-with-session (session root)
    (xmtn-automate-with-command (handle session command)
      (xmtn-automate-command-check-for-and-report-error handle)
      (xmtn-automate-command-wait-until-finished handle)
      (with-current-buffer buffer
        (xmtn--insert-buffer-substring-no-properties
         (xmtn-automate-command-buffer handle))))))

(defun xmtn-automate-command-output-lines (handle)
  (xmtn-automate-command-check-for-and-report-error handle)
  (xmtn-automate-command-wait-until-finished handle)
  ;; Maybe a simple buffer-substring-no-properties and split-string
  ;; would be more efficient.  I don't know.
  (save-excursion
    (set-buffer (xmtn-automate-command-buffer handle))
    (goto-char (point-min))
    (loop while (< (point) (point-max))
          collect (buffer-substring-no-properties (point)
                                                  (progn (end-of-line)
                                                         (point)))
          do
          (forward-line 1)
          (xmtn--assert-optional (bolp)))))

;; This one is useful.
(defun xmtn-automate-simple-command-output-lines (root command)
  (xmtn-automate-with-session (session root)
    (xmtn-automate-with-command (handle session command)
      (xmtn-automate-command-output-lines handle))))

;; This one is used twice.  I think the error checking it provides is
;; a reasonable simplification for its callers.
(defun xmtn-automate-simple-command-output-line (root command)
  "Return the one line output from mtn automate as a string.

Signals an error if output contains zero lines or more than one line."
  (let ((lines (xmtn-automate-simple-command-output-lines root command)))
    (unless (eql (length lines) 1)
      (error "Expected precisely one line of output from mtn automate, got %s: %s %S"
             (length lines)
             xmtn-executable
             command))
    (first lines)))


(defun xmtn-automate--set-process-session (process session)
  (xmtn--assert-optional (typep session 'xmtn-automate--session) t)
  (xmtn--process-put process 'xmtn-automate--session session))

(defun xmtn-automate--process-session (process)
  (xmtn--assert-optional (processp process) t)
  (let ((session (xmtn--process-get process 'xmtn-automate--session)))
    ;; This seems to fail sometimes with session being nil.  Not sure
    ;; why.  The problem seems to be reproducible by calling
    ;; (dvc-dvc-revision-nth-ancestor `(xmtn (local-tree ,(dvc-tree-root))) 10).
    (xmtn--assert-optional (typep session 'xmtn-automate--session) t)
    session))

(defstruct (xmtn-automate--decoder-state
            (:constructor xmtn-automate--%make-raw-decoder-state))
  (read-marker)
  (remaining-chars 0)
  (last-p nil))

(defstruct (xmtn-automate--session
            (:constructor xmtn-automate--%make-raw-session)
            (:copier xmtn-automate--copy-session))
  (root)
  (name)
  (buffer nil)
  (process nil)
  (decoder-state)
  (next-mtn-command-number)
  (next-session-command-number 0)
  (must-not-kill-counter)
  (remaining-command-handles)
  (sent-kill-p)
  (closed-p nil))

(defstruct (xmtn-automate--command-handle
            (:constructor xmtn-automate--%make-raw-command-handle))
  (arguments)
  (mtn-command-number)
  (session-command-number)
  (session)
  (buffer)
  (write-marker)
  (may-kill-p)
  (finished-p nil)
  (error-code nil))

(defun* xmtn-automate--initialize-session (session &key root name)
  (xmtn--assert-optional (equal root (file-name-as-directory root)) t)
  (setf (xmtn-automate--session-root session) root
        (xmtn-automate--session-name session) name
        (xmtn-automate--session-process session) nil
        (xmtn-automate--session-closed-p session) nil)
  nil)

(defun xmtn-automate--make-session (root key)
  (let* ((name (format "xmtn automate session for %s" key)))
    (let ((session (xmtn-automate--%make-raw-session)))
      (xmtn-automate--initialize-session session :root root :name name)
      session)))

(defun xmtn-automate--session-send-process-kill (session)
  (let ((process (xmtn-automate--session-process session)))
    ;; Stop parser.
    (setf (xmtn-automate--session-sent-kill-p session) t)
    (with-current-buffer (xmtn-automate--session-buffer session)
      (let ((inhibit-read-only t)
            deactivate-mark)
        (save-excursion
          (goto-char (process-mark process))
          (insert "\n(killing process)\n")
          (set-marker (process-mark process) (point)))))
    ;; Maybe this should really be a sigpipe.  But let's not get too
    ;; fancy (ha!) and non-portable.
    ;;(signal-process (xmtn-automate--session-process session) 'PIPE)
    ;; This call to `sit-for' is apparently needed in some situations to
    ;; make sure the process really gets killed.
    (sit-for 0)
    (interrupt-process process))
  nil)

(defun xmtn-automate--close-session (session)
  (setf (xmtn-automate--session-closed-p session) t)
  (let ((process (xmtn-automate--session-process session)))
    (cond
     ((null process)
      ;; Process died for some reason - most likely 'mtn not found in
      ;; path'. Don't warn if buffer hasn't been deleted; that
      ;; obscures the real error message
      ;; FIXME: if that is the reason, this assert fails. Disable assertions for now, fix later
      (xmtn--assert-optional (null (xmtn-automate--session-buffer session))))
     ((ecase (process-status process)
        (run nil)
        (exit t)
        (signal t))
      (unless xmtn-automate--*preserve-buffers-for-debugging*
        (kill-buffer (xmtn-automate--session-buffer session))))
     (t
      (process-send-eof process)
      (if (zerop (xmtn-automate--session-must-not-kill-counter session))
          (xmtn-automate--session-send-process-kill session)
        ;; We can't kill the buffer yet.  We need to dump mtn's output
        ;; in there so we can parse it and determine when the critical
        ;; commands are finished so we can then kill mtn.
        (dvc-trace
         "Not killing process %s yet: %s out of %s remaining commands are critical"
         (process-name process)
         (xmtn-automate--session-must-not-kill-counter session)
         (length (xmtn-automate--session-remaining-command-handles session))))
      (with-current-buffer (xmtn-automate--session-buffer session)
        ;; This isn't essential but helps debugging.
        (rename-buffer (format "*%s: killed session*"
                               (xmtn-automate--session-name session))
                       t))
      (let ((fake-session (xmtn-automate--copy-session session)))
        (xmtn-automate--set-process-session process fake-session)))))
  nil)

(defun xmtn-automate--start-process (session)
  (xmtn--check-cached-command-version)
  (xmtn--assert-optional (not (xmtn-automate--session-closed-p session)))
  (xmtn--assert-optional (typep session 'xmtn-automate--session))
  (let ((name (xmtn-automate--session-name session))
        (buffer (xmtn-automate--new-buffer session))
        (root (xmtn-automate--session-root session)))
    (let ((process-connection-type nil)
          (default-directory root))
      (let ((process
             (xmtn--with-environment-for-subprocess ()
               (apply #'start-process name buffer xmtn-executable
                      "automate" "stdio" xmtn-additional-arguments))))
        (xmtn-automate--set-process-session process session)
        (set-process-filter process 'xmtn-automate--process-filter)
        (set-process-sentinel process 'xmtn-automate--process-sentinel)
        (xmtn--set-process-query-on-exit-flag process nil)
        ;; Need binary (or no-conversion or maybe raw-text-unix?)
        ;; since this is the format in which mtn automate stdio
        ;; computes the size of the output.
        (set-process-coding-system process 'binary 'binary)
        (setf (xmtn-automate--session-process session) process)
        (setf (xmtn-automate--session-decoder-state session)
              (xmtn-automate--%make-raw-decoder-state
               :read-marker (with-current-buffer buffer
                              (xmtn--assert-optional (eql (point-min) (point)) t)
                              (set-marker (make-marker)
                                          (point-min)))))
        (setf (xmtn-automate--session-next-mtn-command-number session) 0)
        (setf (xmtn-automate--session-must-not-kill-counter session) 0)
        (setf (xmtn-automate--session-remaining-command-handles session) (list))
        (setf (xmtn-automate--session-sent-kill-p session) nil)
        process))))

(defun xmtn-automate--ensure-process (session)
  (let ((process (xmtn-automate--session-process session)))
    (when (or (null process)
              (ecase (process-status process)
                (run nil)
                (exit t)
                (signal t)))
      (setq process (xmtn-automate--start-process session))
      (setf (xmtn-automate--session-process session) process))
    (xmtn--assert-optional (buffer-live-p (xmtn-automate--session-buffer
                                           session)))
    process))

(defun xmtn-automate--new-buffer (session)
  (let* ((buffer-base-name (format "*%s: session*"
                                   (xmtn-automate--session-name session)))
         (buffer (generate-new-buffer buffer-base-name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (xmtn--set-buffer-multibyte nil)
      (setq buffer-read-only t))
    (setf (xmtn-automate--session-buffer session) buffer)
    buffer))

(defun xmtn-automate-terminate-processes-in-root (root)
  (xmtn-automate-with-session (session root)
    (xmtn-automate--close-session session)
    (let ((process (xmtn-automate--session-process session)))
      (when process
        (while (ecase (process-status process)
                 (run t)
                 (exit nil)
                 (signal nil))
          (accept-process-output process))
        ;;(dvc-trace "Process in root %s terminated" root)
        ))
    (xmtn-automate--initialize-session
     session
     :root (xmtn-automate--session-root session)
     :name (xmtn-automate--session-name session))))

(defun xmtn-automate--append-encoded-strings (strings)
  "Encode STRINGS (a list of strings or nil) in automate stdio format,
insert into current buffer.  Assumes that point is at the end of
the buffer."
  (xmtn--assert-optional (eql (point) (point-max)))
  (dolist (string strings)
    (if string
        (progn
          (save-excursion (insert string))
          (encode-coding-region (point) (point-max) 'xmtn--monotone-normal-form)
          (insert (number-to-string (- (point-max) (point))) ":")
          (goto-char (point-max)))))
  nil)

(defun xmtn-automate--send-command-string (session command option-plist
                                                   mtn-number session-number)
  "Send COMMAND and OPTION-PLIST to SESSION."
  (let* ((buffer-name (format "*%s: input for command %s(%s)*"
                              (xmtn-automate--session-name session)
                              mtn-number
                              session-number))
         (buffer nil))
    (unwind-protect
        (progn
          (when (get-buffer buffer-name)
            ;; Make sure the buffer is in a clean state.
            (with-current-buffer buffer-name
              (let ((inhibit-read-only t))
                (erase-buffer))
              (fundamental-mode)))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            (buffer-disable-undo)
            (xmtn--set-buffer-multibyte t)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (when option-plist
                (insert "o")
                (xmtn-automate--append-encoded-strings option-plist)
                (insert "e"))
              (insert "l")
              (xmtn-automate--append-encoded-strings command)
              (insert "e\n"))

            (dvc-trace "mtn automate: '%s'" (buffer-substring (point-min) (point-max)))

            (process-send-region (xmtn-automate--session-process session)
                                 (point-min) (point-max))))
      (when buffer
        (unless xmtn-automate--*preserve-buffers-for-debugging*
          (kill-buffer buffer))))))

(defun xmtn-automate--new-command (session command may-kill-p)
  "Send COMMAND (a list of strings, or cons of lists of strings)
to the current automate stdio session. If car COMMAND is a list,
car COMMAND is options, cdr is command."
  ;; For debugging.
  ;;(xmtn-automate-terminate-processes-in-root
  ;; (xmtn-automate--session-root session))
  (xmtn-automate--ensure-process session)
  (let* ((mtn-number (1- (incf (xmtn-automate--session-next-mtn-command-number
                                session))))
         (session-number
          (1- (incf (xmtn-automate--session-next-session-command-number
                     session))))
         (buffer-name (format "*%s: output for command %s(%s)*"
                              (xmtn-automate--session-name session)
                              mtn-number
                              session-number))
         (buffer
          (progn (when (get-buffer buffer-name)
                   ;; Make sure no local variables or mode changes
                   ;; remain from the previous command parser.
                   (with-current-buffer buffer-name
                     (let ((inhibit-read-only t))
                       (erase-buffer))
                     (fundamental-mode)))
                 (get-buffer-create buffer-name))))
    (if (not (listp (car command)))
        (xmtn-automate--send-command-string session command '()
                                            mtn-number session-number)
      (xmtn-automate--send-command-string session (cdr command) (car command)
                                          mtn-number session-number))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (xmtn--set-buffer-multibyte nil)
      (setq buffer-read-only t)
      (xmtn--assert-optional (and (eql (point) (point-min))
                                  (eql (point) (point-max))))
      (let ((handle (xmtn-automate--%make-raw-command-handle
                     :session session
                     :arguments command
                     :mtn-command-number mtn-number
                     :session-command-number session-number
                     :may-kill-p may-kill-p
                     :buffer buffer
                     :write-marker (set-marker (make-marker) (point)))))
        (setf
         (xmtn-automate--session-remaining-command-handles session)
         (nconc (xmtn-automate--session-remaining-command-handles session)
                (list handle)))
        (when (not may-kill-p)
          (incf (xmtn-automate--session-must-not-kill-counter session))
          (xmtn--set-process-query-on-exit-flag
           (xmtn-automate--session-process session)
           t))
        handle))))

(defun xmtn-automate--cleanup-command (handle)
  (unless xmtn-automate--*preserve-buffers-for-debugging*
    (kill-buffer (xmtn-automate--command-handle-buffer handle))))

(defsubst xmtn-automate--process-new-output--copy (session)
  (let* ((session-buffer (xmtn-automate--session-buffer session))
         (state (xmtn-automate--session-decoder-state session))
         (read-marker (xmtn-automate--decoder-state-read-marker state))
         (command (first (xmtn-automate--session-remaining-command-handles
                          session)))
         (command-output-buffer
          (xmtn-automate--command-handle-buffer command))
         (write-marker
          (xmtn-automate--command-handle-write-marker command)))
    (xmtn--assert-optional (not (xmtn-automate--session-sent-kill-p session)))
    (with-current-buffer session-buffer
      (let* ((end (min (+ read-marker
                          (xmtn-automate--decoder-state-remaining-chars state))
                       (point-max)))
             (chars-to-read (- end read-marker)))
        (cond
         ((= chars-to-read 0)
          nil)
         ((> chars-to-read 0)
          (if (not (buffer-live-p command-output-buffer))
              ;; Buffer has already been killed, just discard input.
              (progn)
            (with-current-buffer command-output-buffer
              (save-excursion
                (goto-char write-marker)
                (let ((inhibit-read-only t)
                      deactivate-mark)
                  (xmtn--insert-buffer-substring-no-properties session-buffer
                                                               read-marker
                                                               end))
                (set-marker write-marker (point))))
            ;;(xmtn--debug-mark-text-processed session-buffer read-marker end nil)
            )
          (set-marker read-marker end)
          (decf (xmtn-automate--decoder-state-remaining-chars state)
                chars-to-read)
          t)
         (t (xmtn--assert-nil))))))
  ;; Return value matters!
  )

(defun xmtn--debug-mark-text-processed (buffer start end bold-p)
  (xmtn--assert-optional (< start end) t)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (if bold-p
          (xmtn--assert-for-effect
           (add-text-properties start end
                                '(face
                                  (:strike-through
                                   t
                                   :weight semi-bold))))
        (xmtn--assert-for-effect
         (add-text-properties start end '(face (:strike-through
                                                t))))))))

(defsubst xmtn-automate--process-new-output (session new-string)
  (let* ((session-buffer (xmtn-automate--session-buffer session))
         (state (xmtn-automate--session-decoder-state session))
         (read-marker (xmtn-automate--decoder-state-read-marker state))
         (write-marker (process-mark (xmtn-automate--session-process session)))
         (tag 'check-for-more))
    (with-current-buffer session-buffer
      ;; Why oh why doesn't (require 'cl) provide tagbody...
      (loop
       for command = (first (xmtn-automate--session-remaining-command-handles
                             session))
       do
       (xmtn--assert-optional (or (eql tag 'exit-loop)
                                  (not (xmtn-automate--session-sent-kill-p
                                        session))))
       (ecase tag
         (check-for-more
          (xmtn--assert-optional (<= read-marker write-marker) t)
          (if (= read-marker write-marker)
              (setq tag 'exit-loop)
            (setq tag 'again)))
         (again
          (cond
           ((> (xmtn-automate--decoder-state-remaining-chars state) 0)
            (if (xmtn-automate--process-new-output--copy session)
                (setq tag 'again)
              (setq tag 'check-for-more)))
           ((and (= (xmtn-automate--decoder-state-remaining-chars state) 0)
                 (xmtn-automate--decoder-state-last-p state))
            (xmtn--assert-optional command)
            (setf (xmtn-automate--command-handle-finished-p command) t)
            (xmtn--with-no-warnings
             (pop (xmtn-automate--session-remaining-command-handles session)))
            (setq tag 'check-for-more)
            (when (not (xmtn-automate--command-handle-may-kill-p command))
              (when (zerop (decf (xmtn-automate--session-must-not-kill-counter
                                  session)))
                (xmtn--set-process-query-on-exit-flag
                 (xmtn-automate--session-process session)
                 nil)
                (when (xmtn-automate--session-closed-p session)
                  (xmtn-automate--session-send-process-kill session)
                  (setq tag 'exit-loop))))
            (setf (xmtn-automate--decoder-state-last-p state) nil))
           ((and (= (xmtn-automate--decoder-state-remaining-chars state) 0)
                 (not (xmtn-automate--decoder-state-last-p state)))
            (unless command
              (error "Unexpected output from mtn: %s" new-string))
            (save-excursion
              (goto-char read-marker)
              (cond ((looking-at
                      "\\([0-9]+\\):\\([012]\\):\\([lm]\\):\\([0-9]+\\):")
                     (let ((command-number (parse-integer (match-string 1)))
                           (error-code (parse-integer (match-string 2)))
                           (last-p (cond
                                    ((string= (match-string 3) "l") t)
                                    ((string= (match-string 3) "m") nil)
                                    (t (xmtn--assert-nil))))
                           (size (parse-integer (match-string 4))))
                       (xmtn--assert-optional (typep command-number
                                                     '(integer 0 *))
                                              t)
                       (xmtn--assert-optional (typep error-code '(member 0 1 2))
                                              t)
                       (xmtn--assert-optional (typep size '(integer 0 *)) t)
                       (xmtn--assert-optional
                        (eql
                         command-number
                         (xmtn-automate--command-handle-mtn-command-number
                          command)))
                       (setf (xmtn-automate--command-handle-error-code command)
                             error-code)
                       (setf (xmtn-automate--decoder-state-remaining-chars
                              state)
                             size)
                       (setf (xmtn-automate--decoder-state-last-p state)
                             last-p)
                       ;;(xmtn--debug-mark-text-processed session-buffer
                       ;;                                 read-marker
                       ;;                                 (match-end 0)
                       ;;                                 t)
                       (set-marker read-marker (match-end 0)))
                     (setq tag 'again))
                    ;; This is just a simple heuristic, there are many
                    ;; kinds of invalid input that it doesn't detect.
                    ;; FIXME: This can errorneously be triggered by
                    ;; warnings that mtn prints on stderr; but Emacs
                    ;; interleaves stdout and stderr (see (elisp)
                    ;; Output from Processes) with no way to
                    ;; distinguish between them.  We'll probably have
                    ;; to spawn mtn inside a shell that redirects
                    ;; stderr to a file.  But I don't think that's
                    ;; possible in a portable way...
                    ((looking-at "[^0-9]")
                     (error "Invalid output from mtn: %s"
                            (buffer-substring-no-properties (point)
                                                            (point-max))))
                    (t
                     (xmtn--assert-optional command)
                     (setq tag 'exit-loop)))))
           (t (xmtn--assert-nil))))
         (exit-loop (return))))))
  nil)


(defvar xmtn-automate--*preserve-buffers-for-debugging* nil)

(defun xmtn-automate--process-sentinel (process event-string)
  (let ((status (process-status process))
        (session (xmtn-automate--process-session process)))
    (let ((buffer (xmtn-automate--session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                deactivate-mark)
            (save-excursion
              ;; This seems to fail in XEmacs when running the test
              ;; `file-diff'.  I don't know why.
              (xmtn--assert-optional (marker-position (process-mark process))
                                     t)
              (goto-char (process-mark process))
              (insert (format "\n(process exited: %S)\n"
                              (if (eql (aref event-string
                                             (1- (length event-string)))
                                       ?\n)
                                  (subseq event-string 0
                                          (1- (length event-string)))
                                event-string)))
              (set-marker (process-mark process) (point))))))
      (flet ((reclaim-buffer ()
               (unless xmtn-automate--*preserve-buffers-for-debugging*
                 ;; Maybe it's not such a good idea to kill the buffer
                 ;; from here since that will run `kill-buffer-hook',
                 ;; and the functions in there might not be prepared to
                 ;; run inside a sentinel.  But let's wait until someone
                 ;; actually encounters this problem.
                 (kill-buffer buffer)
                 )))
        (ecase status
          (exit
           (xmtn--assert-optional (eql (process-exit-status process) 0) t)
           (reclaim-buffer))
          (signal
           (if (xmtn-automate--session-sent-kill-p session)
               (reclaim-buffer)
             (message "Process %s died due to signal" (process-name process))
             (when (not (zerop (xmtn-automate--session-must-not-kill-counter
                                session)))
               (xmtn--lwarn
                'xmtn ':error
                "Process %s died due to signal during a critical operation"
                (process-name process))))))))))

(defun xmtn-automate--process-filter (process input-string)
  (let ((session (xmtn-automate--process-session process)))
    (let ((buffer (xmtn-automate--session-buffer session)))
      (xmtn--assert-optional (eql (process-buffer process) buffer))
      (xmtn--assert-optional (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((mark (process-mark process))
               (move-point-p (= (point) mark)))
          (save-excursion
            (goto-char mark)
            (let ((inhibit-read-only t)
                  deactivate-mark)
              (insert input-string))
            (set-marker mark (point)))
          (when move-point-p (goto-char mark))))
      ;;(with-local-quit                    ; For debugging.
      ;; Emacs receives a message "mtn: operation canceled: Interrupt"
      ;; from mtn after we kill it.  Ignore such "input".
      (unless (xmtn-automate--session-sent-kill-p session)
        (xmtn-automate--process-new-output session input-string))
      ;;)
      )))

(defun xmtn--map-parsed-certs (xmtn--root xmtn--revision-hash-id xmtn--thunk)
  (lexical-let ((root xmtn--root)
                (revision-hash-id xmtn--revision-hash-id)
                (thunk xmtn--thunk))
    (xmtn--with-automate-command-output-basic-io-parser
        (xmtn--next-stanza root `("certs" ,revision-hash-id))
      (loop
       for xmtn--stanza = (funcall xmtn--next-stanza)
       while xmtn--stanza
       do (xmtn-match xmtn--stanza
            ((("key" (string $xmtn--key))
              ("signature" (string $xmtn--signature))
              ("name" (string $xmtn--name))
              ("value" (string $xmtn--value))
              ("trust" (string $xmtn--trust)))
             (setq xmtn--signature (xmtn-match xmtn--signature
                                     ("ok" 'ok)
                                     ("bad" 'bad)
                                     ("unknown" 'unknown)))
             (let ((xmtn--trusted (xmtn-match xmtn--trust
                                    ("trusted" t)
                                    ("untrusted" nil))))
               (macrolet ((decodef (var)
                            `(setq ,var (decode-coding-string
                                         ,var 'xmtn--monotone-normal-form))))
                 (decodef xmtn--key)
                 (decodef xmtn--name)
                 ;; I'm not sure this is correct.  The documentation
                 ;; mentions a cert_is_binary hook, but it doesn't
                 ;; exist; and even if it did, we would have no way of
                 ;; calling it from here.  But, since cert values are
                 ;; always passed on the command line, and command
                 ;; line arguments are converted to utf-8, I suspect
                 ;; certs will also always be in utf-8.
                 (decodef xmtn--value))
               (funcall thunk
                        xmtn--key xmtn--signature xmtn--name xmtn--value
                        xmtn--trusted))))))))

(defun xmtn--list-parsed-certs (root revision-hash-id)
  "Return a list of the contents of each cert attached to REVISION-HASH-ID.
Each element of the list is a list; key, signature, name, value, trust."
  (lexical-let ((accu '()))
    (xmtn--map-parsed-certs root revision-hash-id
                            (lambda (key signature name value trusted)
                              (push (list key signature name value trusted)
                                    accu)))
    (setq accu (nreverse accu))
    accu))

(defun xmtn--heads (root branch)
  (xmtn-automate-simple-command-output-lines root `("heads" ,branch)))


(provide 'xmtn-automate)

;;; xmtn-automate.el ends here

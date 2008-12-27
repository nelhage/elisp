;;; xmtn-compat.el --- xmtn compatibility with different Emacs versions

;; Copyright (C) 2008 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: extensions

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

;; Wrappers and fallback implementations for various Emacs functions
;; needed by xmtn that don't exist in all versions of Emacs.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl))

(defun xmtn--temp-directory ()
  (if (fboundp 'temp-directory)
      (temp-directory)
    temporary-file-directory))

(defun xmtn--make-temp-file (prefix &optional dirp suffix)
  ;; Do this in a temp buffer to ensure we use the default file output
  ;; encoding.  Emacs 21's `make-temp-file' uses the current buffer's
  ;; output format function while writing the file with `write-region'
  ;; with a string as its first argument, but coding conversion errors
  ;; when `write-region' is called in this way.
  (with-temp-buffer
    ;; XEmacs' `make-temp-file' doesn't automatically use temp
    ;; directory.
    (setq prefix (expand-file-name prefix (xmtn--temp-directory)))
    ;; FIXME: Ignoring suffix for now since Emacs 21 doesn't support it.
    (make-temp-file prefix dirp)))

(defvar xmtn--*process-plists* (make-hash-table :weakness 'key))

;;; These should probably use `process-get' and `process-put' if
;;; available, but that's not important.
(defun xmtn--process-put (process propname value)
  (setf (getf (gethash process xmtn--*process-plists*) propname) value)
  ;; Mimic the return value that `process-put' would yield.
  (gethash process xmtn--*process-plists*))

(defsubst xmtn--process-get (process propname)
  (getf (gethash process xmtn--*process-plists*) propname nil))

(defmacro xmtn--set-process-query-on-exit-flag (process value)
  (if (fboundp 'set-process-query-on-exit-flag)
      `(set-process-query-on-exit-flag ,process ,value)
    `(progn
       (process-kill-without-query ,process ,value)
       ,value)))

(defmacro xmtn--insert-buffer-substring-no-properties (from-buffer
                                                       &optional start end)
  (if (fboundp 'insert-buffer-substring-no-properties)
      `(insert-buffer-substring-no-properties ,from-buffer ,start ,end)
    `(progn
       (insert (with-current-buffer ,from-buffer
                 (buffer-substring-no-properties (or ,start (point-min))
                                                 (or ,end (point-max)))))
       nil)))

(defun xmtn--lwarn (tag level message &rest args)
  (if (fboundp 'lwarn)
      (apply #'lwarn tag level message args)
    (apply #'message message args))
  ;; The return value of `lwarn' seems to be pretty much undefined, so
  ;; we don't try to replicate it here.
  nil)

(defmacro* xmtn--with-no-warnings (&body body)
  (if (fboundp 'with-no-warnings)
      `(with-no-warnings ,@body)
    `(progn ,@body)))

(defmacro* xmtn--with-temp-message (message &body body)
  (declare (indent 1) (debug (form body)))
  (if (fboundp 'with-temp-message)
      `(with-temp-message ,message ,@body)
    `(progn ,@body)))

(defmacro* xmtn--dotimes-with-progress-reporter ((i n-form &optional res-form)
                                                 message-form
                                                 &body body)
  (declare (indent 2) (debug (sexp form body)))
  (if (fboundp 'dotimes-with-progress-reporter)
      `(dotimes-with-progress-reporter (,i ,n-form ,res-form)
           ,message-form ,@body)
    (let ((message (gensym)))
      `(let ((,message ,message-form))
         (prog1
             (xmtn--with-temp-message ,message
               (dotimes (,i ,n-form ,res-form)
                 ,@body))
           (message "%sdone" ,message))))))

(defmacro xmtn--set-buffer-multibyte (flag)
  (when (fboundp 'set-buffer-multibyte)
    `(set-buffer-multibyte ,flag)))

(provide 'xmtn-compat)

;;; xmtn-compat.el ends here

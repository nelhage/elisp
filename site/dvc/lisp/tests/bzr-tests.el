;;; bzr-tests.el --- Automated regression tests for bzr

;; Copyright (C) 2007, 2008 Stephen Leake

;; Author: Stephen Leake

;; adapted from xmtn-tests.el

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

;; Automated regression tests for bzr-dvc.

;;; Code:

;; These tests require elunit.el from dvc/lisp/contrib, originally
;; from http://dev.technomancy.us/phil/wiki/ElUnit

(require 'bzr-dvc)
(require 'cl)
(require 'dvc-tests-utils "tests/dvc-tests-utils.el")
(require 'elunit)

;;; This is preferable over separate set-up and tear-down functions
;;; since it allows us to make use of `unwind-protect' and dynamic
;;; bindings.

(defun bzr-tests--call-with-test-environment (bzr--body)
  "Initialize a bzr workspace, call BODY"
  (lexical-let ((body bzr--body)
                (temp-dir nil))
    (unwind-protect
        (progn
          (setq temp-dir (file-name-as-directory (make-temp-file "bzr-tests-" t)))
          (let ((default-directory temp-dir))
            (dvc-run-dvc-sync 'bzr '("init"))
            (funcall body)
            (dvc-tests-wait-async)))
      (if temp-dir
          ;; If this delete doesn't succeed, there is a real problem,
          ;; so we don't try to handle the error.
          (dired-delete-file temp-dir 'always)))))

(defun bzr-tests--call-with-test-history (bzr--body)
  "Create a test environment with one file with some change
history. Call BODY with one key arg :file-name; the file name of
the test file."
  (lexical-let ((body bzr--body))
    (bzr-tests--call-with-test-environment
     (function*
      (lambda ()
        (lexical-let ((file-name "file-1"))
          (with-temp-file file-name (insert "a\n"))
          (bzr-add file-name)
          (dvc-run-dvc-sync 'bzr '("commit" "--message" "\"commit 1\""))
          (with-temp-file file-name (insert "b\n"))
          (dvc-run-dvc-sync 'bzr '("commit" "--message" "\"commit 2\""))
          (funcall body
                   :file-name file-name)))))))

(defmacro* bzr-tests--with-test-environment ((&rest keys) &body body)
  (declare (indent 1) (debug sexp body))
  `(bzr-tests--call-with-test-environment (function* (lambda (,@keys) ,@body))))

(defmacro* bzr-tests--with-test-history ((&rest keys) &body body)
  (declare (indent 1) (debug sexp body))
  `(bzr-tests--call-with-test-history (function* (lambda (,@keys) ,@body))))


(defsuite bzr
  (log
   (save-window-excursion
     (bzr-tests--with-test-history (&key &allow-other-keys)
       ;; The test is simply that this doesn't crash.
       (dvc-log)
       (dvc-tests-wait-async)           ; let log display
       (dvc-revlist-show-item))))

  (file-diff
   ;; The test is simply that this doesn't crash.
   (save-window-excursion
     (bzr-tests--with-test-history (&key file-name &allow-other-keys)
       (find-file file-name)
       (unwind-protect
           (progn
             (insert "x")
             (save-excursion
               (call-interactively #'dvc-file-diff)))
         (revert-buffer t t)))))

  (diff
   ;; The test is simply that this doesn't crash.
   (save-window-excursion
     (bzr-tests--with-test-history (&key file-name &allow-other-keys)
       (find-file file-name)
       (insert "x")
       (write-file file-name)
       (call-interactively #'dvc-diff))))

  (diff-from-revlog
   ;; The test is simply that this doesn't crash.
   (save-window-excursion
     (bzr-tests--with-test-history (&key &allow-other-keys)
       (dvc-changelog)
       (dvc-tests-wait-async)           ; let log display
       (dvc-revision-next)
       (dvc-revlist-diff))))

  )
;;(elunit "bzr")

(defsuite bzr-one
  (log
   (save-window-excursion
     (bzr-tests--with-test-history
         (&key &allow-other-keys)
       (dvc-diff))))
  )
;;(elunit "bzr-one")

(provide 'bzr-tests)
;;; bzr-tests.el ends here

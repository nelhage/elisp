;;; elunit.el --- Emacs Lisp Unit Testing framework

;; Copyright (C) 2006 Phil Hagelberg

;; Adapted-By: Christian M. Ohler

;; Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
;; by Nathaniel Talbott, and xUnit by Kent Beck

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; See http://dev.technomancy.us/phil/wiki/ElUnit for usage details.

(require 'cl)
(require 'compile)

(defvar *elunit-suites*
  '()
  "A list of unit test suites")

(defvar *elunit-default-suite* nil
  "Choice to use for default suite to run (gets updated to last suite run)")

(defun elunit-suite (name)
  (cdr (assoc name *elunit-suites*)))

(defun elunit-get-test (name suite)
  (when (symbolp suite) (setq suite (elunit-suite suite)))
  (assoc name suite))


;;; Defining tests

(defmacro defsuite (suite-name &rest tests)
  "This is what you use to set things up."
  (dolist (test tests)
     (elunit-add-to-suite (make-test test) suite-name)))

(defun make-test (body)
  (let ((name (pop body)))
    (list name body buffer-file-name
          (save-excursion
            (condition-case var
                (progn (search-backward (symbol-name name))
                       (if (fboundp 'line-number-at-pos)
                           (line-number-at-pos)
                         'unknown)) ; not a foolproof heuristic to get line number, but good enough.
              (error 'unknown))))))

(defun elunit-add-to-suite (test suite)
  (unless (elunit-suite suite) (elunit-make-suite suite))
  (elunit-delete-test (car test) suite)
  (push test (cdr (assoc suite *elunit-suites*))))

(defun elunit-make-suite (suite) 
  (push (list suite) *elunit-suites*))

(defun elunit-delete-test (name suite)
  (when (elunit-get-test name suite)
    (setf (cdr (assoc suite *elunit-suites*)) (assq-delete-all name (elunit-suite suite)))))

(defun elunit-clear-suites ()
  (setq *elunit-suites* '((default-suite ()))))


;;; Running the unit tests

(defun elunit (suite &optional force-prompt)
  "Run all tests in SUITE (a string), and display the results.

Prompt for a suite if FORCE-PROMPT is non-nil, or if both SUITE
and `*elunit-default-suite*' are nil."
  (interactive "i\nP")
  (unless suite (setq suite *elunit-default-suite*))
  (cond ((null suite)
         (setq suite
               (completing-read
                "Run test suite: "
                (mapcar (lambda (suite) (cons (symbol-name (car suite))
                                              (symbol-name (car suite))))
                        *elunit-suites*)
                nil t)))
        (force-prompt
         (setq suite
               (completing-read
                (format "Run test suite (default %s): " suite)
                (mapcar (lambda (suite) (cons (symbol-name (car suite))
                                              (symbol-name (car suite)))) 
                        *elunit-suites*)
                nil t nil nil suite)))
        (t (progn)))
  (setq *elunit-default-suite* suite)
  (setq *elunit-fail-count* 0)
  (run-hooks (intern (concat suite "-setup-hook")))
  (with-output-to-temp-buffer "*elunit*"
    (princ (concat "Loaded suite: " suite "\n\n"))
    (let* ((tests (elunit-suite (intern suite)))
           (start-time (cadr (current-time)))
           (total (length tests)))
      (let ((results (loop for test-id from 1
                           for test in (reverse tests)
                           ;; This used to be `with-temp-message', but
                           ;; writing the boundaries between test cases
                           ;; into the *Messages* buffer can be
                           ;; helpful.
                           do (message "Running test \"%s\" (%s of %s)..."
                                       (first test) test-id total)
                           collect (apply #'elunit-run-test test))))
        (message "Ran %s tests; %s failed" total *elunit-fail-count*)
        (elunit-report-results results))
      (princ (format " in %d seconds." (- (cadr (current-time)) start-time)))))
  (run-hooks (intern (concat suite "-teardown-hook"))))

(defun elunit-run-test (name body file-name line-number)
  (let* ((passed nil)
	 (docstring (if (stringp (car body)) (pop body) ""))
	 (result (condition-case err
		     (save-excursion (eval (cons 'progn body)) (setq passed t))
		   (error err))))
    (elunit-status passed)
    (if passed t
      (list name docstring result body file-name line-number *elunit-fail-count*))))


;;; Showing the results

(defun elunit-status (pass) 
  "Output status while the tests are running"
  (princ (if pass "." "F"))
  (unless pass (incf *elunit-fail-count*)
	  (switch-to-buffer "*elunit*")
          ;; This doesn't work in XEmacs.
;;	  (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
	  (switch-to-buffer nil)))

(defun elunit-report-results (tests) 
  "For when the tests are finished and we want details"
  (dolist (test tests)
    (unless (eq t test)
      (apply 'elunit-report-result test)))
  (princ (format "\n\n\n%d tests total, %d failures" (length tests) *elunit-fail-count*)))
    
(defun elunit-report-result (name docstring result body file-name line-number index)
  "Report a single test failure"
  (princ (format "\n\n%d) Failure: %s [%s:%s]
            %s
    Result: %s
      Form: %s" index name file-name line-number docstring result (car body))))

;(add-hook 'temp-buffer-show-hook 'compilation-minor-mode)
;(add-to-list 'compilation-error-regexp-alist '("\\[\\([^:]*\\):\\([0-9]+\\)" 1 2))
;;(add-to-list 'compilation-error-regexp-alist '("\\[\\([^\]]*\\):\\([0-9]+\\)\\]" 1 2))

(provide 'elunit)

;; end of file

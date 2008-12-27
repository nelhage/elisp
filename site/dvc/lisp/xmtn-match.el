;;; xmtn-match.el --- A macro for pattern-matching

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

;; A pattern-matching macro.  See its docstring for details.
;;
;; This was originally implemented for xmtn (and for fun), and is
;; heavily used there, but isn't specific to that context at all.
;;
;; The main difference between this package and Luke Goerrie's
;; patmatch.el, as far as I can see, is that this package attempts to
;; be efficient by analyzing the patterns statically, at
;; macroexpansion time.
;;
;; If this macro causes `max-lisp-eval-depth' or `max-specpdl-size' to
;; be exceeded, it is probably running interpreted.  I haven't
;; investigated this; maybe there's a simple fix to reduce nesting
;; significantly.  For now, be sure to compile this file.  Possibly,
;; functions using this macro also need to be compiled.  An
;; alternative is to increase the value of the respective variable.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl))

;; Note:
;; (equal (nth-value 0 (read-from-string "(x (`(foo)))")) (nth-value 0 (read-from-string "(x `(foo))"))) !

;; Why dollar sign as prefix character?  Question mark, customarily
;; used for similar purposes in Common Lisp, is already taken in Emacs
;; Lisp.  Dollar sign is used as a prefix character for variables in
;; some shell scripting languages, so it's somewhat familiar.

;; (pprint (macroexpand '(xmtn-match x ([t $y ($y . t)] y))))

(deftype xmtn-match--bool-vector ()
  (if (fboundp 'bool-vector-p)
      ;; For Emacs.
      `bool-vector
    ;; For XEmacs.
    `nil))

(deftype xmtn-match--atom ()
  `(not cons))

(defun xmtn-match--match-variable-p (thing var-name-prefix-char)
  (and (symbolp thing)
       (eql (aref (symbol-name thing) 0) var-name-prefix-char)))

(defun xmtn-match--contains-match-variable-p (thing var-name-prefix-char)
  (labels ((walk (thing)
            (or
             (xmtn-match--match-variable-p thing var-name-prefix-char)
             (etypecase thing
               (cons (or (walk (car thing))
                         (walk (cdr thing))))
               ((and array (not string) (not xmtn-match--bool-vector))
                (some #'walk thing))
               (xmtn-match--atom nil)))))
    (walk thing)))

;; They say it's bad style if function definitions are too big to fit
;; on a screen.  A small font is recommended for this one.
(defun xmtn-match--generate-branch (var-name-prefix-char
                                    match-block object pattern body)
  (let ((var-accu (list))
        (pattern-block (gensym "pattern-test-")))
    (let ((test
           `(and
             ,@(labels
                   ;; The 'contains variable' check, the way it is
                   ;; implemented here, is grossly inefficient at
                   ;; compile-time.
                   ((walk-part (subsubpattern subsubobject-form)
                     ;; Be smart and try not to introduce temporary
                     ;; variables that would be accessed only once.
                     ;; Since they are dynamic variables, Emacs might
                     ;; not be able to optimize them away.  They also
                     ;; make the generated code harder to understand
                     ;; when debugging expansions.
                     (if (or
                          (not (xmtn-match--contains-match-variable-p
                                subsubpattern var-name-prefix-char))
                          (etypecase subsubpattern
                            (cons nil)
                            (array nil)
                            (t t)))
                         (walk subsubobject-form subsubpattern)
                       (let ((subsubobject (gensym)))
                         `((let ((,subsubobject ,subsubobject-form))
                             (and
                              ,@(walk subsubobject subsubpattern)))))))
                    (walk (subobject subpattern)
                     ;; Returns a list of conditions for an `and'
                     ;; expression.
                     (cond
                      ((xmtn-match--match-variable-p subpattern
                                                     var-name-prefix-char)
                       (let ((var (intern (subseq (symbol-name subpattern) 1))))
                         (cond ((member var var-accu)
                                `((equal ,subobject ,var)))
                               (t
                                (push var var-accu)
                                `((progn (setq ,var ,subobject) t))))))
                      ((not (xmtn-match--contains-match-variable-p
                             subpattern var-name-prefix-char))
                       (etypecase subpattern
                         ;; The byte-compiler doesn't do this
                         ;; optimization as of GNU Emacs 22.0.50.1.
                         ;; Maybe that means it's not worth doing...
                         (symbol
                          `((eq ,subobject ',subpattern)))
                         (t
                          `((equal ,subobject ',subpattern)))))
                      (t
                       (etypecase subpattern
                         (cons
                          `((consp ,subobject)
                            ,@(loop for part-reader in '(car cdr)
                                    append (walk-part
                                            (funcall part-reader subpattern)
                                            `(,part-reader ,subobject)))))
                         ;; I think this will also allow char-tables.
                         ;; Not sure how useful that is.
                         ((and array (not string) (not xmtn-match--bool-vector))
                          `((typep ,subobject ',(type-of
                                                 subpattern))
                            (eql (length ,subobject) ,(length subpattern))
                            ,@(loop for index below (length subpattern)
                                    append (walk-part
                                            (aref subpattern index)
                                            `(aref ,subobject ,index))))))))))
                 (walk object pattern)))))
      (setq var-accu (nreverse var-accu))
      `(let (,@var-accu)
         (when
             ;;(xmtn-match--test (lambda () ,test))
             ,test
           (return-from ,match-block (progn ,@body)))))))

;; Make sure the function is compiled to avoid stack overflows.
;; Without this, DVC fails to build (in my configuration), since it
;; initially loads the elisp file as source.
(byte-compile 'xmtn-match--generate-branch)
;; I think the same may hold for this function (see message from Sam
;; Steingold on the dvc-dev list, 2007-07-09), although I haven't
;; tried very hard to reproduce it.
(byte-compile 'xmtn-match--contains-match-variable-p)


;; Factored out for profiling.
;;;###autoload
(defun xmtn-match--test (xmtn--thunk)
  (funcall xmtn--thunk))


(defmacro* xmtn-match (object-form &body cases)
  "Similar to `ecase', but with pattern matching.

Eval EXPR, find the first PATTERN that matches its value, execute
the corresponding BODY and return its result.  If no PATTERN
matches, an error is signalled.

The matching is done as with `equal', except that subexpressions
of PATTERN that are symbols whose name starts with $ are treated
specially.  Such symbols are free variables that match any
subexpression.  If the same variable occurs more than once, each
occurrence must match a similar \(as in `equal'\) subexpression.
During the execution of BODY, each variable, with the leading $
removed, will be bound to the subexpression that it matched.

Variables may only occur in conses and arrays except strings and
bool-vectors.

\(fn EXPR \(PATTERN BODY...\)...\)"
  (declare (indent 1) (debug (form &rest (sexp body))))
  ;; It would be interesting (very interesting, in fact, but also
  ;; fairly complex) to generate an expansion here that walks the
  ;; object only /once/ at run-time, not once for every clause as the
  ;; current expansion does.  Might also be more efficient, but that's
  ;; hard to say for sure, and I don't think the matching currently is
  ;; a bottleneck anywhere.  But it would allow detecting whether one
  ;; clause subsumes a subsequent one and issuing a warning.
  (let ((macro-name 'xmtn-match)
        (var-name-prefix-char ?$)
        (object (gensym "object-"))
        (match-block (gensym "match-form-")))
    `(let ((,object ,object-form))
       (block ,match-block
         ,@(loop
            for (pattern . body) in cases
            collect (xmtn-match--generate-branch var-name-prefix-char
                                                 match-block object pattern
                                                 body))
         (error "Fell through %S: %S" ',macro-name ,object)))))

(provide 'xmtn-match)

;;; xmtn-match.el ends here

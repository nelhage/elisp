;;; dvc-lisp.el --- DVC lisp helper functions

;; Copyright (C) 2003-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>
;;    Michael Olson <mwolson@gnu.org>

;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Helper functions unrelated from GNU Arch.

;;; History:
;;
;; Created in May 2005 by Matthieu Moy
;;
;; Overhauled in Aug 2007 by Michael Olson

(defvar dvc-gensym-counter 0)

(defun dvc-gensym (&optional prefix)
  "Generate a new uninterned symbol.

If PREFIX is a string, then the name is made by appending a
number to PREFIX.  The default is to use \"dvc\".

If PREFIX is a number, then use that number at the end of the
symbol name."
  (let* ((prefix (if (stringp prefix) prefix "dvc-gensym-uniq-"))
         (num (if (integerp prefix) prefix
                (prog1
                    dvc-gensym-counter
                  (setq dvc-gensym-counter (1+ dvc-gensym-counter)))))
         (symbol (make-symbol (format "%s%d" prefix num))))
    (eval `(defvar ,symbol nil "lint trap"))
    symbol))

(defun dvc-capturing-lambda-helper (l)
  "Traverse list L, replacing captured symbols with newly generated
symbols.

A pair is added to `captured-values' for each new symbol,
containing the name of the new symbol and the name of the old
symbol.

This is used by `dvc-capturing-lambda'."
  (cond ((atom l) l)
        ((eq (car l) 'capture)
         (let ((sym (cadr l)))
           (unless (symbolp sym)
             (error "Expected a symbol in capture statement: %S" sym))
           (let ((g (car (rassq sym captured-values))))
             (unless g
               (setq g (dvc-gensym))
               (push (cons g sym) captured-values))
             g)))
        (t (mapcar 'dvc-capturing-lambda-helper l))))

(eval-and-compile
  ;; NOTE: We keep the contents of this block flush against the left
  ;; margin, so that C-M-x continues to work.
(defmacro dvc-capturing-lambda (args &rest body)
  "Return a `lambda' form with ARGS, containing BODY, after capturing
symbol values in BODY from the defining context.

Symbols to be captured should be surrounded by (capture ...).
The remainder of BODY's forms are left as-is.

For development on DVC, using either `dvc-capturing-lambda' or
`lexical-let' is acceptable, with the condition that you must use
one consistently within a particular source file.

A practical example:

  ;; Using dvc-capturing-lambda
  (defun sort-by-nearness-1 (values middle)
    \"Sort VALUES in order of how close they are to MIDDLE.\"
    (sort values (dvc-capturing-lambda (a b)
                   (< (abs (- a (capture middle)))
                      (abs (- b (capture middle)))))))

  (sort-by-nearness-1 '(1 2 3 4 8 5 9) 6)
  => (5 4 8 3 9 2 1)

  ;; Using backquote
  (defun sort-by-nearness-2 (values middle)
    \"Sort VALUES in order of how close they are to MIDDLE.\"
    (sort values `(lambda (a b)
                    (< (abs (- a ,middle))
                       (abs (- b ,middle))))))

  (sort-by-nearness-2 '(1 2 3 4 8 5 9) 6)
  => (5 4 8 3 9 2 1)

  ;; Using lexical-let
  (defun sort-by-nearness-3 (values middle)
    \"Sort VALUES in order of how close they are to MIDDLE.\"
    (lexical-let ((middle middle))
      (sort values (lambda (a b)
                     (< (abs (- a middle))
                        (abs (- b middle)))))))

  (sort-by-nearness-3 '(1 2 3 4 8 5 9) 6)
  => (5 4 8 3 9 2 1)

An example for the well-read Lisp fan:

  (let* ((x 'lexical-x)
         (y 'lexical-y)
         (l (dvc-capturing-lambda (arg)
              (list x (capture y) arg))))
    (let ((y 'dynamic-y)
          (x 'dynamic-x))
      (funcall l 'dummy-arg)))

  => (dynamic-x lexical-y dummy-arg)"
  (declare (indent 1) (debug (sexp body)))
  (let* ((captured-values nil)
         (body (dvc-capturing-lambda-helper body)))
    `(list 'lambda ',args
           (list 'apply
                 (lambda ,(append args (mapcar #'car captured-values))
                   . ,body)
                 ,@(mapcar #'(lambda (arg) (list 'quote arg)) args)
                 (list 'quote (list ,@(mapcar #'cdr captured-values))))))))

(defun dvc-lexical-let-perform-replacement-in-source ()
  "Replace instances of quoted lambda forms with `lexical-let'
in the current buffer."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "`(lambda" nil t)
    (search-backward "(")
    (save-excursion (forward-sexp 1) (insert ")"))
    (backward-delete-char 1)
    (insert "(lexical-let ")
    (search-backward "(lex")
    (let ((beginning (point))
          (letlist "")
          (namelist nil))
      (forward-sexp 1)
      (save-restriction
        (narrow-to-region beginning (point))
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (backward-delete-char 1)
          (let* ((beg (point))
                 (end (progn (forward-sexp 1) (point)))
                 (name (buffer-substring-no-properties beg end))
                 (var (concat (replace-regexp-in-string "[^a-zA-Z\\-]" "-"
                                                        name) "-lex")))
            (when (not (member name namelist))
              (push name namelist)
              (setq letlist (concat
                             letlist (when (not (string= letlist ""))
                                       " ")
                             "(" var " "
                             name
                             ")")))
            (delete-region beg end)
            (goto-char beg)
            (insert var)
            ))
        (goto-char (point-min))
        (search-forward "(lexical-let ")
        (insert "(" letlist ")")
        (newline-and-indent)))))

(defun dvc-capturing-lambda-perform-replacement-in-source ()
  "Replace instances of quoted lambda forms with `dvc-capturing-lambda'
in the current buffer."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "`(lambda" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "(dvc-capturing-lambda")
    (search-backward "(")
    (let ((beginning (point)))
      (forward-sexp 1)
      (save-restriction
        (narrow-to-region beginning (point))
        (goto-char (point-min))
        (while (search-forward "," nil t)
          (backward-delete-char 1)
          (insert "(capture ")
          (forward-sexp 1)
          (insert ")"))))))

(provide 'dvc-lisp)
;;; dvc-lisp.el ends here

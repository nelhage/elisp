;;; xmtn-basic-io.el --- A parser for monotone's basic_io output format

;; Copyright (C) 2008 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Maintainer: Stephen Leake stephen_leake@stephe-leake.org
;; Keywords: tools, extensions

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

;; This library helps parse data in monotone's basic_io format.
;;
;; See docstrings for details.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

;;; I haven't seen a specification for monotone's basic_io format.
;;; I'm implementing this parser somewhat defensively.

;;; Maybe strings in basic_io are always encoded as UTF-8?  In that
;;; case, the decoding code for filenames and cert names/values that
;;; is currently spread across several functions could be moved
;;; directly in here.

;;; `parse-partial-sexp'/`scan-sexps', with an appropriate syntax
;;; table, looks like the best way to do this kind of parsing.  It is
;;; very likely faster than anything we can implement by hand in Emacs
;;; Lisp.

;;; Much of the code in here has been tuned for speed quite a bit.
;;; Careful with refactorings!  For example, introducing a variable
;;; binding that the byte-compiler can't optimize away can mean a
;;; major slowdown.

;;; Using cons cells instead of two-element lists is only a very minor
;;; performance advantage (<.5%).  Also, with cons cells, `null-id'
;;; would have to be a bare symbol, while `id' and `string' would be
;;; cons cells; with lists, the representation is more uniform.

(eval-and-compile
  (require 'cl)
  (require 'xmtn-base)                  ; for xmtn--hash-id
  )

(defvar xmtn-basic-io--*syntax-table*
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?\[ "("  table)
    (modify-syntax-entry ?\] ")"  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "/"  table)
    table))

(defsubst xmtn-basic-io--unescape-field (string)
  (loop with start = 0
        while (string-match "\\\\" string start)
        do (setq string (replace-match "" t t string))
        do (setq start (1+ (match-end 0))))
  string)

(defsubst xmtn-basic-io--read-key ()
  ;; Calling `xmtn--debug-mark-text-processed' from here is way too
  ;; slow.
  (let ((start (point)))
    (skip-syntax-forward "w_")
    (xmtn--assert-optional (> (point) start))
    (xmtn--assert-optional (member (char-after (point)) '(?\  ?\n)))
    (let ((key (buffer-substring-no-properties start (point))))
      (xmtn--assert-optional (string-match "\\`[a-z_]+\\'" key) t)
      ;;(xmtn--debug-mark-text-processed (current-buffer) start (point) t)
      key)))

(defsubst xmtn-basic-io--read-field ()
  "Return a list containing the class and value of the field at point.
Possible classes are `string', `null-id', `id', `symbol'."
  ;; Calling `xmtn--debug-mark-text-processed' from here is way too
  ;; slow.
  (let ((end (scan-sexps (point) 1)))
    (xmtn--assert-optional end)
    (xmtn--assert-optional (> end (point)))
    (prog1
        (case (char-after (point))
          (?\" ; a string
           (list 'string (xmtn-basic-io--unescape-field
                              (buffer-substring-no-properties (1+ (point))
                                                              (1- end)))))
          (?\[ ; an id
             (cond ((eq (1+ (point)) (1- end)) ;see (elisp) Equality Predicates
                    (list 'null-id))
                   (t
                    (xmtn--assert-optional
                     (typep (buffer-substring-no-properties (1+ (point))
                                                            (1- end))
                            'xmtn--hash-id))
                    (list 'id (buffer-substring-no-properties (1+ (point))
                                                              (1- end))))))
          (t ; a symbol
           (list 'symbol (buffer-substring-no-properties (point) end))))
      (goto-char end)
      (xmtn--assert-optional (member (char-after) '(?\n ?\ ))))))

(defsubst xmtn-basic-io--skip-white-space ()
  ;; Calling `xmtn--debug-mark-text-processed' from here is way too slow.
  (skip-chars-forward " "))

(defun xmtn-basic-io-skip-blank-lines ()
  "Skip blank lines (if any), so parser starts on a stanza."
  (beginning-of-line)
  (while
      (case (char-after)
        ((?\n)
         (forward-char 1)
         t)
        ((? )
         (skip-chars-forward " ")
         t)
        (t
         nil)))
  (beginning-of-line))

(defsubst xmtn-basic-io--parse-nonempty-line ()
  (xmtn-basic-io--skip-white-space)
  (prog1
      (list* (xmtn-basic-io--read-key)
             (loop while (progn
                      (xmtn-basic-io--skip-white-space)
                      (not (eq (char-after) ?\n)))
                   collect (xmtn-basic-io--read-field)))
    (forward-char 1)))

(defsubst xmtn-basic-io--peek ()
  (case (char-after)
    ((?\n) 'empty)
    ((nil) 'eof)
    (t t)))

(defun xmtn-basic-io--next-parsed-line ()
  (case (char-after)
    ((?\n)
     (forward-char 1)
     'empty)
    ((nil)
     'eof)
    (t
     (xmtn-basic-io--parse-nonempty-line))))

(defun xmtn-basic-io--next-stanza ()
  (let ((stanza (let ((accu nil)
                      (line nil))
                  (loop do (setq line (xmtn-basic-io--next-parsed-line))
                        do (xmtn--assert-optional (not (and (null accu)
                                                            (eq line 'empty))))
                        until (memq line '(empty eof))
                        do
                        (xmtn--assert-optional (listp line))
                        (xmtn--assert-optional (not (endp line)))
                        (push line accu))
                  (nreverse accu))))
    stanza))

(defun xmtn-basic-io--next-parsed-line-notinline ()
  (xmtn-basic-io--next-parsed-line))

(eval-and-compile
  (defun xmtn-basic-io--generate-body-for-with-parser-form (parser-fn
                                                            parser-var
                                                            buffer-form body)
    (let ((buffer (gensym)))
      `(let ((,buffer ,buffer-form))
         (with-current-buffer ,buffer
           (set-syntax-table xmtn-basic-io--*syntax-table*)
           (goto-char (point-min)))
         (let ((,parser-var (lambda ()
                              (with-current-buffer ,buffer
                                (,parser-fn)))))
           ,@body)))))

(defmacro xmtn-basic-io-parse-line (body)
  "Read next basic-io line at point. Error if it is `empty' or
`eof'. Otherwise execute BODY with `symbol' bound to key (a
string), `value' bound to list containing parsed rest of line.
List is of form ((category value) ...)."
  (declare (indent 1) (debug (sexp body)))
  `(let ((line (xmtn-basic-io--next-parsed-line)))
     (if (member line '(empty eof))
         (error "expecting a line, found %s" line)
       (let ((symbol (car line))
             (value (cdr line)))
         ,body))))

(defmacro xmtn-basic-io-optional-line (expected-key body-present body-absent)
  "Read next basic-io line at point. If its key is
EXPECTED-KEY (a string), execute BODY-PRESENT with `value' bound to list
containing parsed rest of line. List is of form ((category value)
...). Else execute BODY-ABSENT, with `value' bound to nil."
  (declare (indent 1) (debug (sexp body)))
  `(let ((line (xmtn-basic-io--next-parsed-line)))
     (if (and (not (member line '(empty eof)))
              (string= (car line) ,expected-key))
         (let ((value (cdr line)))
           ,body-present)
       (let ((value nil))
         ,body-absent)
       )))

(defmacro xmtn-basic-io-check-line (expected-key body)
  "Read next basic-io line at point. Error if it is `empty' or
`eof', or if its key is not EXPECTED-KEY (a string). Otherwise
execute BODY with `value' bound to list containing parsed rest of
line. List is of form ((category value) ...)."
  (declare (indent 1) (debug (sexp body)))
  `(let ((line (xmtn-basic-io--next-parsed-line)))
     (if (or (member line '(empty eof))
             (not (string= (car line) ,expected-key)))
         (error "expecting \"%s\", found %s" ,expected-key line)
       (let ((value (cdr line)))
         ,body))))

(defun xmtn-basic-io-check-empty ()
  "Read next basic-io line at point. Error if it is not `empty' or `eof'."
  (let ((line (xmtn-basic-io--next-parsed-line)))
    (if (not (member line '(empty eof)))
        (error "expecting an empty line, found %s" line))))

(defmacro* xmtn-basic-io-with-line-parser ((line-parser buffer-form) &body body)
  "Run BODY with LINE-PARSER bound to a parser that parses BUFFER-FORM.

BUFFER-FORM should evaluate to a buffer that contains, between
\(point-min\) and \(point-max\), zero or more lines in monotone's
basic_io format.

BODY will be evaluated with LINE-PARSER \(a symbol\) bound to a
closure that will, each time it is called, return the next line
in parsed form, or the symbol `eof' if there are no more lines.

Empty lines are returned as the symbol `empty'.

Each non-empty line is a list of a key and zero or more fields.
The key is a string.  Each field is either a one-element list
\(null-id\) and represents an empty ID field \(what monotone
prints as \[\] in basic_io format\), a two-element list \(id
HASH-ID\), where HASH-ID is a string of forty hexadecimal digits
\(what monotone prints as \[HASH-ID\]\), or a two-element list
\(string STRING\), where STRING is a string (what monotone prints
as \"STRING\"\).

Lines and their contents are always fresh objects.

The macro `xmtn-match' is a useful way to process basic_io lines
parsed this way.

The parser should be assumed to have dynamic extent.  If the
contents of the buffer that BUFFER-FORM evaluates to, or the
position of point in that buffer, are modified from within BODY
\(other than by calling the parser\), the parser becomes invalid
and must not be called any more."
  (declare (indent 1) (debug (sexp body)))
  (xmtn-basic-io--generate-body-for-with-parser-form
   ;; Use a notinline variant to avoid copying the full parser into
   ;; every user of this macro.  The performance advantage of this
   ;; would be small.
   'xmtn-basic-io--next-parsed-line-notinline
   line-parser buffer-form body))

(defmacro* xmtn-basic-io-with-stanza-parser ((stanza-parser buffer-form)
                                             &body body)
  "Run BODY with STANZA-PARSER bound to a parser that parses BUFFER-FORM.

BUFFER-FORM should evaluate to a buffer that contains,
between (point-min) and (point-max), zero or more lines in
monotone's basic_io format.

BODY will be evaluated with STANZA-PARSER \(a symbol\) bound to a
closure that will, each time it is called, return the next stanza
in parsed form, or the symbol `nil' if there are no more stanzas.

Each stanza will be returned as a fresh, non-empty list of
so-called lines.  See `xmtn-basic-io-with-line-parser' for a
definition of the term \"line\" in this context.

The macro `xmtn-match' and the function `assoc' are useful to
process basic_io stanzas parsed this way.

The parser should be assumed to have dynamic extent.  If the
contents of the buffer that BUFFER-FORM evaluates to, or the
position of point in that buffer, are modified from within BODY
\(other than by calling the parser\), the parser becomes invalid
and must not be called any more."
  (declare (indent 1) (debug (sexp body)))
  (xmtn-basic-io--generate-body-for-with-parser-form
   'xmtn-basic-io--next-stanza
   stanza-parser buffer-form body))

(defun xmtn-basic-io-write-id (key id)
  "Write a basic-io line with KEY, hex ID."
  (insert key)
  (insert " [")
  (insert id)
  (insert ?\])
  (insert ?\n))

(defun xmtn-basic-io-write-str (key str)
  "Write a basic-io line with KEY, string STR."
  (insert key)
  (insert " \"")
  (insert str)
  (insert ?\")
  (insert ?\n))

(defun xmtn-basic-io-write-sym (key sym)
  "Write a basic-io line with KEY, symbol SYM."
  (insert key)
  (insert " ")
  (insert sym)
  (insert ?\n))

(provide 'xmtn-basic-io)

;;; xmtn-basic-io.el ends here

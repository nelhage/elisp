;;; xmtn-base.el --- Basic definitions for accessing monotone

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

;; This file contains basic definitions for accessing the distributed
;; version control system monotone.

;;; Code:

;;; There are some notes on the design of xmtn and its related
;;; files in docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl))

(defvar xmtn-executable "mtn"
  "*The monotone executable command.

After changing the value of this variable, be sure to run
`xmtn-check-command-version' to clear xmtn's command version
cache.")

(defvar xmtn-additional-arguments '()
  "*Additional arguments to pass to monotone.

A list of strings.")

(deftype xmtn--hash-id ()
  `(and string
        (satisfies xmtn--hash-id-p)))

(defun xmtn--hash-id-p (thing)
  (and (stringp thing)
       ;; This is twenty times faster than an equivalent Elisp loop.
       (save-match-data
         (string-match "\\`[0-9a-f]\\{40\\}\\'" thing))))

(defvar xmtn--*enable-assertions* nil
  "Effective at macroexpansion time.")

;; (setq xmtn--*enable-assertions* t)

(defmacro xmtn--assert-for-effect (form &rest more-assert-args)
  (if xmtn--*enable-assertions*
      `(assert ,form ,@more-assert-args)
    `(progn ,form nil)))

(defmacro xmtn--assert-optional (form &rest more-assert-args)
  (if xmtn--*enable-assertions*
      `(assert ,form ,@more-assert-args)
    `nil))

(defmacro xmtn--assert-nil ()
  `(assert nil))

(provide 'xmtn-base)

;;; xmtn-base.el ends here

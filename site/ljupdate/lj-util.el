;;; lj-util.el --- misc elisp utilities for ljupdate

;; Copyright (C) 2002, 2003, 2004, 2005 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is part of ljupdate, a LiveJournal client for Emacs.

;; ljupdate is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; ljupdate is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(require 'md5)

(require 'lj-compat)

(defun lj-md5 (string)
  "MD5s STRING and downcases the result.
The LiveJournal server code doesn't accept upcased MD5sums. Case
sensitivity is dumb."
  (downcase (md5 string nil nil lj-coding-system)))

(defun lj-number (thing)
  "Convert THING to a number, if necessary."
  (cond ((numberp thing) thing)
        ((stringp thing) (string-to-number thing))
        (t 0)))

(defun lj-exp2 (n)
  "Return a string representation of 2^N for 0 <= N <= 30."
  (cond ((or (< n 0) (> n 30))
         (signal 'args-out-of-range n))
	((< n 27) (number-to-string (lsh 1 n)))
        ;; Emacs integers aren't 32-bit quantities, so we cheat.
	((= n 27) "134217728")
	((= n 28) "268435456")
	((= n 29) "536870912")
	((= n 30) "1073741824")))

(provide 'lj-util)

;;; lj-util.el ends here

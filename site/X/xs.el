;;; Xs --- Xstructure routines

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xs.el,v 1.4 1998/02/28 15:19:29 zappo Exp $
;;
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
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;; Structures of messages are interpreted here.

(require 'xmath)

;;; Code:
(defconst X-byte-order ?l "Byte order used by emacs X.  B MSB, l LSB.")
(defconst X-protocol-minor-version 0 "Minor version of client.")
(defconst X-protocol-major-version 11 "Major version of client.")

(defconst X-client-to-open
  (list [1 X-byte-order]
	[1 0]				;unused
	[2 X-protocol-major-version]
	[2 X-protocol-minor-version]
	[2 0]				;auth name
	[2 0]				;auth data
	[2 0]				;unused
	;; No auth name or data, so empty
	)
  "XStruct list of sizes when opening a connection.")

(defmacro X-force-char-num (maybechar)
  "Force MAYBECHAR to be a number for XEmacs platform."
  ;; This is an annoying XEmacs problem  To bad it slows down
  ;; Emacs too.
  nil)

(defun X-create-message (message-s)
  "Takes the MESSAGE-S structure and builds a net string.
MESSAGE-S is a list of vectors and symbols which formulate the message
to be sent to the XServer.  Each vector is of this form:
  [ SIZE VALUE ]
  SIZE is the number of BYTES used by the message.
  VALUE is the lisp object whose value is to take up SIZE bytes.
  If VALUE or SIZE is a symbol or list, extract that elements value.
    If the resulting value is still a list or symbol, extract it's value
    until it is no longer a symbol or a list.
  If VALUE is a number, massage it to the correct size.
  If VALUE is a string, append that string verbatum.
  If VALUE is nil, fill it with that many NULL characters."
  
  (let ((news nil)
	(ts   nil)
	(tvec nil)
	(tval nil)
	(tlen nil))
    (while message-s
      (setq tvec (car message-s))
      (setq tval (aref tvec 1))
      (setq tlen (aref tvec 0))
      ;; check for symbols, or symbols containing symbols.
      (while (and tlen (or (listp tlen) (symbolp tlen)))
	(setq tlen (eval tlen)))
      ;; check for symbols, or symbols containing symbols.
      (while (and tval (or (listp tval) (symbolp tval)))
	(setq tval (eval tval)))
      ;; Fix XEmacs 20 broken characters
      (X-force-char-num tval)
      ;; numbers, put in.
      (cond
       ;; numbers get converted based on size.
       ((numberp tval)
	(cond
	 ((= tlen 1)
	  (setq ts (int->string1 tval)))
	 ((= tlen 2)
	  (setq ts (int->string tval)))
	 ((= tlen 4)
	  (setq ts (int->string4 tval)))
	 (t
	  (error "Wrong size for a message part to be a number!"))))
       ;; strings get appended onto the end.
       ((stringp tval)
	(setq ts tval))
       ;; nil is usually filler, so stuff on some 0s
       ((equal tval nil)
	(setq ts (make-string tlen ?\C-@)))
       ;; some sort of error
       (t
	(error "Invalid type to be put into an Xmessage")))
      (setq ts (concat ts "\0\0\0\0"))	;make sure we fill length req.
      (setq ts (substring ts 0 tlen))
      (setq news (concat news ts))
      (setq message-s (cdr message-s)))
    (if (/= (% (length news) 4) 0)
	(let ((s "\0\0\0\0"))
	  (setq news (concat news (substring s 0 (- 4 (% (length news) 4)))))))
    news))


(provide 'xs)
;;; xs.el ends here

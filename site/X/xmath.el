;;; Xmath --- icky math things such as 4 byte ints, and int->string stuff

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xmath.el,v 1.5 1998/02/28 15:10:47 zappo Exp $
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
;;  numbers->string string->numbers
;;
;; These routines are needed to convert numbers into strings which
;; are passed over the network.
;;

;;; Code:
(defmacro Xtruncate (val)
  "Do a safe truncate of VAL that might be larger than MAXINT."
  (list 'truncate (list 'mod val 65536)))

(defmacro XCharacter (val)
  "Convert VAL (a float) into a truncated character value."
  (if (fboundp 'int-to-char)
      (list 'int-to-char
	    (list 'logand (list 'truncate (list 'mod val 65536)) 255))
    (if (>= emacs-major-version 20)
	(list 'logand (list 'truncate (list 'mod val 65536)) 255)
      (list 'truncate (list 'mod val 65536)))))

(defmacro Xforcenum (val)
  "Force VAL (a character) to be a number.
This macro forces XEmacs 20.3 to behave."
  (if (fboundp 'char-to-int) (list 'char-to-int val) val))

(defmacro int->string1 (num)
  "Convert NUM into a 1 byte string in network order and return."
  (list 'char-to-string num))
    
(defmacro string1->int (string)
  "Convert STRING characters into an integer and return."
  (list 'string->int string))

(defun int->string (num)
  "Convert NUM into a 2 byte string in network order and return."
  (let ((l (logand num 255))		;top byte
	(h (ash num -8)))		;upper byte
    (concat (char-to-string l) (char-to-string h))))
    
(defun string->int (string)
  "Convert STRING 1st two characters into an integer and return."
  (let ((l (aref string 0))
	(h (if (> (length string) 1) (aref string 1) 0)))
    (+ l (ash h 8))))

(defun int->string4 (num)
  "Convert NUM (a float or int) into a 4 byte network order string."
  (if (integerp num)
      ;; if it isn't a float, then do int things
      (concat (int->string num) (int->string 0)) ;0 upper part
    (if (> 0 num)
	(error "4 byte number is negative during conversion."))
    (let ((tmp (float num))
	  (ts nil))
      ;; We only need to truncate the first part.  After the first
      ;; 8 bit shift, the number is small enought that a regular
      ;; truncate is safe.
      (setq ts (concat ts (char-to-string (XCharacter tmp))))
      (setq tmp (/ tmp (float 256)))
      (setq ts (concat ts (char-to-string (XCharacter tmp))))
      (setq tmp (/ tmp (float 256)))
      (setq ts (concat ts (char-to-string (XCharacter tmp))))
      (setq tmp (/ tmp (float 256)))
      (setq ts (concat ts (char-to-string (XCharacter tmp)))))))

(defun string4->int (string)
  "Convert STRING 1st four characters into a float and return."
  ;; do nothing yet until we know what we need to do.
  (+ (float (Xforcenum (aref string 0)))
     (* (float (Xforcenum (aref string 1))) 256)
     (* (float (Xforcenum (aref string 2))) 256 256)
     (* (float (Xforcenum (aref string 3))) 256 256 256)))

(defun X-pad (number)
  "Return a number which is the padding for an X message of length NUMBER."
  (% (- 4 (% number 4)) 4))

(defun X-padlen (string)
  "Return a number which is length of STRING / 4.
If string is not divisible by 4, return string/4 + 1"
  (if (= (% (length string) 4) 0)
      (/ (length string) 4)
    (+ (/ (length string) 4) 1)))

;;; MASK routines:
;;
;; These routines are needed to handle the 4 byte masks used in X.
;; We won't implement the whole set, just the functionality we need
;; to make the checks we want.
;;
(defun Xmask (pos)
  "Create a mask with a bit set in position POS.
This routine will not work for position 32 and up because we sim
4 bytes of info"
  (if (< pos 16)
      (float (lsh 1 pos))		;put in first byte
    (setq pos (- pos  16))		;divide pos by 16
    (* (float (lsh 1 pos)) (float 65536)) ;push into high byte
    ))

(defun Xmask-and (val &rest args)
  "Logically `and' VAL and MASK together.
They are floats to be broken down into two two byte ints.
MASK is stored in ARGS which is a list of *fill in when I remember*"
  (while args
    (let ((mask (car args)))
      (setq args (cdr args))
      (let ((lv (logand (Xtruncate val) 65535))
	    (hv (Xtruncate (/ val (float 65536))))
	    (lm (logand (Xtruncate mask) 65535))
	    (hm (Xtruncate (/ mask (float 65536)))))
	(setq val (+ (float (logand lv lm))
		     (* (float (logand hv hm)) 65536))))))
  val)

(defun Xmask-or (val &rest args)
  "Logically or VAL and MASK together.
They are floats to be broken down into two two byte ints.
MASK is stored in ARGS which is a list of *fill in when I remember*"
  (while args
    (let ((mask (car args)))
      (setq args (cdr args))
      (let ((lv (logand (Xtruncate val) 65535))
	    (hv (Xtruncate (/ val (float 65536))))
	    (lm (logand (Xtruncate mask) 65535))
	    (hm (Xtruncate (/ mask (float 65536)))))
	(setq val (+ (float (logior lv lm))
		     (* (float (logior hv hm)) 65536))))))
  val)

(defun Xtest (val flag)
  "Test value of bytes VAL for presence of FLAG.
Return t if it exists, nil otherwise."
  (if (= (Xmask-and val flag) 0) nil t))

;;; BITWISE routines:
;;
;; These routines are used to do other things to bits, necessary for
;; calculating out new resource IDs for objects.
;;

(defun Xcount-bits-int (mask)
  "Count the number of bits in a given integer (16 bit) MASK."
  (let ((ret 0))
    (while (/= mask 0)
      (if (Xtest mask 1) (setq ret (1+ ret)))
      (setq mask (ash mask -1)))
    ret))

(defun Xmask-count (mask)
  "Count the number of bits set in the mask MASK.
This is needed to identify new objects (client-selectable) thingies."
  (let ((lv (Xtruncate mask))
	(hv (Xtruncate (/ mask (float 65536)))))
    (+ (Xcount-bits-int lv) (Xcount-bits-int hv))))

(defun Xmask-int-string (mask)
  "Convert MASK as an integer into a string of 0s and 1s."
  (let ((cnt 15)
	(s nil))
    (while (/= cnt -1)
      (setq s (concat s (if (= (logand mask (lsh 1 cnt)) 0) "0" "1")))
      (setq cnt (1- cnt)))
    s))

(defun Xmask-string (mask)
  "Convert MASK into a string of 0s and 1s."
  (let ((lv (Xtruncate mask))
	(hv (Xtruncate (/ mask (float 65536)))))
    (concat (Xmask-int-string hv) (Xmask-int-string lv))))

(defun Xmask-int-hex-string (mask &optional fill)
  "Convert the integer MASK into a full hexidecimal number.
Optional argument FILL means to add 0s as necessary."
  (let ((s (format "%x" mask)))
    (if fill (substring (concat "0000" s) (length s)) s)))

(defun Xmask-hex-string (mask)
  "Convert MASK into a hexidecimal string."
  (let ((lv (Xtruncate mask))
	(hv (Xtruncate (/ mask (float 65536)))))
    (concat "0x"
	    (Xmask-int-hex-string hv)
	    (Xmask-int-hex-string lv (/= hv 0)))))

;;; IDs:
;;
;; These routines handle the generation of new IDs which are unique
;; to a given system.
;;
(defvar X-id-counter 1
  "The next available ID to be used.
It must, however, be massaged into the 18 bit mask we get from the server.")

(defun Xid-get (display)
  "Return a float (4 byte id) which will be a unique id.
The id will be for some resource on DISPLAY."
  (let* ((newid X-id-counter)
	 (newword (float 0))
	 (bitcnt 0)			;bit counter in mask
	 (idcnt 0)			;bit counter in id
	 (servmask (aref display 5))	;service mask (our unique bits)
	 (servbase (aref display 4)))	;service base (always set)
    ;; we can say <30 because top 3 bits are always 0
    (while (< bitcnt 30)		;while there is more in the mask
      (if (Xtest servmask (Xmask bitcnt))
	  (progn
	    (if (Xtest newid (Xmask idcnt)) ;set bit in id if it is
					    ;set in the id value.
		(setq newword (Xmask-or newword (Xmask bitcnt))))
	    (setq idcnt (1+ idcnt))))	;inc idcnt when we have a mask match
      (setq bitcnt (1+ bitcnt)))	;always inc bitmask cnter
    (setq X-id-counter (1+ X-id-counter)) ;inc to next id counter value
    (Xmask-or newword servbase)))	;return the id with base attached

(provide 'xmath)
;;; xmath.el ends here

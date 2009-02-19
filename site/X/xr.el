;;; xr --- X-receive parts of X stuff

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xr.el,v 1.6 1998/03/10 23:42:36 zappo Exp $
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

;;; Contributors:
;;  Some events contributed by emarsden@mail.dotcom.fr

;;; Code:
(defvar X-interpolator-message-buffer nil
  "Buffer used in interpolating messages received in segments.")

;;; All receive message types will exclude the first byte which IDs it.
;;
;; a symbol gets 'set, functions such as integerp mean turn it into that,
;; and put it into the return list. 'arg means use next arg as this value.
(defun X-mod-4 (len) "Return a the number LEN moded to 4."
  (if (= (% len 4) 0) 0 (- 4 (% len 4))))

(defconst X-connect-response
  (list [1 success]
	(list [1 length-1]		;fail message len
	      [2 integerp]		;major version
	      [2 integerp]		;minor version
	      [2 length-2]		;pad length
	      [length-1 stringp]	;error conditions
	      [(X-mod-4 length-1) nil]	;padding
	      )
	(list [1 nil]			;successful list (this is unused)
	      [2 integerp]		;major version
	      [2 integerp]		;minor version
	      [2 length-1]		;len additional data (pad)
	      [4 integerp]		;release number
	      [4 integerp]		;resource id base
	      [4 integerp]		;resource id mask
	      [4 integerp]		;motion buffer size
	      [2 length-2]		;vendor length
	      [2 integerp]		;max request len
	      [1 length-4]		;number of screens
	      [1 length-3]		;number of formats in pix list
	      [1 integerp]		;image byte order
	      [1 integerp]		;bitmap byte order
	      [1 integerp]		;bitmap format scanline thingy
	      [1 integerp]		;bitmap format scanline pad
	      [1 integerp]		;min keycode
	      [1 integerp]		;max keycode
	      [4 nil]			;unused
	      [length-2 stringp]	;the vendor
	      [(X-mod-4 length-2) nil]	;padding
	      [length-3 		;sublist of formats
	       ( [1 integerp]		;depth
		 [1 integerp]		;bits/pixel
		 [1 integerp]		;scanline-pad
		 [5 nil] ) ]		;padding
	      [length-4
	       ( [4 integerp]		;root window
		 [4 integerp]		;colormap
		 [4 integerp]		;white-pixel
		 [4 integerp]		;black-pixel
		 [4 integerp]		;event-flags
		 [2 integerp]		;screen-width
		 [2 integerp]		;screen-height
		 [2 integerp]		;milimeters width
		 [2 integerp]		;milimeters height
		 [2 integerp]		;min-installed-maps
		 [2 integerp]		;max installed maps
		 [4 integerp]		;visualid
		 [1 integerp]		;backingstores
		 [1 booleanp]		;save-unders
		 [1 integerp]		;root depth
		 [1 length-1]		;# depths in depth
		 [length-1		;list of depths
		  ( [1 integerp]	;depth
		    [1 nil]
		    [2 length-1]	;# visual types
		    [4 nil]
		    [length-1		;the visuals
		     ( [4 integerp]	;visual id
		       [1 integerp]	;class
		       [1 integerp]	;bits/rgb value
		       [2 integerp]	;colormap entities
		       [4 integerp]	;red mask
		       [4 integerp]	;green mask
		       [4 integerp]	;blue mask
		       [4 nil])
		     ] )
		  ] )
	       ] )
	)
  "Connection response structure.")

(defun X-grab-bytes (dpy num)
  "On display DPY, wait for at least NUM bytes to show up and return string."
  (X-dpy-p dpy 'X-grab-bytes)
  (let ((tmp-io nil)
	(tmp-int 0))
    (save-excursion
      (set-buffer (aref dpy 0))
      (while (and (< (length X-interpolator-message-buffer) num)
		  (< tmp-int 10))
	(accept-process-output (get-buffer-process (aref dpy 0)))
	(setq tmp-int (1+ tmp-int)))
      (if (< tmp-int 10)
	  nil ;(if waited (message "X: Reading ... done"))
	(error "X: Timeout reading from server."))
      (setq tmp-io (substring X-interpolator-message-buffer 0 num))
      (setq X-interpolator-message-buffer
	    (substring X-interpolator-message-buffer num))
      tmp-io)))

(defvar X-reading-mode nil
  "Flag indicating data is expected, so don't even think about parsing.")

(defun X-parse-message-guess (dpy)
  "There is data waiting on DPY, but no-one is reading it.
Try to guess what it is."
  (if (not X-reading-mode)
      (while ( > (length X-interpolator-message-buffer) 0)
	(let* ((ListOfFields (list [1 integerp])) ;opcode (event/error)
	       (answer (car (X-parse-message ListOfFields t dpy))))
	  (if (= (logand X-SyntheticMask answer) X-SyntheticMask)
	      (X-eval-response dpy (- answer X-SyntheticMask) t)
	    (X-eval-response dpy answer nil))))))


(defconst X-EventLists
  [ nil nil
   [ "KeyPress"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; same_screen
       [1 nil] )
     3 ]
   [ "KeyRelease"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; same_screen
       [1 nil] )
     3 ]
   [ "ButtonPress"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; same_screen
       [1 nil] )
     3 ]
   [ "ButtonRelease"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; same_screen
       [1 nil] )
     3 ]
   [ "MotionNotify"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; same_screen
       [1 nil] )
     3 ]
   [ "EnterNotify"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; mode
       [1 nil] )
     3 ]
   [ "LeaveNotify"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; time
       [4 integerp] ; root
       [4 integerp] ; event
       [4 integerp] ; child
       [2 integerp] ; root_x
       [2 integerp] ; root_y
       [2 integerp] ; event_x
       [2 integerp] ; event_y
       [2 integerp] ; state
       [1 integerp] ; mode
       [1 nil] )
     3 ]
   [ "FocusIn"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; event
       [1 integerp] ; mode
       [23 nil] )
     3 ]
   [ "FocusOut"
     ( [1 nil]
       [2 integerp] ; sequence
       [4 integerp] ; event
       [1 integerp] ; mode
       [23 nil] )
     3 ]

   [ "KeymapNotify"  () nil ]
   [ "Expose"
     ( [1 nil]
       [2 integerp] ;sequence
       [4 integerp] ;window
       [2 integerp] ;x
       [2 integerp] ;y
       [2 integerp] ;width
       [2 integerp] ;height
       [2 integerp] ;count
       [14 nil] )
     1 ]
   [ "GraphicsExpose"   () nil ]
   [ "NoExpose"         () nil ]
   [ "VisibilityNotify" () nil ]
   [ "CreateNotify"     () nil ]
   [ "DestroyNotify"    () nil ]
   [ "UnmapNotify"      () nil ]
   [ "MapNotify"        () nil ]
   [ "MapRequest"       () nil ]
   [ "ReparentNotify"   () nil ]
   [ "ConfigureNotify"
     ( [1 nil]
       [2 integerp] ;sequence
       [4 integerp] ;event
       [4 integerp] ;window
       [4 integerp] ;above-sibling
       [2 integerp] ;x
       [2 integerp] ;y
       [2 integerp] ;width
       [2 integerp] ;height
       [2 integerp] ;border-width
       [1 integerp] ;override-redirect
       [5 nil] )
     2 ]
   [ "ConfigureRequest" () nil ]
   [ "GravityNotify"    () nil ]
   [ "ResizeRequest"
     ( [1 nil]
       [2 integerp] ;sequence
       [4 integerp] ;window
       [2 integerp] ;width
       [2 integerp] ;height
       [20 nil] )
     1 ]
   [ "CirculateNotify"   () nil ]
   [ "CirculateRequest"  () nil ]
   [ "PropertyNotify"    () nil ]
   [ "SelectionClear"    () nil ]
   [ "SelectionRequest"  () nil ]
   [ "SelectionNotify"   () nil ]
   [ "ColormapNotify"    () nil ]
   [ "ClientMessage"
     ([1 length-1] ;format
      [2 integerp] ;sequence number
      [4 integerp] ;window
      [4 integerp] ;atom
      ;; This reads in the correct number of integers of a type
      ;; specified by the format which is 8, 16, or 32.
      [(/ 20 (/ length-1 8)) ( [ (/ length-1 8) integerp ] ) ] )
     1 ]
   [ "MappingNotify" () nil ]
   ]
  "Matrix of all event names, and their corresponding decoding list.")

(defvar X-synthetic-event nil
  "This variable is non-nil if the currently processed event is synthetic.")

(defun X-eval-response (dpy answer X-synthetic-event)
  "On DPY, handles opcodes in ANSWER that are not 1.
X-SYNTHETIC-EVENT is non-nil iff this event was generated with a `XSendEvent'
command."
  (cond
   ((= answer 0)
    ;; autoclose on errors!
    (X-parse-errors dpy t))
   ;; expose event
   ((= answer 12)
    (let* ((ListOfFields (aref (aref X-EventLists answer) 1))
	   (ans (X-parse-message ListOfFields t dpy))
	   (win (X-window-find (car (cdr ans)))))
      (if (and win (aref win 3) (fboundp (aref win 3)))
	  (funcall (aref win 3) win ans)
	(message "Window %S needs to be exposed.\n" win))))
   ;; reconfigure request
   ((= answer 22)
    (let* ((ListOfFields (aref (aref X-EventLists answer) 1))
	   (ans (X-parse-message ListOfFields t dpy))
	   (win (X-window-find (car (cdr ans)))))
      (if (and win (aref win 5) (fboundp (aref win 5)))
	  (funcall (aref win 5) win answer ans)
	(message "Window %S needs to be reconfigured.\n" win))))
   ;; resize request
   ((= answer 25)
    (let* ((ListOfFields (aref (aref X-EventLists answer) 1))
	   (ans (X-parse-message ListOfFields t dpy))
	   (win (X-window-find (car (cdr ans)))))
      (if (and win (aref win 5) (fboundp (aref win 5)))
	  (funcall (aref win 5) win answer ans)
	(message "Window %S needs to be resized.\n" win))))
   ((or (< answer 2) (>= answer X-MaxEvent))
    (let* ((ListOfFields (list [31 nil]))
 	   (ans (X-parse-message ListOfFields t dpy)))
      (X-log dpy "Got out-of-bounds event %d\n" answer)))
   (t
    (let* ((es (aref X-EventLists answer))
	   (ListOfFields (aref es 1))
	   (ans (X-parse-message (or ListOfFields (list [31 nil])) t dpy))
	   (handlers nil)
	   (win nil))
      (if (aref es 2)
          (setq win (X-window-find (nth (aref es 2) ans))
                handlers (aref win 6)))

      (if (not win)
          (X-log dpy "Event %s not associated with a window.\n" (aref es 0))
	(X-log dpy "Event %s to be handled.\n" (aref es 0))
	(setq handlers (assoc answer handlers))
	(if handlers
	    (funcall (cdr handlers) win ans)))))))
  
(defun X-parse-errors (dpy close)
  "Parse an error on DPY.  They all have similar forms. Close if CLOSE.
Error opcode is always already read by message-guess er."
  (let* ((ListOfFields (list [1 integerp])) ;opcode (error event)
	 (answer nil)
	 (ans (car (X-parse-message ListOfFields t dpy))))
    (cond
     ((= ans 2)
      (let* ((ListOfFields (list [2 integerp] ;sequence
				 [4 integerp] ;resource id
				 [2 integerp] ;minor op code
				 [1 integerp] ;major op code
				 [21 nil])))
	(setq answer (X-parse-message ListOfFields t dpy))
	(X-log dpy "Bad value %s sequence %d ops %d %d\n"
	       (Xmask-string (car (cdr answer)))
	       (car answer)
	       (car (cdr (cdr (cdr answer))))
	       (car (cdr (cdr answer))))))
     ((= ans 3)
      (let* ((ListOfFields (list [2 integerp] ;sequence
				 [4 integerp] ;resource id
				 [2 integerp] ;minor op code
				 [1 integerp] ;major op code
				 [21 nil])))
	(setq answer (X-parse-message ListOfFields t dpy))
	(X-log dpy "Bad window %.0f sequence %d ops %d %d\n"
	       (car (cdr answer)) (car answer)
	       (car (cdr (cdr (cdr answer))))
	       (car (cdr (cdr answer))))))
     ((= ans 11)
      (let* ((ListOfFields (list [2 integerp] ;sequence
				 [4 nil] ;resource id
				 [2 integerp] ;minor op code
				 [1 integerp] ;major op code
				 [21 nil])))
	(setq answer (X-parse-message ListOfFields t dpy))
	(X-log dpy "Alloc failure sequence %d ops %d %d\n"
	       (car (cdr (cdr (cdr answer))))
	       (car (cdr (cdr answer))))))
     ((= ans 14)
      (let* ((ListOfFields (list [2 integerp] ;sequence
				 [4 integerp] ;resource id
				 [2 integerp] ;minor op code
				 [1 integerp] ;major op code
				 [21 nil])))
	(setq answer (X-parse-message ListOfFields t dpy))
	(X-log dpy "Bad id %s sequence %d ops %d %d\n"
	       (Xmask-string (car (cdr answer)))
	       (car answer)
	       (car (cdr (cdr (cdr answer))))
	       (car (cdr (cdr answer))))))
     ((= ans 16)
      (let* ((ListOfFields (list [2 integerp] ;sequence
				 [4 nil]      ;unused
				 [2 integerp] ;minor op code
				 [1 integerp] ;major op code
				 [21 nil])))
	(setq answer (X-parse-message ListOfFields t dpy))
	(X-log dpy "Length error! sequence %d ops %S %d\n"
	       (car answer)
	       (car (cdr (cdr (cdr answer))))
	       (car (cdr (cdr answer))))))
     (t
      (X-log dpy "Got error event %d!!!\n" ans)))
    ;; make sure connection closes when there is an error!
    (if close
	(X-log dpy "Closing on event %d!\n" ans)
	(X-close dpy)
      ;; otherwise, we want that response back!
      answer)))

;; These are defined so we can use them recursivly below
(defvar length-1 nil)
(defvar length-2 nil)
(defvar length-3 nil)
(defvar length-4 nil)

(defun X-parse-message (message-s may-guess dpy &rest arglist)
  "Receive (via filter and waiting) a response from  the X server.
Parses MESSAGE-S structure.  When MAY-GUESS is t then if 1st el is not 1 or 0,
we must process as an event instead.  Then keep looping on guess until we get
a 0 or 1.  If not, then we are processing sub-lists.  Processing is done for
DPY.  ARGLIST is some list of arguments.

MESSAGE-S is made of size vectors `X-create-message':

  [SIZE ENCODING]

  SIZE is how many bytes it occupies in the message.
  ENCODING is how to interpret it.

  If encoding is 'success, then the following vectors are two lists.
The first is the Failure case.  nil is a generic failure.
The second is the Success case.

  Encoding can also be one of the following:
  nil      -- Not used
  integerp -- Format integer
  stringp  -- Formatted string
  length-# -- Number stored in variable `length-#' where # is 0-4.

The length-# variables are used to read a length from one section
of a message, and use it as the size field of a later occuring field.
A variable-length string can occur like this:

  [2 length-0]       ; length of string, does not appear in the list
  [length-0 stringp] ; name"

  (if (processp dpy)
      (setq dpy (make-vector 1 (process-buffer dpy))))
  (X-dpy-p dpy 'X-parse-message)
  (let ((rlist nil)
	(X-reading-mode t)		;Set reading mode to non-list
	(reverse-me t)
	(length-1 (if (boundp 'length-1) length-1 nil))
	(length-2 (if (boundp 'length-2) length-2 nil))
	(length-3 (if (boundp 'length-3) length-3 nil))
	(length-4 (if (boundp 'length-4) length-4 nil)) )
    (while (and message-s (listp message-s))
      (let* ((tvec (car message-s))
	     (tlen (aref tvec 0))
	     (tval1 (aref tvec 1))
	     (tval (if (and (listp tval1) (member (car tval1) '(or if)))
		       (eval tval1) tval1))
	     (result (if (not (and tval (listp tval)))
			 ;;do not grab bytes for sub-lists
			 (if (or (symbolp tlen) (listp tlen))
			     (X-grab-bytes dpy (eval tlen))
			   (X-grab-bytes dpy tlen)))))
	;; we need to put in code to represent sizes sometimes,
	;; this will get that size.
	(if (or (listp tlen) (symbolp tlen))
	    (setq tlen (eval tlen)))
	;; check for use of an argument.
	(if (equal tval 'arg)
	    (progn
	      (setq tval (car args))
	      (setq args (cdr args))))
	;; If the val is a list, and it is an if statement, then
	;; we want to evaluate it to get the real tval type.
	(if (and (listp tval) (member tval '(if or)))
	    (setq tval (eval tval)))
	(cond
	 ;; boolean success stories.
	 ((equal tval 'success)
	  (let ((sublst
		 (cond
		  ;; aref result because it is a string
		  ((= (aref result 0) 0)
		   ;; error condition
		   (if (car (cdr message-s))
		       ;; parse error list
		       (X-parse-message (car (cdr message-s)) nil dpy arglist)
		     ;; else get error, but don't quit
		     (X-parse-errors dpy nil)))
		   ((= (aref result 0) 1)
		    ;; success condition
		    (X-parse-message (car (cdr (cdr message-s))) nil dpy arglist))
		   (t
		    ;; other weirdness.  What of may-guess?  Remove?
		    (if may-guess
			(X-eval-response dpy (aref result 0) nil))))))
	    (if (= (aref result 0) 0)
		(setq rlist (cons nil sublst))
	      (setq rlist (cons t sublst))))
	  (setq message-s nil)
	  (setq reverse-me nil))
	 ;; integerp means tac onto end of list as an int
	 ((eq tval 'integerp)
	  (if (<= tlen 2)
	      (setq rlist (cons (string->int result) rlist))
	    (setq rlist (cons (string4->int result) rlist))))
	 ;; stringp means tac onto end of list as string (verbatim)
	 ((eq tval 'stringp)
	  (setq rlist (cons result rlist)))
	 ;; booleans don't really exist, but turn a 0 into nil, and 1 into t
	 ((eq tval 'booleanp)
	  (setq rlist (cons (if (= 0 (string->int result)) nil t) rlist)))
	 ;; if it is a list, then we need to recursivly call ourselvs X
	 ;; times on it.
	 ((and tval (listp tval))
	  ;; WARNING: subparts cannot use args. ;(
	  (let ((sublst nil))
	    (while (> tlen 0)
	      (setq sublst (cons (X-parse-message tval nil dpy) sublst))
	      (setq tlen (1- tlen)))
	    ;; The sub-list of items is backwards: fix
	    (setq rlist (cons (nreverse sublst) rlist))))
	 ;; not a type, but some other symbol, then put it there!
	 ;; if it is one of the lengththings, intify it.
	 ((and tval (symbolp tval))
	  (if (string-match "length" (symbol-name tval))
	      (set tval (string->int result))
	    (set tval result)))
	 ;; do nothing
	 ((equal tval nil))
	 ;; error case.
	 (t
	  (error "Error parsing X response!!!"))))
      (setq message-s (cdr message-s)))

    ;; Now that that is over, conditionally reverse the list.
    (if reverse-me
	(nreverse rlist)
      rlist)))


(provide 'xr)
;;; xr.el ends here

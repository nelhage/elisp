;;; xlib --- The normal X routine function names with lisp bindings.

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xlib.el,v 1.8 1998/03/10 23:51:54 zappo Exp $
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
;; Basic X protocol management.

(require 'xc)
(require 'xconst)
(require 'xwin)
(require 'xs)				;send
(require 'xr)				;receive
(require 'xmath)

;;; Code:
(defvar X-dpy nil
  "The display information associated with a log buffer's process.")

(defun XVectorizeList (lst)
  "Take list LST and turn it into a vector.
This makes random access of its fields much faster."
  (let ((nv (make-vector (length lst) nil))
	(cnt 0))
    (while lst
      (aset nv cnt (if (and (car lst) (listp (car lst)))
		       (XVectorizeList (car lst))
		     (car lst)))
      (setq cnt (1+ cnt))
      (setq lst (cdr lst)))
    nv))

(defun XOpenDisplay (name &optional screen)
  "Open an X connection to the display named NAME such as host:0.
Optional argument SCREEN represents the screen to connect to."
  ;; first, open a connection to name
  (if (string-match "\\(:\\)" name)
      (setq screen
	    (or screen
		(truncate (string-to-int (substring name (match-end 1)))))
	    name (substring name 0 (match-beginning 1))))
  (let ((p (X-open-connection name screen)))
    ;; send intro message to server
    (X-send p (X-create-message X-client-to-open))
    ;; Connection is open, and X-info contains connection informaion.
    (let ((X-info nil))
      (setq X-info (XVectorizeList (X-parse-message X-connect-response
						    nil p)))
      ;; Alert user
      (if (aref X-info 0)
	  (save-excursion
	    (set-buffer (process-buffer p))
	    (message "Connection opened to %s...done" name)
	    ;; save the buffer associated with dpy as part of success
	    (setq X-dpy X-info)
	    (aset X-info 0 (process-buffer p)))
	(message "X: %s" (aref X-info 3)))
      ;; we  must eventually make this a little more object oriented.
      X-info)))

;;; X Info macros
;;
(defun XDefaultScreen (display)
  "On DISPLAY, return the default screen."
  (save-excursion (set-buffer (aref display 0)) X-dpy-screen))

(defun XServerName (display)
  "Return the server name of DISPLAY."
  (save-excursion (set-buffer (aref display 0)) X-dpy-name))
  
(defun XBlackPixel (display &optional screen)
  "On DISPLAY and SCREEN, return the pixel representing black."
  (aref (aref (aref display 16) (or screen (XDefaultScreen display))) 3))

(defun XWhitePixel (display &optional screen)
  "On DISPLAY and SCREEN, return the pixel representing black."
  (aref (aref (aref display 16) (or screen (XDefaultScreen display))) 2))

(defun XDefaultColormap (display &optional screen)
  "On DISPLAY and SCREEN, return the default colormap."
  (X-cmap display (aref (aref (aref display 16)
			      (or screen (XDefaultScreen display))) 1)))

(defun XDefaultVisual (display &optional screen)
  "On DISPLAY and SCREEN, return the default visual."
  (aref (aref (aref display 16) (or screen (XDefaultScreen display))) 11))

;;;(defun XDefaultDepth (display &optional screen)
;;;  "On DISPLAY and SCREEN, return the default depth."
;;;  The depth structure is just the start.  We then have to search.
;;;  which requires all the visual routines before this is convenient.
;;;  (aref (aref (aref display 16) (or screen) 15))

(defun XDefaultGC (display &optional screen)
  "On DISPLAY and SCREEN, return the default GC."
  (aref (aref (aref display 16) (or screen (XDefaultScreen display))) 2))

(defun XDefaultRootWindow (display &optional screen)
  "On DISPLAY and SCREEN, return the default Root Window."
  (X-window-get-vector
   (aref (aref (aref display 16) (or screen (XDefaultScreen display))) 0)
   display))

(defun XServerVendor (display)
  "On DISPLAY, return the server's vendor."
  (aref display 14))

(defun XVendorRelease (display)
  "On DISPLAY, return the vendors release."
  (aref display 3))

;;; Simple Noise
;;
(defun XBell (display percent)
  "Ring the bell on DISPLAY at PERCENT volume."
  (X-dpy-p display 'XBell)
  (let ((ListOfFields
	 (list [1 104]			;opcode
	       [1 percent]		;percentage
	       [2 1])))			;length of request (in 4s)
    (X-send display (X-create-message ListOfFields))))
  
;;; Window Creation and maintenance
;;
(defun XCreateWindow (display &optional parent x y width height
			      border-width depth class visual
			      attributes )
  "Create a window based on all this gobbledygook.
If the optional parameters are not supplied, then the best guess for each are
supplied.  The parameters are:
DISPLAY      - The list returned by XOpenDisplay
PARENT       - Parent window id (ROOT window used if nil)
X            - X position (100 if nil)
Y            - Y position (100 if nil)
WIDTH        - Width of window (100 if nil)
HEIGHT       - Height of window (100 if nil)
BORDER-WIDTH - Internal border width of window (1 if nil)
DEPTH        - The depth to use (Use first in list if nil (default))
CLASS        - Class of window (Copy from the parent if nil)
VISUAL       - The visual of the window (copyfromparent)
ATTRIBUTES   - list of other attributes (None if nil)"

  (X-dpy-p display 'XCreateWindow)
  (let* ((wid (Xid-get display))
	 (attrmsg (X-attribute-message attributes))
	 (ListOfFields
	  (list [1 1]			;create window opcode
		[1 (if depth depth X-CopyFromParent)] ;depth
		[2 (+ 7 (/ (length attrmsg) 4))] ;8 means no attributes yet
		[4 wid]			;newly alloced wid.
		[4 (X-get-id
		    (if parent parent	;the parent
		      (XDefaultRootWindow display))) ]
		[2 (if x x 100)]	;x position
		[2 (if y y 100)]	;y position
		[2 (if width width 100)] ;width
		[2 (if height height 100)] ;height
		[2 (if border-width border-width 5)] ;border width
		[2 (if class class X-CopyFromParent)] ;class
		[4 (if visual visual X-CopyFromParent)] ;visual
		))
	 (msg (concat (X-create-message ListOfFields)
		      attrmsg)))
    (X-send display msg)
    (X-window-alloc wid display)))

(defun XChangeWindowAttributes (display w attributes)
  "On DISPLAY and window W, change to the ATTRIBUTES."
  (X-dpy-p display 'XChangeWindowAttributes)
  (X-attribute-p attributes 'XChangeWindowAttributes)
  (X-window-p w)
  (let* ((attrmsg (X-attribute-message attributes))
	 (ListOfFields
	  (list [1 2]			;opcode
		[1 nil]			;unused
		[2 (+ 2 (/ (length attrmsg) 4))] ;length
		[4 (X-get-id w)]))	;window
	 (msg (concat (X-create-message ListOfFields) attrmsg)))
    (X-send display msg)))

(defun XSelectInput (display window event)
  "On DISPLAY for WINDOW, set the EVENT mask."
  (XChangeWindowAttributes display window
			   (X-attribute nil 'X-attr-event-mask event)))

(defun XSetWindowBackground (display window pixel)
  "On DISPLAY for WINDOW, set the background to PIXEL."
  (XChangeWindowAttributes display window
			   (X-attribute nil 'X-attr-background-pixel pixel)))

(defun XSetWindowForeground (display window pixel)
  "On DISPLAY for WINDOW, set the foreground to PIXEL."
  (XChangeWindowAttributes display window
			   (X-attribute nil 'X-attr-foreground-pixel pixel)))
  
(defun XSetWindowBorder (display window pixel)
  "On DISPLAY for WINDOW, set the border color to PIXEL."
  (XChangeWindowAttributes display window
			   (X-attribute nil 'X-attr-border-pixel pixel)))

(defun XSetWindowColormap (display window cmap-id)
  "On DISPLAY for WINDOW, set the colormap to CMAP-ID."
  (XChangeWindowAttributes display window
			   (X-attribute nil 'X-attr-colormap cmap-id)))

(defun XConfigureWindow (display window configuration)
  "On DISPLAY, change WINDOW to have CONFIGURATION.
CONFIGURATION is an `X-configure' vector."
  (X-dpy-p display 'XConfigureWindow)
  (X-configure-p configuration 'XConfigureWindow)
  (X-window-p window)
  (let* ((cfgmsg (X-configure-message configuration))
	 (ListOfFields
	  (list [1 12]			;opcode
		[1 nil]			;unused
		[2 (+ 2 (/ (length cfgmsg) 4))]	;length
		[4 (X-get-id window)]))	;window
	 (msg (concat (X-create-message ListOfFields) cfgmsg)))
    (X-send display msg)))

(defun XLowerWindow (display window)
  "On DISPLAY, lower WINDOW."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-stackmode X-Below)))

(defun XRaiseWindow (display window)
  "On DISPLAY, raise WINDOW."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-stackmode X-Above)))

(defun XMoveWindow (display window x y)
  "On DISPLAY, move WINDOW to position X Y."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-x x
				 'X-configure-y y)))

(defun XResizeWindow (display window w h)
  "On DISPLAY, resize WINDOW to dimentions W H."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-width w
				 'X-configure-height h)))

(defun XMoveResizeWindow (display window x y w h)
  "On DISPLAY, move and resize WINDOW to X, Y, W, H."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-x x
				 'X-configure-y y
				 'X-configure-width w
				 'X-configure-height h)))

(defun XSetWindowBorderWidth (display window width)
  "On DISPLAY, set WINDOW's border to be WIDTH pixels wide."
  (XConfigureWindow display window 
		    (X-configure nil 'X-configure-border-width width)))

(defun XGetWindowAttributes (display window)
  "On DISPLAY, get WINDOWs attributes as an `X-attribute'.
*UNTESTED*"
  (X-dpy-p display 'XMapWindow)
  (X-window-p window 'XMapWindow)
  (let ((ListOfFields
	 (list [ 1 3 ]			;opcode
	       [ 1 nil ]		;unused
	       [ 2 2 ]			;request length
	       [ 4 (X-get-id window) ])) ;the window
	(ReceiveFields
	 (list [1 success ]		;status
	       nil			;generic bad response
	       (list
		[ 1 integerp ]		;reply
		[ 1 integerp ]		;backingstore
		[ 2 integerp ]		;sequence number
		[ 4 integerp ]		;reply length
		[ 4 integerp ]		;visual id
		[ 2 integerp ]		;class
		[ 1 integerp ]		;bit gravity
		[ 1 integerp ]		;win gravity
		[ 4 integerp ]		;backing planes
		[ 4 integerp ]		;backing pixel
		[ 1 integerp ]		;save under
		[ 1 integerp ]		;map is installed
		[ 1 integerp ]		;map state
		[ 1 integerp ]		;override-redirect
		[ 4 integerp ]		;colormap
		[ 4 integerp ]		;all event masks
		[ 4 integerp ]		;my event masks
		[ 2 integerp ]		;do not propagate mask
		[ 2 nil ] )))		;pad
	(r nil))
    (X-send display (X-create-message ListOfFields))
    (setq r (X-parse-message ReceiveFields t display))
    (if (not (car r))
	nil
      (X-attribute nil
		   X-attr-bit-gravity          (nth 7 r)
		   X-attr-win-gravity          (nth 8 r)
		   X-attr-backing-store        (nth 2 r)
		   X-attr-backing-planes       (nth 9 r)
		   X-attr-backing-pixel        (nth 10 r)
		   X-attr-override-redirect    (nth 14 r)
		   X-attr-save-under           (nth 11 r)
		   X-attr-event-mask           (nth 17 r)
		   X-attr-do-not-propagate-mask(nth 18 r)
		   X-attr-colormap             (nth 15 r)
		   X-attr-visualid             (nth 5 r)))))

(defun XMapWindow (display window)
  "On DISPLAY, map WINDOW to the screen (to make it visible.)."
  (X-dpy-p display 'XMapWindow)
  (X-window-p window 'XMapWindow)
  (let ((ListOfFields
	 (list [ 1 8 ]			;opcode
	       [ 1 nil ]		;unused
	       [ 2 2 ]			;length of request (in 4s)
	       [ 4 (aref window 1) ])))	;window to map
    (X-send display (X-create-message ListOfFields))))

(defun XDestroyWindow(display window)
  "On DISPLAY, destroy WINDOW."
  (X-dpy-p display 'XDestroyWindow)
  (X-window-p window 'XDestroyWindow)
  (let ((ListOfFields
	 (list [1 4]			;opcode
	       [1 nil]			;unused
	       [2 2]			;length of request (in 4s)
	       [4 (aref window 1)])))	;window to map
    (X-send display (X-create-message ListOfFields))
    ;; We don't invalidate windows because of DestroyNotify events.
    ))

(defun XDestroySubwindows(display window)
  "On DISPLAY, destroy WINDOW."
  (X-dpy-p display 'XDestroySubwindows)
  (X-window-p window 'XDestroySubwindows)
  (let ((ListOfFields
	 (list [1 5]			;opcode
	       [1 nil]			;unused
	       [2 2]			;length of request (in 4s)
	       [4 (aref window 1)])))	;window to map
    (X-send display (X-create-message ListOfFields))))

(defun XCloseDisplay (dpy)
  "Close the connection to display DPY."
  (X-close dpy))

;;; How about window querying functions?
;;
(defun XQueryTree (display window)
  "Query DISPLAY for all children of WINDOW.
Returns a list of the form (ROOT PARENT CHILD1 CHILD2 ...)
on success, or nil on failure.  ROOT is the root window for the display that
WINDOW is on.  PARENT is the parent of WINDOW, and CHILDN are the
children of WINDOW."
  (X-dpy-p display 'XQueryTree)
  (X-window-p window 'XQueryTree)
  (let* ((w (X-get-id window))
	 (ListOfFields
	  (list [ 1 15 ]		;opcode
		[ 1 nil ]		;unused
		[ 2 2 ]			;request length
		[ 4 w ]))		;window we are querying.
	 (ReceiveFields
	  (list [ 1 success]		;status
		nil			;generic bad response
		(list [ 1 nil]		;unused
		      [ 2 integerp ]	;sequence number
		      [ 4 length-1 ]	;length of the return in 4 blocks
		      [ 4 integerp ]	;root window
		      [ 4 integerp ]	;parent window
		      [ 2 length-2 ]	;number of children
		      [ 14 nil ]	;unused
		      [ length-2	;list of the children
			( [ 4 integerp ] ) ] )))
	 (r nil)
	 (winlist nil))
    (X-send display (X-create-message ListOfFields))
    (setq r (X-parse-message ReceiveFields t display))
    (if (or (not (car r)) (not (cdr r)))
	nil
      (setq r (cdr r))
      ;; Make winlist backwards
      (setq winlist (list (X-window-get-vector (nth 2 r) display)
			  (X-window-get-vector (nth 1 r) display)))
      (setq r (nth 3 r))
      (while r	(setq winlist (cons (X-window-get-vector (car (car r)) display)
				    winlist)
		      r (cdr r)))
      (nreverse winlist))))

;;; Time to play with properties and Atoms
;;
(defun XInternAtom (display name only-if-exists)
  "On DISPLAY, return the Atom with NAME.
If ONLY-IF-EXISTS is nil, then the atom is created if it does not already
exist.  The Atom object is returned."
  (X-dpy-p display 'XInternAtom)
  (if (not (stringp name))
      (signal 'wrong-type-argument (list 'signal 'stringp name)))
  (let ((a (X-atom-find-name display name)))
    (if a
	a
      (let ((ListOfFields
	     (list [ 1 16 ]			;opcode
		   [ 1 (if only-if-exists 1 0)] ;forcecreate flag.
		   [ 2 (+ 2 (X-padlen name))] ;message length
		   [ 2 (length name)]	;name length
		   [ 2 nil]			;unused
		   [ (length name) name]	;name
		   ;; Auto-padded
		   ))
	    (ReceiveFields
	     (list [1 success]		;status message
		   nil			;generic bad response
		   (list [ 1 nil ]	;unused
			 [ 2 integerp ]	;sequence
			 [ 4 nil ]	;reply length
			 [ 4 integerp ]	;atom id
			 [ 20 nil ])))
	    r)
	(X-send display (X-create-message ListOfFields))
	(setq r (X-parse-message ReceiveFields t display))
	(X-atom display (nth 2 r) name)))))

(defun XGetAtomName (display atom)
  "On DISPLAY, get the textual name of ATOM.
*UNTESTED*"
  (X-dpy-p display 'XGetAtomName)
  (X-atom-p atom 'XGetAtomName)
  (let ((ListOfFields
	 (list [ 1 17 ]			;opcode
	       [ 1 nil ]			;unused
	       [ 2 2 ]			;length
	       [ 4 (X-get-id atom) ]))		;atom id
	    (ReceiveFields
	     (list
	      [1 success ]		;status message
	      nil			;generic bad response
	      (list [ 1 nil ]		;unused
		    [ 2 integerp ]	;sequence
		    [ 4 length-1 ]	;reply length
		    [ 2 length-2 ]	;length of name
		    [ 22 nil ]		;unused
		    [ length-2 stringp ] ;the name
		    [ (X-pad length-2) ] ;padding
		    )))
	    (r nil))
    (X-send display (X-create-message ListOfFields))
    (setq r (X-parse-message ReceiveFields t display))
    (nth 2 r)))

(defun XChangeProperty (display window property type format mode data)
  "On DISPLAY for WINDOW, change PROPERTY.
PROPERTY is changed based on a TYPE, FORMAT, and MODE with DATA.
There are NElements."
  (X-dpy-p display 'XChangeProperty)
  (X-window-p window 'XChangeProperty)
  (X-atom-p property 'XChangeProperty)
  (X-atom-p type 'XChangeProperty)
  (let* ((n (* (length data) (/ format 8)))
	 (p (X-pad n))
	 (ListOfFields
	  (list [1 18]			;opcode
		[1 mode]		;Mode: Replace Prepend, Append
		[2 (+ 6 (/ (+ n p) 4))]	;length
		[4 (X-get-id window)]	;window
		[4 (X-get-id property)]	;property atom
		[4 (X-get-id type)]	;property type
		[1 format]		;property format
		[3 nil]
		[4 (/ n (/ format 8))]	;length of the list-byte thing
		)))
    (if (and (= format 8) (stringp data))
	(setq ListOfFields
	      (append ListOfFields (list (vector (length data) data))))
      (while data
	(let ((d (if (X-generic-vector-p (car data))
		     (X-get-id (car data)) (car data))))
	  (setq ListOfFields
		(append ListOfFields (list (vector (/ format 8) d )))
		data (cdr data)))))
    (X-send display (X-create-message ListOfFields))))

(defun XGetWindowProperty (display window property &optional
				   offset length delete required-type)
  "On DISPLAY, get WINDOW's PROPERTY atom value.
Get the data from optional OFFSET, and a maximum of LENGTH bytes.
OFFSET and LENGTH refer to 32 bit chunks, not 8 bit chunks.
Third optional argument DELETE will delete the property if Non-nil.
Fourth argument REQUIRED-TYPE filters only properties of the desired type.
If REQUIRED-TYPE is `XA-AnyPropertyType', or nil then no filtering is done.
The returned list is of the form:
  (TYPE_RETURN BYTES_AFTER PROP1 PROP2 ...)
Where TYPE_RETURN is the type (of same for as REQUIRED-TYPE) is the actual
type of the data being returned.
FORMAT_RETURN is the format of the data (such as 8, 16, or 32).
BYTES_AFTER is the number of bytes still attached to the property.
If there are extra bytes, then a second call to `XGetWindowProperty' will
be needed.  Lastly, PROP1 through PROPN is the list of properties
originally requested.
  It is common to call `XGetWindowProperty' asking for no data so that
BYTES_AFTER contains the exact amount of data we want to request."
  (X-dpy-p display 'XGetWindowProperty)
  (X-window-p window 'XGetWindowProperty)
  (X-atom-p property 'XGetWindowProperty)
  (if (not offset) (setq offset 0))
  (if (not length) (setq length 1024))
  (if (not required-type) (setq required-type XA-AnyPropertyType))
  (let ((ListOfFields
	 (list [ 1 20 ]			;opcode
	       [ 1 (if delete 1 0) ]	;delete flag
	       [ 2 6 ]			;request length
	       [ 4 (X-get-id window) ]	;the window whose property we want
	       [ 4 (X-get-id property) ] ;The property atom we want
	       [ 4 (X-get-id required-type) ] ;required type filter.
	       [ 4 offset ]		;offset in the property data
	       [ 4 length ]))		;length of data we want
	(ReceiveFields
	 (list [1 success]		;status message
	       nil			;generic bad response
	       (list
		[ 1 length-1 ]		;format of returned data
		[ 2 integerp ]		;sequence number
		[ 4 length-3 ]		;length of this request
		[ 4 integerp ]		;atom representing return type
		[ 4 integerp ]		;bytes left on server
		[ 4 length-2 ]		;length of value in format units
		[ 12 nil ]		;unused
		[ (if (= length-1 0)
		      0
		    length-2)
		  (if (= length-1 8)
		      'stringp		;format-8 means a string
		    '([ (/ length-1 8) integerp])) ;otherwise a list of #s
		  ]
		[ (X-pad (* length-2 (/ length-1 8))) nil ]
		)))
	(r nil)
	(proplist nil))
    (X-send display (X-create-message ListOfFields))
    (setq r (X-parse-message ReceiveFields t display))
    ;; (X-log display (format "%S\n" r))
    (if (not (car r))
	nil				;oops
      (setq proplist (list (nth 2 r) (nth 1 r))) ; start backwards
      (setq r (nth 4 r))
      (if (stringp r)
	  (setq proplist (cons r proplist))
	(while r
	  (setq proplist (cons (car (car r)) proplist)
		r (cdr r))))
      (nreverse proplist))))

;; A few functions based on GetProperty
(defun XFetchName (display window)
  "On DISPLAY, get WINDOWs name."
  (let ((propdata
	 (XGetWindowProperty display window XA-wm-name 0 1024 nil XA-string))
	(pd2 nil)
	(name ""))
    (if (and propdata (nth 2 propdata))
	(progn
	  (setq name (nth 2 propdata))
	  (if (< (nth 1 propdata) 0.0)
	      (setq pd2 (XGetWindowProperty display window XA-wm-name 1024
					    (nth 1 propdata) nil XA-string)))
	  (if pd2 (setq name (concat name (nth 2 pd2))))))
    name))

;; These are Xlib convenience routines
;;
(defun XSetWMProtocols (display window protocol_atoms)
  "On DISPLAY, set WINDOW's protocols to PROTOCOL_ATOMS.
Convenience routine which calls `XChangeProperty'"
  (XChangeProperty display window (XInternAtom display "WM_PROTOCOLS" nil)
		   XA-atom X-format-32 X-PropModeReplace protocol_atoms))

;;; Lets allocate some color stuff.
;;
(defun XCreateColormap (display w &optional v alloc)
  ;; checkdoc-params: (v alloc)
  "Create a colormap. Default values are:

VISUAL    - CopyFromParent
ALLOCATE  - All are writable (1) (0 -> none writable)

args (DISPLAY W &optional VISUAL ALLOCTE)"
  (X-dpy-p display 'XCreateColormap)
  (X-window-p w 'XCreateColormap)
  (if (not v) (setq v (XDefaultVisual display)))
  (if (not alloc) (setq alloc X-AllocAll))
  (let* ((nid (X-cmap display nil))
	 (ListOfFields
	  (list [1 78]			;opcode
		[1 alloc]		;alloc type
		[2 4]			;length
		[4 (X-get-id nid)]	;id to use
		[4 (X-get-id w)]	;window id
		[4 v]))
	 (msg (X-create-message ListOfFields)))
    (X-send display msg)
    nid))

(defun XFreeColormap (display cmap)
  "Frees a colormap.
args (DISPLAY CMAP)"
  (X-dpy-p display 'XFreeColormap)
  (X-cmap-p cmap 'XFreeColormap)
  (let* ((nid (X-cmap display nil))
	 (ListOfFields
	  (list [1 79]			;opcode
		[1 nil]
		[2 2]			;length
		[4 (X-get-id nid)]))	;id to use
	 (msg (X-create-message ListOfFields)))
    (X-send display msg)
    (X-generic-invalidate cmap)))

(defun XAllocColor (display cmap color)
  "On DISPLAY allocate in CMAP the color struct COLOR.
Use `X-Color' to create."
  (X-dpy-p display 'XAllocColor)
  (X-cmap-p cmap 'XAllocColor)
  (X-Color-p color 'XAllocColor)
  (let* ((ListOfFields
	  (list [1 84]			;opcode
		[1 nil]			;unused
		[2 4]			;request length
		[4 (X-get-id cmap)]	;colormap handle
		[2 (aref color 2)]	;red
		[2 (aref color 3)]	;green
		[2 (aref color 4)]	;blue
		[2 nil]))		;padding
	 (msg (X-create-message ListOfFields))
	 (ReceiveFields
	  (list [1 success]		;status message
		nil			;generic bad response
		(list [1 nil]		;unused
		      [2 integerp]	;sequence
		      [4 nil]		;reply length
		      [2 integerp]	;red
		      [2 integerp]	;green
		      [2 integerp]	;blue
		      [2 nil]		;unused
		      [4 integerp]	;pixel value
		      [12 nil]))))	;padding
    (X-send display msg)
    ;; Now receive the answer
    (let ((resp (X-parse-message ReceiveFields t display)))
      (if (car resp)
	  (progn
	    (aset color 1 (nth 5 resp))	;set pixel value
	    (aset color 2 (nth 2 resp))	;set red value
	    (aset color 3 (nth 3 resp))	;set green value
	    (aset color 4 (nth 4 resp)) ;set blue value
	    ;; return status of the call
	    t)
	nil))))

(defun XAllocNamedColor (display cmap name color-visual &optional color-exact)
  "Allocate a color based on the color struct COLOR-VISUAL and COLOR-EXACT.
If COLOR-EXACT is nil or absent, ignore.
args (DISPLAY CMAP NAME COLOR-VISUAL &optional COLOR-EXACT)"
  ;; checkdoc-order: nil
  (X-dpy-p display 'XAllocNamedColor)
  (X-cmap-p cmap 'XAllocNamedColor)
  (X-Color-p color-visual 'XAllocNamedColor)
  (if color-exact (X-Color-p color-exact 'XAllocNamedColor))
  (let* ((ListOfFields
	  (list [1 85]			;opcode
		[1 nil]
		[2 (+ 3 (X-padlen name))] ;length
		[4 (X-get-id cmap)]	;colormap
		[2 (length name)]	;length of name
		[2 nil]			;unused
		[(length name) name]	;the name
		));; autopadded
	 (msg (X-create-message ListOfFields))
	 (X-reading-mode t)
	 (ReceiveFields
	  (list [1 success]		;success field
		nil
		(list [1 nil]		;unused
		      [2 integerp]	;sequence
		      [4 nil]		;length
		      [4 integerp]	;pixel id
		      [2 integerp]	;exact red
		      [2 integerp]	;exact green
		      [2 integerp]	;exact blue
		      [2 integerp]	;visual red
		      [2 integerp]	;visual green
		      [2 integerp]	;visual blue
		      [8 nil]))))	;padding
    (X-send display msg)
    ;; Now receive the answer
    (let ((resp (X-parse-message ReceiveFields nil display)))
      (if (car resp)
	  (progn
	    (aset color-visual 1 (nth 2 resp))	;set pixel value
	    (aset color-visual 2 (nth 6 resp))	;set red value
	    (aset color-visual 3 (nth 7 resp))	;set green value
	    (aset color-visual 4 (nth 8 resp)) ;set blue value
	    (if color-exact
		(progn
		  (aset color-exact 1 (nth 2 resp)) ;set pixel value
		  (aset color-exact 2 (nth 3 resp)) ;set red value
		  (aset color-exact 3 (nth 4 resp)) ;set green value
		  (aset color-exact 4 (nth 5 resp)))) ;set blue value
	    ;; return status of the call
	    t)
	nil))))

(defun XStoreColors(display cmap colors)
  "On DISPLAY in CMAP, store COLORS. (A list of 'X-Color)
These colors are X-Color lists containing the PIXEL, RGB values and
FLAGs (which indicates what part of the RGB value is stored into
PIXEL's slot."
  (X-dpy-p display 'XAllocNamedColor)
  (X-cmap-p cmap 'XAllocNamedColor)
  (let* ((ListOfFields
	  (list [1 89]			;opcode
		[1 nil]			;unused
		[2 (+ 2 (* 3 length colors))] ;request length
		[4 (X-get-id cmap)]	;COLORMAP
		))
	  (msg (X-create-message ListOfFields)))
    (while colors
      (setq msg (concat msg (X-Color-message (car colors)))
	    colors (cdr colors)))
    (X-send display msg)))

(defun XStoreColor(display cmap color &optional R G B)
  "On DISPLAY in CMAP, store COLORS.
These colors are X-Color lists containing the PIXEL, RGB values and
FLAGs (which indicates what part of the RGB value is stored into
PIXEL's slot.
  Optionally, COLOR can be a float, and it's new value indicated by
the values of RGB."
  (XStorecolors display cmap
		(if (X-Color-p color)
		    (list color)
		  (X-Color 'X-Color-pixel color
			   'X-color-red R
			   'X-color-green G
			   'X-color-blue B
			   'X-color-flags
			   (Xmask-or (if R X-DoRed 0)
				     (if G X-DoGreen 0)
				     (if B X-DoBlue 0))))))

(defun XFreeColors (display cmap colors planes)
  "On DISPLAY in CMAP, free COLORS from the server.
The colors are deallocated on PLANES, which is a mask.  Use 0 for
PLANES if you don't know what it's for."
  (X-dpy-p display 'XFreeColors)
  (X-cmap-p cmap 'XFreeColors)
  (if (not (listp colors))
      (signal 'wrong-type-argument (list 'signal 'listp colors)))
  (mapcar 'X-Color-p colors)
  (let* ((ListOfFields
	  (list [1 88]			;opcode
		[1 nil]
		[2 (+ 3 (length colors))];length
		[4 (X-get-id cmap)]	;Colormap
		[4 planes]))		;plane mask
	 (clst "")
	 (msg (X-create-message ListOfFields)))
    (while colors
      (setq clst (concat clst (X-create-message
			       (list [4 (X-get-id (car colors))] ; pixel
				     )))
	    colors (cdr colors)))
    (setq msg (concat msg clst))
    (X-send display msg)
    (mapcar 'X-generic-invalidate colors)))
    
;;; How about some grafic context type o stuff?
;;
(defun XCreateGC (display d gc)
  "Allocate a graphic context display DISPLAY on the drawable D.
Base this new context on GC." 
  (X-dpy-p display 'XCreateGC)
  (X-drawable-p d 'XCreateGC)
  (X-GC-p gc)
  (let* ((attrmsg (X-GC-message gc))
	 (ListOfFields 
	  (list [1 55]			;opcode
		[1 nil]			;unused
		;; 4 fields, but 1 is in attrmsg, making 3
		[2 (+ 3 (/ (length attrmsg) 4))] ;request length
		[4 (X-get-id gc)]	;GC id
		[4 (X-get-id d)]	;drawable id
		))
	 (msg (concat (X-create-message ListOfFields) attrmsg)))
    (X-send display msg)
    ;; seems lame, but return the GC we were passed originally.
    gc))

(defun XChangeGC (display gc values)
  "On DISPLAY change GC to have new VALUES.
VALUES is actually a GC created with X-GC, and GC is also created first with
X-GC, but then created with `XCreateGC'.  VALUES is discarded afterwards."
  (X-dpy-p display 'XCreateGC)
  (X-drawable-p d 'XCreateGC)
  (X-GC-p gc)
  (let* ((attrmsg (X-GC-message values))
	 (ListOfFields 
	  (list [1 56]			;opcode
		[1 nil]			;unused
		[2 (+ 2 (/ (length attrmsg) 4))] ;request length
		[4 (X-get-id gc)]	;the GC
		))
	 (msg (concat (X-create-message ListOfFields) attrmsg)))
    (X-send display msg)
    ;; Change the structure of the GC we are changing.
    ;(X-generic-merge gc values)
    ))

(defun XFreeGC (display gc)
  "Allocate a graphic context display DISPLAY on the drawable D.
Base this new context on GC." 
  (X-dpy-p display 'XFreeGC)
  (X-GC-p gc)
  (let* ((ListOfFields 
	  (list [1 60]			;opcode
		[1 nil]
		[2 2]			;length
		[4 (X-get-id gc)]	;GC id
		))
	 (msg (X-create-message ListOfFields)))
    (X-send display msg)
    (X-generic-invalidate gc)))

;;; How about some basic drawing routines?
;;

(defun XDrawArc (display d gc x y width height angle1 angle2)
  "Draw an arc on DISPLAY in drawable D.
args (DISPLAY D GC X Y WIDTH HEIGHT ANGLE1 ANGLE2)"
  (XDrawArcs display d gc (list (vector x y width height angle1 angle2))))

(defun XDrawArcs (display d gc arcs &optional fill)
  "Draw arcs. (DISPLAY D GC ARCS &optional FILL)."
  (X-dpy-p display 'XDrawArcs)
  (X-drawable-p d 'XDrawArcs)
  (X-GC-p gc 'XDrawArcs)
  (let* ((ListOfFields
	  (list [1 (if fill 71 68)]	;opcode
		[1 nil]			;mode of drawing
		[2 (+ 3 (* (length arcs) 3))] ; number of arcs * 3
		[4 (X-get-id d)]	;drawable id
		[4 (X-get-id gc)]))	;id of the GC
	 (ptlst "")
	 (msg (X-create-message ListOfFields))
	 (r nil))
    (while arcs
      (setq r (car arcs)
	    ptlst (concat ptlst (X-create-message
				 (list [2 (aref r 0)]
				       [2 (aref r 1)]
				       [2 (aref r 2)]
				       [2 (aref r 3)]
				       [2 (aref r 4)]
				       [2 (aref r 5)])))
	    arcs (cdr arcs)))
    (setq msg (concat msg ptlst))
    (X-send display msg)))

(defun XFillArc (display d gc x y width height angle1 angle2)
  "Draw a filled arc on DISPLAY in drawable D.
args (DISPLAY D GC X Y WIDTH HEIGHT ANGLE1 ANGLE2)"
  (XDrawArcs display d gc (list (vector x y width height angle1 angle2)) t))

(defun XFillArcs (display d gc arcs)
  "Draw filled arcs. (DISPLAY D GC ARCS)."
  (XDrawArcs display d gc arcs t))

(defun XFillRectangle (display d gc x y width height)
  "Draw a rectangle. (DISPLAY D GC X Y WIDTH HEIGHT)."
  (XDrawRectangles display d gc (list (vector x y width height)) t))

(defun XFillRectangles (display d gc rectangles)
  "Draw rectangles. (DISPLAY D GC RECTANGLES)."
  (XDrawRectangles display d gc rectangles t))

(defun XDrawLine (display d gc x y x2 y2)
  "Draw a line on DISPLAY in drawable D.
args (DISPLAY D GC X Y X2 Y2)."
  (XDrawLines display d gc (list x y x2 y2)))

(defun XDrawLines (display d gc pts &optional mode)
  "Draw a multipoint line. (DISPLAY D GC PTS &optional MODE)."
  (X-dpy-p display 'XDrawLines)
  (X-drawable-p d 'XDrawLines)
  (X-GC-p gc 'XDrawLines)
  (let* ((mmode (if mode mode X-Origin))
	 (ListOfFields
	  (list [1 65]			;opcode
		[1 mmode]		;mode of drawing
		[2 (+ 3 (/ (length pts) 2))] ;len/2 because 2 elst is 4 byte
		[4 (X-get-id d)]
		[4 (X-get-id gc)]))
	 (ptlst "")
	 (msg (X-create-message ListOfFields)))
    (while pts
      (setq ptlst (concat ptlst (X-create-message
				 (list [2 (car pts)]
				       [2 (car (cdr pts))]))))
      (setq pts (cdr (cdr pts))))
    (setq msg (concat msg ptlst))
    (X-send display msg)))

(defun XDrawPoint (display d gc x y)
  "Draw a point. (DISPLAY D GC X Y)."
  (XDrawPoints display d gc (list x y) X-Origin))

(defun XDrawPoints (display d gc pts &optional mode)
  "Draw points on a drawable. (DISPLAY D GC PTS &optional MODE)."
  (X-dpy-p display 'XDrawPoints)
  (X-drawable-p d 'XDrawPoints)
  (X-GC-p gc 'XDrawPoints)
  (let* ((mmode (if mode mode X-Origin))
	 (ListOfFields
	  (list [1 64]			;opcode
		[1 mmode]		;mode of drawing
		[2 (+ 3 (/ (length pts) 2))] ;len/2 because 2 elst is 4 byte
		[4 (X-get-id d)]	;drawable id
		[4 (X-get-id gc)]))	;id of the GC
	 (ptlst "")
	 (msg (X-create-message ListOfFields)))
    (while pts
      (setq ptlst (concat ptlst (X-create-message
				 (list [2 (car pts)]
				       [2 (car (cdr pts))]))))
      (setq pts (cdr (cdr pts))))
    (setq msg (concat msg ptlst))
    (X-send display msg)))


(defun XDrawRectangle (display d gc x y width height)
  "Draw a rectangle. (DISPLAY D GC X Y WIDTH HEIGHT)."
  (XDrawRectangles display d gc (list (vector x y width height))))

(defun XDrawRectangles (display d gc rectangles &optional fill)
  "Draw rectangles. (DISPLAY D GC RECTANGLES &optional FILL)."
  (X-dpy-p display 'XDrawRectangles)
  (X-drawable-p d 'XDrawRectangles)
  (X-GC-p gc 'XDrawRectangles)
  (let* ((ListOfFields
	  (list [1 (if fill 70 67)]	;opcode
		[1 nil]			;mode of drawing
		[2 (+ 3 (* (length rectangles) 2))] ; number of rects *2
		[4 (X-get-id d)]	;drawable id
		[4 (X-get-id gc)]))	;id of the GC
	 (ptlst "")
	 (msg (X-create-message ListOfFields))
	 (r nil))
    (while rectangles
      (setq r (car rectangles)
	    ptlst (concat ptlst (X-create-message
				 (list [2 (aref r 0)]
				       [2 (aref r 1)]
				       [2 (aref r 2)]
				       [2 (aref r 3)])))
	    rectangles (cdr rectangles)))
    (setq msg (concat msg ptlst))
    (X-send display msg)))

(defun XDrawSegments (display d gc pts)
  "Draw Segments. (DISPLAY D GC PTS &optional MODE).
Drawing segments is different from lines in that segments are disconnected
every other pair of points."
  (X-dpy-p display 'XDrawSegments)
  (X-drawable-p d 'XDrawSegments)
  (X-GC-p gc 'XDrawSegments)
  (if (/= (% (length pts) 4) 0)
      (signal 'wrong-type-argument (list 'XDrawSegments 'wrong-list-length
					 'pts)))
  (let* ((ListOfFields
	  (list [1 66]			;opcode
		[1 nil]
		[2 (+ 3 (/ (length pts) 2))] ;len/2 because 2 elst is 4 byte
		[4 (X-get-id d)]
		[4 (X-get-id gc)]))
	 (ptlst "")
	 (msg (X-create-message ListOfFields)))
    (while pts
      (setq ptlst (concat ptlst (X-create-message
				 (list [2 (car pts)]
				       [2 (car (cdr pts))]
				       [2 (car (cdr (cdr pts)))]
				       [2 (car (cdr (cdr (cdr pts))))])))
	    pts (cdr (cdr (cdr (cdr pts))))))
    (setq msg (concat msg ptlst))
    (X-send display msg)))
	
(defun XDrawString (display d gc x y str &optional len)
  "Draw a string at specified point. (DISPLAY D GC X Y STR &optional LEN)."
  (X-dpy-p display 'XDrawString)
  (X-drawable-p d 'XDrawString)
  (X-GC-p gc 'XDrawString)
  (let* ((slen (if len len (length str))) ;make len optional
	 (ListOfFields
	  (list [1 74]			;request value
		[1 nil]			;unused
		[2 (+ 4 (X-padlen str))] ;length
		[4 (X-get-id d)]	;drawable id
		[4 (X-get-id gc)]	;gc id
		[2 x]
		[2 y]
		[1 slen]		;text length
		[1 0]			;delta????????
		[slen str]		;the string
		))
		;; auto-padding in X-create
	 (msg (X-create-message ListOfFields)))
    (X-send display msg)))

(provide 'xlib)
;;; xlib.el ends here

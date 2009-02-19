;;; xwin --- handle display window, context etc resources

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xwin.el,v 1.5 1998/03/10 23:44:48 zappo Exp $
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
;; This file will define several new types, they will be:
;;
;; XWINDOW    - an X window resource
;; XATTRIBUTE - an X window attribute list
;;
;; All new types will be of the form:
;;
;; [ type resource-id attributes ]
;; where TYPE is the 'alloc style function to use to create it.
;;

(require 'xs)
;;; Code:

;;; Atoms are how we give properties to things.
(defvar X-atom-type '( 0 . 0))
(defvar X-atom-id   '( 1 . 0))
(defvar X-atom-name '( 2 . l))

(defvar X-atom-list nil
  "The list of atoms on this server.")

(defun X-atom (dpy atomid name)
  "Create an Atom struct for DPY.
If ATOMID then that is what ATOMID is set to.  NAME is it's ISO encoded name."
  (let ((v (make-vector 3 nil)))
    (aset v 0 'X-atom)
    (if (and (numberp atomid) (not (integerp atomid)))
	(aset v 1 atomid)
      (signal 'wrong-type-argument (list 'X-atom atomid)))
    (aset v 2 name)
    ;; Defaults don't qualify under a specific DPY
    (if dpy
	(save-excursion
	  (set-buffer (aref dpy 0))
	  ;; append window, but only in the logging buffer!
	  (setq X-atom-list (cons v X-atom-list))))
    v))

(defun X-atom-p (atom &optional sig)
  "Return t if ATOM is an Atom vector.
If SIG is specified, signal error."
  (X-generic-p atom 'X-atom sig))

(defun X-atom-find (dpy aid)
  "On DPY, find the atom based on AID atom handle."
  (save-excursion
    (set-buffer (aref dpy 0))
    (let ((l X-atom-list))
      (while (and l (not (= (aref (car l) 1) aid)))
	(setq l (cdr l)))
      (car l))))

(defun X-atom-find-name (dpy name)
  "On DPY, find the atom based on the atom's NAME."
  (save-excursion
    (set-buffer (aref dpy 0))
    (let ((l X-atom-list))
      (while (and l (not (string= (aref (car l) 2) name)))
	(setq l (cdr l)))
      (car l))))

;;; attribute lists arn't resouces, so no resouce id.
;;
;; values are ( position . size )
;;
(defvar X-attr-type                  '( 0 . 0))
(defvar X-attr-id                    '( 1 . 0))
(defvar X-attr-background-pixmap     '( 2 . 4 ))
(defvar X-attr-background-pixel      '( 3 . 4 ))
(defvar X-attr-border-pixmap         '( 4 . 4 ))
(defvar X-attr-border-pixel          '( 5 . 4 ))
(defvar X-attr-bit-gravity           '( 6 . 1 ))
(defvar X-attr-win-gravity           '( 7 . 1 ))
(defvar X-attr-backing-store         '( 8 . 1 ))
(defvar X-attr-backing-planes        '( 9 . 4 ))
(defvar X-attr-backing-pixel         '( 10 . 4 ))
(defvar X-attr-override-redirect     '( 11 . 1 ))
(defvar X-attr-save-under            '( 12 . 1 ))
(defvar X-attr-event-mask            '( 13 . 4 ))
(defvar X-attr-do-not-propagate-mask '( 14 . 4 ))
(defvar X-attr-colormap              '( 15 . 4 ))
(defvar X-attr-cursor                '( 16 . 4 ))
(defvar X-attr-visualid              '( 17 . 0 ))
(defvar X-attribute-list
  '(X-attr-background-pixmap X-attr-background-pixel
    X-attr-border-pixmap X-attr-border-pixel X-attr-bit-gravity
    X-attr-win-gravity X-attr-backing-store X-attr-backing-planes
    X-attr-backing-pixel X-attr-override-redirect X-attr-save-under
    X-attr-event-mask X-attr-do-not-propagate-mask X-attr-colormap
    X-attr-cursor X-attr-visualid)
  "List of previously decaired attributes.")

(defun X-attribute (attrib &rest args)
  "Fill in ATTRIB vector, or create a new one if nil, and fill in with ARGS.
ARGS are a series of pairs.  A call would be:

       (X-attribute nil 'X-attr-event-mask X-Exposure)
       (X-attribute AV 'X-attr-cursor MyCursor)

to create new vector with only events set.
Valid parts are all prefixed with: X-attr-"
  ;; X attributes don't have IDs
  (X-generic-attribute attrib 18 'X-attribute nil args))

(defun X-attribute-p (a &optional sig) "Return t if A is an attribute list."
  ;; checkdoc-params: (sig)
  (X-generic-p a 'X-attribute sig))

(defun X-attribute-message (a)
  "Return a string representing the attributes A.
It starts with a mask of included parts and each part is appended."
  (X-generic-attribute-message a X-attribute-list))

;;; Configure window structure
;;
;; values are ( position . size )
;;
(defvar X-configure-type         '( 0 . 0))
(defvar X-configure-id           '( 1 . 0))
(defvar X-configure-x            '( 2 . 2))
(defvar X-configure-y            '( 3 . 2))
(defvar X-configure-width        '( 4 . 2))
(defvar X-configure-height       '( 5 . 2))
(defvar X-configure-border-width '( 6 . 2))
(defvar X-configure-sibling      '( 7 . 4))
(defvar X-configure-stackmode    '( 8 . 1))
(defvar X-configure-list 
  '(X-configure-x X-configure-y X-configure-width X-configure-height
		  X-configure-border-width X-configure-sibling
		  X-configure-stackmode)
  "List of window configurable attributes.")

(defun X-configure (config &rest args)
  "Fill in CONFIG vector, or create a new one if nil. and fill in with ARGS.
ARGS are a series of pairs.  A call would be:

   (X-configure nil 'X-configure-x 10)
   (X-configure CV 'X-configure-border-width 2)

Valid parts are prefixed with: X-confgiure-"
  (X-generic-attribute config (+ (length X-configure-list) 2)
		       'X-configure nil args))

(defun X-configure-p (c &optional sig) "Return t if C is a configure list."
  ;; checkdoc-params: (sig)
  (X-generic-p c 'X-configure sig))

(defun X-configure-message (c)
  "Return a string representing the configuration C.
It starts with a mask of included parts and each part is appended."
  (X-generic-attribute-message c X-configure-list 2))

;;; Window allocation/testing/setting routines.
;;
;; The elts in a window vector are as follows
;; [ TYPE WINDOW-ID DISPLAY 
;;   EXPOSE-FN
;;   INPUT-FN
;;   RECONFIGURE-FN
;;   EVENT-HANDLERS
;; ]
(defvar X-window-list nil "List of all active Xwindows.")

(defun X-window-alloc (wid dpy)
  "Create a window vector with the window ID as WID on display DPY."
  (X-dpy-p dpy 'X-window-alloc)
  (let ((nw (make-vector 7 nil)))
    (aset nw 0 'X-window-alloc)		;type
    (aset nw 1 wid)			;the window id
    (aset nw 2 dpy)			;display backpointer
    (save-excursion
      (set-buffer (aref dpy 0))
      ;; append window, but only in the logging buffer!
      (setq X-window-list (cons nw X-window-list)))
    nw))

(defvar X-window-dealloc nil
  "Deallocation of window list.")

(defun X-window-dealloc (w)
  "Remove W from list of windows."
  (X-window-p w 'X-window-dealloc)
  (let ((tp X-window-list))
    (if (and tp (equal (car tp) w))
	(setq X-window-dealloc (cdr X-window-dealloc))
      (while tp
	(if (and (cdr tp) (equal (car (cdr tp)) w))
	    (setcdr tp (cdr (cdr tp))))
	(setq tp (cdr tp))))))

(defun X-window-find (wid)
  "Find window structure based on WID window handle."
  (let ((l X-window-list))
    (while (and l (not (= (aref (car l) 1) wid)))
      (setq l (cdr l)))
    (car l)))

(defun X-window-get-vector (wid dpy)
  "If WID is a known window on DPY, return it, otherwise allocate a new one."
  (save-excursion
    (set-buffer (aref dpy 0))
    (or (X-window-find wid)
	(X-window-alloc wid dpy))))

(defun X-window-get-display (w)
  "Get the display vector for window W."
  (X-window-p w 'X-window-get-display)
  (aref w 2))

(defun X-window-p (w &optional sig)
  "Return t if W is an Xwindow.
If SIG, then signal that error."
  (let ((v (and				;check both window, and dpy
	    (X-generic-p w 'X-window-alloc nil)
	    (X-dpy-p (aref w 2)))))
    (if (and (not v) sig)
	(signal 'wrong-type-argument (list 'X-window sig w))
      v)))

(defun X-add-event-handler (window event handler)
  "On WINDOW, when EVENT occurs, call HANDLER.
Each handler must accept two parameters.  The WINDOW, and an
event list.  The event list is specific to the type of event that was
recieved.

Use `X-remove-event-handler' to remove the handler."
  (X-window-p window 'X-add-event-handler)
  (let ((l (aref window 6)))
    (add-to-list 'l (cons event handler))
    (aset window 6 l)))

(defun X-remove-event-handler (window event handler)
  "On WINDOW, remove the event handler specified by EVENT and HANDLER.
Use `X-add-event-handler' to add a handler."
  (X-window-p window 'X-remove-event-handler)
  (let ((l1 (aref window 6)) l2 l3 (c (cons event handler)))
    (setq l2 l1)
    (while (and l2 (not (equal (car l2) c)))
      (setq l3 l2
	    l2 (cdr l2)))
    (if l2
	(if l3
	    (setcdr l3 (cdr l2))
	  (setq l1 (cdr l1))))
    (aset window 6 l1)))

(defun X-set-window-expose (win exposefn)
  "Set the Xwindow WIN's exposure event function to be EXPOSEFN.
An expose function must take the parameters:
   WINDOW PARAMLIST, where paramelist is the parts of the expose msg."
  (X-window-p win 'X-set-window-expose)
  (aset win 3 exposefn))

(defun X-set-window-input (win inputfn)
  "Set the Xwindow WIN's input event function to be INPUTFN.
An input function must take the parameters:
   WINDOW TYPE PARAMELIST, where type is the message id number, and
PARAMLIST is the list of message parts from X server."
  (X-window-p win 'X-set-window-input)
  (aset win 4 inputfn))

(defun X-set-window-reconfigure (win reconfig)
  "Set the Xwindow WIN's input event function to be INPUTFN.
An input function must take the parameters:
   WINDOW TYPE PARAMELIST, where type is the message id number, and
PARAMLIST is the list of message parts from X server.  RECONFIG is
stored in the window vector as a flag."
  (X-window-p win 'X-set-window-input)
  (aset win 5 reconfig))

;;;
;; DRAWABLE stuff.  A drawable is something you can draw to,
;; therefore, the only fn we need, is a drawable-p function.
;;
;; Each time we make a new drawable surface, add that to the list
;; of checks here!
;;
(defun X-drawable-p (d &optional sig)
  "Return t if D is a drawable.  If SIG, then signal an error."
  (let ((v (or (X-window-p d nil) t)))
    (if (and (not v) sig)
	(if sig (signal 'wrong-type-argument (list sig 'X-drawable d)) nil))))

;;;
;; How about some colormap type o stuff.  Very simple, but make like
;; all other abstract X types.
;;
(defun X-cmap (dpy mapid)
  "Create a colormap struct for DPY.
If MAPID then that is what MAPID is set to."
  (let ((v (make-vector 2 nil)))
    (aset v 0 'X-cmap)
    (if (and (numberp mapid) (not (integerp mapid)))
	(aset v 1 mapid) 
      (aset v 1 (Xid-get dpy)))
    v))

(defun X-cmap-p (cmap &optional sig)
  "Return t if CMAP is a colormap vector.
If SIG is specified, signal error."
  (X-generic-p cmap 'X-cmap sig))

;;; and how about old faithful XColor struct?
(defvar X-Color-type  '(0 . 0))
(defvar X-Color-pixel '(1 . 4))
(defvar X-Color-red   '(2 . 2))
(defvar X-Color-green '(3 . 2))
(defvar X-Color-blue  '(4 . 2))
(defvar X-Color-flags '(5 . 1))

(defun X-Color (dpy co &rest args)
  "Create a color vector for DPY with color CO.
If CO is nil, alloc one.  Typical color creation will look like this (for red):
ARGS are used to initialize.

   (X-color dpy nil 'X-Color-red 250 'X-Color-green 0 'X-color-blue 0)"
  ;; we must default the id to 0 ;(
  (let ((c (X-generic-attribute co 6 'X-Color 0 args)))
    ;; we must set colors to 0 if not specified.
    (if (not (aref c 2)) (aset c 2 0))
    (if (not (aref c 3)) (aset c 3 0))
    (if (not (aref c 4)) (aset c 4 0))
    c))

(defun X-Color-p (co &optional sig)
  "Return t if CO is an XColor.  If SIG, signal error."
  (X-generic-p co 'X-Color sig))

(defun X-color-message (co)
  "Convert CO into an X request message."
  (X-create-message (list [4 (X-get-id co)]
			  [2 (or (aref co 2) 0)] ;red
			  [2 (or (aref co 3) 0)] ;green
			  [2 (or (aref co 4) 0)] ;blue
			  [1 (or (aref co 5) X-RedGreenBlue)]
			  [1 nil])))

;;;
;; Graphic context stuff
;;
;; values are ( position . size )
;;
;; where position is position in vector, (which is (- position 2) in mask

(defvar X-GC-type                  '( 0 . 0 ))
(defvar X-GC-style                 '( 1 . 0))
;;; the value parts
(defvar X-GC-function              '( 2 .  1 ))
(defvar X-GC-plane-mask            '( 3 .  4 ))
(defvar X-GC-foreground            '( 4 .  4 ))
(defvar X-GC-background            '( 5 .  4 ))
(defvar X-GC-line-width            '( 6 .  2 ))
(defvar X-GC-line-style            '( 7 .  1 ))
(defvar X-GC-cap-style             '( 8 .  1 ))
(defvar X-GC-join-style            '( 9 .  1 ))
(defvar X-GC-fill-style            '( 10 . 1 ))
(defvar X-GC-fill-rule             '( 11 . 1 ))
(defvar X-GC-tile                  '( 12 . 4 ))
(defvar X-GC-stipple               '( 13 . 4 ))
(defvar X-GC-tile-stipple-x-origin '( 14 . 2 ))
(defvar X-GC-tile-stipple-y-origin '( 15 . 2 ))
(defvar X-GC-font                  '( 16 . 4 ))
(defvar X-GC-subwindow-mode        '( 17 . 1 ))
(defvar X-GC-graphics-exposures    '( 18 . 1 ))
(defvar X-GC-clip-x-origin         '( 19 . 2 ))
(defvar X-GC-clip-y-origin         '( 20 . 2 ))
(defvar X-GC-clip-mask             '( 21 . 4 ))
(defvar X-GC-dash-offset           '( 22 . 2 ))
(defvar X-GC-dashes                '( 23 . 1 ))
(defvar X-GC-arc-mode              '( 24 . 1 ))
;;; the list o parts

(defvar X-GC-list '( X-GC-function X-GC-plane-mask X-GC-foreground
     X-GC-background X-GC-line-width X-GC-line-style X-GC-cap-style
     X-GC-join-style X-GC-fill-style X-GC-fill-rule X-GC-tile
     X-GC-stipple X-GC-tile-stipple-x-origin
     X-GC-tile-stipple-y-origin X-GC-font X-GC-subwindow-mode
     X-GC-graphics-exposures X-GC-clip-x-origin X-GC-clip-mask
     X-GC-dash-offset X-GC-dashes X-GC-arc-mode )
"The list of GC parts for creating messages.")

(defun X-GC (dpy gc &rest args)
  "Add attributes ARGS to GC on display DPY.
If GC is nil, create GC. (X-GC GC attr val)"
  (if (vectorp gc)
      ;; make with GC already existing, no id necessary
      (X-generic-attribute gc 25 'X-GC nil args)
    ;; crate a new one
    (X-generic-attribute nil 25 'X-GC (Xid-get dpy) args)))

(defun X-GC-data (dpy gc &rest args)
  "Add attributes ARGS to GC on display DPY.
If GC is nil, create GC. (X-GC-data GC attr val).
This is different from X-GC because no new ID is created for the data,
and the data is suitable for passing to XChangeGC."
  (if (vectorp gc)
      ;; make with GC already existing, no id necessary
      (X-generic-attribute gc 25 'X-GC nil args)
    ;; crate a new one
    (X-generic-attribute nil 25 'X-GC nil args)))

(defun X-GC-p (gc &optional sig)
  "Return t if GC is a graphic context vector.
If SIG is specified, signal error."
  (X-generic-p gc 'X-GC sig))

(defun X-GC-message (gc)
  "Turn GC into the text of a message.
Include the VALUEMASK, which masks the parts to be used in the
graphic context."
  (X-generic-attribute-message gc X-GC-list))

;;;
;; Generic random-length mask-based message stuff
;;
(defun X-get-id (a)
  "Return the id from the attribute list A."
  (if (vectorp a) (aref a 1) (float 0)))

(defun X-generic-attribute (attrib vsize vtype vid &optional args)
  "Fill in ATTRIB vector, or create a new one of size VSIZE if nil.
Fill in with ARGS.  ARGS are a series of pairs.  A call would be:

       (X-generic-attribute nil 16 'X-attr-event-mask X-Exposure) 
       (X-generic-attribute AV 16 'X-attr-cursor MyCursor)

Args must be passed in as a list, as a &rest argument would be passed to
another function."
  (if (not (vectorp attrib))
      (setq attrib (make-vector vsize nil)))
  (aset attrib 0 vtype)
  (aset attrib 1 vid)
  
  (while args
    (if (listp (eval (car args)))
	(aset attrib (car (eval (car args))) (car (cdr args)))
      (error "Cannot set attribute %S" (car args)))
    (setq args (cdr (cdr args))))
  attrib)

(defun X-generic-p (a type &optional sig)
  "Return t if A is a vector of type TYPE.  If SIG, then signal an error."
  (let ((v (and (vectorp a) (equal (aref a 0) type))))
    (if (and (not v) sig)
	(if sig (signal 'wrong-type-argument (list sig type v)) nil))
    v))

(defun X-generic-vector-p (a &optional sig)
  "Return t if A is a generic vector with an associated ID.
If SIG, then signal an error."
  (let ((v (and (vectorp a) (floatp (aref a 1)))))
    (if (and (not v) sig)
	(if sig (signal 'wrong-type-argument 
			(list sig 'X-generic-vector v)) nil))
    v))

(defun X-generic-invalidate (a)
  "Invalidate the attribute list A so it can no longer be used."
  (if (not (vectorp a))
      (signal 'wrong-type-argument (list 'X-generic-invalidate a)))
  (aset a 0 nil))

(defun X-generic-attribute-message (a al &optional bitmask-size)
  "Convert the attribute vector A and the attribute list AL to a string.
The string is the message starting with VALUE_MASK, needed for
variable length requests, and the LISTofVALUE parts, depending if
those parts have been set.
Optional BITMASK-SIZE determines how much space is used by the bitmask
used in the message.  If it is excluded, then it defaults to 4."

  (if (not (and (vectorp a) (listp al)))
      (error "Can't make message of attributes."))
  (if (not bitmask-size) (setq bitmask-size 4))
  (let ((xal al)			;the attributes
	(m (float 0))			;mask o parts
	(l (cond ((= bitmask-size 4)	;the mask o given parts
		  (list [4 'm] ))
		 ((= bitmask-size 2)
		  (list [2 nil] ; reversed later
			[2 'm] ))
		 (t (error "Unsupported bitmask-size! Update the code."))))
	(tempv nil))			;temp vector
    (while xal
      (if (aref a (car (eval (car xal))))
	  (progn
	    ;; set the value part
	    (setq l (cons (progn (setq tempv (make-vector 2 nil))
				 (aset tempv 0 (cdr (eval (car xal)))) ;size
				 (aset tempv 1 (aref a (car (eval (car xal)))))
				 tempv) l))
	    ;; put in padding if we need it.
	    (if (< (cdr (eval (car xal))) 4)
		(setq l (cons
			 (progn
			   (setq tempv (make-vector 2 nil))
			   (aset tempv 0 (- 4 (cdr (eval (car xal)))))
			   tempv)
			 l)))
	    (setq m (Xmask-or m (Xmask (- (car (eval (car xal))) 2))))))
      (setq xal (cdr xal)))
    (if (<= bitmask-size 2) (setq m (truncate m)))
    (X-create-message (reverse l))))

(provide 'xwin)
;;; xwin.el ends here

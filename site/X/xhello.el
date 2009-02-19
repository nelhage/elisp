;;; hello --- hello world X demo/testbed

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xhello.el,v 1.4 1998/03/10 23:38:59 zappo Exp $
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
;; Example HELLO WORLD Emacs X program

(require 'xlib)

;;; Code:
(defvar Xhello-gc-1 nil)
(defvar Xhello-gc-2 nil)

(defun XX (host)
  "Do as much as I know so far.  Connect to HOST."

  (interactive "sHost: ")
  (let* ((dpy (XOpenDisplay host))
	 (w (if (aref dpy 0)
		(XCreateWindow dpy nil 20 20 100 100 20 nil nil nil
			       (X-attribute
				nil	;new one
				'X-attr-background-pixel (XWhitePixel dpy)
				'X-attr-border-pixel (XBlackPixel dpy)
				'X-attr-event-mask
				(Xmask-or XM-Exposure
					  XM-StructureNotify
					  )))
	      nil))
	 (cmap (XDefaultColormap dpy)) ;(XCreateColormap dpy w))
	 (co (X-Color dpy nil)))
    (if w
	(progn
	  ;; Set the WM protocols so that if we are killed, the X connection
	  ;; is not lost.
	  (let ((wmdw (XInternAtom dpy "WM_DELETE_WINDOW" nil)))
	    (if (not wmdw) (error "Failed to allocate atoms!"))
	    (XSetWMProtocols dpy w (list wmdw))
	    (X-add-event-handler w 33 'Xhello-close-window))
	  ;; Lets set up some colors and GCs for drawing.
	  (XAllocNamedColor dpy cmap "Red" co)
	  (setq Xhello-gc-1
		(XCreateGC dpy w
			   (X-GC dpy nil
				 'X-GC-foreground (X-get-id co)
				 'X-GC-background (XWhitePixel dpy 0)
				 'X-GC-line-style X-LineSolid
				 'X-GC-line-width 1)))
	  (XAllocNamedColor dpy cmap "Green" co)
	  (setq Xhello-gc-2
		(XCreateGC dpy w
			   (X-GC dpy nil
				 'X-GC-foreground (X-get-id co)
				 'X-GC-background (XWhitePixel dpy 0)
				 'X-GC-line-style X-LineDoubleDash
				 'X-GC-line-width 2)))
	  ;(X-set-window-close w 'Xhello-close-window)
	  (X-set-window-expose w 'Xhello-expose)
	  (X-set-window-reconfigure w 'Xhello-reconfigure)
	  (XMapWindow dpy w)))
    (XBell dpy 100)
    ))

(defun Xhello-expose (win params)
  ;; checkdoc-params: (params)
  "Expose the hello window WIN."
  (let ((dpy (X-window-get-display win)))
    (XDrawLine dpy win Xhello-gc-2 5 5 100 50)
    (XDrawPoint dpy win Xhello-gc-1 20 5)
    (XFillRectangle dpy win Xhello-gc-2 2 38 38 15)
    (XDrawRectangle dpy win Xhello-gc-1 2 38 38 15)
    (XDrawString dpy win Xhello-gc-1 5 50 "HELLO!")
    (XDrawSegments dpy win Xhello-gc-2 '(100 0 50 10 100 100 50 90))
    (XDrawArc dpy win Xhello-gc-1 50 50 20 20 0 (* 360 64))
    (XFillArc dpy win Xhello-gc-2 55 55 10 10 0 (* 360 64))
    ))

(defun Xhello-reconfigure (win event params)
  "Reconfigure the HELLO window WIN.
For this pass, mearly print out EVENT and PARAMS."
  (message "Event %d: %S" event params))

(defun Xhello-close-window (win params)
  "Called when WIN is closed.  Called with PARAMS."
  ;; This is a client message
  (let* ((dpy (X-window-get-display win))
	 (data (nth 3 params))
	 (a2 (XInternAtom dpy "WM_DELETE_WINDOW" nil))
	 )
    ;(message "Expect: %f Msg: %f" (X-get-id a2) (car (car data)))
    (if (= (X-get-id a2) (car (car data)))
	(XDestroyWindow dpy win))))

(provide 'xhello)
;;; xhello.el ends here

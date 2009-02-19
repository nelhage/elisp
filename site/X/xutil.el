;;; xutil --- Some utilities which require xlib for emacs

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xutil.el,v 1.4 1998/03/10 23:43:09 zappo Exp $
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
;; X service utility functions.

(require 'xlib)

;;; Code:

;;; List the existing windows...
;;
(defun X-tree-list (host)
  "Create a list of all named windows off the root window on HOST."
  (interactive "sHost: ")
  (let* ((dpy (XOpenDisplay host))
	 (rw (XDefaultRootWindow dpy)))
    (switch-to-buffer (format "*Windows %s*" (XServerName dpy)))
    (erase-buffer)
    (insert "Root:   " (Xmask-hex-string (X-get-id rw)) "\n")
    (X-tree-list-query dpy rw 1))
  (goto-char (point-min)))

(defun X-tree-list-query (dpy window depth)
  "On DPY, with WINDOW and DEPTH, query the tree and display children."
  (let ((wins (XQueryTree dpy window)))
    (setq wins (cdr (cdr wins)))
    (while wins
      (insert (make-string depth ? )
	      (Xmask-hex-string (X-get-id (car wins)))
	      "(")
      (sit-for 0)
      (insert (XFetchName dpy (car wins)) ")\n")
      (X-tree-list-query dpy (car wins) (1+ depth))
      (setq wins (cdr wins)))))

;;; Color viewer
;;
(defvar X-preview-window nil
  "The window used for previewing colors.")

(defun X-preview-color (color)
  "Preview COLOR in a little X window."
  (interactive "sColor: ")
  (let* ((dpy (if (X-window-p X-preview-window)
		  (X-window-get-display X-preview-window)
		(setq X-preview-window nil)
		(XOpenDisplay (getenv "DISPLAY"))))
	 (cmap (XDefaultColormap dpy))
	 (co (X-Color dpy nil)))
    (if (XAllocNamedColor dpy cmap color co)
	(progn
	  (if (X-window-p X-preview-window nil)	;don't err if wrong.
	      (XSetWindowBackground dpy X-preview-window (X-get-id co))
	    (setq X-preview-window
		  (XCreateWindow dpy nil 20 20 100 100 20 nil nil nil
				 (X-attribute
				  nil	;new one
				  'X-attr-background-pixel (X-get-id co)
				  'X-attr-border-pixel (XBlackPixel dpy))))
	    (if X-preview-window (XMapWindow dpy X-preview-window))))
      (error "Color %S could not be allocated." co))))

;;; xdpyinfo
;;
(defun X-info-buffer (name)
  "Display a buffer with information about the remote X display NAME."
  (interactive "sHost: ")
  (switch-to-buffer (get-buffer-create (format "*X %s*" name)))
  (delete-region (point-min) (point-max))
  (let* ((dpy (XOpenDisplay name))
	 (inf 0))
    (insert (format "%d:Connect Buffer: %s\n" inf (aref dpy inf)))
    (if (aref dpy inf)
	(progn
	  (setq inf (1+ inf))
	  (insert (format "%d:Major version : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Minor version : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Release       : %.0f\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Resource id   : %s\n" inf (Xmask-string (aref dpy inf))))
	  (setq inf (1+ inf))
	  (insert (format "%d:Resource mask : %s\n" inf (Xmask-string (aref dpy inf))))
	  (setq inf (1+ inf))
	  (insert (format "%d:Motion Buffsiz: %.0f\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Request len   : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Img byte order: %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:bit byte order: %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Scan thing    : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Scan pads     : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Min key       : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Max key       : %d\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Vendor ID     : %s\n" inf (aref dpy inf)))
	  (setq inf (1+ inf))
	  (insert (format "%d:Num formats   : %d\n" inf (length (aref dpy inf))))
	  (let ((dpy (aref dpy inf))
		(inf 0)
		(cnt 1))
	    (while (< inf (length dpy))
	      (let ((dpy (aref dpy inf))
		    (inf 0))
		(insert (format "Format # %d    :\n" cnt))
		(insert (format "  Depth         : %d\n" (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  Bits per pixel: %d\n" (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  Scanline pad  : %d\n" (aref dpy inf))))
	      (setq cnt (+ 1 cnt))
	      (setq inf (1+ inf))))
	  (setq inf (1+ inf))
	  (insert (format "%d:Num screens   : %d\n" inf (length (aref dpy inf))))
	  (let ((dpy (aref dpy inf))
		(inf 0)
		(cnt 1))
	    (while (< inf (length dpy))
	      (let ((dpy (aref dpy inf))
		    (inf 0))
		(insert (format "  Screen # %d   :\n" cnt))
		(insert (format "  %d:Root window id: 0x%x\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Dflt colormap : 0x%x\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:White pixel   : %.0f\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Black pixel   : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Event flags   : %s\n" inf (Xmask-string (aref dpy inf))))
		(setq inf (1+ inf))
		(insert (format "  %d:Width pixel   : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Height pixel  : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Width mm      : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Height mm     : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Min Maps      : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Max Maps      : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Visual id     : 0x%x\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Backing Stores: %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Save unders   : %S\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:Root Depth    : %d\n" inf (aref dpy inf)))
		(setq inf (1+ inf))
		(insert (format "  %d:# depths      : %d\n" inf (length (aref dpy inf))))
		(let ((dpy (aref dpy inf))
		      (inf 0)
		      (cnt 1))
		  (while (< inf (length dpy))
		    (let ((dpy (aref dpy inf))
			  (inf 0))
		      (insert (format "  Depth # %d   :\n" cnt))
		      (insert (format "    Depth        : %d\n" (aref dpy inf)))
		      (setq inf (1+ inf))
		      (insert (format "    # visuals      : %d\n" (length (aref dpy inf))))
		      (let ((dpy (aref dpy inf))
			    (inf 0)
			    (cnt 1))
			(while (< inf (length dpy))
			  (let ((nv [ "StaticGray" "GrayScale" "StaticColor"
				      "PseudoColor" "TrueColor" "DirectColor"])
				(dpy (aref dpy inf))
				(inf 0))
			    (insert (format "    Visual # %d   :\n" cnt))
			    (insert (format "     VisualId      : 0x%x\n" (aref dpy inf)))
			    (setq inf (1+ inf))
			    (insert (format "     Visual Class  : %s\n" (aref nv (aref dpy inf))))
			    (setq inf (1+ inf))
			    (insert (format "     Bits/RGB      : %d\n" (aref dpy inf)))
			    (setq inf (1+ inf))
			    (insert (format "     Cmap entries  : %d\n" (aref dpy inf)))
			    (setq inf (1+ inf))
			    (insert (format "     red mask      : %s\n" (Xmask-string (aref dpy inf))))
			    (setq inf (1+ inf))
			    (insert (format "     green mask    : %s\n" (Xmask-string (aref dpy inf))))
			    (setq inf (1+ inf))
			    (insert (format "     blue mask     : %s\n" (Xmask-string (aref dpy inf))))
			    )
			  (setq cnt (+ 1 cnt))
			  (setq inf (1+ inf)))))
		    (setq cnt (+ 1 cnt))
		    (setq inf (1+ inf)))))
	      (setq cnt (+ 1 cnt))
	      (setq inf (1+ inf))))
	  (XCloseDisplay dpy))
      (setq inf (1+ inf))
      (insert (format "%d:Major version : %d\n" inf (aref dpy inf)))
      (setq inf (1+ inf))
      (insert (format "%d:Minor version : %d\n" inf (aref dpy inf)))
      (setq inf (1+ inf))
      (insert (format "%d:Failure Reason: %s\n" inf (aref dpy inf)))))
  (goto-char 0)
  (not-modified))


(provide 'xutil)
;;; xutil.el ends here

;;; xconst --- Constants used in Xlib for masks and the like.

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xconst.el,v 1.5 1998/03/10 23:38:25 zappo Exp $
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
;; Constants used with our X connection.

(require 'xmath)
(require 'xc)
(require 'xwin)

;;; Code:
(defconst X-CopyFromParent 0 "CopyFromParent opcode.")
(defconst X-InputOutput 1 "InputOutput opcode.")
(defconst X-InputOnly 2 "InputOnly opcode.")

;;; Gravity

(defconst X-Unmap 0 "Unmap gravity.")
(defconst X-NorthWest 1 "NorthWest gravity.")
(defconst X-North 2 "North gravity.")
(defconst X-NorthEast 3 "NorthEast gravity.")
(defconst X-West 4 "West gravity.")
(defconst X-Center 5 "Center gravity.")
(defconst X-East 6 "East gravity.")
(defconst X-SouthWest 7 "SouthWest gravity.")
(defconst X-South 8 "South gravity.")
(defconst X-SouthEast 9 "SouthEast gravity.")
(defconst X-Static 10 "Static gravity.")

;; backing store

(defconst X-NotUseful 0 "NotUseful backing store.")
(defconst X-WhenMapped 1 "WhenMapped backing store.")
(defconst X-Always 2 "Always backing store.")

;;; Event Masks

(defconst XM-KeyPress (Xmask 0) "KeyPress bitmask.")
(defconst XM-KeyRelease (Xmask 1) "KeyRelease bitmask.")
(defconst XM-ButtonPress (Xmask 2) "ButtonPress bitmask.")
(defconst XM-ButtonRelease (Xmask 3) "ButtonRelease bitmask.")
(defconst XM-EnterWindow (Xmask 4) "EnterWindow bitmask.")
(defconst XM-LeaveWindow (Xmask 5) "LeaveWindow bitmask.")
(defconst XM-PointerMotion (Xmask 6) "PointerMotion bitmask.")
(defconst XM-PointerMotionHint (Xmask 7) "PointerMotionHint bitmask.")
(defconst XM-Button1Motion (Xmask 8) "Button2Motion bitmask.")
(defconst XM-Button2Motion (Xmask 9) "Button2Motion bitmask.")
(defconst XM-Button3Motion (Xmask 10) "Button3Motion bitmask.")
(defconst XM-Button4Motion (Xmask 11) "Button4Motion bitmask.")
(defconst XM-Button5Motion (Xmask 12) "Button5Motion bitmask.")
(defconst XM-Button (Xmask 13) "Button bitmask.")
(defconst XM-KeymapState (Xmask 14) "KeymapState bitmask.")
(defconst XM-Exposure (Xmask 15) "Exposure bitmask.")
(defconst XM-VisibilityChange (Xmask 16) "VisibilityChange bitmask.")
(defconst XM-StructureNotify (Xmask 17) "StructureNotify bitmask.")
(defconst XM-ResizeRedirect (Xmask 18) "ResizeRedirect bitmask.")
(defconst XM-SubstructureNotify (Xmask 19) "SubstructureNotify bitmask.")
(defconst XM-SubstructureRedirect (Xmask 20) "SubstructureRedirect bitmask.")
(defconst XM-FocusChange (Xmask 21) "FocusChange bitmask.")
(defconst XM-PropertyChange (Xmask 22) "PropertyChange bitmask.")
(defconst XM-ColormapChange (Xmask 23) "ColormapChange bitmask.")
(defconst XM-OwnerGrabButton (Xmask 24) "OwnerGrabButton bitmask.")

;; Event OpCodes

(defconst X-SyntheticMask 128 "Mask the synthetic part off.")
(defconst X-KeyPress 2 "KeyPress event.")
(defconst X-KeyRelease 3 "KeyRelease event.")
(defconst X-ButtonPress 4 "ButtonPress event.")
(defconst X-ButtonRelease 5 "ButtonRelease event.")
(defconst X-MotionNotify 6 "MotionNotify event.")
(defconst X-EnterNotify 7 "EnterNotify event.")
(defconst X-LeaveNotify 8 "LeaveNotify event.")
(defconst X-FocusIn 9 "FocusIn event.")
(defconst X-FocusOut 10 "FocusOut event.")
(defconst X-KeymapNotify 11 "KeymapNotify event.")
(defconst X-Expose 12 "Expose event.")
(defconst X-GraphicsExpose 13 "GraphicsExpose event.")
(defconst X-NoExpose 14 "NoExpose event.")
(defconst X-VisibilityNotify 15 "VisibilityNotify event.")
(defconst X-CreateNotify 16 "CreateNotify event.")
(defconst X-DestroyNotify 17 "DestroyNotify event.")
(defconst X-UnmapNotify 18 "UnmapNotify event.")
(defconst X-MapNotify 19 "MapNotify event.")
(defconst X-MapRequest 20 "MapRequest event.")
(defconst X-ReparentNotify 21 "ReparentNotify event.")
(defconst X-ConfigureNotify 22 "ConfigureNotify event.")
(defconst X-ConfigureRequest 23 "ConfigureRequest event.")
(defconst X-GravityNotify 24 "GravityNotify event.")
(defconst X-ResizeRequest 25 "ResizeRequest event.")
(defconst X-CirculateNotify 26 "CirculateNotify event.")
(defconst X-CirculateRequest 27 "CirculateRequest event.")
(defconst X-PropertyNotify 28 "PropertyNotify event.")
(defconst X-SelectionClear 29 "SelectionClear event.")
(defconst X-SelectionRequest 30 "SelectionRequest event.")
(defconst X-SelectionNotify 31 "SelectionNotify event.")
(defconst X-ColormapNotify 32 "ColormapNotify event.")
(defconst X-ClientMessage 33 "ClientMessage event.")
(defconst X-MappingNotify 34 "MappingNotify event.")
(defconst X-MaxEvent 35 "1 greater than the largest event opcode.")

;;; Stacking constants
(defconst X-Above 0 "Stacking mode Above.")
(defconst X-Below 1 "Stacking mode Below.")
(defconst X-TopIf 2 "Stacking mode TopIf.")
(defconst X-BottomIf 3 "Stacking mode BottomIf.")
(defconst X-Opposite 4 "Stacking mode Opposite.")

;;; Atom format
(defconst X-format-8 8 "8 bit formatting for Atoms.")
(defconst X-format-16 16 "16 bit formatting for Atoms.")
(defconst X-format-32 32 "32 bit formatting for Atoms.")

;;; Predefined Atoms
(defconst XA-AnyPropertyType (X-atom nil 0.0 "") "Any atom.")
(defconst XA-primary (X-atom nil 1.0 "PRIMARY") "Atom primary encoding.")
(defconst XA-secondary (X-atom nil 2.0 "SECONDARY") "Atom secondary encoding.")
(defconst XA-arc (X-atom nil 3.0 "ARC") "Atom arc encoding.")
(defconst XA-atom (X-atom nil 4.0 "ATOM") "Atom atom encoding.")
(defconst XA-bitmap (X-atom nil 5.0 "BITMAP") "Atom bitmap encoding.")
(defconst XA-cardinal (X-atom nil 6.0 "CARDINAL") "Atom cardinal encoding.")
(defconst XA-colormap (X-atom nil 7.0 "COLORMAP") "Atom colormap encoding.")
(defconst XA-cursor (X-atom nil 8.0 "CURSOR") "Atom cursor encoding.")
(defconst XA-cut-buffer0 (X-atom nil 9.0 "XA-CUT-BUFFER0") "Atom cut-buffer0 encoding.")
(defconst XA-cut-buffer1 (X-atom nil 10.0 "CUT-BUFFER1") "Atom cut-buffer1 eoncoding.")
(defconst XA-cut-buffer2 (X-atom nil 11.0 "CUT-BUFFER2") "Atom cut-buffer2 eoncoding.")
(defconst XA-cut-buffer3 (X-atom nil 12.0 "CUT-BUFFER3") "Atom cut-buffer3 eoncoding.")
(defconst XA-cut-buffer4 (X-atom nil 13.0 "CUT-BUFFER4") "Atom cut-buffer4 eoncoding.")
(defconst XA-cut-buffer5 (X-atom nil 14.0 "CUT-BUFFER5") "Atom cut-buffer5 eoncoding.")
(defconst XA-cut-buffer6 (X-atom nil 15.0 "CUT-BUFFER6") "Atom cut-buffer6 eoncoding.")
(defconst XA-cut-buffer7 (X-atom nil 16.0 "CUT-BUFFER7") "Atom cut-buffer7 eoncoding.")
(defconst XA-drawable (X-atom nil 17.0 "XA-DRAWABLE") "Atom drawable eoncoding.")
(defconst XA-font (X-atom nil 18.0 "FONT") "Atom font eoncoding.")
(defconst XA-integer (X-atom nil 19.0 "INTEGER") "Atom integer eoncoding.")
(defconst XA-pixmap (X-atom nil 20.0 "PIXMAP") "Atom pixmap eoncoding.")
(defconst XA-point (X-atom nil 21.0 "POINT") "Atom point eoncoding.")
(defconst XA-rectangle (X-atom nil 22.0 "RECTANGLE") "Atom rectangle eoncoding.")
(defconst XA-resource-manager (X-atom nil 23.0 "RESOURCE-MANAGER") "Atom resource-manager eoncoding.")
(defconst XA-rgb-color-map (X-atom nil 24.0 "RGB-COLOR-MAP") "Atom rgb-color-map eoncoding.")
(defconst XA-rgb-best-map (X-atom nil 25.0 "RGB-BEST-MAP") "Atom rgb-best-map eoncoding.")
(defconst XA-rgb-blue-map (X-atom nil 26.0 "RGB-BLUE-MAP") "Atom rgb-blue-map eoncoding.")
(defconst XA-rgb-default-map (X-atom nil 27.0 "RGB-DEFAULT-MAP") "Atom rgb-default-map eoncoding.")
(defconst XA-rgb-gray-map (X-atom nil 28.0 "RGB-GRAY-MAP") "Atom rgb-gray-map eoncoding.")
(defconst XA-rgb-green-map (X-atom nil 29.0 "RGB-GREEN-MAP") "Atom rgb-green-map eoncoding.")
(defconst XA-rgb-red-map (X-atom nil 30.0 "RGB-RED-MAP") "Atom rgb-red-map eoncoding.")
(defconst XA-string (X-atom nil 31.0 "STRING") "Atom string eoncoding.")
(defconst XA-visualid (X-atom nil 32.0 "VISUALID") "Atom visualid eoncoding.")
(defconst XA-window (X-atom nil 33.0 "WINDOW") "Atom window eoncoding.")
(defconst XA-wm-command (X-atom nil 34.0 "WM-COMMAND") "Atom wm-command eoncoding.")
(defconst XA-wm-hints (X-atom nil 35.0 "WM-HINTS") "Atom wm-hints eoncoding.")
(defconst XA-wm-client-machine (X-atom nil 36.0 "WM-CLIENT-MACHINE") "Atom wm-client-machine eoncoding.")
(defconst XA-wm-icon-name (X-atom nil 37.0 "WM-ICON-NAME") "Atom wm-icon-name eoncoding.")
(defconst XA-wm-icon-size (X-atom nil 38.0 "WM-ICON-SIZE") "Atom wm-icon-size eoncoding.")
(defconst XA-wm-name (X-atom nil 39.0 "WM-NAME") "Atom wm-name eoncoding.")
(defconst XA-wm-normal-hints (X-atom nil 40.0 "WM-NORMAL-HINTS") "Atom wm-normal-hints eoncoding.")
(defconst XA-wm-size-hints (X-atom nil 41.0 "WM-SIZE-HINTS") "Atom wm-size-hints eoncoding.")
(defconst XA-wm-zoom-hints (X-atom nil 42.0 "WM-ZOOM-HINTS") "Atom wm-zoom-hints eoncoding.")
(defconst XA-min-space (X-atom nil 43.0 "MIN-SPACE") "Atom min-space eoncoding.")
(defconst XA-norm-space (X-atom nil 44.0 "NORM-SPACE") "Atom norm-space eoncoding.")
(defconst XA-max-space (X-atom nil 45.0 "MAX-SPACE") "Atom max-space eoncoding.")
(defconst XA-end-space (X-atom nil 46.0 "END-SPACE") "Atom end-space eoncoding.")
(defconst XA-superscript-x (X-atom nil 47.0 "SUPERSCRIPT-X") "Atom superscript-x eoncoding.")
(defconst XA-superscript-y (X-atom nil 48.0 "SUPERSCRIPT-Y") "Atom superscript-y eoncoding.")
(defconst XA-subscript-x (X-atom nil 49.0 "SUBSCRIPT-X") "Atom subscript-x eoncoding.")
(defconst XA-subscript-y (X-atom nil 50.0 "SUBSCRIPT-Y") "Atom subscript-y eoncoding.")
(defconst XA-underline-position (X-atom nil 51.0 "UNDERLINE-POSITION") "Atom underline-position eoncoding.")
(defconst XA-underline-thickness (X-atom nil 52.0 "UNDERLINE-THICKNESS") "Atom underline-thickness eoncoding.")
(defconst XA-strikeout-ascent (X-atom nil 53.0 "STRIKEOUT-ASCENT") "Atom strikeout-ascent eoncoding.")
(defconst XA-strikeout-descent (X-atom nil 54.0 "STRIKEOUT-DESCENT") "Atom strikeout-descent eoncoding.")
(defconst XA-italic-angle (X-atom nil 55.0 "ITALIC-ANGLE") "Atom italic-angle eoncoding.")
(defconst XA-x-height (X-atom nil 56.0 "X-HEIGHT") "Atom x-height eoncoding.")
(defconst XA-quad-width (X-atom nil 57.0 "QUAD-WIDTH") "Atom quad-width eoncoding.")
(defconst XA-weight (X-atom nil 58.0 "WEIGHT") "Atom weight eoncoding.")
(defconst XA-point-size (X-atom nil 59.0 "POINT-SIZE") "Atom point-size eoncoding.")
(defconst XA-resolution (X-atom nil 60.0 "RESOLUTION") "Atom resolution eoncoding.")
(defconst XA-copyright (X-atom nil 61.0 "COPYRIGHT") "Atom copyright eoncoding.")
(defconst XA-notice (X-atom nil 62.0 "NOTICE") "Atom notice eoncoding.")
(defconst XA-font-name (X-atom nil 63.0 "FONT-NAME") "Atom font-name eoncoding.")
(defconst XA-family-name (X-atom nil 64.0 "FAMILY-NAME") "Atom family-name eoncoding.")
(defconst XA-full-name (X-atom nil 65.0 "FULL-NAME") "Atom full-name eoncoding.")
(defconst XA-cap-height (X-atom nil 66.0 "CAP-HEIGHT") "Atom cap-height eoncoding.")
(defconst XA-wm-class (X-atom nil 67.0 "WM-CLASS") "Atom wm-class eoncoding.")
(defconst XA-wm-transient-for (X-atom nil 68.0 "WM-TRANSIENT-FOR") "Atom wm-transient-for eoncoding.")

;;; Property Modes for atoms
(defconst X-PropModeReplace 0 "Property Mode Replace")
(defconst X-PropModePrepend 1 "Property Mode Prepend")
(defconst X-PropModeAppend  2 "Property Mode Append")

;;; KeyButtonMask

(defconst X-Shift (Xmask 0) "Shift bitmask.")
(defconst X-Lock (Xmask 1) "Lock bitmask.")
(defconst X-Control (Xmask 2) "Control bitmask.")
(defconst X-Mod1 (Xmask 3) "Mod1 bitmask.")
(defconst X-Mod2 (Xmask 4) "Mod2 bitmask.")
(defconst X-Mod3 (Xmask 5) "Mod3 bitmask.")
(defconst X-Mod4 (Xmask 6) "Mod4 bitmask.")
(defconst X-Mod5 (Xmask 7) "Mod5 bitmask.")
(defconst X-Button1 (Xmask 8) "Button1 bitmask.")
(defconst X-Button2 (Xmask 9) "Button2 bitmask.")
(defconst X-Button3 (Xmask 10) "Button3 bitmask.")
(defconst X-Button4 (Xmask 11) "Button4 bitmask.")
(defconst X-Button5 (Xmask 12) "Button5 bitmask.")

;;; Graphic context stuff
;;

;;; functions
(defconst X-GXClear 0 "GC function type id.")
(defconst X-GXAnd 1 "GC function type id.")
(defconst X-GXAndReverse 2 "GC function type id.")
(defconst X-GXCopy 3 "GC function type id.")
(defconst X-GXAndInverted 4 "GC function type id.")
(defconst X-GXNoOp 5 "GC function type id.")
(defconst X-GXXor 6 "GC function type id.")
(defconst X-GXOr 7 "GC function type id.")
(defconst X-GXNor 8 "GC function type id.")
(defconst X-GXEquiv 9 "GC function type id.")
(defconst X-GXInvert 10 "GC function type id.")
(defconst X-GXOrReverse 11 "GC function type id.")
(defconst X-GXCopyInverted 12 "GC function type id.")
(defconst X-GXOrInverted 13 "GC function type id.")
(defconst X-GXNand 14 "GC function type id.")
(defconst X-GXSet 15 "GC function type id.")

;; line styles
(defconst X-LineSolid 0 "GC line-style.")
(defconst X-LineOnOffDash 1 "GC line-style.")
(defconst X-LineDoubleDash 2 "GC line-style.")

;; cap-styles
(defconst X-CapNotLast 0 "GC cap-styles.")
(defconst X-CapButt 1 "GC cap-styles.")
(defconst X-CapRound 2 "GC cap-styles.")
(defconst X-CapProjecting 3 "GC cap-styles.")

;; join styles
(defconst X-JoinMiter 0 "GC join-style.")
(defconst X-JoinRound 1 "GC join-style.")
(defconst X-JoinBevel 2 "GC join-style.")

;; fill style
(defconst X-FillSolid 0 "GC fill-style.")
(defconst X-FillTiled 1 "GC fill-style.")
(defconst X-FillStippled 2 "GC fill-style.")
(defconst X-FillOpaqueStippled 3 "GC fill-style.")

;; fill rule
(defconst X-EvenOddRule 0 "GC fill-rule.")
(defconst X-WindingRule 1 "GC fill-rule.")

;; arc-mode
(defconst X-ArcChord 0 "GC arc mode.")
(defconst X-ArcPieSlice 1 "GC arc mode.")

;; Subwindow mode
(defconst X-ClipByChildren 0 "GC subwindow-mode.")
(defconst X-IncludeInferiors 1 "GC subwindow-mode.")

;;; Some color type stuff
;;
(defconst X-AllocNone 0 "No color entries writable.")
(defconst X-AllocAll  1 "All color entries writable.")

(defconst X-DoRed 1 "Do Red mask.")
(defconst X-DoGreen 2 "Do Green mask.")
(defconst X-DoBlue 4 "Do blue mask.")
(defconst X-DoRedGreenBlue 7 "All Color Dos ored together.")

;;; Some drawing constants
;;
(defconst X-Origin 0 "Specifies point drawn with relation to origin.")
(defconst X-Previous 1 "Specifies points draw with relation to previous point.")

(provide 'xconst)
;;; xconst.el ends here

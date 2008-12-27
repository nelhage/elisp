;;; dvc-cmenu.el --- code implementing a context menu with keyboard

;; Copyright (C) 2006 by all contributors

;; This file is part of DVC.
;;
;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Generally context menu is supported only mouse pressing(or clicking).
;; In Xtla, I proposed a context menu supporting operation by keyboard:
;; an user can type C-j to pop the context menu under the point up.
;; I think it is quite useful, so I decide to separate the code from
;; xtla.el.
;; In addition dvc-cmenu supports target item highlighting during popup.
;; So during popup, a user can recognize the context of menu popup now.

;;; Code:
(eval-when-compile (require 'dvc-utils))

(defvar dvc-cmenu 'dvc-cmenu
  "Name of property for embedding a context menu to text.")

(defun dvc-cmenu-beginning (point)
  "Search backward the position where `dvc-cmenu' property is changed."
  (previous-single-property-change point dvc-cmenu))

(defun dvc-cmenu-end (point)
  "Search forward the position where `dvc-cmenu' property is changed."
  (next-single-property-change point dvc-cmenu))

(defun dvc-cmenu-popup-by-mouse (event prefix)
  "Generic function to popup a menu.

The menu is defined in the text property under the point which is
given by mouse.  EVENT is the mouse event that called the function.
PREFIX is passed to `dvc-cmenu-popup'."
  (interactive "e\nP")
  (mouse-set-point event)
  (dvc-cmenu-popup prefix))

;; Copied from avoid.el.
(defun dvc-cmenu-mouse-avoidance-point-position (point)
  "Return the position of POINT as (FRAME X . Y).
Analogous to `mouse-position'.  Copied from avoid.el."
  (dvc-do-in-gnu-emacs
    (let* ((w (selected-window))
           (edges (window-edges w))
           (list
            (compute-motion (max (window-start w) (point-min)) ; start pos
                            ;; window-start can be < point-min if the
                            ;; latter has changed since the last redisplay
                            '(0 . 0)                              ; start XY
                            point                                 ; stop pos
                            (cons (window-width) (window-height)) ; stop XY: none
                            (1- (window-width))                   ; width
                            (cons (window-hscroll w) 0) ; 0 may not be right?
                            (selected-window))))
      ;; compute-motion returns (pos HPOS VPOS prevhpos contin)
      ;; we want:               (frame hpos . vpos)
      (cons (selected-frame)
            (cons (+ (car edges)       (car (cdr list)))
                  (+ (car (cdr edges)) (car (cdr (cdr list)))))))))

(defun dvc-cmenu-popup (prefix)
  "Popup a menu defined in the text property under the point.

PREFIX is passed to `popup-menu'."
  (interactive "P")
  (if (get-text-property (point) dvc-cmenu)
      (let* ((menu (get-text-property (point) dvc-cmenu))
             (p (previous-single-property-change (point) dvc-cmenu nil
                                                 (line-beginning-position)))
             (n (next-single-property-change (point) dvc-cmenu nil
                                             (line-end-position)))
             (b (if (and p (get-text-property p dvc-cmenu)) p (point)))
             (e (if n n (point))))
        (if (and (not (featurep 'xemacs)) (interactive-p))
            (let* ((pos (dvc-cmenu-mouse-avoidance-point-position e))
                   (object (car pos))
                   (x (cadr pos))
                   (y (cddr pos)))
              (set-mouse-position object x y)))
        (dvc-cmenu-popup-with-highlight 'dvc-highlight
                                        b e
                                        menu
                                        prefix))
    (error "No context-menu under the point")))

(defun dvc-cmenu-popup-with-highlight (face begin end menu &optional prefix)
  "Put FACE on BEGIN and END in the buffer during Popup MENU.
PREFIX is passed to `popup-menu'."
  (let (o)
    (unwind-protect
        (progn
          (setq o (make-overlay begin end))
          (overlay-put o 'face face)
          (sit-for 0)
          (popup-menu menu prefix))
      (delete-overlay o))))

(provide 'dvc-cmenu)

;; Local Variables:
;; End:

;;; dvc-cmenu.el ends here

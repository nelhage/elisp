;;;; psgml-lucid.el --- Part of SGML-editing mode with parsing support
;; $Id: psgml-lucid.el,v 2.7 2002/04/25 20:50:27 lenst Exp $

;; Copyright (C) 1994 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;;	   William M. Perry <wmperry@indiana.edu>

;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;;; Part of psgml.el

;;; Menus for use with Lucid Emacs


;;;; Code:

(require 'psgml)
;;(require 'easymenu)

(eval-and-compile
  (autoload 'sgml-do-set-option "psgml-edit"))

(defvar sgml-max-menu-size (/ (* (frame-height) 2) 3)
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")

;;;; Pop Up Menus

(defun sgml-popup-menu (event title entries)
  "Display a popup menu."
  (setq entries
	(loop for ent in entries collect
	      (vector (car ent)
		      (list 'setq 'value (list 'quote (cdr ent)))
		      t)))
  (cond ((> (length entries) sgml-max-menu-size)
	 (setq entries
	       (loop for i from 1 while entries collect
		     (let ((submenu
			    (subseq entries 0 (min (length entries)
						   sgml-max-menu-size))))
		       (setq entries (nthcdr sgml-max-menu-size
					     entries))
		       (cons
			(format "%s '%s'-'%s'"
				title
				(sgml-range-indicator (aref (car submenu) 0))
				(sgml-range-indicator
				 (aref (car (last submenu)) 0)))
			submenu))))))
  (sgml-lucid-get-popup-value (cons title entries)))


(defun sgml-range-indicator (string)
  (substring string
	     0
	     (min (length string) sgml-range-indicator-max-length)))


(defun sgml-lucid-get-popup-value (menudesc)
  (let ((value nil)
	(event nil))
    (popup-menu menudesc)
    (while (popup-up-p)
      (setq event (next-command-event event))
      (cond ((misc-user-event-p event)
	     (cond
	      ((eq (event-object event) 'abort)
	       (signal 'quit nil))
	      ((eq (event-object event) 'menu-no-selection-hook)
	       nil)
	      (t
	       (eval (event-object event)))))
	    ((button-release-event-p event) ; don't beep twice
	     nil)
	    (t
	     (beep)
	     (message "please make a choice from the menu."))))
    value))

(defun sgml-popup-multi-menu (pos title menudesc)
  "Display a popup menu.
MENUS is a list of menus on the form (TITLE ITEM1 ITEM2 ...).
ITEM should have to form (STRING EXPR) or STRING.  The EXPR gets evaluated
if the item is selected."
  (popup-menu
   (cons title
	 (loop for menu in menudesc collect
	       (cons (car menu)		; title
		     (loop for item in (cdr menu) collect
			   (if (stringp item)
			       item
			     (vector (car item) (cadr item) t))))))))


;;;; Lucid menu bar

(defun sgml-make-options-menu (vars)
  (loop for var in vars 
	for type = (sgml-variable-type var)
	for desc = (sgml-variable-description var)
	collect
	(cond
	 ((eq type 'toggle)
	  (vector desc (list 'setq var (list 'not var))
		  ':style 'toggle ':selected var))
	 ((consp type)
	  (cons desc
		(loop for c in type collect
		      (if (atom c)
			  (vector (prin1-to-string c)
				  (`(setq (, var) (, c)))
				  :style 'toggle
				  :selected (`(eq (, var) '(, c))))
			(vector (car c)
				(`(setq (, var) '(,(cdr c))))
				:style 'toggle
				:selected (`(eq (, var) '(,(cdr c)))))))))
	 (t
	  (vector desc
		  (`(sgml-do-set-option '(, var)))
		  t)))))


(unless (or (not (boundp 'emacs-major-version))
	    (and (boundp 'emacs-minor-version)
		 (< emacs-minor-version 10)))
  (loop for ent on sgml-main-menu
	if (vectorp (car ent))
	do (cond
	    ((equal (aref (car ent) 0) "File Options >")
	     (setcar ent
		     (cons "File Options"
			   (sgml-make-options-menu sgml-file-options))))
	    ((equal (aref (car ent) 0) "User Options >")
	     (setcar ent
		     (cons "User Options"
			   (sgml-make-options-menu sgml-user-options)))))))


;;;; Key definitions

(define-key sgml-mode-map [button3] 'sgml-tags-menu)


;;;; Insert with properties

(defun sgml-insert (props format &rest args)
  (let ((start (point))
	tem)
    (insert (apply (function format)
		   format
		   args))
    (remf props 'rear-nonsticky)	; not useful in Lucid

    ;; Copy face prop from category
    (when (setq tem (getf props 'category))
      (when (setq tem (get tem 'face))
	  (set-face-underline-p (make-face 'underline) t)
	  (setf (getf props 'face) tem)))

    (add-text-properties start (point) props)

    ;; A read-only value of 1 is used for the text after values
    ;; and this should in Lucid be open at the front.
    (if (eq 1 (getf props 'read-only))
	(set-extent-property
	 (extent-at start nil 'read-only)
	 'start-open t))))


;;;; Set face of markup

(defun sgml-set-face-for (start end type)
  (let ((face (cdr (assq type sgml-markup-faces)))
	o)
    (loop for e being the extents from start to end
	  do (when (extent-property e 'sgml-type)
	       (cond ((and (null o)
			   (eq type (extent-property e 'sgml-type)))
		      (setq o e))
		     (t (delete-extent e)))))

    (cond (o
	   (set-extent-endpoints o start end))
	  (face
	   (setq o (make-extent start end))
	   (set-extent-property o 'sgml-type type)
	   (set-extent-property o 'face face)
	   (set-extent-property o 'start-open t)
	   (set-extent-face o face)))))

(defun sgml-set-face-after-change (start end &optional pre-len)
  ;; This should not be needed with start-open t
  (when sgml-set-face
    (let ((o (extent-at start nil 'sgml-type)))
      (cond
       ((null o))
       ((= start (extent-start-position o))
	(set-extent-endpoints o end (extent-end-position o)))
       (t (delete-extent o))))))

;(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (extent-property o 'type)
	do (delete-extent o)))


;;;; Functions not in Lucid Emacs

(unless (fboundp 'frame-width)
  (defalias 'frame-width 'screen-width))

(unless (fboundp 'buffer-substring-no-properties)
  (defalias 'buffer-substring-no-properties 'buffer-substring))


;;;; Provide

(provide 'psgml-lucid)


;;; psgml-lucid.el ends here

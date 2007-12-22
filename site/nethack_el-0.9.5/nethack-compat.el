;;; nethack-compat.el --- compatibility file for various emacsen

;; Copyright (C) 2003,2005 Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Any stupid hacks required to get this thing to compile and run on
;; different emacsen should go in this file.  The goal is to keep the
;; rest of the files free from feature or version testing, if
;; possible.

;;; Code:

;; make sure the common lisp compatibility library is available
(eval-when-compile
  (require 'cl))

;; overlay is "deprecated" in XEmacs, but still exists
(if (featurep 'xemacs)
    (require 'overlay))


;;; utility/compatibility functions
(defun nh-propertize (string &rest properties)
  "Add text PROPERTIES to STRING and return the new string."
  (add-text-properties 0 (length string) properties string)
  string)

(defun nh-assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  ;; this is defined in emacs21 as `assq-delete-all'.
  (let ((tail alist))
    (while tail
      (if (eq (car (car tail)) key)
	  (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun nh-window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (save-excursion
    (set-buffer (window-buffer window))
    (count-lines (point-min) (point-max))))

;; XEmacs chars are not ints
(defalias 'nh-char-to-int (if (fboundp 'char-to-int)
			      'char-to-int
			    'identity))

(defun nh-read-char (&optional prompt)
  (message prompt)
  (let ((char (read-char-exclusive)))
    (message "")
    (nh-char-to-int char)))

(provide 'nethack-compat)
;;; nethack-compat.el ends here

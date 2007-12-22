;;; poe-xemacs.el --- poe submodule for XEmacs

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, XEmacs

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(require 'pym)


;;; @ color
;;;

(defun-maybe set-cursor-color (color-name)
  "Set the text cursor color of the selected frame to COLOR.
When called interactively, prompt for the name of the color to use."
  (interactive "sColor: ")
  (set-frame-property (selected-frame) 'cursor-color
                      (if (color-instance-p color-name)
                          color-name
                        (make-color-instance color-name))))


;;; @ face
;;;

(defalias-maybe 'face-list 'list-faces)

(or (memq 'underline (face-list))
    (and (fboundp 'make-face)
	 (make-face 'underline)))

(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t))


;;; @ overlay
;;;

(condition-case nil
    (require 'overlay)
  (error
   (defalias 'make-overlay 'make-extent)
   (defalias 'overlayp 'extentp)
   (defalias 'overlay-put 'set-extent-property)
   (defalias 'overlay-buffer 'extent-buffer)
   (defun move-overlay (extent start end &optional buffer)
     (set-extent-endpoints extent start end))
   (defalias 'delete-overlay 'detach-extent)))


;;; @ dired
;;;

(defun-maybe dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))


;;; @ timer
;;;

(condition-case nil
    (require 'timer)
  (error
   (require 'itimer)
   (defun-maybe run-at-time (time repeat function &rest args)
     (start-itimer (make-temp-name "rat")
		   `(lambda ()
		      (,function ,@args))
		   time repeat))
   (defalias 'cancel-timer 'delete-itimer)
   (defun with-timeout-handler (tag)
     (throw tag 'timeout))
   (defmacro-maybe with-timeout (list &rest body)
     (let ((seconds (car list))
	   (timeout-forms (cdr list)))
     `(let ((with-timeout-tag (cons nil nil))
	    with-timeout-value with-timeout-timer)
	(if (catch with-timeout-tag
	      (progn
		(setq with-timeout-timer
		      (run-at-time ,seconds nil
				   'with-timeout-handler
				   with-timeout-tag))
		(setq with-timeout-value (progn . ,body))
		nil))
	    (progn . ,timeout-forms)
	  (cancel-timer with-timeout-timer)
	  with-timeout-value))))))


;;; @ to avoid bug of XEmacs 19.14
;;;

(or (string-match "^../"
		  (file-relative-name "/usr/local/share" "/usr/local/lib"))
    ;; This function was imported from Emacs 19.33.
    (defun file-relative-name (filename &optional directory)
      "Convert FILENAME to be relative to DIRECTORY
(default: default-directory)."
      (setq filename (expand-file-name filename)
	    directory (file-name-as-directory
		       (expand-file-name
			(or directory default-directory))))
      (let ((ancestor ""))
	(while (not (string-match (concat "^" (regexp-quote directory))
				  filename))
	  (setq directory (file-name-directory (substring directory 0 -1))
		ancestor (concat "../" ancestor)))
	(concat ancestor (substring filename (match-end 0))))))


;;; @ Emacs 20.3 emulation
;;;

(defalias-maybe 'line-beginning-position 'point-at-bol)
(defalias-maybe 'line-end-position 'point-at-eol)

;;; @ XEmacs 21 emulation
;;;

;; XEmacs 20.5 and later: (set-extent-properties EXTENT PLIST)
(defun-maybe set-extent-properties (extent plist)
  "Change some properties of EXTENT.
PLIST is a property list.
For a list of built-in properties, see `set-extent-property'."
  (while plist
    (set-extent-property extent (car plist) (cadr plist))
    (setq plist (cddr plist))))  

;;; @ end
;;;

(require 'product)
(product-provide (provide 'poe-xemacs) (require 'apel-ver))

;;; poe-xemacs.el ends here

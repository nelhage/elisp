;;; Random VM macros
;;; Copyright (C) 1989-1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;(provide 'vm-macro)

(defsubst vm-marker (pos &optional buffer)
  (set-marker (make-marker) pos buffer))

(defsubst vm-select-folder-buffer ()
  (cond (vm-mail-buffer
	 (or (buffer-name vm-mail-buffer)
	     (error "Folder buffer has been killed."))
	 (set-buffer vm-mail-buffer))
	((not (memq major-mode '(vm-mode vm-virtual-mode)))
	 (error "No VM folder buffer associated with this buffer"))))

(defsubst vm-select-folder-buffer-if-possible ()
  (cond ((and (bufferp vm-mail-buffer)
	      (buffer-name vm-mail-buffer))
	 (set-buffer vm-mail-buffer))))

(defsubst vm-error-if-folder-read-only ()
  (while vm-folder-read-only
    (signal 'folder-read-only (list (current-buffer)))))

(defsubst vm-error-if-virtual-folder ()
  (and (eq major-mode 'vm-virtual-mode)
       (error "%s cannot be applied to virtual folders." this-command)))

(defsubst vm-build-threads-if-unbuilt ()
  (if (not (vectorp vm-thread-obarray))
      (vm-build-threads nil)))

(defsubst vm-binary-coding-system ()
  (cond (vm-xemacs-mule-p 'binary)
	(vm-xemacs-file-coding-p 'binary)
	(t 'no-conversion)))

(defsubst vm-line-ending-coding-system ()
  (cond (vm-xemacs-mule-p 'no-conversion)
	(vm-xemacs-file-coding-p 'no-conversion)
	(t 'raw-text)))

;;; can't use defsubst where quoting is needed in some places but
;;; not others.

;; save-restriction flubs restoring the clipping region if you
;; (widen) and modify text outside the old region.
;; This should do it right.
(defmacro vm-save-restriction (&rest forms)
  (let ((vm-sr-clip (make-symbol "vm-sr-clip"))
	(vm-sr-min (make-symbol "vm-sr-min"))
	(vm-sr-max (make-symbol "vm-sr-max")))
    `(let ((,vm-sr-clip (> (buffer-size) (- (point-max) (point-min))))
	   ;; this shouldn't be necessary but the
	   ;; byte-compiler turns these into interned symbols
	   ;; which utterly defeats the purpose of the
	   ;; make-symbol calls above.  Soooo, until the compiler
	   ;; is fixed, these must be made into (let ...)
	   ;; temporaries so that nested calls to this macros
	   ;; won't misbehave.
	   ,vm-sr-min ,vm-sr-max)
	  (and ,vm-sr-clip
	       (setq ,vm-sr-min (set-marker (make-marker) (point-min)))
	       (setq ,vm-sr-max (set-marker (make-marker) (point-max))))
	  (unwind-protect
	      (progn ,@forms)
	    (widen)
	    (and ,vm-sr-clip
		 (progn
		   (narrow-to-region ,vm-sr-min ,vm-sr-max)
		   (set-marker ,vm-sr-min nil)
		   (set-marker ,vm-sr-max nil)))))))

(defmacro vm-save-buffer-excursion (&rest forms)
  `(let ((vm-sbe-buffer (current-buffer)))
    (unwind-protect
	(progn ,@forms)
      (and (not (eq vm-sbe-buffer (current-buffer)))
	   (buffer-name vm-sbe-buffer)
	   (set-buffer vm-sbe-buffer)))))

(defmacro vm-assert (expression)
  (list 'or expression
	(list 'let
	      (list (list 'debug-on-error t))
	      (list 'error "assertion failed: %S"
		    (list 'quote expression)))))

(defmacro vm-increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro vm-decrement (variable)
  (list 'setq variable (list '1- variable)))

(provide 'vm-macro)

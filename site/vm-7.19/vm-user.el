;;; Interface functions to VM internal data
;;; Copyright (C) 1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
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

;;(provide 'vm-user)

(defun vm-user-composition-folder-buffer ()
  "Returns the folder buffer associated with the current buffer.
The current buffer must be a composition buffer created by VM for
a reply, resend or forward.

Nil is returned if the current buffer is not associated with any
VM folder.

Note that the buffer returned might be a virtual folder buffer,
which might have several underlying real folders associated with
it.  To get the list of real folder buffers associated with a
composition buffer, use vm-user-composition-real-folder-buffers
instead."
  (if (eq major-mode 'mail-mode)
      vm-mail-buffer
    nil ))

(defun vm-user-composition-real-folder-buffers ()
  "Returns a list of the real folder buffers associated with the current
buffer.  The current buffer must be a composition buffer created
by VM for a reply, resend or forward."
  (if (eq major-mode 'mail-mode)
      (let ((list nil) (newlist nil))
	(cond ((eq vm-system-state 'replying)
	       (setq list vm-reply-list))
	      ((eq vm-system-state 'forwarding)
	       (setq list vm-forward-list))
	      ((eq vm-system-state 'redistributing)
	       (setq list vm-redistribute-list)))
	(while list
	  (setq newlist (cons (vm-buffer-of (vm-real-message-of (car list)))
			      newlist)
		list (cdr list)))
	newlist )
    nil ))

(provide 'vm-user)

;;; Thread support for VM
;;; Copyright (C) 1994, 2001 Kyle E. Jones
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

;;(provide 'vm-thread)

(defun vm-toggle-threads-display ()
  "Toggle the threads display on and off.
When the threads display is on, the folder will be sorted by
thread and thread indentation (via the %I summary format specifier)
will be visible."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  ;; get numbering of new messages done now
  ;; so that the sort code only has to worry about the
  ;; changes it needs to make.
  (vm-update-summary-and-mode-line)
  (vm-set-summary-redo-start-point t)
  (setq vm-summary-show-threads (not vm-summary-show-threads))
  (if vm-summary-show-threads
      (vm-sort-messages "thread")
    (vm-sort-messages "physical-order")))

(defun vm-build-threads (message-list)
  (if (not (vectorp vm-thread-obarray))
      (setq vm-thread-obarray (make-vector 641 0)
	    vm-thread-subject-obarray (make-vector 641 0)))
  (let ((mp (or message-list vm-message-list))
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 40))
	;; no need to schedule reindents of reparented messages
	;; unless there were already messages present.
	(schedule-reindents message-list)
	m parent parent-sym id id-sym date refs old-parent-sym)
    (while mp
      (setq m (car mp)
	    parent (vm-th-parent m)
	    id (vm-su-message-id m)
	    id-sym (intern id vm-thread-obarray)
	    date (vm-so-sortable-datestring m))
      (put id-sym 'messages (cons m (get id-sym 'messages)))
      (put id-sym 'date date)
      (if (and (null (cdr (get id-sym 'messages)))
	       schedule-reindents)
	  (vm-thread-mark-for-summary-update (get id-sym 'children)))
      (if parent
	  (progn
	    (setq parent-sym (intern parent vm-thread-obarray))
	    (cond ((or (not (boundp id-sym))
		       (null (symbol-value id-sym))
		       (eq (symbol-value id-sym) parent-sym))
		   (set id-sym parent-sym))
		  (t
		   (setq old-parent-sym (symbol-value id-sym))
		   (put old-parent-sym 'children
			(let ((kids (get old-parent-sym 'children))
			      (msgs (get id-sym 'messages)))
			  (while msgs
			    (setq kids (delq (car msgs) kids)
				  msgs (cdr msgs)))
			  kids ))
		   (set id-sym parent-sym)
		   (if schedule-reindents
		       (vm-thread-mark-for-summary-update
			(get id-sym 'messages)))))
	    (put parent-sym 'children
		 (cons m (get parent-sym 'children))))
	(if (not (boundp id-sym))
	    (set id-sym nil)))
      ;; use the references header to set parenting information
      ;; for ancestors of this message.  This does not override
      ;; a parent pointer for a message if it already exists.
      (if (cdr (setq refs (vm-th-references m)))
	  (let (parent-sym id-sym msgs)
	    (setq parent-sym (intern (car refs) vm-thread-obarray)
		  refs (cdr refs))
	    (while refs
	      (setq id-sym (intern (car refs) vm-thread-obarray))
	      (if (and (boundp id-sym) (symbol-value id-sym))
		  nil
		(set id-sym parent-sym)
		(if (setq msgs (get id-sym 'messages))
		    (put parent-sym 'children
			 (append msgs (get parent-sym 'children))))
		(if schedule-reindents
		    (vm-thread-mark-for-summary-update msgs)))
	      (setq parent-sym id-sym
		    refs (cdr refs)))))
      (setq mp (cdr mp) n (1+ n))
      (if (zerop (% n modulus))
	  (message "Building threads (by reference)... %d" n)))
    (if vm-thread-using-subject
	(progn
	  (setq n 0 mp (or message-list vm-message-list))
	  (while mp
	    (setq m (car mp)
		  parent (vm-th-parent m)
		  id (vm-su-message-id m)
		  id-sym (intern id vm-thread-obarray)
		  date (vm-so-sortable-datestring m))
	    ;; inhibit-quit because we need to make sure the asets
	    ;; below are an atomic group.
	    (let* ((inhibit-quit t)
		   (subject (vm-so-sortable-subject m))
		   (subject-sym (intern subject vm-thread-subject-obarray)))
	      ;; if this subject was never seen before create the
	      ;; information vector.
	      (if (not (boundp subject-sym))
		  (set subject-sym
		       (vector id-sym date
			       nil (list m)))
		;; this subject seen before 
		(aset (symbol-value subject-sym) 3
		      (cons m (aref (symbol-value subject-sym) 3)))
		(if (string< date (aref (symbol-value subject-sym) 1))
		    (let* ((vect (symbol-value subject-sym))
			   (i-sym (aref vect 0)))
		      ;; optimization: if we know that this message
		      ;; already has a parent, then don't bother
		      ;; adding it to the list of child messages
		      ;; since we know that it will be threaded and
		      ;; unthreaded using the parent information.
		      (if (or (not (boundp i-sym))
			      (null (symbol-value i-sym)))
			  (aset vect 2 (append (get i-sym 'messages)
					       (aref vect 2))))
		      (aset vect 0 id-sym)
		      (aset vect 1 date)
		      ;; this loops _and_ recurses and I'm worried
		      ;; about it going into a spin someday.  So I
		      ;; unblock interrupts here.  It's not critical
		      ;; that it finish... the summary will just be out
		      ;; of sync.
		      (if schedule-reindents
			  (let ((inhibit-quit nil))
			    (vm-thread-mark-for-summary-update (aref vect 2)))))
		  ;; optimization: if we know that this message
		  ;; already has a parent, then don't bother adding
		  ;; it to the list of child messages, since we
		  ;; know that it will be threaded and unthreaded
		  ;; using the parent information.
		  (if (null parent)
		      (aset (symbol-value subject-sym) 2
			    (cons m (aref (symbol-value subject-sym) 2)))))))
	    (setq mp (cdr mp) n (1+ n))
	    (if (zerop (% n modulus))
		(message "Building threads (by subject)... %d" n)))))
    (if (> n modulus)
	(message "Building threads... done"))))

;; used by the thread sort code.
;;
;; vm-th-thread-list initializes the oldest-date property on
;; the message-id symbols.  Since this property is used as an
;; ordering key by the thread sort the oldest-date properties
;; must be computed before the sort begins, not during it.
;; Otherwise the sort won't be stable and there will be chaos.
(defun vm-build-thread-lists ()
  (let ((mp vm-message-list))
    (while mp
      (vm-th-thread-list (car mp))
      (setq mp (cdr mp)))))

(defun vm-thread-mark-for-summary-update (message-list)
  (let (m)
    (while message-list
      (setq m (car message-list))
      ;; if thread-list is null then we've already marked this
      ;; message, or it doesn't need marking.
      (if (null (vm-thread-list-of m))
	  nil
	(vm-mark-for-summary-update m t)
	(vm-set-thread-list-of m nil)
	(vm-set-thread-indentation-of m nil)
	(vm-thread-mark-for-summary-update
	 (get (intern (vm-su-message-id m) vm-thread-obarray)
	      'children)))
      (setq message-list (cdr message-list)))))

(defun vm-thread-list (message)
  (let ((done nil)
	(m message)
	(loop-recovery-point nil)
	(date (vm-so-sortable-datestring message))
	thread-list id-sym subject-sym loop-sym root-date youngest-date)
    (save-excursion
      (set-buffer (vm-buffer-of m))
      (fillarray vm-thread-loop-obarray 0)
      (setq id-sym (intern (vm-su-message-id m) vm-thread-obarray)
	    thread-list (list id-sym))
      (set (intern (symbol-name id-sym) vm-thread-loop-obarray) t)
      (while (not done)
	;; save the date of the oldest message in this thread
	(setq root-date (get id-sym 'oldest-date))
	(if (or (null root-date)
		(string< date root-date))
	    (put id-sym 'oldest-date date))
	;; save the date of the youngest message in this thread
	(setq youngest-date (get id-sym 'youngest-date))
	(if (or (null root-date)
		(string< youngest-date date))
	    (put id-sym 'youngest-date date))
	(if (and (boundp id-sym) (symbol-value id-sym))
	    (progn
	      (setq id-sym (symbol-value id-sym)
		    loop-sym (intern (symbol-name id-sym)
				     vm-thread-loop-obarray))
	      (if (boundp loop-sym)
		  ;; loop detected, bail...
		  (setq done t
			thread-list (or loop-recovery-point thread-list))
		(set loop-sym t)
		(setq thread-list (cons id-sym thread-list)
		      m (car (get id-sym 'messages)))))
	  (if (null m)
	      (setq done t)
	    (if (null vm-thread-using-subject)
		(setq done t)
	      (setq subject-sym
		    (intern (vm-so-sortable-subject m)
			    vm-thread-subject-obarray))
	      (if (or (not (boundp subject-sym))
		      (eq (aref (symbol-value subject-sym) 0) id-sym))
		  (setq done t)
		(setq id-sym (aref (symbol-value subject-sym) 0)
;; seems to cause more trouble than it fixes
;; revisit this later.
;;		      loop-recovery-point (or loop-recovery-point
;;					      thread-list)
		      loop-sym (intern (symbol-name id-sym)
				       vm-thread-loop-obarray))
		(if (boundp loop-sym)
		    ;; loop detected, bail...
		    (setq done t
			  thread-list (or loop-recovery-point thread-list))
		  (set loop-sym t)
		  (setq thread-list (cons id-sym thread-list)
			m (car (get id-sym 'messages)))))))))
      thread-list )))

;; remove message struct from thread data.
;;
;; optional second arg non-nil means forget information that
;; might be different if the message contents changed.
;;
;; message must be a real (non-virtual) message
(defun vm-unthread-message (message &optional message-changing)
  (save-excursion
    (let ((mp (cons message (vm-virtual-messages-of message)))
	  m id-sym subject-sym vect p-sym)
      (while mp
	(setq m (car mp))
	(set-buffer (vm-buffer-of m))
	(if (not (vectorp vm-thread-obarray))
	    nil
	  (let ((inhibit-quit t))
	    (vm-set-thread-list-of m nil)
	    (vm-set-thread-indentation-of m nil)
	    (setq id-sym (intern (vm-su-message-id m) vm-thread-obarray)
		  subject-sym (intern (vm-so-sortable-subject m)
				      vm-thread-subject-obarray))
	    (if (boundp id-sym)
		(progn
		  (put id-sym 'messages (delq m (get id-sym 'messages)))
		  (vm-thread-mark-for-summary-update (get id-sym 'children))
		  (setq p-sym (symbol-value id-sym))
		  (and p-sym (put p-sym 'children
				  (delq m (get p-sym 'children))))
		  (if message-changing
		      (set id-sym nil))))
	    (if (and (boundp subject-sym) (setq vect (symbol-value subject-sym)))
		(if (not (eq id-sym (aref vect 0)))
		    (aset vect 2 (delq m (aref vect 2)))
		  (if message-changing
		      (if (null (cdr (aref vect 3)))
			  (makunbound subject-sym)
			(let ((p (aref vect 3))
			      oldest-msg oldest-date children)
			  (setq oldest-msg (car p)
				oldest-date (vm-so-sortable-datestring (car p))
				p (cdr p))
			  (while p
			    (if (and (string-lessp (vm-so-sortable-datestring (car p))
						   oldest-date)
				     (not (eq m (car p))))
				(setq oldest-msg (car p)
				      oldest-date (vm-so-sortable-datestring (car p))))
			    (setq p (cdr p)))
			  (aset vect 0 (intern (vm-su-message-id oldest-msg)
					       vm-thread-obarray))
			  (aset vect 1 oldest-date)
			  (setq children (delq oldest-msg (aref vect 2)))
			  (aset vect 2 children)
			  (aset vect 3 (delq m (aref vect 3)))
			  ;; I'm not sure there aren't situations
			  ;; where this might loop forever.
			  (let ((inhibit-quit nil))
			    (vm-thread-mark-for-summary-update children)))))))))
	  (setq mp (cdr mp))))))

(defun vm-th-references (m)
  (or (vm-references-of m)
      (vm-set-references-of
       m
       (let (references)
	 (setq references (vm-get-header-contents m "References:" " "))
	 (and references (vm-parse references "[^<]*\\(<[^>]+>\\)"))))))

(defun vm-th-parent (m)
  (or (vm-parent-of m)
      (vm-set-parent-of
       m
       (or (car (vm-last (vm-th-references m)))
	   (let (in-reply-to ids id)
	     (setq in-reply-to (vm-get-header-contents m "In-Reply-To:" " ")
		   ids (and in-reply-to (vm-parse in-reply-to
						  "[^<]*\\(<[^>]+>\\)")))
	     (while ids
	       (if (< (length id) (length (car ids)))
		   (setq id (car ids)))
	       (setq ids (cdr ids)))
	     (and id (vm-set-references-of m (list id)))
	     id )))))

(defun vm-th-thread-indentation (m)
  (or (vm-thread-indentation-of m)
      (let ((p (vm-th-thread-list m)))
	(while (and p (null (get (car p) 'messages)))
	  (setq p (cdr p)))
	(vm-set-thread-indentation-of m (1- (length p)))
	(vm-thread-indentation-of m))))

(defun vm-th-thread-list (m)
  (or (vm-thread-list-of m)
      (progn
	(vm-set-thread-list-of m (vm-thread-list m))
	(vm-thread-list-of m))))

(provide 'vm-thread)

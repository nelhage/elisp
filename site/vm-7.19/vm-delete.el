;;; Delete and expunge commands for VM.
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

;;(provide 'vm-delete)

(defun vm-delete-message (count)
  "Add the `deleted' attribute to the current message.

The message will be physically deleted from the current folder the next
time the current folder is expunged.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are deleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are deleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count))
	(del-count 0))
    (while mlist
      (if (not (vm-deleted-flag (car mlist)))
	  (progn
	    (vm-set-deleted-flag (car mlist) t)
	    (vm-increment del-count)))
      (setq mlist (cdr mlist)))
    (vm-display nil nil '(vm-delete-message vm-delete-message-backward)
		(list this-command))
    (if (and used-marks (interactive-p))
	(if (zerop del-count)
	    (message "No messages deleted")
	  (message "%d message%s deleted"
		   del-count
		   (if (= 1 del-count) "" "s"))))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-deleting (not used-marks))
	(let ((vm-circular-folders (and vm-circular-folders
					(eq vm-move-after-deleting t))))
	  (vm-next-message count t executing-kbd-macro)))))

(defun vm-delete-message-backward (count)
  "Like vm-delete-message, except the deletion direction is reversed."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-delete-message (- count)))

(defun vm-undelete-message (count)
  "Remove the `deleted' attribute from the current message.

With a prefix argument COUNT, the current message and the next
COUNT - 1 messages are undeleted.  A negative argument means
the current message and the previous |COUNT| - 1 messages are
deleted.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only marked messages are undeleted, other messages are ignored."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist (vm-select-marked-or-prefixed-messages count))
	(undel-count 0))
    (while mlist
      (if (vm-deleted-flag (car mlist))
	  (progn
	    (vm-set-deleted-flag (car mlist) nil)
	    (vm-increment undel-count)))
      (setq mlist (cdr mlist)))
    (if (and used-marks (interactive-p))
	(if (zerop undel-count)
	    (message "No messages undeleted")
	  (message "%d message%s undeleted"
		   undel-count
		   (if (= 1 undel-count)
		       "" "s"))))
    (vm-display nil nil '(vm-undelete-message) '(vm-undelete-message))
    (vm-update-summary-and-mode-line)
    (if (and vm-move-after-undeleting (not used-marks))
	(let ((vm-circular-folders (and vm-circular-folders
					(eq vm-move-after-undeleting t))))
	  (vm-next-message count t executing-kbd-macro)))))

(defun vm-kill-subject (&optional arg)
  "Delete all messages with the same subject as the current message.
Message subjects are compared after ignoring parts matched by
the variables vm-subject-ignored-prefix and vm-subject-ignored-suffix.

The optional prefix argument ARG specifies the direction to move
if vm-move-after-killing is non-nil.  The default direction is
forward.  A positive prefix argument means move forward, a
negative arugment means move backward, a zero argument means
don't move at all."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((subject (vm-so-sortable-subject (car vm-message-pointer)))
	(mp vm-message-list)
	(n 0)
	(case-fold-search t))
    (while mp
      (if (and (not (vm-deleted-flag (car mp)))
	       (string-equal subject (vm-so-sortable-subject (car mp))))
	  (progn
	    (vm-set-deleted-flag (car mp) t)
	    (vm-increment n)))
      (setq mp (cdr mp)))
    (and (interactive-p)
	 (if (zerop n)
	     (message "No messages deleted.")
	   (message "%d message%s deleted" n (if (= n 1) "" "s")))))
  (vm-display nil nil '(vm-kill-subject) '(vm-kill-subject))
  (vm-update-summary-and-mode-line)
  (cond ((or (not (numberp arg)) (> arg 0))
	 (setq arg 1))
	((< arg 0)
	 (setq arg -1))
	(t (setq arg 0)))
  (if vm-move-after-killing
      (let ((vm-circular-folders (and vm-circular-folders
				      (eq vm-move-after-killing t))))
	(vm-next-message arg t executing-kbd-macro))))

(defun vm-delete-duplicate-messages ()
  "Delete duplicate messages in the current folder.
This command works by computing an MD5 hash for the body ofeach
non-deleted message in the folder and deleting messages that have
a hash that has already been seen.  Messages that already deleted
are never hashed, so VM will never delete the last copy of a
message in a folder.  'Deleting' means flagging for deletion; you
will have to expunge the messages with `vm-expunge-folder' to
really get rid of them, as usual.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only duplicate messages among the marked messages are deleted,
unmarked messages are not hashed or considerd for deletion."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((used-marks (eq last-command 'vm-next-command-uses-marks))
	(mlist vm-message-list)
	(table (make-vector 61 0))
	hash m
	(del-count 0))
    (if used-marks
	(setq mlist (vm-select-marked-or-prefixed-messages 0)))
    (save-excursion
      (save-restriction
	(widen)
	(while mlist
	  (if (vm-deleted-flag (car mlist))
	      nil
	    (setq m (vm-real-message-of (car mlist)))
	    (set-buffer (vm-buffer-of m))
	    (setq hash (vm-md5-region (vm-text-of m) (vm-text-end-of m)))
	    (if (intern-soft hash table)
		(progn
		  (vm-set-deleted-flag (car mlist) t)
		  (vm-increment del-count))
	      (intern hash table)))
	  (setq mlist (cdr mlist)))))
    (vm-display nil nil '(vm-delete-duplicate-messages)
		(list this-command))
    (if (zerop del-count)
	(message "No messages deleted")
      (message "%d message%s deleted"
	       del-count
	       (if (= 1 del-count) "" "s")))
    (vm-update-summary-and-mode-line)))

(defun vm-expunge-folder (&optional shaddap just-these-messages
				    messages-to-expunge)
  "Expunge messages with the `deleted' attribute.
For normal folders this means that the deleted messages are
removed from the message list and the message contents are
removed from the folder buffer.

For virtual folders, messages are removed from the virtual
message list.  If virtual mirroring is in effect for the virtual
folder, the corresponding real messages are also removed from real
message lists and the message contents are removed from real folders.

When invoked on marked messages (via `vm-next-command-uses-marks'),
only messages both marked and deleted are expunged, other messages are
ignored."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  ;; do this so we have a clean slate.  code below depends on the
  ;; fact that the numbering redo start point begins as nil in
  ;; all folder buffers.
  (vm-update-summary-and-mode-line)
  (if (not shaddap)
      (message "Expunging..."))
  (let ((use-marks (and (eq last-command 'vm-next-command-uses-marks)
			(null just-these-messages)))
	(mp vm-message-list)
	(virtual (eq major-mode 'vm-virtual-mode))
	(buffers-altered (make-vector 29 0))
	prev virtual-messages)
    (while mp
      (cond
       ((if just-these-messages
	    (memq (car mp) messages-to-expunge)
	  (and (vm-deleted-flag (car mp))
	       (or (not use-marks)
		   (vm-mark-of (car mp)))))
	;; remove the message from the thread tree.
	(if (vectorp vm-thread-obarray)
	    (vm-unthread-message (vm-real-message-of (car mp))))
	;; expunge from the virtual side first, removing all
	;; references to this message before actually removing
	;; the message itself.
	(cond
	 ((setq virtual-messages (vm-virtual-messages-of (car mp)))
	  (let (vms prev curr)
	    (if virtual
		(setq vms (cons (vm-real-message-of (car mp))
				(vm-virtual-messages-of (car mp))))
	      (setq vms (vm-virtual-messages-of (car mp))))
	    (while vms
	      (save-excursion
		(set-buffer (vm-buffer-of (car vms)))
		(setq prev (vm-reverse-link-of (car vms))
		      curr (or (cdr prev) vm-message-list))
		(intern (buffer-name) buffers-altered)
		(vm-set-numbering-redo-start-point (or prev t))
		(vm-set-summary-redo-start-point (or prev t))
		(if (eq vm-message-pointer curr)
		    (setq vm-system-state nil
			  vm-message-pointer (or prev (cdr curr))))
		(if (eq vm-last-message-pointer curr)
		    (setq vm-last-message-pointer nil))
		;; lock out interrupts to preserve message-list integrity
		(let ((inhibit-quit t))
		  ;; vm-clear-expunge-invalidated-undos uses
		  ;; this to recognize expunged messages.
		  ;; If this stuff is mirrored we'll be
		  ;; setting this value multiple times if there
		  ;; are multiple virtual messages referencing
		  ;; the underlying real message.  Harmless.
		  (vm-set-deleted-flag-of (car curr) 'expunged)
		  ;; disable any summary update that may have
		  ;; already been scheduled.
		  (vm-set-su-start-of (car curr) nil)
		  (vm-increment vm-modification-counter)
		  (if (null prev)
		      (progn
			(setq vm-message-list (cdr vm-message-list))
			(and (cdr curr)
			     (vm-set-reverse-link-of (car (cdr curr)) nil)))
		    (setcdr prev (cdr curr))
		    (and (cdr curr)
			 (vm-set-reverse-link-of (car (cdr curr)) prev)))
		  (vm-set-virtual-messages-of (car mp) (cdr vms))
		  (vm-set-buffer-modified-p t)))
	      (setq vms (cdr vms))))))
	(cond
	 ((or (not virtual-messages)
	      (not virtual))
	  (and (not virtual-messages) virtual
	       (vm-set-virtual-messages-of
		(vm-real-message-of (car mp))
		(delq (car mp) (vm-virtual-messages-of
				(vm-real-message-of (car mp))))))
	  (if (eq vm-message-pointer mp)
	      (setq vm-system-state nil
		    vm-message-pointer (or prev (cdr mp))))
	  (if (eq vm-last-message-pointer mp)
	      (setq vm-last-message-pointer nil))
	  (intern (buffer-name) buffers-altered)
	  (if (null vm-numbering-redo-start-point)
	      (progn 
		(vm-set-numbering-redo-start-point (or prev t))
		(vm-set-summary-redo-start-point (or prev t))))
	  ;; lock out interrupt to preserve message list integrity
	  (let ((inhibit-quit t))
	    (if (null prev)
		(progn (setq vm-message-list (cdr vm-message-list))
		       (and (cdr mp)
			    (vm-set-reverse-link-of (car (cdr mp)) nil)))
	      (setcdr prev (cdr mp))
	      (and (cdr mp) (vm-set-reverse-link-of (car (cdr mp)) prev)))
	    ;; vm-clear-expunge-invalidated-undos uses this to recognize
	    ;; expunged messages.
	    (vm-set-deleted-flag-of (car mp) 'expunged)
	    ;; disable any summary update that may have
	    ;; already been scheduled.
	    (vm-set-su-start-of (car mp) nil)
	    (vm-set-buffer-modified-p t)
	    (vm-increment vm-modification-counter))))
	(if (eq (vm-attributes-of (car mp))
		(vm-attributes-of (vm-real-message-of (car mp))))
	    (save-excursion
	      (set-buffer (vm-buffer-of (vm-real-message-of (car mp))))
	      (cond ((eq vm-folder-access-method 'pop)
		     (setq vm-pop-messages-to-expunge
			   (cons (vm-pop-uidl-of (vm-real-message-of (car mp)))
				 vm-pop-messages-to-expunge)
			   ;; Set this so that if Emacs crashes or
			   ;; the user quits without saving, we
			   ;; have a record of messages that were
			   ;; retrieved and expunged locally.
			   ;; When the user does M-x recover-file
			   ;; we won't re-retrieve messages the
			   ;; user has already dealt with.
			   vm-pop-retrieved-messages
			   (cons (list (vm-pop-uidl-of
					(vm-real-message-of (car mp)))
				       (vm-folder-pop-maildrop-spec)
				       'uidl)
				 vm-pop-retrieved-messages)))
		    ((eq vm-folder-access-method 'imap)
		     (setq vm-imap-messages-to-expunge
			   (cons (cons
				  (vm-imap-uid-of (vm-real-message-of (car mp)))
				  (vm-imap-uid-validity-of
				   (vm-real-message-of (car mp))))
				 vm-imap-messages-to-expunge)
			   ;; Set this so that if Emacs crashes or
			   ;; the user quits without saving, we
			   ;; have a record of messages that were
			   ;; retrieved and expunged locally.
			   ;; When the user does M-x recover-file
			   ;; we won't re-retrieve messages the
			   ;; user has already dealt with.
			   vm-imap-retrieved-messages
			   (cons (list (vm-imap-uid-of
					(vm-real-message-of (car mp)))
				       (vm-imap-uid-validity-of
					(vm-real-message-of (car mp)))
				       (vm-folder-imap-maildrop-spec)
				       'uid)
				 vm-imap-retrieved-messages))))
	      (vm-increment vm-modification-counter)
	      (vm-save-restriction
	       (widen)
	       (let ((buffer-read-only nil))
		 (delete-region (vm-start-of (vm-real-message-of (car mp)))
				(vm-end-of (vm-real-message-of (car mp)))))))))
       (t (setq prev mp)))
      (setq mp (cdr mp)))
    (vm-display nil nil '(vm-expunge-folder) '(vm-expunge-folder))
    (cond
     (buffers-altered
      (save-excursion
	(mapatoms
	 (function
	  (lambda (buffer)
	    (set-buffer (symbol-name buffer))
	    (if (null vm-system-state)
		(progn
		  (vm-garbage-collect-message)
		  (if (null vm-message-pointer)
		      ;; folder is now empty
		      (progn (setq vm-folder-type nil)
			     (vm-update-summary-and-mode-line))
		    (vm-preview-current-message)))
	      (vm-update-summary-and-mode-line))
	    (if (not (eq major-mode 'vm-virtual-mode))
		(setq vm-message-order-changed
		      (or vm-message-order-changed
			  vm-message-order-header-present)))
	    (vm-clear-expunge-invalidated-undos)))
	 buffers-altered))
      (if (not shaddap)
	  (message "Deleted messages expunged.")))
     (t (message "No messages are flagged for deletion.")))))

(provide 'vm-delete)

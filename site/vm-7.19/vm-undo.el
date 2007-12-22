;;; Commands to undo message attribute changes in VM
;;; Copyright (C) 1989-1995 Kyle E. Jones
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

;;(provide 'vm-undo)

(defun vm-set-buffer-modified-p (flag &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (set-buffer-modified-p flag)
    (vm-increment vm-modification-counter)
    (intern (buffer-name) vm-buffers-needing-display-update)
    (if (null flag)
	(setq vm-messages-not-on-disk 0))))

(defun vm-undo-boundary ()
  (if (car vm-undo-record-list)
      (setq vm-undo-record-list (cons nil vm-undo-record-list))))

(defun vm-add-undo-boundaries ()
  (save-excursion
    (mapatoms (function
	       (lambda (b)
		 (setq b (get-buffer (symbol-name b)))
		 (if b
		     (progn
		       (set-buffer b)
		       (vm-undo-boundary)))))
	      vm-buffers-needing-undo-boundaries)
    (fillarray vm-buffers-needing-undo-boundaries 0)))

(defun vm-clear-expunge-invalidated-undos ()
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((null (car udp))
	     (setq udp-prev udp))
	    ((and (not (eq (car (car udp)) 'vm-set-buffer-modified-p))
		  ;; delete flag == expunged is the
		  ;; indicator of an expunged message
		  (eq (vm-deleted-flag (car (cdr (car udp)))) 'expunged))
	     (cond (udp-prev (setcdr udp-prev (cdr udp)))
		   (t (setq vm-undo-record-list (cdr udp)))))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp))))
  (vm-clear-modification-flag-undos))
	    
(defun vm-clear-virtual-quit-invalidated-undos ()
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((null (car udp))
	     (setq udp-prev udp))
	    ((and (not (eq (car (car udp)) 'vm-set-buffer-modified-p))
		  ;; message-id-number == "Q" is the
		  ;; indicator of a dead message
		  (equal (vm-message-id-number-of (car (cdr (car udp)))) "Q"))
	     (cond (udp-prev (setcdr udp-prev (cdr udp)))
		   (t (setq vm-undo-record-list (cdr udp)))))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp))))
  (vm-clear-modification-flag-undos))
	    
(defun vm-clear-modification-flag-undos ()
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((null (car udp))
	     (setq udp-prev udp))
	    ((eq (car (car udp)) 'vm-set-buffer-modified-p)
	     (cond (udp-prev (setcdr udp-prev (cdr udp)))
		   (t (setq vm-undo-record-list (cdr udp)))))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp)))
    (vm-squeeze-consecutive-undo-boundaries)))

;; squeeze out consecutive record separators left by record deletions
(defun vm-squeeze-consecutive-undo-boundaries ()
  (let ((udp vm-undo-record-list) udp-prev)
    (while udp
      (cond ((and (null (car udp)) udp-prev (null (car udp-prev)))
	     (setcdr udp-prev (cdr udp)))
	    (t (setq udp-prev udp)))
      (setq udp (cdr udp)))
    (if (equal '(nil) vm-undo-record-list)
	(setq vm-undo-record-list nil)))
  ;; for the Undo button on the menubar, if present
  (and (null vm-undo-record-list)
       (vm-menu-support-possible-p)
       (vm-menu-xemacs-menus-p)
       (vm-menu-set-menubar-dirty-flag)))
	    
(defun vm-undo-record (sexp)
  ;; for the Undo button on the menubar, if present
  (and (null vm-undo-record-list)
       (vm-menu-support-possible-p)
       (vm-menu-xemacs-menus-p)
       (vm-menu-set-menubar-dirty-flag))
  (setq vm-undo-record-list (cons sexp vm-undo-record-list)))

(defun vm-undo-describe (record)
  (let ((cell
	 (assq (car record)
	       '((vm-set-new-flag "new" "old")
		 (vm-set-unread-flag "unread" "read")
		 (vm-set-deleted-flag "deleted" "undeleted")
		 (vm-set-forwarded-flag "forwarded" "unforwarded")
		 (vm-set-replied-flag "answered" "unanswered")
		 (vm-set-redistributed-flag "redistributed" "unredistributed")
		 (vm-set-filed-flag "filed" "unfiled")
		 (vm-set-written-flag "written" "unwritten"))))
	(m (nth 1 record))
	labels)
    (cond (cell
	   (message "VM Undo! %s/%s %s -> %s"
		    (buffer-name (vm-buffer-of m))
		    (vm-number-of m)
		    (if (nth 2 record)
			(nth 2 cell)
		      (nth 1 cell))
		    (if (nth 2 record)
			(nth 1 cell)
		      (nth 2 cell))))
	  ((eq (car cell) 'vm-set-labels)
	   (setq labels (nth 2 record))
	   (message "VM Undo! %s/%s %s%s"
		    (buffer-name (vm-buffer-of m))
		    (vm-number-of m)
		    (if (null labels)
			"lost all its labels"
		      "labels set to ")
		    (if (null labels)
			""
		      (mapconcat 'identity labels ", ")))))))

(defun vm-undo-set-message-pointer (record)
  (if (and (not (eq (car record) 'vm-set-buffer-modified-p))
	   (not (eq (nth 1 record) vm-message-pointer)))
      (progn
	(vm-record-and-change-message-pointer
	 vm-message-pointer
	 (or (cdr (vm-reverse-link-of (nth 1 record)))
	     vm-message-list))
	;; make folder read-only to avoid modifications when we
	;; do this.
	(let ((vm-folder-read-only t))
	  (vm-preview-current-message)))))

(defun vm-undo ()
  "Undo last change to message attributes in the current folder.
Consecutive invocations of this command cause sequentially earlier
changes to be undone.  After an intervening command between undos,
the undos themselves become undoable."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-display nil nil '(vm-undo) '(vm-undo))
  (let ((modified (buffer-modified-p)))
    (if (not (eq last-command 'vm-undo))
	(setq vm-undo-record-pointer vm-undo-record-list))
    (if (not vm-undo-record-pointer)
	(error "No further VM undo information available"))
    ;; skip current record boundary
    (setq vm-undo-record-pointer (cdr vm-undo-record-pointer))
    (while (car vm-undo-record-pointer)
      (vm-undo-set-message-pointer (car vm-undo-record-pointer))
      (vm-undo-describe (car vm-undo-record-pointer))
      (eval (car vm-undo-record-pointer))
      (setq vm-undo-record-pointer (cdr vm-undo-record-pointer)))
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary))
    (vm-update-summary-and-mode-line)))

(defun vm-set-message-attributes (string count)
  "Set message attributes.
Use this command to change attributes like `deleted' or
`replied'.  Interactively you will be prompted for the attributes
to be changed, and only the attributes you enter will be altered.
You can use completion to expand the attribute names.  The names
should be entered as a space separated list.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have their attributes altered.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     ;; so the user can see what message they are about to
     ;; modify.
     (vm-follow-summary-cursor)
     (list
      (vm-read-string "Set attributes: " vm-supported-attribute-names t)
      (prefix-numeric-value current-prefix-arg))))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (vm-display nil nil '(vm-set-message-attributes)
	      '(vm-set-message-attributes))
  (let ((name-list (vm-parse string "[ \t]*\\([^ \t]+\\)"))
	(m-list (vm-select-marked-or-prefixed-messages count))
	n-list name m)
    (while m-list
      (setq m (car m-list)
	    n-list name-list)
      (while n-list
	(setq name (car n-list))
	(cond ((string= name "new")
	       (vm-set-new-flag m t))
	      ((string= name "recent")
	       (vm-set-new-flag m t))
	      ((string= name "unread")
	       (vm-set-unread-flag m t))
	      ((string= name "unseen")
	       (vm-set-unread-flag m t))
	      ((string= name "read")
	       (vm-set-new-flag m nil)
	       (vm-set-unread-flag m nil))
	      ((string= name "deleted")
	       (vm-set-deleted-flag m t))
	      ((string= name "replied")
	       (vm-set-replied-flag m t))
	      ((string= name "answered")
	       (vm-set-replied-flag m t))
	      ((string= name "forwarded")
	       (vm-set-forwarded-flag m t))
	      ((string= name "redistributed")
	       (vm-set-redistributed-flag m t))
	      ((string= name "filed")
	       (vm-set-filed-flag m t))
	      ((string= name "written")
	       (vm-set-written-flag m t))
	      ((string= name "edited")
	       (vm-set-edited-flag-of m t))
	      ((string= name "undeleted")
	       (vm-set-deleted-flag m nil))
	      ((string= name "unreplied")
	       (vm-set-replied-flag m nil))
	      ((string= name "unanswered")
	       (vm-set-replied-flag m nil))
	      ((string= name "unforwarded")
	       (vm-set-forwarded-flag m nil))
	      ((string= name "unredistributed")
	       (vm-set-redistributed-flag m nil))
	      ((string= name "unfiled")
	       (vm-set-filed-flag m nil))
	      ((string= name "unwritten")
	       (vm-set-written-flag m nil))
	      ((string= name "unedited")
	       (vm-set-edited-flag-of m nil)))
	(setq n-list (cdr n-list)))
      (setq m-list (cdr m-list)))
    (vm-update-summary-and-mode-line)))

(defun vm-add-message-labels (string count)
  "Attach some labels to a message.
These are arbitrary user-defined labels, not to be confused with
message attributes like `new' and `deleted'.  Interactively you
will be prompted for the labels to be added.  You can use
completion to expand the label names, with the completion list
being all the labels that have ever been used in this folder.
The names should be entered as a space separated list.  Label
names are compared case-insensitively.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have the labels added.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (vm-completion-auto-correct nil)
	 (completion-ignore-case t))
     ;; so the user can see what message they are about to
     ;; modify.
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list
      (vm-read-string "Add labels: "
		      (vm-obarray-to-string-list vm-label-obarray) t)
      (prefix-numeric-value current-prefix-arg))))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (vm-add-or-delete-message-labels string count 'all))

(defun vm-add-existing-message-labels (string count)
  "Attach some already existing labels to a message.
Only labels that are currently attached to some message in this
folder or labels that have previously been attached to messages
in this folder will be added.  Other labels will be silently
ignored.

These are arbitrary user-defined labels, not to be confused with
message attributes like `new' and `deleted'.  Interactively you
will be prompted for the labels to be added.  You can use
completion to expand the label names, with the completion list
being all the labels that have ever been used in this folder.
The names should be entered as a space separated list.  Label
names are compared case-insensitively.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 messages to have the labels added.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (vm-completion-auto-correct nil)
	 (completion-ignore-case t))
     ;; so the user can see what message they are about to
     ;; modify.
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list
      (vm-read-string "Add labels: "
		      (vm-obarray-to-string-list vm-label-obarray) t)
      (prefix-numeric-value current-prefix-arg))))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((ignored-labels
	 (vm-add-or-delete-message-labels string count 'existing-only)))
    (if ignored-labels
	(progn
	  (set-buffer (get-buffer-create "*Ignored Labels*"))
	  (erase-buffer)
	  (insert "These labels do not exist and were not added:\n\n")
	  (while ignored-labels
	    (insert (car ignored-labels) "\n")
	    (setq ignored-labels (cdr ignored-labels)))
	  (display-buffer (current-buffer))))))

(defun vm-delete-message-labels (string count)
  "Delete some labels from a message.
These are arbitrary user-defined labels, not to be confused with
message attributes like `new' and `deleted'.  Interactively you
will be prompted for the labels to be deleted.  You can use
completion to expand the label names, with the completion list
being all the labels that have ever been used in this folder.
The names should be entered as a space separated list.  Label
names are compared case-insensitively.

A numeric prefix argument COUNT causes the current message and
the next COUNT-1 message to have the labels deleted.  A
negative COUNT arg causes the current message and the previous
COUNT-1 messages to be altered.  COUNT defaults to one."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command)
	 (vm-completion-auto-correct nil)
	 (completion-ignore-case t))
     ;; so the user can see what message they are about to
     ;; modify.
     (vm-follow-summary-cursor)
     (vm-select-folder-buffer)
     (list
      (vm-read-string "Delete labels: "
		      (vm-obarray-to-string-list vm-label-obarray) t)
      (prefix-numeric-value current-prefix-arg))))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (vm-add-or-delete-message-labels string count nil))

(defun vm-add-or-delete-message-labels (string count add)
  (vm-display nil nil '(vm-add-message-labels vm-delete-message-labels)
	      (list this-command))
  (setq string (downcase string))
  (let ((m-list (vm-select-marked-or-prefixed-messages count))
	(action-labels (vm-parse string
"[\000-\040,\177-\377]*\\([^\000-\040,\177-\377]+\\)[\000-\040,\177-\377]*"))
	(ignored-labels nil)
	labels act-labels m mm-list)
    (if (and add m-list)
	(if (eq add 'all)
	    (progn
	      (setq act-labels action-labels)
	      (while act-labels
		(intern (car act-labels) vm-label-obarray)
		(setq act-labels (cdr act-labels))))
	  (let ((newlist nil))
	    (setq act-labels action-labels)
	    (while act-labels
	      (if (intern-soft (car act-labels) vm-label-obarray)
		  (setq newlist (cons (car act-labels) newlist))
		(setq ignored-labels (cons (car act-labels) ignored-labels)))
	      (setq act-labels (cdr act-labels)))
	    (setq action-labels newlist))))
    (if (null action-labels)
	(setq m-list nil))
    (while m-list
      (setq m (car m-list))
      (if (and add (vm-virtual-message-p m))
	  (let ((labels action-labels))
	    (save-excursion
	      (set-buffer (vm-buffer-of (vm-real-message-of m)))
	      (while labels
		(intern (car labels) vm-label-obarray)
		(setq labels (cdr labels))))))
      (if add
	  (save-excursion
	    (setq mm-list (vm-virtual-messages-of m))
	    (while mm-list
	      (let ((labels action-labels))
		(set-buffer (vm-buffer-of (car mm-list)))
		(while labels
		  (intern (car labels) vm-label-obarray)
		  (setq labels (cdr labels))))
	      (setq mm-list (cdr mm-list)))))
      (setq act-labels action-labels
	    labels (copy-sequence (vm-labels-of (car m-list))))
      (if add
	  (while act-labels
	    (setq labels (cons (car act-labels) labels)
		  act-labels (cdr act-labels)))
	(while act-labels
	  (setq labels (vm-delqual (car act-labels) labels)
		act-labels (cdr act-labels))))
      (if add
	  (setq labels (vm-delete-duplicates labels)))
      (vm-set-labels (car m-list) labels)
      (setq m-list (cdr m-list)))
    (vm-update-summary-and-mode-line)
    ignored-labels))

(defun vm-set-xxxx-flag (m flag norecord function attr-index)
  (let ((m-list nil) vmp)
    (cond
     ((and (not vm-folder-read-only)
	   (or (not (vm-virtual-messages-of m))
	       (not (save-excursion
		      (set-buffer
		       (vm-buffer-of
			 (vm-real-message-of m)))
		      vm-folder-read-only))))
      (cond
       ((not norecord)
	(setq vmp (cons (vm-real-message-of m) (vm-virtual-messages-of m)))
	(while vmp
	  (if (eq (vm-attributes-of m) (vm-attributes-of (car vmp)))
	      (setq m-list (cons (car vmp) m-list)))
	  (setq vmp (cdr vmp)))
	(if (null m-list)
	    (setq m-list (cons m m-list)))
	(while m-list
	  (save-excursion
	    (set-buffer (vm-buffer-of (car m-list)))
	    (cond ((not (buffer-modified-p))
		   (vm-set-buffer-modified-p t)
		   (vm-undo-record (list 'vm-set-buffer-modified-p nil))))
	    (vm-undo-record (list function (car m-list) (not flag)))
;;;	    (vm-undo-boundary)
	    (vm-increment vm-modification-counter))
	  (setq m-list (cdr m-list)))))
      (aset (vm-attributes-of m) attr-index flag)
      (vm-mark-for-summary-update m)
      (if (not norecord)
	  (progn
	    (vm-set-attribute-modflag-of m t)
	    (if (eq vm-flush-interval t)
		(vm-stuff-virtual-attributes m)
	      (vm-set-stuff-flag-of m t))))))))


(defun vm-set-labels (m labels)
  (let ((m-list nil)
	(old-labels (vm-labels-of m))
	vmp)
    (cond
     ((and (not vm-folder-read-only)
	   (or (not (vm-virtual-messages-of m))
	       (not (save-excursion
		      (set-buffer
		       (vm-buffer-of
			 (vm-real-message-of m)))
		      vm-folder-read-only))))
      (setq vmp (cons (vm-real-message-of m) (vm-virtual-messages-of m)))
      (while vmp
	(if (eq (vm-attributes-of m) (vm-attributes-of (car vmp)))
	    (setq m-list (cons (car vmp) m-list)))
	(setq vmp (cdr vmp)))
      (if (null m-list)
	  (setq m-list (cons m m-list)))
      (while m-list
	(save-excursion
	  (set-buffer (vm-buffer-of (car m-list)))
	  (cond ((not (buffer-modified-p))
		 (vm-set-buffer-modified-p t)
		 (vm-undo-record (list 'vm-set-buffer-modified-p nil))))
	  (vm-undo-record (list 'vm-set-labels m old-labels))
;;;	  (vm-undo-boundary)
	  (vm-increment vm-modification-counter))
	(setq m-list (cdr m-list)))
      (vm-set-labels-of m labels)
      (vm-set-label-string-of m nil)
      (vm-mark-for-summary-update m)
      (if (eq vm-flush-interval t)
	  (vm-stuff-virtual-attributes m)
	(vm-set-stuff-flag-of m t))))))


(defun vm-set-new-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-new-flag 0))

(defun vm-set-unread-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-unread-flag 1))

(defun vm-set-deleted-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-deleted-flag 2))

(defun vm-set-filed-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-filed-flag 3))

(defun vm-set-replied-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-replied-flag 4))

(defun vm-set-written-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-written-flag 5))

(defun vm-set-forwarded-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-forwarded-flag 6))

(defun vm-set-redistributed-flag (m flag &optional norecord)
  (vm-set-xxxx-flag m flag norecord 'vm-set-redistributed-flag 8))

;; use these to avoid undo and summary update.
(defun vm-set-new-flag-of (m flag) (aset (aref m 2) 0 flag))
(defun vm-set-unread-flag-of (m flag) (aset (aref m 2) 1 flag))
(defun vm-set-deleted-flag-of (m flag) (aset (aref m 2) 2 flag))
(defun vm-set-filed-flag-of (m flag) (aset (aref m 2) 3 flag))
(defun vm-set-replied-flag-of (m flag) (aset (aref m 2) 4 flag))
(defun vm-set-written-flag-of (m flag) (aset (aref m 2) 5 flag))
(defun vm-set-forwarded-flag-of (m flag) (aset (aref m 2) 6 flag))
(defun vm-set-redistributed-flag-of (m flag) (aset (aref m 2) 8 flag))

;; this is solely for the use of vm-stuff-attributes and appears here
;; only because this function should be grouped with others of its kind
;; for maintenance purposes.
(defun vm-set-deleted-flag-in-vector (v flag)
  (aset v 2 flag))
;; ditto.  this is for vm-read-attributes.
(defun vm-set-new-flag-in-vector (v flag)
  (aset v 0 flag))

(provide 'vm-undo)

;;; Sorting and moving messages inside VM
;;; Copyright (C) 1993, 1994 Kyle E. Jones
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

;;(provide 'vm-sort)

(defun vm-move-message-forward (count)
  "Move a message forward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT messages forward.
A negative COUNT causes movement to be backward instead of forward.
COUNT defaults to 1.  The current message remains selected after being
moved.

If vm-move-messages-physically is non-nil, the physical copy of
the message in the folder is moved.  A nil value means just
change the presentation order and leave the physical order of
the folder undisturbed."
  (interactive "p")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if vm-move-messages-physically
      (vm-error-if-folder-read-only))
  (vm-display nil nil '(vm-move-message-forward
			vm-move-message-backward
			vm-move-message-forward-physically
			vm-move-message-backward-physically)
	      (list this-command))
  (let* ((ovmp vm-message-pointer) vmp-prev ovmp-prev
	 (vm-message-pointer vm-message-pointer)
	 (direction (if (> count 0) 'forward 'backward))
	 (count (vm-abs count)))
    (while (not (zerop count))
      (vm-move-message-pointer direction)
      (vm-decrement count))
    (if (> (string-to-int (vm-number-of (car vm-message-pointer)))
	   (string-to-int (vm-number-of (car ovmp))))
	(setq vm-message-pointer (cdr vm-message-pointer)))
    (if (eq vm-message-pointer ovmp)
	()
      (if (null vm-message-pointer)
	  (setq vmp-prev (vm-last vm-message-list))
	(setq vmp-prev (vm-reverse-link-of (car vm-message-pointer))))
      (setq ovmp-prev (vm-reverse-link-of (car ovmp)))
      ;; lock out interrupts to preserve message list integrity.
      (let ((inhibit-quit t))
	(if ovmp-prev
	    (progn
	      (setcdr ovmp-prev (cdr ovmp))
	      (and (cdr ovmp)
		   (vm-set-reverse-link-of (car (cdr ovmp)) ovmp-prev)))
	  (setq vm-message-list (cdr ovmp))
	  (vm-set-reverse-link-of (car vm-message-list) nil))
	(if vmp-prev
	    (progn
	      (setcdr vmp-prev ovmp)
	      (vm-set-reverse-link-of (car ovmp) vmp-prev))
	  (setq vm-message-list ovmp)
	  (vm-set-reverse-link-of (car vm-message-list) nil))
	(setcdr ovmp vm-message-pointer)
	(and vm-message-pointer
	     (vm-set-reverse-link-of (car vm-message-pointer) ovmp))
	(if (and vm-move-messages-physically
		 (not (eq major-mode 'vm-virtual-mode)))
	    (vm-physically-move-message (car ovmp) (car vm-message-pointer)))
	(setq vm-ml-sort-keys nil)
	(if (not vm-folder-read-only)
	    (progn
	      (setq vm-message-order-changed t)
	      (vm-set-buffer-modified-p t)
	      (vm-clear-modification-flag-undos))))
      (cond ((null ovmp-prev)
	     (setq vm-numbering-redo-start-point vm-message-list
		   vm-numbering-redo-end-point vm-message-pointer
		   vm-summary-pointer (car vm-message-list)))
	    ((null vmp-prev)
	     (setq vm-numbering-redo-start-point vm-message-list
		   vm-numbering-redo-end-point (cdr ovmp-prev)
		   vm-summary-pointer (car ovmp-prev)))
	    ((or (not vm-message-pointer)
		 (< (string-to-int (vm-number-of (car ovmp-prev)))
		    (string-to-int (vm-number-of (car vm-message-pointer)))))
	     (setq vm-numbering-redo-start-point (cdr ovmp-prev)
		   vm-numbering-redo-end-point (cdr ovmp)
		   vm-summary-pointer (car (cdr ovmp-prev))))
	    (t
	     (setq vm-numbering-redo-start-point ovmp
		   vm-numbering-redo-end-point (cdr ovmp-prev)
		   vm-summary-pointer (car ovmp-prev))))
      (if vm-summary-buffer
	  (let (list mp)
	    (vm-copy-local-variables vm-summary-buffer 'vm-summary-pointer)
	    (setq vm-need-summary-pointer-update t)
	    (setq mp vm-numbering-redo-start-point)
	    (while (not (eq mp vm-numbering-redo-end-point))
	      (vm-mark-for-summary-update (car mp))
	      (setq list (cons (car mp) list)
		    mp (cdr mp)))
	    (vm-mapc
	     (function
	      (lambda (m p)
		(vm-set-su-start-of m (car p))
		(vm-set-su-end-of m (car (cdr p)))))
	     (setq list (nreverse list))
	     (sort
	      (mapcar
	       (function
		(lambda (p)
		  (list (vm-su-start-of p) (vm-su-end-of p))))
	       list)
	      (function
	       (lambda (p q)
		 (< (car p) (car q))))))))))
  (if vm-move-messages-physically
      ;; clip region is messed up
      (vm-preview-current-message)
    (vm-update-summary-and-mode-line)))

(defun vm-move-message-backward (count)
  "Move a message backward in a VM folder.
Prefix arg COUNT causes the current message to be moved COUNT
messages backward.  A negative COUNT causes movement to be
forward instead of backward.  COUNT defaults to 1.  The current
message remains selected after being moved.

If vm-move-messages-physically is non-nil, the physical copy of
the message in the folder is moved.  A nil value means just
change the presentation order and leave the physical order of
the folder undisturbed."
  (interactive "p")
  (vm-move-message-forward (- count)))

(defun vm-move-message-forward-physically (count)
  "Like vm-move-message-forward but always move the message physically."
  (interactive "p")
  (let ((vm-move-messages-physically t))
    (vm-move-message-forward count)))

(defun vm-move-message-backward-physically (count)
  "Like vm-move-message-backward but always move the message physically."
  (interactive "p")
  (let ((vm-move-messages-physically t))
    (vm-move-message-backward count)))

;; move message m to be before m-dest
;; and fix up the location markers afterwards.
;; m better not equal m-dest.
;; of m-dest is nil, move m to the end of buffer.
;;
;; consider carefully the effects of insertion on markers
;; and variables containg markers before you modify this code.
(defun vm-physically-move-message (m m-dest)
  (save-excursion
    (vm-save-restriction
     (widen)

     ;; Make sure vm-headers-of and vm-text-of are non-nil in
     ;; their slots before we try to move them.  (Simply
     ;; referencing the slot with their slot function is
     ;; sufficient to guarantee this.)  Otherwise, they be
     ;; initialized in the middle of the message move and get the
     ;; offset applied to them twice by way of a relative offset
     ;; from one of the other location markers that has already
     ;; been moved.
     ;;
     ;; Also, and more importantly, vm-vheaders-of might run
     ;; vm-reorder-message-headers, which can add text to
     ;; message.  This MUST NOT happen after offsets have been
     ;; computed for the message move or varying levels of chaos
     ;; will ensue.  In the case of BABYL files, where
     ;; vm-reorder-message-headers can add a lot of new text,
     ;; folder curroption can be massive.
     (vm-text-of m)
     (vm-vheaders-of m)

     (let ((dest-start (if m-dest (vm-start-of m-dest) (point-max)))
	   (buffer-read-only nil)
	   offset doomed-start doomed-end)
       (goto-char dest-start)
       (insert-buffer-substring (current-buffer) (vm-start-of m) (vm-end-of m))
       (setq doomed-start (marker-position (vm-start-of m))
	     doomed-end (marker-position (vm-end-of m))
	     offset (- (vm-start-of m) dest-start))
       (set-marker (vm-start-of m) (- (vm-start-of m) offset))
       (set-marker (vm-headers-of m) (- (vm-headers-of m) offset))
       (set-marker (vm-text-end-of m) (- (vm-text-end-of m) offset))
       (set-marker (vm-end-of m) (- (vm-end-of m) offset))
       (set-marker (vm-text-of m) (- (vm-text-of m) offset))
       (set-marker (vm-vheaders-of m) (- (vm-vheaders-of m) offset))
       ;; now fix the start of m-dest since it didn't
       ;; move forward with its message.
       (and m-dest (set-marker (vm-start-of m-dest) (vm-end-of m)))
       ;; delete the old copy of the message
       (delete-region doomed-start doomed-end)))))

(defun vm-so-sortable-datestring (m)
  (or (vm-sortable-datestring-of m)
      (progn
	(vm-set-sortable-datestring-of
	 m
	 (condition-case nil
	     (vm-timezone-make-date-sortable
	      (or (vm-get-header-contents m "Date:")
		  (vm-grok-From_-date m)
		  "Thu, 1 Jan 1970 00:00:00 GMT"))
	   (error "1970010100:00:00")))
	(vm-sortable-datestring-of m))))

(defun vm-so-sortable-subject (m)
  (or (vm-sortable-subject-of m)
      (progn
	(vm-set-sortable-subject-of
	 m
	 (let ((case-fold-search t)
	       (subject (vm-su-subject m)))
	   (if (and vm-subject-ignored-prefix
		    (string-match vm-subject-ignored-prefix subject)
		    (zerop (match-beginning 0)))
	       (setq subject (substring subject (match-end 0))))
	   (if (and vm-subject-ignored-suffix
		    (string-match vm-subject-ignored-suffix subject)
		    (= (match-end 0) (length subject)))
	       (setq subject (substring subject 0 (match-beginning 0))))
	   (setq subject (vm-with-string-as-temp-buffer
			  subject
			  (function vm-collapse-whitespace)))
	   (if (and vm-subject-significant-chars
		    (natnump vm-subject-significant-chars)
		    (< vm-subject-significant-chars (length subject)))
	       (setq subject
		     (substring subject 0 vm-subject-significant-chars)))
	   subject ))
	(vm-sortable-subject-of m))))

(defun vm-sort-messages (keys &optional lets-get-physical)
  "Sort message in a folder by the specified KEYS.
You may sort by more than one particular message key.  If
messages compare equal by the first key, the second key will be
compared and so on.  When called interactively the keys will be
read from the minibuffer.  Valid keys are

\"date\"		\"reversed-date\"
\"author\"		\"reversed-author\"
\"subject\"		\"reversed-subject\"
\"recipients\"		\"reversed-recipients\"
\"line-count\"		\"reversed-line-count\"
\"byte-count\"		\"reversed-byte-count\"
\"physical-order\"	\"reversed-physical-order\"

Optional second arg (prefix arg interactively) means the sort
should change the physical order of the messages in the folder.
Normally VM changes presentation order only, leaving the
folder in the order in which the messages arrived."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
   (list (vm-read-string (if (or current-prefix-arg
				 vm-move-messages-physically)
			     "Physically sort messages by: "
			   "Sort messages by: ")
			 vm-supported-sort-keys t)
	 current-prefix-arg)))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  ;; only squawk if interactive.  The thread display uses this
  ;; function and doesn't expect errors.
  (if (interactive-p)
      (vm-error-if-folder-empty))
  ;; ditto
  (if (and (interactive-p) (or vm-move-messages-physically lets-get-physical))
      (vm-error-if-folder-read-only))

  (vm-display nil nil '(vm-sort-messages) '(vm-sort-messages))
  (let (key-list key-funcs key ml-keys
	physical-order-list old-message-list new-message-list mp-old mp-new
	old-start
	doomed-start doomed-end offset
	(order-did-change nil)
	virtual
	physical)
    (setq key-list (vm-parse keys "[ \t]*\\([^ \t]+\\)")
	  ml-keys (and key-list (mapconcat (function identity) key-list "/"))
	  key-funcs nil
	  old-message-list vm-message-list
	  virtual (eq major-mode 'vm-virtual-mode)
	  physical (and (or lets-get-physical
			    vm-move-messages-physically)
			(not vm-folder-read-only)
			(not virtual)))
    (or key-list (error "No sort keys specified."))
    (while key-list
      (setq key (car key-list))
      (cond ((equal key "thread")
	     (vm-build-threads-if-unbuilt)
	     (vm-build-thread-lists)
	     (setq key-funcs (cons 'vm-sort-compare-thread key-funcs)))
	    ((equal key "author")
	     (setq key-funcs (cons 'vm-sort-compare-author key-funcs)))
	    ((equal key "reversed-author")
	     (setq key-funcs (cons 'vm-sort-compare-author-r key-funcs)))
	    ((equal key "date")
	     (setq key-funcs (cons 'vm-sort-compare-date key-funcs)))
	    ((equal key "reversed-date")
	     (setq key-funcs (cons 'vm-sort-compare-date-r key-funcs)))
	    ((equal key "subject")
	     (setq key-funcs (cons 'vm-sort-compare-subject key-funcs)))
	    ((equal key "reversed-subject")
	     (setq key-funcs (cons 'vm-sort-compare-subject-r key-funcs)))
	    ((equal key "recipients")
	     (setq key-funcs (cons 'vm-sort-compare-recipients key-funcs)))
	    ((equal key "reversed-recipients")
	     (setq key-funcs (cons 'vm-sort-compare-recipients-r key-funcs)))
	    ((equal key "byte-count")
	     (setq key-funcs (cons 'vm-sort-compare-byte-count key-funcs)))
	    ((equal key "reversed-byte-count")
	     (setq key-funcs (cons 'vm-sort-compare-byte-count-r key-funcs)))
	    ((equal key "line-count")
	     (setq key-funcs (cons 'vm-sort-compare-line-count key-funcs)))
	    ((equal key "reversed-line-count")
	     (setq key-funcs (cons 'vm-sort-compare-line-count-r key-funcs)))
	    ((equal key "physical-order")
	     (setq key-funcs (cons 'vm-sort-compare-physical-order key-funcs)))
	    ((equal key "reversed-physical-order")
	     (setq key-funcs (cons 'vm-sort-compare-physical-order-r key-funcs)))
	    (t (error "Unknown key: %s" key)))
      (setq key-list (cdr key-list)))
    ;; if this is not a thread sort and threading is enabled,
    ;; then disable threading and make sure the whole summary is
    ;; regenerated (to recalculate %I everywhere).
    (if (and vm-summary-show-threads
	     (not (equal key-funcs '(vm-sort-compare-thread))))
	(progn
	  (setq vm-summary-show-threads nil)
	  (vm-set-summary-redo-start-point t)))
    (message "Sorting...")
    (let ((vm-key-functions (nreverse key-funcs)))
      (setq new-message-list (sort (copy-sequence old-message-list)
				   'vm-sort-compare-xxxxxx))
      ;; only need to do this sort if we're going to physically
      ;; move messages later.
      (if physical
	  (setq vm-key-functions '(vm-sort-compare-physical-order)
		physical-order-list (sort (copy-sequence old-message-list)
					  'vm-sort-compare-xxxxxx))))
    (message "Sorting... done")
    (let ((inhibit-quit t))
      (setq mp-old old-message-list
	    mp-new new-message-list)
      (while mp-new
	(if (eq (car mp-old) (car mp-new))
	    (setq mp-old (cdr mp-old)
		  mp-new (cdr mp-new))
	  (setq order-did-change t)
	  ;; unless a full redo has been requested, the numbering
	  ;; start point now points to a cons in the old message
	  ;; list.  therefore we just change the variable
	  ;; directly to avoid the list scan that
	  ;; vm-set-numbering-redo-start-point does.
	  (cond ((not (eq vm-numbering-redo-start-point t))
		 (setq vm-numbering-redo-start-point mp-new
		       vm-numbering-redo-end-point nil)))
	  (if vm-summary-buffer
	      (progn
		(setq vm-need-summary-pointer-update t)
		;; same logic as numbering reset above...
		(cond ((not (eq vm-summary-redo-start-point t))
		       (setq vm-summary-redo-start-point mp-new)))
		;; start point of this message's summary is now
		;; wrong relative to where it is in the
		;; message list.  fix it and the summary rebuild
		;; will take care of the rest.
		(vm-set-su-start-of (car mp-new)
				    (vm-su-start-of (car mp-old)))))
	  (setq mp-new nil)))
      (if (and order-did-change physical)
	  (let ((buffer-read-only nil))
	    ;; the folder is being physically ordered so we don't
	    ;; need a message order header to be stuffed, nor do
	    ;; we need to retain one in the folder buffer.  so we
	    ;; strip out any existing message order header and
	    ;; say there are no changes to prevent a message
	    ;; order header from being stuffed later.
	    (vm-remove-message-order)
	    (setq vm-message-order-changed nil)
	    (message "Moving messages... ")
	    (widen)
	    (setq mp-old physical-order-list
		  mp-new new-message-list)
	    (setq old-start (vm-start-of (car mp-old)))
	    (while mp-new
	      (if (< (vm-start-of (car mp-old)) old-start)
		  ;; already moved this message
		  (setq mp-old (cdr mp-old))
		(if (eq (car mp-old) (car mp-new))
		    (setq mp-old (cdr mp-old)
			  mp-new (cdr mp-new))
		  ;; move message
		  (vm-physically-move-message (car mp-new) (car mp-old))
		  ;; record start position.  if vm-start-of
		  ;; mp-old ever becomes less than old-start
		  ;; we're running into messages that have
		  ;; already been moved.
		  (setq old-start (vm-start-of (car mp-old)))
		  ;; move mp-new but not mp-old because we moved
		  ;; mp-old down one message by inserting a
		  ;; message in front of it.
		  (setq mp-new (cdr mp-new)))))
	    (message "Moving messages... done")
	    (vm-set-buffer-modified-p t)
	    (vm-clear-modification-flag-undos))
	(if (and order-did-change (not vm-folder-read-only))
	    (progn
	      (setq vm-message-order-changed t)
	      (vm-set-buffer-modified-p t)
	      (vm-clear-modification-flag-undos))))
      (setq vm-ml-sort-keys ml-keys)
      (intern (buffer-name) vm-buffers-needing-display-update)
      (cond (order-did-change
	     (setq vm-message-list new-message-list)
	     (vm-reverse-link-messages)
	     (if vm-message-pointer
		 (setq vm-message-pointer
		       (or (cdr (vm-reverse-link-of (car vm-message-pointer)))
			   vm-message-list)))
	     (if vm-last-message-pointer
		 (setq vm-last-message-pointer
		       (or (cdr (vm-reverse-link-of
				 (car vm-last-message-pointer)))
			   vm-message-list))))))
    (if (and vm-message-pointer
	     order-did-change
	     (or lets-get-physical vm-move-messages-physically))
	;; clip region is most likely messed up
	(vm-preview-current-message)
      (vm-update-summary-and-mode-line))))

(defun vm-sort-compare-xxxxxx (m1 m2)
  (let ((key-funcs vm-key-functions) result)
    (while (and key-funcs
		(eq '= (setq result (funcall (car key-funcs) m1 m2))))
      (setq key-funcs (cdr key-funcs)))
    (and key-funcs result) ))

(defun vm-sort-compare-thread (m1 m2)
  (let ((list1 (vm-th-thread-list m1))
	(list2 (vm-th-thread-list m2))
	p1 p2 d1 d2)
    (catch 'done
      (if (not (eq (car list1) (car list2)))
	  (let ((date1 (get (car list1) 'youngest-date))
		(date2 (get (car list2) 'youngest-date)))
	    (cond ((string-lessp date1 date2) t)
		  ((string-equal date1 date2)
		   (string-lessp (car list1) (car list2)))
		  (t nil)))
	(setq list1 (cdr list1) list2 (cdr list2))
	(while (and list1 list2)
	  (setq p1 (car list1) p2 (car list2))
	  (cond ((not (string-equal p1 p2))
		 (setq d1 (or (get p1 'date) "0") d2 (or (get p2 'date) "0"))
		 (cond ((string-lessp d1 d2)
			(throw 'done t))
		       ((string-lessp d2 d1)
			(throw 'done nil))
		       ((string-lessp p1 p2)
			(throw 'done t))
		       (t
			(throw 'done t)))))
	  (setq list1 (cdr list1)
		list2 (cdr list2)))
	(cond ((and list1 (not list2)) nil)
	      ((and list2 (not list1)) t)
	      (t '=))))))

(defun vm-sort-compare-author (m1 m2)
  (let ((s1 (vm-su-from m1))
	(s2 (vm-su-from m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-author-r (m1 m2)
  (let ((s1 (vm-su-from m1))
	(s2 (vm-su-from m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-date (m1 m2)
  (let ((s1 (vm-so-sortable-datestring m1))
	(s2 (vm-so-sortable-datestring m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-date-r (m1 m2)
  (let ((s1 (vm-so-sortable-datestring m1))
	(s2 (vm-so-sortable-datestring m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-recipients (m1 m2)
  (let ((s1 (vm-su-to m1))
	(s2 (vm-su-to m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-recipients-r (m1 m2)
  (let ((s1 (vm-su-to m1))
	(s2 (vm-su-to m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-subject (m1 m2)
  (let ((s1 (vm-so-sortable-subject m1))
	(s2 (vm-so-sortable-subject m2)))
    (cond ((string-lessp s1 s2) t)
	  ((string-equal s1 s2) '=)
	  (t nil))))

(defun vm-sort-compare-subject-r (m1 m2)
  (let ((s1 (vm-so-sortable-subject m1))
	(s2 (vm-so-sortable-subject m2)))
    (cond ((string-lessp s1 s2) nil)
	  ((string-equal s1 s2) '=)
	  (t t))))

(defun vm-sort-compare-line-count (m1 m2)
  (let ((n1 (string-to-int (vm-su-line-count m1)))
	(n2 (string-to-int (vm-su-line-count m2))))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-line-count-r (m1 m2)
  (let ((n1 (string-to-int (vm-su-line-count m1)))
	(n2 (string-to-int (vm-su-line-count m2))))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-byte-count (m1 m2)
  (let ((n1 (string-to-int (vm-su-byte-count m1)))
	(n2 (string-to-int (vm-su-byte-count m2))))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-byte-count-r (m1 m2)
  (let ((n1 (string-to-int (vm-su-byte-count m1)))
	(n2 (string-to-int (vm-su-byte-count m2))))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-physical-order (m1 m2)
  (let ((n1 (vm-start-of m1))
	(n2 (vm-start-of m2)))
    (cond ((< n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(defun vm-sort-compare-physical-order-r (m1 m2)
  (let ((n1 (vm-start-of m1))
	(n2 (vm-start-of m2)))
    (cond ((> n1 n2) t)
	  ((= n1 n2) '=)
	  (t nil))))

(provide 'vm-sort)

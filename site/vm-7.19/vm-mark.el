;;; Commands for handling messages marks
;;; Copyright (C) 1990, 1993, 1994 Kyle E. Jones
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

;;(provide 'vm-mark)

(defun vm-clear-all-marks ()
  "Removes all message marks in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Clearing all marks...")
  (let ((mp vm-message-list))
    (while mp
      (if (vm-mark-of (car mp))
	  (progn
	    (vm-set-mark-of (car mp) nil)
	    (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp))))
  (vm-display nil nil '(vm-clear-all-marks)
	      '(vm-clear-all-marks marking-message))
  (vm-update-summary-and-mode-line)
  (message "Clearing all marks... done"))

(defun vm-toggle-all-marks ()
  "Toggles all message marks in the current folder.
Messages that are unmarked will become marked and messages that are
marked will become unmarked."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Toggling all marks...")
  (let ((mp vm-message-list))
    (while mp
      (vm-set-mark-of (car mp) (not (vm-mark-of (car mp))))
      (vm-mark-for-summary-update (car mp) t)
      (setq mp (cdr mp))))
  (vm-display nil nil '(vm-toggle-all-marks)
	      '(vm-toggle-all-marks marking-message))
  (vm-update-summary-and-mode-line)
  (message "Toggling all marks... done"))

(defun vm-mark-all-messages ()
  "Mark all messages in the current folder."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Marking all messages...")
  (let ((mp vm-message-list))
    (while mp
      (vm-set-mark-of (car mp) t)
      (vm-mark-for-summary-update (car mp) t)
      (setq mp (cdr mp))))
  (vm-display nil nil '(vm-mark-all-messages)
	      '(vm-mark-all-messages marking-message))
  (vm-update-summary-and-mode-line)
  (message "Marking all messages... done"))

(defun vm-mark-message (count)
  "Mark the current message.
Numeric prefix argument N means mark the current message and the next
N-1 messages.  A negative N means mark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((direction (if (< count 0) 'backward 'forward))
	(count (vm-abs count))
	(oldmp vm-message-pointer)
	(vm-message-pointer vm-message-pointer))
    (while (not (zerop count))
      (if (not (vm-mark-of (car vm-message-pointer)))
	  (progn
	    (vm-set-mark-of (car vm-message-pointer) t)
	    (vm-mark-for-summary-update (car vm-message-pointer) t)))
      (vm-decrement count)
      (if (not (zerop count))
	  (vm-move-message-pointer direction))))
  (vm-display nil nil '(vm-mark-message)
	      '(vm-mark-message marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-unmark-message (count)
  "Remove the mark from the current message.
Numeric prefix argument N means unmark the current message and the next
N-1 messages.  A negative N means unmark the current message and the
previous N-1 messages."
  (interactive "p")
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((mlist (vm-select-marked-or-prefixed-messages count)))
    (while mlist
      (if (vm-mark-of (car mlist))
	  (progn
	    (vm-set-mark-of (car mlist) nil)
	    (vm-mark-for-summary-update (car mlist) t)))
      (setq mlist (cdr mlist))))
  (vm-display nil nil '(vm-unmark-message)
	      '(vm-unmark-message marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-summary-region ()
  "Mark all messages with summary lines contained in the region."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (null vm-summary-buffer)
      (error "No summary."))
  (set-buffer vm-summary-buffer)
  (if (not (mark))
      (error "The region is not active now"))
  (vm-mark-or-unmark-summary-region t)
  (vm-display nil nil '(vm-mark-summary-region)
	      '(vm-mark-summary-region marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-unmark-summary-region ()
  "Remove marks from messages with summary lines contained in the region."
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (if (null vm-summary-buffer)
      (error "No summary."))
  (set-buffer vm-summary-buffer)
  (if (not (mark))
      (error "The region is not active now"))
  (vm-mark-or-unmark-summary-region nil)
  (vm-display nil nil '(vm-unmark-summary-region)
	      '(vm-unmark-summary-region marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-or-unmark-summary-region (markit)
  ;; The folder buffers copy of vm-message-list has already been
  ;; propagated to the summary buffer.
  (let ((mp vm-message-list)
	(beg (point))
	(end (mark))
	tmp m)
    (if (> beg end)
	(setq tmp beg beg end end tmp))
    (while mp
      (setq m (car mp))
      (if (not (eq (not markit) (not (vm-mark-of m))))
	  (if (or (and (>  (vm-su-end-of m) beg)
		       (<  (vm-su-end-of m) end))
		  (and (>= (vm-su-start-of m) beg)
		       (<  (vm-su-start-of m) end))
		  (and (>= beg (vm-su-start-of m))
		       (<  beg (vm-su-end-of m))))
	      (progn
		(vm-set-mark-of m markit)
		(vm-mark-for-summary-update m t))))
      (setq mp (cdr mp)))))

(defun vm-mark-or-unmark-messages-with-selector (val selector arg)
  (let ((mlist vm-message-list)
	(virtual (eq major-mode 'vm-virtual-mode))
	(arglist (if arg (list arg) nil))
	(count 0))
    (setq selector (intern (concat "vm-vs-" (symbol-name selector))))
    (while mlist
      (if (if virtual
	      (save-excursion
		(set-buffer
		 (vm-buffer-of
		  (vm-real-message-of
		   (car mlist))))
		(apply selector (vm-real-message-of (car mlist)) arglist))
	    (apply selector (car mlist) arglist))
	  (progn
	    (vm-set-mark-of (car mlist) val)
	    (vm-mark-for-summary-update (car mlist) t)
	    (vm-increment count)))
      (setq mlist (cdr mlist)))
    (vm-display nil nil
		'(vm-mark-matching-messages vm-unmark-matching-messages)
		(list this-command 'marking-message))
    (vm-update-summary-and-mode-line)
    (message "%s message%s %smarked"
	     (if (> count 0) count "No")
	     (if (= 1 count) "" "s")
	     (if val "" "un"))))

(defun vm-mark-matching-messages (selector &optional arg)
  "Mark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-select-folder-buffer)
     (vm-read-virtual-selector "Mark messages: ")))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-selector t selector arg))

(defun vm-unmark-matching-messages (selector &optional arg)
  "Unmark messages matching some criterion.
You can use any of the virtual folder selectors, except for the
`and', `or' and `not' selectors.  See the documentation for the
variable vm-virtual-folder-alist for more information."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-select-folder-buffer)
     (vm-read-virtual-selector "Unmark messages: ")))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-selector nil selector arg))

(defun vm-mark-thread-subtree ()
  "Mark all messages in the thread tree rooted at the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-thread-subtree t))

(defun vm-unmark-thread-subtree ()
  "Unmark all messages in the thread tree rooted at the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-thread-subtree nil))

(defun vm-mark-or-unmark-thread-subtree (mark)
  (vm-build-threads-if-unbuilt)
  (let ((list (list (car vm-message-pointer)))
	(loop-obarray (make-vector 29 0))
	subject-sym id-sym)
    (while list
      (if (not (eq (vm-mark-of (car list)) mark))
	  (progn
	    (vm-set-mark-of (car list) mark)
	    (vm-mark-for-summary-update (car list))))
      (setq id-sym (car (vm-last (vm-th-thread-list (car list)))))
      (if (null (intern-soft (symbol-name id-sym) loop-obarray))
	  (progn
	    (intern (symbol-name id-sym) loop-obarray)
	    (nconc list (copy-sequence (get id-sym 'children)))
	    (setq subject-sym (intern (vm-so-sortable-subject (car list))
				      vm-thread-subject-obarray))
	    (if (and (boundp subject-sym) 
		     (eq id-sym (aref (symbol-value subject-sym) 0)))
		(nconc list (copy-sequence
			     (aref (symbol-value subject-sym) 2))))))
      (setq list (cdr list))))
  (vm-display nil nil
	      '(vm-mark-thread-subtree vm-unmark-thread-subtree)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-messages-same-subject ()
  "Mark all messages with the same subject as the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-same-subject t))

(defun vm-unmark-messages-same-subject ()
  "Unmark all messages with the same subject as the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-same-subject nil))

(defun vm-mark-or-unmark-messages-same-subject (mark)
  (let ((mp vm-message-list)
	(mark-count 0)
	(subject (vm-so-sortable-subject (car vm-message-pointer))))
    (while mp
      (if (and (not (eq (vm-mark-of (car mp)) mark))
	       (string-equal subject (vm-so-sortable-subject (car mp))))
	  (progn
	    (vm-set-mark-of (car mp) mark)
	    (vm-increment mark-count)
	    (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp)))
    (if (zerop mark-count)
	(message "No messages %smarked" (if mark "" "un"))
      (message "%d message%s %smarked"
	       mark-count
	       (if (= 1 mark-count) "" "s")
	       (if mark "" "un"))))
  (vm-display nil nil
	      '(vm-mark-messages-same-subject
		vm-unmark-messages-same-subject)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-messages-same-author ()
  "Mark all messages with the same author as the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-same-author t))

(defun vm-unmark-messages-same-author ()
  "Unmark all messages with the same author as the current message."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-same-author nil))

(defun vm-mark-or-unmark-messages-same-author (mark)
  (let ((mp vm-message-list)
	(mark-count 0)
	(author (vm-su-from (car vm-message-pointer))))
    (while mp
      (if (and (not (eq (vm-mark-of (car mp)) mark))
	       (vm-string-equal-ignore-case author (vm-su-from (car mp))))
	  (progn
	    (vm-set-mark-of (car mp) mark)
	    (vm-increment mark-count)
	    (vm-mark-for-summary-update (car mp) t)))
      (setq mp (cdr mp)))
    (if (zerop mark-count)
	(message "No messages %smarked" (if mark "" "un"))
      (message "%d message%s %smarked"
	       mark-count
	       (if (= 1 mark-count) "" "s")
	       (if mark "" "un"))))
  (vm-display nil nil
	      '(vm-mark-messages-same-author
		vm-unmark-messages-same-author)
	      (list this-command 'marking-message))
  (vm-update-summary-and-mode-line))

(defun vm-mark-or-unmark-messages-with-virtual-folder (val name)
  (let* ((vfolder (assoc name vm-virtual-folder-alist))
	 vm-virtual-folder-definition m mlist clauses
	 (count 0))
    (or vfolder (error "No such virtual folder, %s" name))
    (setq vfolder (vm-copy vfolder))
    (setq clauses (cdr vfolder))
    (while clauses
      (setcar (car clauses) (list (list 'get-buffer (buffer-name))))
      (setq clauses (cdr clauses)))
    (setq vm-virtual-folder-definition vfolder)
    (setq mlist (vm-build-virtual-message-list vm-message-list t))
    (if (null vm-real-buffers)
	(while mlist
	  (setq m (vm-real-message-of (car mlist)))
	  (vm-set-mark-of m val)
	  (vm-mark-for-summary-update m t)
	  (vm-increment count)
	  (setq mlist (cdr mlist)))
      (let ((curbuf (current-buffer)) vmlist)
	(while mlist
	  (setq m (vm-real-message-of (car mlist))
		vmlist (vm-virtual-messages-of m))
	  (while vmlist
	    (cond ((eq curbuf (vm-buffer-of (car vmlist)))
		   (vm-set-mark-of (car vmlist) val)
		   (vm-mark-for-summary-update (car vmlist) t)
		   (vm-increment count)
		   (setq vmlist nil))
		  (t (setq vmlist (cdr vmlist)))))
	  (setq mlist (cdr mlist)))))
    (vm-display nil nil
		'(vm-mark-matching-messages-with-virtual-folder
		  vm-unmark-matching-messages-with-virtual-folder)
		(list this-command 'marking-message))
    (vm-update-summary-and-mode-line)
    (message "%s message%s %smarked"
	     (if (> count 0) count "No")
	     (if (= 1 count) "" "s")
	     (if val "" "un"))))

(defun vm-mark-matching-messages-with-virtual-folder (name)
  "Mark messages that are matched by the selectors of virtual folder NAME."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (completing-read
       "Mark messages matching this virtual folder's selectors: "
       vm-virtual-folder-alist nil t))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-virtual-folder t name))

(defun vm-unmark-matching-messages-with-virtual-folder (name)
  "Unmark messages that are matched by the selectors of virtual folder NAME."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (completing-read
       "Unmark message matching this virtual folder's selectors: "
       vm-virtual-folder-alist nil t))))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-mark-or-unmark-messages-with-virtual-folder nil name))

(defun vm-next-command-uses-marks ()
  "Does nothing except insure that the next VM command will operate only
on the marked messages in the current folder.  This only works for
commands bound to key, menu or button press events.  M-x vm-command will
not work."
  (interactive)
  (setq this-command 'vm-next-command-uses-marks)
  (message "Next command uses marks...")
  (vm-display nil nil '(vm-next-command-uses-marks)
	      '(vm-next-command-uses-marks)))

(defun vm-marked-messages ()
  (let (list (mp vm-message-list))
    (while mp
      (if (vm-mark-of (car mp))
	  (setq list (cons (car mp) list)))
      (setq mp (cdr mp)))
    (nreverse list)))

(defun vm-mark-help ()
  (interactive)
  (vm-display nil nil '(vm-mark-help) '(vm-mark-help))
  (message "MM = mark, MU = unmark, Mm = mark all, Mu = unmark all, MN = use marks, ..."))

(provide 'vm-mark)

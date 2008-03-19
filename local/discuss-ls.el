(defvar discuss-ls-filename nil)

(defun discuss-ls (arg)
  "Display txn headers of the meeting.  If no ARG, lists from current trn to
last trn.
If ARG is positive, list last ARG transactions.
If ARG is negative, prompt for range to display."
  (interactive "P")
  (message "Listing meeting %s..." discuss-current-meeting)
  (let ((start discuss-current-transaction) ;lower limit
	(finish (nth 4 discuss-current-meeting-info))) ;upper limit
    (if (and discuss-ls-filename
	     (file-exists-p discuss-ls-filename))
	(delete-file discuss-ls-filename))
    ;;  (setq discuss-ls-filename (make-temp-name (format "/tmp/discuss-ls-%s" 
    ;;						     discuss-current-meeting)))
    (setq discuss-ls-filename (make-temp-name (format "/tmp/discuss-ls-%s" 
						      (user-login-name))))
    (if arg
	(progn
	  ;; this listp/setq is needed since negative args are not passed
	  ;; as the head of a list.
	  (if (listp arg)
	      (setq arg (car arg)))
	  (if (< arg 0)
	      (let ((range (discuss-ls-get-params)))
		(setq start (car range)
		      finish (car (cdr range))))
	    (setq start (1+ (- finish arg))))))

    (discuss-send-cmd (format "(ls %s %d %d %d %s)\n"
			      discuss-ls-filename
			      start
			      finish
			      0		; filter flags
			      discuss-current-meeting)
		      'discuss-end-list-mtg 'discuss-read-form)))

(defun discuss-ls-get-params ()
  (list (string-to-int (read-string "Beginning of range: "))
	(string-to-int (read-string "End of range: "))))

(defun discuss-end-list-mtg ()
  (message "Listing meeting %s... done." discuss-current-meeting)
  (let ((win (selected-window))
	(buf (current-buffer)))
    (unwind-protect
	(let ((buf (get-buffer "*discuss-ls*"))
	      (pop-up-windows t)
	      )
	  (if buf
	      (pop-to-buffer buf)
	    (setq buf (get-buffer-create "*discuss-ls*"))
	    (pop-to-buffer buf)
	    (setq buffer-read-only t)
	    (setq truncate-lines t)
	    )
	  (setq discuss-ls-buf buf)
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    ;; (discuss-ls-mode)
	    (insert-file-contents discuss-ls-filename)))
      (select-window win)
      (set-buffer buf))))

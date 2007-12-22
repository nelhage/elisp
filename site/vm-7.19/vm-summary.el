;;; Summary gathering and formatting routines for VM
;;; Copyright (C) 1989-1995, 2000 Kyle E. Jones
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

;;(provide 'vm-summary)

(defun vm-summary-mode-internal ()
  (setq mode-name "VM Summary"
	major-mode 'vm-summary-mode
	mode-line-format vm-mode-line-format
	;; must come after the setting of major-mode
	mode-popup-menu (and vm-use-menus
			     (vm-menu-support-possible-p)
			     (vm-menu-mode-menu))
	buffer-read-only t
	vm-summary-pointer nil
	vm-summary-=> (if (stringp vm-summary-arrow) vm-summary-arrow "")
	vm-summary-no-=> (make-string (length vm-summary-=>) ? )
	truncate-lines t)
  ;; horizontal scrollbar off by default
  ;; user can turn it on in summary hook if desired.
  (and vm-xemacs-p (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map vm-summary-mode-map)
  (and (vm-menu-support-possible-p)
       (vm-menu-install-menus))
;; using the 'mouse-face property gives faster highlighting than this.
;;  (and vm-mouse-track-summary
;;       (vm-mouse-support-possible-p)
;;       (vm-mouse-xemacs-mouse-p)
;;       (add-hook 'mode-motion-hook 'mode-motion-highlight-line))
  (if (and vm-mutable-frames (or vm-frame-per-folder vm-frame-per-summary))
      (vm-set-hooks-for-frame-deletion))
  (run-hooks 'vm-summary-mode-hook)
  ;; Lucid Emacs apparently used this name
  (run-hooks 'vm-summary-mode-hooks))

(fset 'vm-summary-mode 'vm-mode)
(put 'vm-summary-mode 'mode-class 'special)

(defun vm-summarize (&optional display raise)
  "Summarize the contents of the folder in a summary buffer. 
The format is as described by the variable vm-summary-format.  Generally
one line per message is most pleasing to the eye but this is not
mandatory."
  (interactive "p\np")
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (if (null vm-summary-buffer)
      (let ((b (current-buffer))
	    (read-only vm-folder-read-only))
	(setq vm-summary-buffer
	      (let ((default-enable-multibyte-characters t))
		(get-buffer-create (format "%s Summary" (buffer-name)))))
	(save-excursion
	  (set-buffer vm-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (setq vm-mail-buffer b
		vm-folder-read-only read-only)
	  (vm-summary-mode-internal))
	(vm-set-summary-redo-start-point t)))
  (if display
      (save-excursion
	(vm-goto-new-summary-frame-maybe)
	(vm-display vm-summary-buffer t
		    '(vm-summarize
		      vm-summarize-other-frame)
		    (list this-command) (not raise))
	;; need to do this after any frame creation because the
	;; toolbar sets frame-specific height and width specifiers.
	(set-buffer vm-summary-buffer)
	(vm-toolbar-install-or-uninstall-toolbar))
    (vm-display nil nil '(vm-summarize vm-summarize-other-frame)
		(list this-command)))
  (vm-update-summary-and-mode-line))

(defun vm-summarize-other-frame (&optional display)
  "Like vm-summarize, but run in a newly created frame."
  (interactive "p")
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'summary))
  (vm-summarize display)
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

(defun vm-do-summary (&optional start-point)
  (let ((m-list (or start-point vm-message-list))
	mp m
	(n 0)
	;; Just for laughs, make the update interval vary.
	(modulus (+ (% (vm-abs (random)) 11) 10))
	(do-mouse-track
	    (and vm-mouse-track-summary
		 (vm-mouse-support-possible-p)))
	summary)
    (setq mp m-list)
    (save-excursion
      (set-buffer vm-summary-buffer)
      (let ((buffer-read-only nil)
	    (modified (buffer-modified-p)))
	(unwind-protect
	    (progn
	      (if start-point
		  (if (vm-su-start-of (car mp))
		      (progn
			(goto-char (vm-su-start-of (car mp)))
			(delete-region (point) (point-max)))
		    (goto-char (point-max)))
		(erase-buffer)
		(setq vm-summary-pointer nil))
	      ;; avoid doing long runs down the marker chain while
	      ;; building the summary.  use integers to store positions
	      ;; and then convert them to markers after all the
	      ;; insertions are done.
	      (while mp
		(setq summary (vm-su-summary (car mp)))
		(vm-set-su-start-of (car mp) (point))
		(insert vm-summary-no-=>)
		(vm-tokenized-summary-insert (car mp) (vm-su-summary (car mp)))
		(vm-set-su-end-of (car mp) (point))
		(setq mp (cdr mp) n (1+ n))
		(if (zerop (% n modulus))
		    (message "Generating summary... %d" n)))
	      ;; now convert the ints to markers.
	      (if (>= n modulus)
		  (message "Generating summary markers... "))
	      (setq mp m-list)
	      (while mp
		(setq m (car mp))
		(and do-mouse-track
		     (vm-set-su-summary-mouse-track-overlay-of
		      m
		      (vm-mouse-set-mouse-track-highlight
		       (vm-su-start-of m)
		       (vm-su-end-of m)
		       (vm-su-summary-mouse-track-overlay-of m))))
		(vm-set-su-start-of m (vm-marker (vm-su-start-of m)))
		(vm-set-su-end-of m (vm-marker (vm-su-end-of m)))
		(setq mp (cdr mp))))
	  (set-buffer-modified-p modified))
	(run-hooks 'vm-summary-redo-hook)))
    (if (>= n modulus)
	(message "Generating summary... done"))))

(defun vm-do-needed-summary-rebuild ()
  (if (and vm-summary-redo-start-point vm-summary-buffer)
      (progn
	(vm-copy-local-variables vm-summary-buffer 'vm-summary-show-threads)
	(vm-do-summary (and (consp vm-summary-redo-start-point)
			    vm-summary-redo-start-point))
	(setq vm-summary-redo-start-point nil)
	(and vm-message-pointer
	     (vm-set-summary-pointer (car vm-message-pointer)))
	(setq vm-need-summary-pointer-update nil))
    (and vm-need-summary-pointer-update
	 vm-summary-buffer
	 vm-message-pointer
	 (progn
	   (vm-set-summary-pointer (car vm-message-pointer))
	   (setq vm-need-summary-pointer-update nil)))))

(defun vm-update-message-summary (m)
  (if (and (vm-su-start-of m)
	   (marker-buffer (vm-su-start-of m)))
      (let ((modified (buffer-modified-p))
	    (do-mouse-track
	     (and vm-mouse-track-summary
		  (vm-mouse-support-possible-p)))
	    summary)
	(save-excursion
	  (setq summary (vm-su-summary m))
	  (set-buffer (marker-buffer (vm-su-start-of m)))
	  (let ((buffer-read-only nil)
		(selected nil)
		(modified (buffer-modified-p)))
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-su-start-of m))
		  (setq selected (not (looking-at vm-summary-no-=>)))
		  ;; We do a little dance to update the text in
		  ;; order to make the markers in the text do
		  ;; what we want.
		  ;;
		  ;; 1. We need to avoid having the su-start-of
		  ;;    and su-end-of markers clumping together at
		  ;;    the start position.
		  ;;
		  ;; 2. We want the window point marker (w->pointm
		  ;;    in the Emacs display code) to move to the
		  ;;    start of the summary entry if it is
		  ;;    anywhere within the su-start-of to
		  ;;    su-end-of region.
		  ;;
		  ;; We achieve (2) by deleting before inserting.
		  ;; Reversing the order of insertion/deletion
		  ;; pushes the point marker into the next
		  ;; summary entry. We achieve (1) by inserting a
		  ;; placeholder character at the end of the
		  ;; summary entry before deleting the region.
		  (goto-char (vm-su-end-of m))
		  (insert-before-markers "z")
		  (goto-char (vm-su-start-of m))
		  (delete-region (point) (1- (vm-su-end-of m)))
		  (if (not selected)
		      (insert vm-summary-no-=>)
		    (insert vm-summary-=>))
		  (vm-tokenized-summary-insert m (vm-su-summary m))
		  (delete-char 1)
		  (run-hooks 'vm-summary-update-hook)
		  (and do-mouse-track
		       (vm-mouse-set-mouse-track-highlight
			(vm-su-start-of m)
			(vm-su-end-of m)
			(vm-su-summary-mouse-track-overlay-of m)))
		  (if (and selected vm-summary-highlight-face)
		      (vm-summary-highlight-region (vm-su-start-of m) (point)
						   vm-summary-highlight-face)))
	      (set-buffer-modified-p modified)))))))

(defun vm-set-summary-pointer (m)
  (if vm-summary-buffer
      (let ((w (vm-get-visible-buffer-window vm-summary-buffer))
	    (do-mouse-track
	       (and vm-mouse-track-summary
		    (vm-mouse-support-possible-p)))
	    (old-window nil))
	(vm-save-buffer-excursion
	  (unwind-protect
	      (progn
		(set-buffer vm-summary-buffer)
		(if w
		    (progn
		      (setq old-window (selected-window))
		      (select-window w)))
		(let ((buffer-read-only nil))
		  (if (and vm-summary-pointer
			   (vm-su-start-of vm-summary-pointer))
		      (progn
			(goto-char (vm-su-start-of vm-summary-pointer))
			(insert vm-summary-no-=>)
			(delete-char (length vm-summary-=>))
			(and do-mouse-track
			     (vm-mouse-set-mouse-track-highlight
			      (vm-su-start-of vm-summary-pointer)
			      (vm-su-end-of vm-summary-pointer)
			      (vm-su-summary-mouse-track-overlay-of
			       vm-summary-pointer)))))
		  (setq vm-summary-pointer m)
		  (goto-char (vm-su-start-of m))
		  (let ((modified (buffer-modified-p)))
		    (unwind-protect
			(progn
			  (insert vm-summary-=>)
			  (delete-char (length vm-summary-=>))
			  (and do-mouse-track
			       (vm-mouse-set-mouse-track-highlight
				(vm-su-start-of m) (vm-su-end-of m)
				(vm-su-summary-mouse-track-overlay-of m))))
		      (set-buffer-modified-p modified)))
		  (forward-char (- (length vm-summary-=>)))
		  (if vm-summary-highlight-face
		      (vm-summary-highlight-region
		       (vm-su-start-of m) (vm-su-end-of m)
		       vm-summary-highlight-face))
		  (and w vm-auto-center-summary (vm-auto-center-summary))
		  (run-hooks 'vm-summary-pointer-update-hook)))
	    (and old-window (select-window old-window)))))))

(defun vm-summary-highlight-region (start end face)
  (vm-summary-xxxx-highlight-region start end face 'vm-summary-overlay))

(defun vm-folders-summary-highlight-region (start end face)
  (vm-summary-xxxx-highlight-region start end face
				    'vm-folders-summary-overlay))

(defun vm-summary-xxxx-highlight-region (start end face var)
  (let ((ooo (symbol-value var)))
    (cond (vm-fsfemacs-p
	   (if (and ooo (overlay-buffer ooo))
	       (move-overlay ooo start end)
	     (setq ooo (make-overlay start end))
	     (set var ooo)
	     (overlay-put ooo 'evaporate nil)
	     (overlay-put ooo 'face face)))
	  (vm-xemacs-p
	   (if (and ooo (extent-end-position ooo))
	       (set-extent-endpoints ooo start end)
	     (setq ooo (make-extent start end))
	     (set var ooo)
	     ;; the reason this isn't needed under FSF Emacs is
	     ;; that insert-before-markers also inserts before
	     ;; overlays!  so a summary update of an entry just
	     ;; before this overlay in the summary buffer won't
	     ;; leak into the overlay, but it _will_ leak into an
	     ;; XEmacs extent.
	     (set-extent-property ooo 'start-open t)
	     (set-extent-property ooo 'detachable nil)
	     (set-extent-property ooo 'face face))))))

(defun vm-auto-center-summary ()
  (if vm-auto-center-summary
      (if (or (eq vm-auto-center-summary t) (not (one-window-p t)))
	  (recenter '(4)))))

(defun vm-summary-sprintf (format message &optional tokenize)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let* ((alist-var (if tokenize
			'vm-summary-tokenized-compiled-format-alist
		      'vm-summary-untokenized-compiled-format-alist))
	 (match (assoc format (symbol-value alist-var))))
    (if (null match)
	(progn
	  (vm-summary-compile-format format tokenize)
	  (setq match (assoc format (symbol-value alist-var)))))
    ;; The local variable name `vm-su-message' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-su-message message))
      (if (or tokenize (null vm-display-using-mime))
	  (eval (cdr match))
	(vm-decode-mime-encoded-words-in-string (eval (cdr match)))))))

(defun vm-summary-compile-format (format tokenize)
  (let ((return-value (nth 1 (vm-summary-compile-format-1 format tokenize))))
    (if tokenize
	(setq vm-summary-tokenized-compiled-format-alist
	      (cons (cons format return-value)
		    vm-summary-tokenized-compiled-format-alist))
      (setq vm-summary-untokenized-compiled-format-alist
	    (cons (cons format return-value)
		  vm-summary-untokenized-compiled-format-alist)))))

(defun vm-tokenized-summary-insert (message tokens)
  (if (stringp tokens)
      (insert tokens)
    (let (token group-list)
      (while tokens
	(setq token (car tokens))
	(cond ((stringp token)
	       (if vm-display-using-mime
		   (insert (vm-decode-mime-encoded-words-in-string token))
		 (insert token)))
	      ((eq token 'group-begin)
	       (setq group-list (cons (list (point) (nth 1 tokens)
					    (nth 2 tokens))
				      group-list)
		     tokens (cdr (cdr tokens))))
	      ((eq token 'group-end)
	       (let* ((space (string-to-char " "))
		      (blob (car group-list))
		      (start (car blob))
		      (field-width (nth 1 blob))
		      (precision (nth 2 blob))
		      (end (vm-marker (point))))
		 (if (integerp field-width)
		     (if (< (- end start) (vm-abs field-width))
			 (if (< field-width 0)
			     (insert-char space (vm-abs (+ field-width
							   (- end start))))
			   (save-excursion
			     (goto-char start)
			     (insert-char space (- field-width
						   (- end start)))))))
		 (if (integerp precision)
		     (if (> (- end start) (vm-abs precision))
			 (if (> precision 0)
			     (delete-char (- precision (- end start)))
			   (save-excursion
			     (goto-char start)
			     (delete-char (vm-abs (+ precision
						     (- end start))))))))
		 (setq group-list (cdr group-list))))
	      ((eq token 'number)
	       (insert (vm-padded-number-of message)))
	      ((eq token 'mark)
	       (insert (vm-su-mark message)))
	      ((eq token 'thread-indent)
	       (if (and vm-summary-show-threads
			(natnump vm-summary-thread-indent-level))
		   (insert-char ?\ (* vm-summary-thread-indent-level
				      (vm-th-thread-indentation message))))))
	(setq tokens (cdr tokens))))))

(defun vm-summary-compile-format-1 (format &optional tokenize start-index)
  (or start-index (setq start-index 0))
  (let ((case-fold-search nil)
	(finished-parsing-format nil)
	(list nil)
	(sexp nil)
	(sexp-fmt nil)
	(saw-close-group nil)
	(last-match-end start-index)
	new-match-end token conv-spec splice)
    (store-match-data nil)
    (while (and (not saw-close-group) (not finished-parsing-format))
      (setq token nil
	    splice nil)
      (while
	  (and (not saw-close-group) (not token)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()aAcdfFhHiIlLmMnstTwyz*%]\\|U[A-Za-z]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (and (memq conv-spec '(?\( ?\) ?a ?A ?c ?d ?f ?F ?h ?H ?i ?I
				   ?l ?L ?M ?m ?n ?s ?t ?T ?U ?w ?y ?z ?* ))
		 ;; for the non-tokenized path, we don't want
		 ;; the close group spcifier processed here, we
		 ;; want to just bail out and return, which is
		 ;; accomplished by setting a flag in the other
		 ;; branch of this 'if'.
		 (or tokenize (not (= conv-spec ?\)))))
	    (progn
	      (cond ((= conv-spec ?\()
		     (if (not tokenize)
			 (save-match-data
			   (let ((retval (vm-summary-compile-format-1
					  format tokenize (match-end 5))))
			     (setq sexp (cons (nth 1 retval) sexp)
				   new-match-end (car retval))))
		       (setq token `('group-begin
				     ,(if (match-beginning 2)
					  (string-to-int
					   (concat (match-string 1 format)
						   (match-string 2 format))))
				     ,(string-to-int
				       (match-string 4 format)))
			     splice t)))
		    ((= conv-spec ?\))
		     (setq token ''group-end))
		    ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-su-attribute-indicators
					    'vm-su-message) sexp)))
		    ((= conv-spec ?A)
		     (setq sexp (cons (list 'vm-su-attribute-indicators-long
					    'vm-su-message) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-su-byte-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-su-monthday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-su-interesting-from
					    'vm-su-message) sexp)))
		    ((= conv-spec ?F)
		     (setq sexp (cons (list 'vm-su-interesting-full-name
					    'vm-su-message) sexp)))
		    ((= conv-spec ?h)
		     (setq sexp (cons (list 'vm-su-hour
					    'vm-su-message) sexp)))
		    ((= conv-spec ?H)
		     (setq sexp (cons (list 'vm-su-hour-short
					    'vm-su-message) sexp)))
		    ((= conv-spec ?i)
		     (setq sexp (cons (list 'vm-su-message-id
					    'vm-su-message) sexp)))
		    ((= conv-spec ?I)
		     (if tokenize
			 (setq token ''thread-indent)
		       (setq sexp (cons (list 'vm-su-thread-indent
					      'vm-su-message) sexp))))
		    ((= conv-spec ?l)
		     (setq sexp (cons (list 'vm-su-line-count
					    'vm-su-message) sexp)))
		    ((= conv-spec ?L)
		     (setq sexp (cons (list 'vm-su-labels
					    'vm-su-message) sexp)))
		    ((= conv-spec ?m)
		     (setq sexp (cons (list 'vm-su-month
					    'vm-su-message) sexp)))
		    ((= conv-spec ?M)
		     (setq sexp (cons (list 'vm-su-month-number
					    'vm-su-message) sexp)))
		    ((= conv-spec ?n)
		     (if tokenize
			 (setq token ''number)
		       (setq sexp (cons (list 'vm-padded-number-of
					      'vm-su-message) sexp))))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-su-subject
					    'vm-su-message) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-su-to-names
					    'vm-su-message) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-su-to
					    'vm-su-message) sexp)))
		    ((= conv-spec ?U)
		     (setq sexp
			   (cons (list 'vm-run-user-summary-function
				       (list 'quote
					     (intern
					      (concat
					       "vm-summary-function-"
					       (substring
						format
						(1+ (match-beginning 5))
						(+ 2 (match-beginning 5))))))
				       'vm-su-message) sexp)))
		    ((= conv-spec ?w)
		     (setq sexp (cons (list 'vm-su-weekday
					    'vm-su-message) sexp)))
		    ((= conv-spec ?y)
		     (setq sexp (cons (list 'vm-su-year
					    'vm-su-message) sexp)))
		    ((= conv-spec ?z)
		     (setq sexp (cons (list 'vm-su-zone
					    'vm-su-message) sexp)))
		    ((= conv-spec ?*)
		     (if tokenize
			 (setq token ''mark)
		       (setq sexp (cons (list 'vm-su-mark
					      'vm-su-message) sexp)))))
	      (cond ((and (not token) vm-display-using-mime)
		     (setcar sexp
			     (list 'vm-decode-mime-encoded-words-in-string
				   (car sexp)))))
	      (cond ((and (not token) (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((and (not token) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((and (not token) (match-beginning 3))
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (cond ((and (not token) vm-display-using-mime)
		     (setcar sexp
			     (list 'vm-reencode-mime-encoded-words-in-string
				   (car sexp)))))
	      (setq sexp-fmt
		    (cons (if token "" "%s")
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq saw-close-group t))
			"%%")
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	  (setq last-match-end new-match-end))
      (if (and (not saw-close-group) (not token))
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		finished-parsing-format t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt))
      (if tokenize
	  (setq list (nconc list (if (equal sexp "") nil (list sexp))
			    (and token (if splice token (list token))))
		sexp nil
		sexp-fmt nil)))
    (list last-match-end (if list (cons 'list list) sexp))))

(defun vm-get-header-contents (message header-name-regexp &optional clump-sep)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)")
	  message (vm-real-message-of message))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-restriction
	(widen)
	(goto-char (vm-headers-of message))
	(let ((case-fold-search t))
	  (while (and (or (null contents) clump-sep)
		      (re-search-forward regexp (vm-text-of message) t)
		      (save-excursion (goto-char (match-beginning 0))
				      (vm-match-header)))
	    (if contents
		(setq contents
		      (concat contents clump-sep (vm-matched-header-contents)))
	      (setq contents (vm-matched-header-contents))))))
      contents )))

;; Do not use Emacs 20's string-width here.
;; It does not consider buffer-display-table.
(defun vm-string-width (string)
  (if (not (fboundp 'char-width))
      (length string)
    (let ((i 0)
	  (lim (length string))
	  (total 0))
      (while (< i lim)
	(setq total (+ total (char-width (aref string i)))
	      i (1+ i)))
      total )))

(defun vm-left-justify-string (string width)
  (let ((sw (vm-string-width string)))
    (if (>= sw width)
	string
      (concat string (make-string (- width sw) ?\ )))))

(defun vm-right-justify-string (string width)
  (let ((sw (vm-string-width string)))
    (if (>= sw width)
	string
      (concat (make-string (- width sw) ?\ ) string))))

;; I don't think number glyphs ever have a width > 1
(defun vm-numeric-left-justify-string (string width)
  (let ((sw (length string)))
    (if (>= sw width)
	string
      (concat string (make-string (- width sw) ?0)))))

;; I don't think number glyphs ever have a width > 1
(defun vm-numeric-right-justify-string (string width)
  (let ((sw (length string)))
    (if (>= sw width)
	string
      (concat (make-string (- width sw) ?0) string))))

(defun vm-truncate-string (string width)
  (cond ((fboundp 'char-width)
	 (cond ((> width 0)
		(let ((i 0)
		      (lim (length string))
		      (total 0))
		  (while (and (< i lim) (< total width))
		    (setq total (+ total (char-width (aref string i)))
			  i (1+ i)))
		  (if (< total width)
		      string
		    (substring string 0 i))))
	       (t
		(let ((i (1- (length string)))
		      (lim -1)
		      (total 0))
		  (setq width (- width))
		  (while (and (> i lim) (< total width))
		    (setq total (+ total (char-width (aref string i)))
			  i (1- i)))
		  (if (< total width)
		      string
		    (substring string (1+ i)))))))
	(t (vm-truncate-roman-string string width))))

(defun vm-truncate-roman-string (string width)
  (cond ((<= (length string) (vm-abs width))
	 string)
	((< width 0)
	 (substring string width))
	(t
	 (substring string 0 width))))

(defun vm-su-attribute-indicators (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (cond ((vm-filed-flag m) "F")
	 ((vm-written-flag m) "W")
	 (t " "))
   (cond ((vm-replied-flag m) "R")
	 ((vm-forwarded-flag m) "Z")
	 ((vm-redistributed-flag m) "B")
	 (t " "))
   (cond ((vm-edited-flag m) "E")
	 (t " "))))

(defun vm-su-attribute-indicators-long (m)
  (concat
   (cond ((vm-deleted-flag m) "D")
	 ((vm-new-flag m) "N")
	 ((vm-unread-flag m) "U")
	 (t " "))
   (if (vm-replied-flag m) "r" " ")
   (if (vm-forwarded-flag m) "z" " ")
   (if (vm-redistributed-flag m) "b" " ")
   (if (vm-filed-flag m) "f" " ")
   (if (vm-written-flag m) "w" " ")
   (if (vm-edited-flag m) "e" " ")))

(defun vm-su-byte-count (m)
  (or (vm-byte-count-of m)
      (vm-set-byte-count-of
       m
       (int-to-string
	(- (vm-text-end-of (vm-real-message-of m))
	   (vm-text-of (vm-real-message-of m)))))))

(defun vm-su-weekday (m)
  (or (vm-weekday-of m)
      (progn (vm-su-do-date m) (vm-weekday-of m))))

(defun vm-su-monthday (m)
  (or (vm-monthday-of m)
      (progn (vm-su-do-date m) (vm-monthday-of m))))

(defun vm-su-month (m)
  (or (vm-month-of m)
      (progn (vm-su-do-date m) (vm-month-of m))))

(defun vm-su-month-number (m)
  (or (vm-month-number-of m)
      (progn (vm-su-do-date m) (vm-month-number-of m))))

(defun vm-su-year (m)
  (or (vm-year-of m)
      (progn (vm-su-do-date m) (vm-year-of m))))

(defun vm-su-hour-short (m)
  (let ((string (vm-su-hour m)))
    (if (> (length string) 5)
	(substring string 0 5)
      string)))

(defun vm-su-hour (m)
  (or (vm-hour-of m)
      (progn (vm-su-do-date m) (vm-hour-of m))))

(defun vm-su-zone (m)
  (or (vm-zone-of m)
      (progn (vm-su-do-date m) (vm-zone-of m))))

(defun vm-su-mark (m) (if (vm-mark-of m) "*" " "))

;; Some yogurt-headed delivery agents don't provide a Date: header.
(defun vm-grok-From_-date (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(BellFrom_ From_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of message)))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (or (looking-at
		     ;; special case this so that the "remote from blah"
		     ;; isn't included.
		     "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\) remote from .*")
		    (looking-at "From [^ \t\n]*[ \t]+\\([^ \t\n].*\\)"))
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-parse-date (date)
  (let ((weekday "")
	(monthday "")
	(month "")
	(year "")
	(hour "")
	(timezone "")
	(start nil)
	string
	(case-fold-search t))
    (if (string-match "sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat" date)
	(setq weekday (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec" date)
	(setq month (substring date (match-beginning 0) (match-end 0))))
    (if (string-match "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?" date)
	(setq hour (substring date (match-beginning 0) (match-end 0))))
    (cond ((string-match "[^a-z][+---][0-9][0-9][0-9][0-9]" date)
	   (setq timezone (substring date (1+ (match-beginning 0))
				     (match-end 0))))
	  ((or (string-match "e[ds]t\\|c[ds]t\\|p[ds]t\\|m[ds]t" date)
	       (string-match "ast\\|nst\\|met\\|eet\\|jst\\|bst\\|ut" date)
	       (string-match "gmt\\([+---][0-9]+\\)?" date))
	   (setq timezone (substring date (match-beginning 0) (match-end 0)))))
    (while (and (or (zerop (length monthday))
		    (zerop (length year)))
		(string-match "\\(^\\| \\)\\([0-9]+\\)\\($\\| \\)" date start))
      (setq string (substring date (match-beginning 2) (match-end 2))
	    start (match-end 0))
      (cond ((and (zerop (length monthday))
		  (<= (length string) 2))
	     (setq monthday string))
	    ((= (length string) 2)
	     (if (< (string-to-int string) 70)
		 (setq year (concat "20" string))
	       (setq year (concat "19" string))))
	    (t (setq year string))))
    
    (aset vm-parse-date-workspace 0 weekday)
    (aset vm-parse-date-workspace 1 monthday)
    (aset vm-parse-date-workspace 2 month)
    (aset vm-parse-date-workspace 3 year)
    (aset vm-parse-date-workspace 4 hour)
    (aset vm-parse-date-workspace 5 timezone)
    vm-parse-date-workspace))

(defun vm-su-do-date (m)
  (let ((case-fold-search t)
	vector date)
    (setq date (or (vm-get-header-contents m "Date:") (vm-grok-From_-date m)))
    (cond
     ((null date)
      (vm-set-weekday-of m "")
      (vm-set-monthday-of m "")
      (vm-set-month-of m "")
      (vm-set-month-number-of m "")
      (vm-set-year-of m "")
      (vm-set-hour-of m "")
      (vm-set-zone-of m ""))
     ((string-match
;; The date format recognized here is the one specified in RFC 822.
;; Some slop is allowed e.g. dashes between the monthday, month and year
;; because such malformed headers have been observed.
"\\(\\([a-z][a-z][a-z]\\),\\)?[ \t\n]*\\([0-9][0-9]?\\)[ \t\n---]*\\([a-z][a-z][a-z]\\)[ \t\n---]*\\([0-9]*[0-9][0-9]\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|\\(-\\|\\+\\)[01][0-9][0-9][0-9]\\)"
       date)
      (if (match-beginning 2)
	  (vm-su-do-weekday m (substring date (match-beginning 2)
					    (match-end 2)))
	(vm-set-weekday-of m ""))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-su-do-month m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (= 2 (length (vm-year-of m)))
	  (save-match-data
	    (cond ((string-match "^[0-6]" (vm-year-of m))
		   (vm-set-year-of m (concat "20" (vm-year-of m))))
		  (t
		   (vm-set-year-of m (concat "19" (vm-year-of m)))))))
      (vm-set-hour-of m (substring date (match-beginning 6) (match-end 6)))
      (vm-set-zone-of m (substring date (match-beginning 7) (match-end 7))))
     ((string-match
;; UNIX ctime(3) format, with slop allowed in the whitespace, and we allow for
;; the possibility of a timezone at the end.
"\\([a-z][a-z][a-z]\\)[ \t\n]*\\([a-z][a-z][a-z]\\)[ \t\n]*\\([0-9][0-9]?\\)[ \t\n]*\\([0-9:]+\\)[ \t\n]*\\([0-9][0-9][0-9][0-9]\\)[ \t\n]*\\([a-z][a-z]?[a-z]?\\|\\(-\\|\\+\\)[01][0-9][0-9][0-9]\\)?"
       date)
      (vm-su-do-weekday m (substring date (match-beginning 1)
				     (match-end 1)))
      (vm-su-do-month m (substring date (match-beginning 2) (match-end 2)))
      (vm-set-monthday-of m (substring date (match-beginning 3) (match-end 3)))
      (vm-set-hour-of m (substring date (match-beginning 4) (match-end 4)))
      (vm-set-year-of m (substring date (match-beginning 5) (match-end 5)))
      (if (match-beginning 6)
	  (vm-set-zone-of m (substring date (match-beginning 6)
				       (match-end 6)))
	(vm-set-zone-of m "")))
     (t
      (setq vector (vm-parse-date date))
      (vm-su-do-weekday m (elt vector 0))
      (vm-set-monthday-of m (elt vector 1))
      (vm-su-do-month m (elt vector 2))
      (vm-set-year-of m (elt vector 3))
      (vm-set-hour-of m (elt vector 4))
      (vm-set-zone-of m (elt vector 5)))))

  ;; Normalize all hour and date specifications to avoid jagged margins.
  ;; If the hour is " 3:..." or "3:...", turn it into "03:...".
  ;; If the date is "03", turn it into " 3".
  (cond ((null (vm-hour-of m)) nil)
	((string-match "\\`[0-9]:" (vm-hour-of m))
	 (vm-set-hour-of m (concat "0" (vm-hour-of m)))))
  (cond ((null (vm-monthday-of m)) nil)
	((string-match "\\`0[0-9]\\'" (vm-monthday-of m))
	 (vm-set-monthday-of m (substring (vm-monthday-of m) 1 2))))
  )

(defun vm-su-do-month (m month-abbrev)
  (let ((val (assoc (downcase month-abbrev) vm-month-alist)))
    (if val
	(progn (vm-set-month-of m (nth 1 val))
	       (vm-set-month-number-of m (nth 2 val)))
      (vm-set-month-of m "")
      (vm-set-month-number-of m ""))))

(defun vm-su-do-weekday (m weekday-abbrev)
  (let ((val (assoc (downcase weekday-abbrev) vm-weekday-alist)))
    (if val
	(vm-set-weekday-of m (nth 1 val))
      (vm-set-weekday-of m ""))))

(defun vm-run-user-summary-function (function message)
  (let ((message (vm-real-message-of message)))
    (save-excursion
      (set-buffer (vm-buffer-of message))
      (save-restriction
	(widen)
	(save-excursion
	  (narrow-to-region (vm-headers-of message) (vm-text-end-of message))
	  (funcall function message))))))

(defun vm-su-full-name (m)
  (or (vm-full-name-of m)
      (progn (vm-su-do-author m) (vm-full-name-of m))))

(defun vm-su-interesting-full-name (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to-names m))
	  (vm-su-full-name m)))
    (vm-su-full-name m)))

(defun vm-su-from (m)
  (or (vm-from-of m)
      (progn (vm-su-do-author m) (vm-from-of m))))

(defun vm-su-interesting-from (m)
  (if vm-summary-uninteresting-senders
      (let ((case-fold-search nil))
	(if (string-match vm-summary-uninteresting-senders (vm-su-from m))
	    (concat vm-summary-uninteresting-senders-arrow (vm-su-to m))
	  (vm-su-from m)))
    (vm-su-from m)))

;; Some yogurt-headed delivery agents don't even provide a From: header.
(defun vm-grok-From_-author (message)
  ;; This works only on the From_ types, obviously
  (if (not (memq (vm-message-type-of message)
		 '(From_ BellFrom_ From_-with-Content-Length)))
      nil
    (save-excursion
      (set-buffer (vm-buffer-of message))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (vm-start-of message))
	  (let ((case-fold-search nil))
	    (if (looking-at "From \\([^ \t\n]+\\)")
		(vm-buffer-substring-no-properties
		 (match-beginning 1)
		 (match-end 1)))))))))

(defun vm-su-do-author (m)
  (let ((full-name (vm-get-header-contents m "Full-Name:"))
	(from (or (vm-get-header-contents m "From:" ", ")
		  (vm-grok-From_-author m)))
	pair i)
    (if (and full-name (string-match "^[ \t]*$" full-name))
	(setq full-name nil))
    (if (null from)
	(progn
	  (setq from "???")
	  (if (null full-name)
	      (setq full-name "???")))
      (setq pair (funcall vm-chop-full-name-function from)
	    from (or (nth 1 pair) from)
	    full-name (or full-name (nth 0 pair) from)))
    (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
 	(setq full-name
 	      (substring full-name (match-beginning 1) (match-end 1))))
    (while (setq i (string-match "\n" full-name i))
      (aset full-name i ?\ ))
    (vm-set-full-name-of m full-name)
    (vm-set-from-of m from)))

(defun vm-default-chop-full-name (address)
  (let ((from address)
	(full-name nil))
    (cond ((string-match
"\\`[ \t\n]*\\([^< \t\n]+\\([ \t\n]+[^< \t\n]+\\)*\\)?[ \t\n]*<\\([^>]+\\)>[ \t\n]*\\'"
			 address)
	   (if (match-beginning 1)
	       (setq full-name
		     (substring address (match-beginning 1) (match-end 1))))
	   (setq from
		 (substring address (match-beginning 3) (match-end 3))))
	  ((string-match
"\\`[ \t\n]*\\(\\(\"[^\"]+\"\\|[^\"( \t\n]\\)+\\)[ \t\n]*(\\([^ \t\n]+\\([ \t\n]+[^ \t\n]+\\)*\\)?)[ \t\n]*\\'"
			 address)
	   (if (match-beginning 3)
	       (setq full-name
		     (substring address (match-beginning 3) (match-end 3))))
	   (setq from
		 (substring address (match-beginning 1) (match-end 1)))))
    (list full-name from)))

;; test for existence and functionality of mail-extract-address-components
;; there are versions out there that don't work right, so we run
;; some test data through it to see if we can trust it.
(defun vm-choose-chop-full-name-function (address)
  (let ((test-data '(("kyle@uunet.uu.net" .
		      (nil "kyle@uunet.uu.net"))
		     ("c++std=lib@inet.research.att.com" .
		      (nil "c++std=lib@inet.research.att.com"))
		     ("\"Piet.Rypens\" <rypens@reks.uia.ac.be>" .
		      ("Piet Rypens" "rypens@reks.uia.ac.be"))
		     ("makke@wins.uia.ac.be (Marc.Gemis)" .
		      ("Marc Gemis" "makke@wins.uia.ac.be"))
		     ("" . (nil nil))))
	(failed nil)
	result)
    (while test-data
      (setq result (condition-case nil
		       (mail-extract-address-components (car (car test-data)))
		     (error nil)))
      (if (not (equal result (cdr (car test-data))))
	  ;; failed test, use default
	  (setq failed t
		test-data nil)
	(setq test-data (cdr test-data))))
    (if failed
	;; it failed, use default
	(setq vm-chop-full-name-function 'vm-default-chop-full-name)
      ;; it passed the tests
      (setq vm-chop-full-name-function 'mail-extract-address-components))
    (funcall vm-chop-full-name-function address)))

(defun vm-su-do-recipients (m)
  (let ((mail-use-rfc822 t) i names addresses to cc all list full-name)
    (setq to (or (vm-get-header-contents m "To:" ", ")
		 (vm-get-header-contents m "Apparently-To:" ", ")
		 (vm-get-header-contents m "Newsgroups:" ", ")
		 ;; desperation....
		 (user-login-name))
	  cc (vm-get-header-contents m "Cc:" ", ")
	  all to
	  all (if all (concat all ", " cc) cc)
	  addresses (rfc822-addresses all))
    (setq list (vm-parse-addresses all))
    (while list
      ;; Just like vm-su-do-author:
      (setq full-name (or (nth 0 (funcall vm-chop-full-name-function
					  (car list)))
			  (car list)))
      ;; If double quotes are around the full name, fish the name out.
      (if (string-match "\\`\"\\([^\"]+\\)\"\\'" full-name)
	  (setq full-name
		(substring full-name (match-beginning 1) (match-end 1))))
      (while (setq i (string-match "\n" full-name i))
	(aset full-name i ?\ ))
      (setq names (cons full-name names))
      (setq list (cdr list)))
    (setq names (nreverse names)) ; added by jwz for fixed vm-parse-addresses
    (vm-set-to-of m (mapconcat 'identity addresses ", "))
    (vm-set-to-names-of m (mapconcat 'identity names ", "))))

(defun vm-su-to (m)
  (or (vm-to-of m) (progn (vm-su-do-recipients m) (vm-to-of m))))

(defun vm-su-to-names (m)
  (or (vm-to-names-of m) (progn (vm-su-do-recipients m) (vm-to-names-of m))))
				  
(defun vm-su-message-id (m)
  (or (vm-message-id-of m)
      (vm-set-message-id-of
       m
       (or (let ((id (vm-get-header-contents m "Message-Id:")))
	     (and id (car (vm-parse id "[^<]*\\(<[^>]+>\\)"))))
	   ;; try running md5 on the message body to produce an ID
	   ;; better than nothing.
	   (save-excursion
	     (set-buffer (vm-buffer-of (vm-real-message-of m)))
	     (save-restriction
	       (widen)
	       (condition-case nil
		   (concat "<fake-VM-id."
			   (vm-pop-md5-string
			    (buffer-substring
			     (vm-text-of (vm-real-message-of m))
			     (vm-text-end-of (vm-real-message-of m))))
			   "@talos.iv>")
		 (error nil))))
	   (concat "<" (int-to-string (vm-abs (random))) "@toto.iv>")))))

(defun vm-su-line-count (m)
  (or (vm-line-count-of m)
      (vm-set-line-count-of
       m
       (save-excursion
	 (set-buffer (vm-buffer-of (vm-real-message-of m)))
	 (save-restriction
	   (widen)
	   (int-to-string
	    (count-lines (vm-text-of (vm-real-message-of m))
			 (vm-text-end-of (vm-real-message-of m)))))))))

(defun vm-su-subject (m)
  (or (vm-subject-of m)
      (vm-set-subject-of
       m
       (let ((subject (or (vm-get-header-contents m "Subject:" " ") ""))
	     (i nil))
	 (while (string-match "\n[ \t]*" subject)
	   (setq subject (replace-match " " nil t subject)))
	 subject ))))

(defun vm-su-summary (m)
  (if (and (vm-virtual-message-p m) (not (vm-virtual-messages-of m)))
      (or (vm-virtual-summary-of m)
	  (save-excursion
	    (vm-select-folder-buffer)
	    (vm-set-virtual-summary-of m (vm-summary-sprintf
					  vm-summary-format m t))
	    (vm-virtual-summary-of m)))
    (or (vm-summary-of m)
	(save-excursion
	  (vm-select-folder-buffer)
	  (vm-set-summary-of m (vm-summary-sprintf vm-summary-format m t))
	  (vm-summary-of m)))))

(defun vm-fix-my-summary!!! ()
  (interactive)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (message "Fixing your summary...")
  (let ((mp vm-message-list))
    (while mp
      (vm-set-summary-of (car mp) nil)
      (vm-mark-for-summary-update (car mp))
      (vm-set-stuff-flag-of (car mp) t)
      (setq mp (cdr mp)))
    (message "Stuffing attributes...")
    (vm-stuff-folder-attributes nil)
    (message "Stuffing attributes... done")
    (set-buffer-modified-p t)
    (vm-update-summary-and-mode-line))
  (message "Fixing your summary... done"))

(defun vm-su-thread-indent (m)
  (if (and vm-summary-show-threads (natnump vm-summary-thread-indent-level))
      (make-string (* (vm-th-thread-indentation m)
		      vm-summary-thread-indent-level)
		   ?\ )
    "" ))

(defun vm-su-labels (m)
  (or (vm-label-string-of m)
      (vm-set-label-string-of
       m
       (mapconcat 'identity (vm-labels-of m) ","))
      (vm-label-string-of m)))

(defun vm-substring (string from &optional to)
  (let ((work-buffer nil))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert string)
	  (if (null to)
	      (setq to (length string))
	    (if (< to 0)
		(setq to (+ (length string) to))))
	  ;; string indices start at 0, buffers start at 1.
	  (setq from (1+ from)
		to (1+ to))
	  (if (> from (point-min))
	      (delete-region (point-min) from))
	  (if (< to (point-max))
	      (delete-region to (point-max)))
	  (buffer-string))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-make-folder-summary ()
  (make-vector vm-folder-summary-vector-length nil))

(defun vm-fs-folder-of (fs) (aref fs 0))
(defun vm-fs-total-count-of (fs) (aref fs 1))
(defun vm-fs-new-count-of (fs) (aref fs 2))
(defun vm-fs-unread-count-of (fs) (aref fs 3))
(defun vm-fs-deleted-count-of (fs) (aref fs 4))
(defun vm-fs-start-of (fs) (aref fs 5))
(defun vm-fs-end-of (fs) (aref fs 6))
(defun vm-fs-folder-key-of (fs) (aref fs 7))
(defun vm-fs-mouse-track-overlay-of (fs) (aref fs 8))
(defun vm-fs-short-folder-of (fs) (aref fs 9))
(defun vm-fs-modflag-of (fs) (aref fs 10))

(defun vm-set-fs-folder-of (fs x) (aset fs 0 x))
(defun vm-set-fs-total-count-of (fs x) (aset fs 1 x))
(defun vm-set-fs-new-count-of (fs x) (aset fs 2 x))
(defun vm-set-fs-unread-count-of (fs x) (aset fs 3 x))
(defun vm-set-fs-deleted-count-of (fs x) (aset fs 4 x))
(defun vm-set-fs-start-of (fs x) (aset fs 5 x))
(defun vm-set-fs-end-of (fs x) (aset fs 6 x))
(defun vm-set-fs-folder-key-of (fs x) (aset fs 7 x))
(defun vm-set-fs-mouse-track-overlay-of (fs x) (aset fs 8 x))
(defun vm-set-fs-short-folder-of (fs x) (aset fs 9 x))
(defun vm-set-fs-modflag-of (fs x) (aset fs 10 x))

(defun vm-fs-spooled (fs)
  (let ((count 0)
	(list (symbol-value
	       (intern-soft (vm-fs-folder-key-of fs)
			    vm-folders-summary-folder-hash))))
    (while list
      (setq count (+ count (car (vm-get-folder-totals (car list))))
	    list (cdr list)))
    (int-to-string count)))

(defun vm-make-folders-summary-key (folder &optional dir)
  (cond ((and (stringp vm-recognize-pop-maildrops)
	      (string-match vm-recognize-pop-maildrops folder))
	 (vm-safe-popdrop-string folder))
	((and (stringp vm-recognize-imap-maildrops)
	      (string-match vm-recognize-imap-maildrops folder))
	 (vm-safe-imapdrop-string folder))
	(t
	 (concat "folder-summary0:"
		 (file-truename
		  (expand-file-name folder (or dir vm-folder-directory)))))))

(defun vm-open-folders-summary-database (mode)
  (condition-case data
      (open-database vm-folders-summary-database 'berkeley-db 'hash mode)
    (error (message "open-database signaled: %S" data)
	   (sleep-for 2)
	   nil )))

(defun vm-get-folder-totals (folder)
  (let ((default "(0 0 0 0)") fs db key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done (read default)))
      (if (not (featurep 'berkeley-db))
	  (throw 'done (read default)))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done (read default)))
      (setq key (vm-make-folders-summary-key folder)
	    data (read (get-database key db default)))
      (close-database db)
      data )))

(defun vm-store-folder-totals (folder totals)
  (let (fs db key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done nil))
      (if (not (featurep 'berkeley-db))
	  (throw 'done nil))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done nil))
      (setq key (vm-make-folders-summary-key folder)
	    data (prin1-to-string totals))
      (put-database key data db t)
      (close-database db)
      (if (null vm-folders-summary-hash)
	  nil
	(setq fs (intern-soft key vm-folders-summary-hash)
	      fs (symbol-value fs))
	(if (null fs)
	    nil
	  (vm-set-fs-total-count-of fs (int-to-string (car totals)))
	  (vm-set-fs-new-count-of fs (int-to-string (nth 1 totals)))
	  (vm-set-fs-unread-count-of fs (int-to-string (nth 2 totals)))
	  (vm-set-fs-deleted-count-of fs (int-to-string (nth 3 totals)))))
      (vm-mark-for-folders-summary-update folder))))

(defun vm-modify-folder-totals (folder action &rest objects)
  (let (fs db totals key data)
    (catch 'done
      (if (null vm-folders-summary-database)
	  (throw 'done nil))
      (if (not (featurep 'berkeley-db))
	  (throw 'done nil))
      (if (null (setq db (vm-open-folders-summary-database "r")))
	  (throw 'done nil))
      (setq key (vm-make-folders-summary-key folder))
      (setq totals (get-database key db))
      (close-database db)
      (if (null totals)
	  (throw 'done nil))
      (setq totals (read totals))
      (cond ((eq action 'arrived)
	     (let ((arrived (car objects)) c n)
	       (setcar totals (+ (car totals) arrived))
	       (setq c (cdr totals))
	       (setcar c (+ (car c) arrived))))
	    ((eq action 'saved)
	     (let ((arrived (car objects))
		   (m (nth 1 objects)) c n)
	       (setcar totals (+ (car totals) arrived))
	       ;; increment new and unread counts if necessary.
	       ;; messages are never saved with the deleted flag
	       ;; set no need to check that.
	       (setq c (cdr totals))
	       (if (eq (car c) -1)
		   nil
		 (if (vm-new-flag m)
		     (setcar c (+ (car c) arrived))))
	       (setq c (cdr c))
	       (if (eq (car c) -1)
		   nil
		 (if (vm-unread-flag m)
		     (setcar c (+ (car c) arrived)))))))
      (setq data (prin1-to-string totals))
      (if (null (setq db (vm-open-folders-summary-database "rw+")))
	  (throw 'done nil))
      (put-database key data db t)
      (close-database db)
      (if (null vm-folders-summary-hash)
	  nil
	(setq fs (intern-soft key vm-folders-summary-hash)
	      fs (symbol-value fs))
	(if (null fs)
	    nil
	  (vm-set-fs-total-count-of fs (int-to-string (car totals)))
	  (vm-set-fs-new-count-of fs (int-to-string (nth 1 totals)))
	  (vm-set-fs-unread-count-of fs (int-to-string (nth 2 totals)))
	  (vm-set-fs-deleted-count-of fs (int-to-string (nth 3 totals)))))
      (vm-mark-for-folders-summary-update folder))))

(defun vm-folders-summary-sprintf (format layout)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let ((match (assoc format vm-folders-summary-compiled-format-alist)))
    (if (null match)
	(progn
	  (vm-folders-summary-compile-format format)
	  (setq match
		(assoc format vm-folders-summary-compiled-format-alist))))
    ;; The local variable name `vm-folder-summary' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-folder-summary layout))
      (eval (cdr match)))))

(defun vm-folders-summary-compile-format (format)
  (let ((return-value (vm-folders-summary-compile-format-1 format 0)))
    (setq vm-folders-summary-compiled-format-alist
	  (cons (cons format (nth 1 return-value))
		vm-folders-summary-compiled-format-alist))))

(defun vm-folders-summary-compile-format-1 (format start-index)
  (let ((case-fold-search nil)
	(done nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end start-index)
	new-match-end conv-spec)
    (store-match-data nil)
    (while (not done)
      (while
	  (and (not done)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()dfnstu%]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (memq conv-spec '(?\( ?d ?f ?n ?s ?t ?u))
	    (progn
	      (cond ((= conv-spec ?\()
		     (save-match-data
		       (let ((retval
			      (vm-folder-summary-compile-format-1
			       format
			       (match-end 5))))
			 (setq sexp (cons (nth 1 retval) sexp)
			       new-match-end (car retval)))))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-fs-deleted-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-fs-short-folder-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?n)
		     (setq sexp (cons (list 'vm-fs-new-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-fs-total-count-of
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-fs-spooled
					    'vm-folder-summary) sexp)))
		    ((= conv-spec ?u)
		     (setq sexp (cons (list 'vm-fs-unread-count-of
					    'vm-folder-summary) sexp))))
	      (cond ((and (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((match-beginning 2)
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((match-beginning 3)
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (setq sexp-fmt
		    (cons "%s"
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq done t))
			"%%")
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	(setq last-match-end new-match-end))
      (if (not done)
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt)))
    (list last-match-end sexp)))

(defun vm-update-folders-summary-entry (fs)
  (if (and (vm-fs-start-of fs)
	   (marker-buffer (vm-fs-start-of fs)))
      (let ((modified (buffer-modified-p))
	    (do-mouse-track
	     (and vm-mouse-track-summary
		  (vm-mouse-support-possible-p)))
	    summary)
	(save-excursion
	  (set-buffer (marker-buffer (vm-fs-start-of fs)))
	  (let ((buffer-read-only nil))
	    (unwind-protect
		(save-excursion
		  (goto-char (vm-fs-start-of fs))
		  ;; We do a little dance to update the text in
		  ;; order to make the markers in the text do
		  ;; what we want.
		  ;;
		  ;; 1. We need to avoid having the start
		  ;;    and end markers clumping together at
		  ;;    the start position.
		  ;;
		  ;; 2. We want the window point marker (w->pointm
		  ;;    in the Emacs display code) to move to the
		  ;;    start of the summary entry if it is
		  ;;    anywhere within the su-start-of to
		  ;;    su-end-of region.
		  ;;
		  ;; We achieve (2) by deleting before inserting.
		  ;; Reversing the order of insertion/deletion
		  ;; pushes the point marker into the next
		  ;; summary entry. We achieve (1) by inserting a
		  ;; placeholder character at the end of the
		  ;; summary entry before deleting the region.
		  (goto-char (vm-fs-end-of fs))
		  (insert-before-markers "z")
		  (goto-char (vm-fs-start-of fs))
		  (delete-region (point) (1- (vm-fs-end-of fs)))
		  (insert
		   (vm-folders-summary-sprintf vm-folders-summary-format fs))
		  (delete-char 1)
		  (and do-mouse-track
		       (vm-mouse-set-mouse-track-highlight
			(vm-fs-start-of fs)
			(vm-fs-end-of fs)
			(vm-fs-mouse-track-overlay-of fs))))
	      (set-buffer-modified-p modified)))))))

(defun vm-folders-summary-mode-internal ()
  (setq mode-name "VM Folders Summary"
	major-mode 'vm-folders-summary-mode
	mode-line-format '("     %b")
	;; must come after the setting of major-mode
	mode-popup-menu (and vm-use-menus
			     (vm-menu-support-possible-p)
			     (vm-menu-mode-menu))
	buffer-read-only t
	buffer-offer-save nil
	truncate-lines t)
  (and vm-xemacs-p (featurep 'scrollbar)
       (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (use-local-map vm-folders-summary-mode-map)
  (and (vm-menu-support-possible-p)
       (vm-menu-install-menus))
  (if (and vm-mutable-frames vm-frame-per-folders-summary)
      (vm-set-hooks-for-frame-deletion))
  (run-hooks 'vm-folders-summary-mode-hook))

(defun vm-do-folders-summary ()
  (catch 'done
    (let ((fs-hash (make-vector 89 0)) db dp fp f key fs totals
          (format vm-folders-summary-format)
	  (do-mouse-track (and vm-mouse-track-summary
			       (vm-mouse-support-possible-p))))
      (save-excursion
	(set-buffer vm-folders-summary-buffer)
	(erase-buffer)
	(let ((buffer-read-only nil))
	  (if (null vm-folders-summary-database)
	      (throw 'done nil))
	  (if (not (featurep 'berkeley-db))
	      (throw 'done nil))
	  (if (null (setq db (vm-open-folders-summary-database "r")))
	      (throw 'done nil))
	  (setq dp vm-folders-summary-directories)
	  (while dp
	    (if (cdr vm-folders-summary-directories)
		(insert (car dp) ":\n"))
	    (let ((default-directory (car dp)))
	      (setq fp (sort (vm-delete-backup-file-names
			      (vm-delete-auto-save-file-names
			       (vm-delete-index-file-names
				(vm-delete-directory-names
				 (directory-files (car dp))))))
			     (function string-lessp))))
	    (while fp
	      (setq f (car fp)
		    key (vm-make-folders-summary-key f (car dp))
		    totals (get-database key db))
	      (if (null totals)
		  (let ((ff (expand-file-name f (car dp))))
		    (setq totals (list (or (vm-count-messages-in-file ff) -1)
				       -1 -1 -1))
		    (if (eq (car totals) -1)
			nil
		      (vm-store-folder-totals ff totals)))
		(setq totals (read totals)))
	      (if (eq (car totals) -1)
		  nil
		(setq fs (vm-make-folder-summary))
		(vm-set-fs-folder-of fs (expand-file-name f (car dp)))
		(vm-set-fs-short-folder-of fs f)
		(vm-set-fs-total-count-of fs (vm-nonneg-string (car totals)))
		(vm-set-fs-new-count-of fs (vm-nonneg-string (nth 1 totals)))
		(vm-set-fs-unread-count-of fs (vm-nonneg-string
					       (nth 2 totals)))
		(vm-set-fs-deleted-count-of fs (vm-nonneg-string
						(nth 3 totals)))
		(vm-set-fs-folder-key-of fs key)
		(vm-set-fs-start-of fs (vm-marker (point)))
		(insert (vm-folders-summary-sprintf format fs))
		(vm-set-fs-end-of fs (vm-marker (point)))
		(and do-mouse-track
		     (vm-set-fs-mouse-track-overlay-of
		      fs
		      (vm-mouse-set-mouse-track-highlight
		       (vm-fs-start-of fs)
		       (vm-fs-end-of fs))))
		(set (intern key fs-hash) fs))
	      (setq fp (cdr fp)))
	    (setq dp (cdr dp)))
	  (close-database db)
	  (setq vm-folders-summary-hash fs-hash))
	(goto-char (point-min))))))

(defun vm-update-folders-summary-highlight ()
  (if (or (null vm-mail-buffer)
	  (null (buffer-file-name vm-mail-buffer))
	  (null vm-folders-summary-hash))
      (progn
	(and vm-folders-summary-overlay
	     (vm-set-extent-endpoints vm-folders-summary-overlay 1 1))
	(setq vm-mail-buffer nil))
    (let ((ooo vm-folders-summary-overlay)
	  (fs (symbol-value (intern-soft (vm-make-folders-summary-key
					  (buffer-file-name vm-mail-buffer))
					 vm-folders-summary-hash))))
      (if (and fs
	       (or (null ooo)
		   (null (vm-extent-object ooo))
		   (/= (vm-extent-end-position ooo)
		       (vm-fs-end-of fs))))
	  (vm-folders-summary-highlight-region
	   (vm-fs-start-of fs) (vm-fs-end-of fs)
	   vm-summary-highlight-face)))))

(defun vm-do-needed-folders-summary-update ()
  (if (null vm-folders-summary-buffer)
      nil
    (save-excursion
      (set-buffer vm-folders-summary-buffer)
      (if (or (eq vm-modification-counter vm-flushed-modification-counter)
	      (null vm-folders-summary-hash))
	  nil
	(mapatoms
	 (function
	  (lambda (sym)
	    (let ((fs (symbol-value sym)))
	      (if (null (vm-fs-modflag-of fs))
		  nil
		(vm-update-folders-summary-entry fs)
		(vm-set-fs-modflag-of fs nil)))))
	  vm-folders-summary-hash)
	(vm-update-folders-summary-highlight)
	(setq vm-flushed-modification-counter vm-modification-counter)))))

(defun vm-mark-for-folders-summary-update (folder &optional dont-descend)
  (let ((key (vm-make-folders-summary-key folder))
	(hash vm-folders-summary-hash)
	(spool-hash vm-folders-summary-spool-hash)
	list fs )
    (setq fs (symbol-value (intern-soft key hash)))
    (if (not fs)
	nil
      (vm-set-fs-modflag-of fs t)
      (vm-check-for-killed-summary)
      (if vm-folders-summary-buffer
	  (save-excursion
	    (set-buffer vm-folders-summary-buffer)
	    (vm-increment vm-modification-counter))))
    (if dont-descend
	nil
      (setq list (symbol-value (intern-soft key spool-hash)))
      (while list
	(vm-mark-for-folders-summary-update (car list) t)
	(setq list (cdr list))))))

(defun vm-make-folders-summary-associative-hashes ()
  (let ((triples (vm-compute-spool-files t))
	(spool-hash (make-vector 61 0))
	(folder-hash (make-vector 61 0))
	s-list f-list folder-key spool-key)
    (while triples
      (setq folder-key (vm-make-folders-summary-key (car (car triples)))
	    spool-key (vm-make-folders-summary-key (nth 1 (car triples)))
	    s-list (symbol-value (intern-soft spool-key spool-hash))
	    s-list (cons (car (car triples)) s-list)
	    f-list (symbol-value (intern-soft folder-key folder-hash))
	    f-list (cons (nth 1 (car triples)) f-list)
	    triples (cdr triples))
      (set (intern spool-key spool-hash) s-list)
      (set (intern folder-key folder-hash) f-list))
    (setq vm-folders-summary-spool-hash spool-hash)
    (setq vm-folders-summary-folder-hash folder-hash)))

(defun vm-follow-folders-summary-cursor ()
  (if (or (not (eq major-mode 'vm-folders-summary-mode))
	  (null vm-folders-summary-hash))
      nil
    (catch 'done
      (mapatoms
       (function
	(lambda (sym)
	  (let ((fs (symbol-value sym)))
	    (if (and (>= (point) (vm-fs-start-of fs))
		     (< (point) (vm-fs-end-of fs))
		     (or (null vm-mail-buffer)
			 (not (eq vm-mail-buffer
				  (vm-get-file-buffer (vm-fs-folder-of fs))))))
		(progn
		  (setq vm-mail-buffer
			(save-excursion
			  (vm-visit-folder (vm-fs-folder-of fs))
			  (current-buffer)))
		  (vm-increment vm-modification-counter)
		  (vm-update-summary-and-mode-line)
		  (throw 'done t))))))
       vm-folders-summary-hash)
      nil )))

(provide 'vm-summary)

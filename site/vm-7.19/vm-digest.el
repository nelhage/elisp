;;; Message encapsulation
;;; Copyright (C) 1989, 1990, 1993, 1994, 1997, 2001 Kyle E. Jones
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

;;(provide 'vm-digest)

(defun vm-no-frills-encapsulate-message (m keep-list discard-regexp)
  "Encapsulate a message M for forwarding, simply.
No message encapsulation standard is used.  The message is
inserted at point in the current buffer, surrounded by two dashed
start/end separator lines.  Point is not moved.

M should be a message struct for a real message, not a virtual message.
This is the message that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (let ((target-buffer (current-buffer))
	source-buffer)
    (save-restriction
      ;; narrow to a zero length region to avoid interacting
      ;; with anything that might have already been inserted
      ;; into the buffer.
      (narrow-to-region (point) (point))
      (insert "------- start of forwarded message -------\n")
      (setq source-buffer (vm-buffer-of m))
      (save-excursion
	(set-buffer source-buffer)
	(save-restriction
	  (widen)
	  (save-excursion
	    (set-buffer target-buffer)
	    (let ((beg (point)))
	      (insert-buffer-substring source-buffer (vm-headers-of m)
				       (vm-text-end-of m))
	      (goto-char beg)
	      (vm-reorder-message-headers nil nil vm-internal-unforwarded-header-regexp)
	      (vm-reorder-message-headers nil keep-list discard-regexp)))))
      (goto-char (point-max))
      (insert "------- end of forwarded message -------\n"))))

(defun vm-mime-encapsulate-messages (message-list keep-list discard-regexp
				     always-use-digest)
  "Encapsulate the messages in MESSAGE-LIST as per the MIME spec.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used.

If ALWAYS-USE-DIGEST is non-nil, always encapsulate for a multipart/digest.
Otherwise if there is only one message to be encapsulated
leave off the multipart boundary strings.  The caller is assumed to
be using message/rfc822 or message/news encoding instead.

If multipart/digest encapsulation is done, the function returns
the multipart boundary parameter (string) that should be used in
the Content-Type header.  Otherwise nil is returned."
  (if message-list
      (let ((target-buffer (current-buffer))
	    (boundary-positions nil)
	    (mlist message-list)
	    (boundary nil)
	    source-buffer m start n beg)
	(save-restriction
	  ;; narrow to a zero length region to avoid interacting
	  ;; with anything that might have already been inserted
	  ;; into the buffer.
	  (narrow-to-region (point) (point))
	  (setq start (point))
	  (while mlist
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (setq m (vm-real-message-of (car mlist))
		  source-buffer (vm-buffer-of m))
	    (setq beg (point))
	    (vm-insert-region-from-buffer source-buffer (vm-headers-of m)
					  (vm-text-end-of m))
	    (goto-char beg)
	    ;; remove the Berkeley and VM status headers and sort
	    ;; the MIME headers to the top of the message.
	    (vm-reorder-message-headers nil vm-mime-header-list
					vm-internal-unforwarded-header-regexp)
	    ;; skip past the MIME headers so that when the
	    ;; user's header filters are applied they won't
	    ;; remove the MIME headers.
	    (while (and (vm-match-header) (looking-at vm-mime-header-regexp))
	      (goto-char (vm-matched-header-end)))
	    ;; apply the user's header filters.
	    (vm-reorder-message-headers nil keep-list discard-regexp)
	    (goto-char (point-max))
	    (setq mlist (cdr mlist)))
	  (if (and (< (length message-list) 2) (not always-use-digest))
	      nil
	    (goto-char start)
	    (setq boundary (vm-mime-make-multipart-boundary))
	    (while (re-search-forward (concat "^--"
					      (regexp-quote boundary)
					      "\\(--\\)?$")
				      nil t)
	      (setq boundary (vm-mime-make-multipart-boundary))
	      (goto-char start))
	    (goto-char (point-max))
	    (insert "\n--" boundary "--\n")
	    (while boundary-positions
	      (goto-char (car boundary-positions))
	      (insert "\n--" boundary "\n\n")
	      (setq boundary-positions (cdr boundary-positions)))
	    (goto-char start)
	    (setq n (length message-list))
	    (insert
	     (format "This is a digest, %d message%s, MIME encapsulation.\n"
		     n (if (= n 1) "" "s"))))
	  (goto-char start))
	boundary )))

(defun vm-mime-burst-message (m)
  "Burst messages from the digest message M.
M should be a message struct for a real message.
MIME encoding is expected.  Somewhere within the MIME layout
there must be at least one part of type message/news, message/rfc822 or
multipart/digest.  If there are multiple parts matching those types,
all of them will be burst."
  (let ((ident-header nil)
	(did-burst nil)
	(list (vm-mime-find-digests-in-layout (vm-mm-layout m))))
    (if vm-digest-identifier-header-format
	(setq ident-header (vm-summary-sprintf
			    vm-digest-identifier-header-format m)))
    (while list
      (setq did-burst (or (vm-mime-burst-layout (car list) ident-header)
			  did-burst))
      (setq list (cdr list)))
    did-burst))

(defun vm-mime-burst-layout (layout ident-header)
  (let ((work-buffer nil)
	(folder-buffer (current-buffer))
	start part-list
	(folder-type vm-folder-type))
    (unwind-protect
	(vm-save-restriction
	 (save-excursion
	   (widen)
	   (setq work-buffer (vm-make-work-buffer))
	   (set-buffer work-buffer)
	   (cond ((not (vectorp layout))
		  (error "Not a MIME message"))
		 ((vm-mime-types-match "message"
				       (car (vm-mm-layout-type layout)))
		  (insert (vm-leading-message-separator folder-type))
		  (and ident-header (insert ident-header))
		  (setq start (point))
		  (vm-mime-insert-mime-body layout)
		  (vm-munge-message-separators folder-type start (point))
		  ;; remove any leading newlines as they will
		  ;; make vm-reorder-message-headers think the
		  ;; header section has ended.
		  (save-excursion
		    (goto-char start)
		    (while (= (following-char) ?\n)
		      (delete-char 1)))
		  (insert (vm-trailing-message-separator folder-type)))
		 ((vm-mime-types-match "multipart/digest"
				       (car (vm-mm-layout-type layout)))
		  (setq part-list (vm-mm-layout-parts layout))
		  (while part-list
		    ;; Maybe we should verify that each part is
		    ;; of type message/rfc822 or message/news in
		    ;; here.  But it seems more useful to just
		    ;; copy whatever the contents are and let the
		    ;; user see the goop, whatever type it really
		    ;; is.
		    (insert (vm-leading-message-separator folder-type))
		    (and ident-header (insert ident-header))
		    (setq start (point))
		    (vm-mime-insert-mime-body (car part-list))
		    (vm-munge-message-separators folder-type start (point))
		    ;; remove any leading newlines as they will
		    ;; make vm-reorder-message-headers think the
		    ;; header section has ended.
		    (save-excursion
		      (goto-char start)
		      (while (= (following-char) ?\n)
			(delete-char 1)))
		    (insert (vm-trailing-message-separator folder-type))
		    (setq part-list (cdr part-list))))
		 (t (error
		     "MIME type is not multipart/digest or message/rfc822 or message/news")))
	   ;; do header conversions.
	   (let ((vm-folder-type folder-type))
	     (goto-char (point-min))
	     (while (vm-find-leading-message-separator)
	       (vm-skip-past-leading-message-separator)
	       (vm-convert-folder-type-headers folder-type folder-type)
	       (vm-find-trailing-message-separator)
	       (vm-skip-past-trailing-message-separator)))
	   ;; now insert the messages into the folder buffer
	   (cond ((not (zerop (buffer-size)))
		  (set-buffer folder-buffer)
		  (let ((old-buffer-modified-p (buffer-modified-p))
			(buffer-read-only nil)
			(inhibit-quit t))
		    (goto-char (point-max))
		    (insert-buffer-substring work-buffer)
		    (set-buffer-modified-p old-buffer-modified-p)
		    ;; return non-nil so caller knows we found some messages
		    t ))
		 ;; return nil so the caller knows we didn't find anything
		 (t nil))))
	 (and work-buffer (kill-buffer work-buffer)))))

(defun vm-rfc934-char-stuff-region (start end)
  "Quote RFC 934 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines beginning with `-' in the region have `- ' prepended to them."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^-" end t))
      (replace-match "- -" t t)))
  (set-marker end nil))

(defun vm-rfc934-char-unstuff-region (start end)
  "Unquote lines in between START and END as per RFC 934.
START and END are buffer positions in the current buffer.
Lines beginning with `- ' in the region have that string stripped
from them."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^- "  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-rfc934-encapsulate-messages (message-list keep-list discard-regexp)
  "Encapsulate the messages in MESSAGE-LIST as per RFC 934.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (if message-list
      (let ((target-buffer (current-buffer))
	    (mlist message-list)
	    source-buffer m start n)
	(save-restriction
	  ;; narrow to a zero length region to avoid interacting
	  ;; with anything that might have already been inserted
	  ;; into the buffer.
	  (narrow-to-region (point) (point))
	  (setq start (point))
	  (while mlist
	    (insert "---------------\n")
	    (setq m (vm-real-message-of (car mlist))
		  source-buffer (vm-buffer-of m))
	    (save-excursion
	      (set-buffer source-buffer)
	      (save-restriction
		(widen)
		(save-excursion
		  (set-buffer target-buffer)
		  (let ((beg (point)))
		    (insert-buffer-substring source-buffer (vm-headers-of m)
					     (vm-text-end-of m))
		    (goto-char beg)
		    ;; remove the Berkeley and VM status headers and sort
		    ;; the MIME headers to the top of the message.
		    (vm-reorder-message-headers nil vm-mime-header-list
						vm-internal-unforwarded-header-regexp)
		    ;; skip past the MIME headers so that when the
		    ;; user's header filters are applied they won't
		    ;; remove the MIME headers.
		    (while (and (vm-match-header)
				(looking-at vm-mime-header-regexp))
		      (goto-char (vm-matched-header-end)))
		    ;; apply the user's header filters.
		    (vm-reorder-message-headers nil keep-list discard-regexp)
		    (vm-rfc934-char-stuff-region beg (point-max))))))
	    (goto-char (point-max))
	    (insert "---------------")
	    (setq mlist (cdr mlist)))
	  (delete-region (point) (progn (beginning-of-line) (point)))
	  (insert "------- end -------\n")
	  (goto-char start)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (setq n (length message-list))
   (insert (format "------- start of %s%s(RFC 934 encapsulation) -------\n"
			  (if (cdr message-list)
			      "digest "
			    "forwarded message ")
			  (if (cdr message-list)
			      (format "(%d messages) " n)
			    "")))
	  (goto-char start)))))

(defun vm-rfc1153-char-stuff-region (start end)
  "Quote RFC 1153 message separators between START and END.
START and END are buffer positions in the current buffer.
Lines consisting only of 30 hyphens have the first hyphen
converted to a space."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward "^------------------------------$" end t))
      (replace-match " -----------------------------" t t)))
  (set-marker end nil))

(defun vm-rfc1153-char-unstuff-region (start end)
  "Unquote lines in between START and END as per RFC 1153.
START and END are buffer positions in the current buffer.
Lines consisting only of a space following by 29 hyphens have the space
converted to a hyphen."
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
		(re-search-forward "^ -----------------------------$" end t))
      (replace-match "------------------------------" t t)))
  (set-marker end nil))

(defun vm-rfc1153-encapsulate-messages (message-list keep-list discard-regexp)
  "Encapsulate the messages in MESSAGE-LIST as per RFC 1153.
The resulting digest is inserted at point in the current buffer.
Point is not moved.

MESSAGE-LIST should be a list of message structs (real or virtual).
These are the messages that will be encapsulated.
KEEP-LIST should be a list of regexps matching headers to keep.
DISCARD-REGEXP should be a regexp that matches headers to be discarded.
KEEP-LIST and DISCARD-REGEXP are used to order and trim the headers
to be forwarded.  See the docs for vm-reorder-message-headers
to find out how KEEP-LIST and DISCARD-REGEXP are used."
  (if message-list
      (let ((target-buffer (current-buffer))
	    (mlist message-list)
	    source-buffer m start)
	(save-restriction
	  ;; narrow to a zero length region to avoid interacting
	  ;; with anything that might have already been inserted
	  ;; into the buffer.
	  (narrow-to-region (point) (point))
	  (setq start (point))
	  (while mlist
	    (insert "---------------\n\n")
	    (setq m (vm-real-message-of (car mlist))
		  source-buffer (vm-buffer-of m))
	    (save-excursion
	      (set-buffer source-buffer)
	      (save-restriction
		(widen)
		(save-excursion
		  (set-buffer target-buffer)
		  (let ((beg (point)))
		    (insert-buffer-substring source-buffer (vm-headers-of m)
					     (vm-text-end-of m))
		    (goto-char beg)
		    ;; remove the Berkeley and VM status headers and sort
		    ;; the MIME headers to the top of the message.
		    (vm-reorder-message-headers nil vm-mime-header-list
						vm-internal-unforwarded-header-regexp)
		    ;; skip past the MIME headers so that when the
		    ;; user's header filters are applied they won't
		    ;; remove the MIME headers.
		    (while (and (vm-match-header)
				(looking-at vm-mime-header-regexp))
		      (goto-char (vm-matched-header-end)))
		    ;; apply the user's header filters.
		    (vm-reorder-message-headers nil keep-list discard-regexp)
		    (vm-rfc1153-char-stuff-region beg (point-max))))))
	    (goto-char (point-max))
	    (insert "\n---------------")
	    (setq mlist (cdr mlist)))
    (insert "---------------\n\nEnd of this Digest\n******************\n")
	  (goto-char start)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (insert (format "This is an RFC 1153 digest.\n(%d message%s)\n----------------------------------------------------------------------\n" (length message-list) (if (cdr message-list) "s" "")))
	  (goto-char start)))))

(defun vm-rfc1153-or-rfc934-burst-message (m rfc1153)
  "Burst messages from the digest message M.
M should be a message struct for a real message.
If RFC1153 is non-nil, assume the digest is of the form specified by
RFC 1153.  Otherwise assume RFC 934 digests."
  (let ((work-buffer nil)
	(match t)
	(prev-sep nil)
	(ident-header nil)
	after-prev-sep prologue-separator-regexp separator-regexp
	temp-marker
	(folder-buffer (current-buffer))
	(folder-type vm-folder-type))
    (if vm-digest-identifier-header-format
	(setq ident-header (vm-summary-sprintf
			    vm-digest-identifier-header-format m)))
    (if rfc1153
	(setq prologue-separator-regexp "^----------------------------------------------------------------------\n"
	      separator-regexp "^------------------------------\n")
      (setq prologue-separator-regexp "\\(^-[^ ].*\n+\\)+"
	    separator-regexp "\\(^-[^ ].*\n+\\)+"))
    (vm-save-restriction
     (save-excursion
       (widen)
       (unwind-protect
	   (catch 'done
	     (setq work-buffer (vm-make-work-buffer))
	     (set-buffer work-buffer)
	     (setq temp-marker (vm-marker (point)))
	     (vm-insert-region-from-buffer (vm-buffer-of m)
					   (vm-text-of m)
					   (vm-text-end-of m))
	     (goto-char (point-min))
	     (if (not (re-search-forward prologue-separator-regexp nil t))
		 (throw 'done nil))
	     ;; think of this as a do-while loop.
	     (while match
	       (cond ((null prev-sep)
		      ;; from (point-min) to end of match
		      ;; is the digest prologue, devour it and
		      ;; carry on.
		      (delete-region (point-min) (match-end 0)))
		     (t
		      ;; save value as mark so that it will move
		      ;; with the text.
		      (set-marker temp-marker (match-beginning 0))
		      (let ((md (match-data)))
			(unwind-protect
			    (progn
			      ;; Undo the quoting of the embedded message
			      ;; separators.
			      (if rfc1153
				  (vm-rfc1153-char-unstuff-region
				   after-prev-sep
				   temp-marker)
				(vm-rfc934-char-unstuff-region after-prev-sep
							       temp-marker))
			      ;; munge previous messages' message separators
			      (vm-munge-message-separators
			       folder-type
			       after-prev-sep
			       temp-marker))
			  (store-match-data md)))))
	       ;; there should be at least one valid header at
	       ;; the beginning of an encapsulated message.  If
	       ;; there isn't a valid header, then assume that
	       ;; the digest was packed improperly and that this
	       ;; isn't a real boundary.
	       (if (not
		    (save-excursion
		      (save-match-data
			;; People who roll digests often think
			;; any old format will do.  Adding blank
			;; lines after the message separator is
			;; common.  Spaces in such lines are an
			;; added delight.
			(skip-chars-forward " \n")
			(or (and (vm-match-header)
				 (vm-digest-get-header-contents "From"))
			    (not (re-search-forward separator-regexp
						    nil t))))))
		   (setq prev-sep (point)
			 after-prev-sep (point))
		 ;; if this isn't the first message, delete the
		 ;; digest separator goop and insert a trailing message
		 ;; separator of the proper type.
		 (if prev-sep
		     (progn
		       ;; eat preceding newlines
		       (while (= (preceding-char) ?\n)
			 (delete-char -1))
		       ;; put one back
		       (insert ?\n)
		       ;; delete the digest separator
		       (delete-region (match-beginning 0) (point))
		       ;; insert a trailing message separator
		       (insert (vm-trailing-message-separator folder-type))))
		 (setq prev-sep (point))
		 ;; insert the leading separator
		 (insert (vm-leading-message-separator folder-type))
		 (setq after-prev-sep (point))
		 ;; eat trailing newlines
		 (while (= (following-char) ?\n)
		   (delete-char 1))
		 (insert ident-header))
	       ;; try to match message separator and repeat.
	       (setq match (re-search-forward separator-regexp nil t)))
	     ;; from the last separator to eof is the digest epilogue.
	     ;; discard it.
	     (delete-region (or prev-sep (point-min)) (point-max))
	     ;; do header conversions.
	     (let ((vm-folder-type folder-type))
	       (goto-char (point-min))
	       (while (vm-find-leading-message-separator)
		 (vm-skip-past-leading-message-separator)
		 (vm-convert-folder-type-headers folder-type folder-type)
		 (vm-find-trailing-message-separator)
		 (vm-skip-past-trailing-message-separator)))
	     ;; now insert the messages into the folder buffer
	     (cond ((not (zerop (buffer-size)))
		    (set-buffer folder-buffer)
		    (let ((old-buffer-modified-p (buffer-modified-p))
			  (buffer-read-only nil)
			  (inhibit-quit t))
		      (goto-char (point-max))
		      (insert-buffer-substring work-buffer)
		      (set-buffer-modified-p old-buffer-modified-p)
		      ;; return non-nil so caller knows we found some messages
		      t ))
		   ;; return nil so the caller knows we didn't find anything
		   (t nil)))
	 (and work-buffer (kill-buffer work-buffer)))))))

(defun vm-rfc934-burst-message (m)
  "Burst messages from the RFC 934 digest message M.
M should be a message struct for a real message."
  (vm-rfc1153-or-rfc934-burst-message m nil))

(defun vm-rfc1153-burst-message (m)
  "Burst messages from the RFC 1153 digest message M.
M should be a message struct for a real message."
  (vm-rfc1153-or-rfc934-burst-message m t))

(defun vm-burst-digest (&optional digest-type)
  "Burst the current message (a digest) into its individual messages.
The digest's messages are assimilated into the folder as new mail
would be.

Optional argument DIGEST-TYPE is a string that tells VM what kind
of digest the current message is.  If it is not given the value
defaults to the value of vm-digest-burst-type.  When called
interactively DIGEST-TYPE will be read from the minibuffer.

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be burst."
  (interactive
   (list
    (let ((type nil)
	  (this-command this-command)
	  (last-command last-command))
      (setq type (completing-read (format "Digest type: (default %s) "
					  vm-digest-burst-type)
				  (append vm-digest-type-alist
					  (list '("guess")))
				  'identity nil))
      (if (string= type "")
	  vm-digest-burst-type
	type ))))
  (or digest-type (setq digest-type vm-digest-burst-type))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((start-buffer (current-buffer)) m totals-blurb
	(mlist (vm-select-marked-or-prefixed-messages 1)))
    (while mlist
      (if (vm-virtual-message-p (car mlist))
	  (progn
	    (setq m (vm-real-message-of (car mlist)))
	    (set-buffer (vm-buffer-of m)))
	(setq m (car mlist)))
      (vm-error-if-folder-read-only)
      (if (equal digest-type "guess")
	  (progn
	    (setq digest-type (vm-guess-digest-type m))
	    (if (null digest-type)
		(error "Couldn't guess digest type."))))
      (message "Bursting %s digest..." digest-type)
      (cond
       ((cond ((equal digest-type "mime")
	       (vm-mime-burst-message m))
	      ((equal digest-type "rfc934")
	       (vm-rfc934-burst-message m))
	      ((equal digest-type "rfc1153")
	       (vm-rfc1153-burst-message m))
	      (t (error "Unknown digest type: %s" digest-type)))
	(message "Bursting %s digest... done" digest-type)
	(vm-clear-modification-flag-undos)
	(vm-set-buffer-modified-p t)
	(vm-increment vm-modification-counter)
	(and vm-delete-after-bursting
	     ;; if start folder was virtual, we're now in the wrong
	     ;; buffer.  switch back.
	     (save-excursion
	       (set-buffer start-buffer)
	       ;; don't move message pointer when deleting the message
	       (let ((vm-move-after-deleting nil))
		 (vm-delete-message 1))))
	(vm-assimilate-new-messages t nil (vm-labels-of (car mlist)))
	;; do this now so if we error later in another iteration
	;; of the loop the summary and mode line will be correct.
	(vm-update-summary-and-mode-line)))
      (setq mlist (cdr mlist)))
    ;; collect this data NOW, before the non-previewers read a
    ;; message, alter the new message count and confuse
    ;; themselves.
    (setq totals-blurb (vm-emit-totals-blurb))
    (vm-display nil nil '(vm-burst-digest
			  vm-burst-mime-digest
			  vm-burst-rfc934-digest
			  vm-burst-rfc1153-digest)
		(list this-command))
    (if (vm-thoughtfully-select-message)
	(vm-preview-current-message)
      (vm-update-summary-and-mode-line))
    (message totals-blurb)))

(defun vm-burst-rfc934-digest ()
  "Burst an RFC 934 style digest"
  (interactive)
  (vm-burst-digest "rfc934"))

(defun vm-burst-rfc1153-digest ()
  "Burst an RFC 1153 style digest"
  (interactive)
  (vm-burst-digest "rfc1153"))

(defun vm-burst-mime-digest ()
  "Burst a MIME digest"
  (interactive)
  (vm-burst-digest "mime"))

(defun vm-burst-digest-to-temp-folder (&optional digest-type)
  "Burst the current message (a digest) into a temporary folder.
The digest's messages are copied to a buffer and vm-mode is
invoked on the buffer.  There is no file associated with this
buffer.  You can use `vm-write-file' to save the buffer, or
`vm-save-message' to save individual messages to a real folder.

Optional argument DIGEST-TYPE is a string that tells VM what kind
of digest the current message is.  If it is not given the value
defaults to the value of vm-digest-burst-type.  When called
interactively DIGEST-TYPE will be read from the minibuffer.

If invoked on marked messages (via vm-next-command-uses-marks),
all marked messages will be burst."
  (interactive
   (list
    (let ((type nil)
	  (this-command this-command)
	  (last-command last-command))
      (setq type (completing-read (format "Digest type: (default %s) "
					  vm-digest-burst-type)
				  (append vm-digest-type-alist
					  (list '("guess")))
				  'identity nil))
      (if (string= type "")
	  vm-digest-burst-type
	type ))))
  (or digest-type (setq digest-type vm-digest-burst-type))
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((start-buffer (current-buffer)) m totals-blurb
	(mlist (vm-select-marked-or-prefixed-messages 1))
	(work-buffer nil))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer
			     (format "digest from %s/%s%s"
				     (current-buffer)
				     (vm-number-of (car vm-message-pointer))
				     (if (cdr mlist) " ..." ""))))
	  (buffer-disable-undo work-buffer)
	  (set-buffer work-buffer)
	  (setq vm-folder-type vm-default-folder-type)
	  (while mlist
	    (if (vm-virtual-message-p (car mlist))
		(setq m (vm-real-message-of (car mlist)))
	      (setq m (car mlist)))
	    (if (equal digest-type "guess")
		(progn
		  (setq digest-type (vm-guess-digest-type m))
		  (if (null digest-type)
		      (error "Couldn't guess digest type."))))
	    (message "Bursting %s digest to folder..." digest-type)
	    (cond ((equal digest-type "mime")
		   (vm-mime-burst-message m))
		  ((equal digest-type "rfc934")
		   (vm-rfc934-burst-message m))
		  ((equal digest-type "rfc1153")
		   (vm-rfc1153-burst-message m))
		  (t (error "Unknown digest type: %s" digest-type)))
	    (message "Bursting %s digest... done" digest-type)
	    (setq mlist (cdr mlist)))
	  (set-buffer-modified-p nil)
	  (vm-save-buffer-excursion
	   (vm-goto-new-folder-frame-maybe 'folder)
	   (vm-mode)
	   (if (vm-should-generate-summary)
	       (progn
		 (vm-goto-new-folder-frame-maybe 'summary)
		 (vm-summarize))))
	  ;; temp buffer, don't offer to save it.
	  (setq buffer-offer-save nil)
	  (vm-display (or vm-presentation-buffer (current-buffer)) t
		      (list this-command) '(vm-mode startup))
	  (setq work-buffer nil))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-guess-digest-type (m)
  "Guess the digest type of the message M.
M should be the message struct of a real message.
Returns either \"rfc934\", \"rfc1153\" or \"mime\"."
  (catch 'return-value
    (save-excursion
      (set-buffer (vm-buffer-of m))
      (let ((layout (vm-mm-layout m)))
	(if (and (vectorp layout)
		 (or (vm-mime-layout-contains-type
		      layout
		      "multipart/digest")
		     (vm-mime-layout-contains-type
		      layout
		      "message/rfc822")
		     (vm-mime-layout-contains-type
		      layout
		      "message/news")))
	    (throw 'return-value "mime"))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (vm-text-of m))
	(cond ((and (search-forward "\n----------------------------------------------------------------------\n" (vm-text-end-of m) t)
		    (search-forward "\n------------------------------\n" (vm-text-end-of m) t))
	       "rfc1153")
	      (t "rfc934"))))))

(defun vm-digest-get-header-contents (header-name-regexp)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^$\\)"))
    (save-excursion
      (let ((case-fold-search t))
	(if (and (re-search-forward regexp nil t)
		 (match-beginning 1)
		 (progn (goto-char (match-beginning 0))
			(vm-match-header)))
	    (vm-matched-header-contents)
	  nil )))))

(provide 'vm-digest)

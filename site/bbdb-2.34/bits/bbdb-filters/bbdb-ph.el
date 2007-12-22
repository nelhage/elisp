;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
;;;  collection of input and output filters for BBDB.
;;; 
;;;  Copyright (C) 1995 Neda Communications, Inc.
;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
;;; 
;;;  This library is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU Library General Public License as
;;;  published by the Free Software Foundation; either version 2 of the
;;;  License, or (at your option) any later version.  This library is
;;;  distributed in the hope that it will be useful, but WITHOUT ANY
;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
;;;  License for more details.  You should have received a copy of the GNU
;;;  Library General Public License along with this library; if not, write
;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
;;;  USA.
;;; 
;;; This is bbdb-ph.el
;;;
;;;
;;; RCS: bbdb-ph.el,v 1.1.1.1 1995/08/07 08:43:08 mohsen Exp
;;;
;;; a copy-and-edit job on bbdb-print.el


;;; To use this, add the following to your .emacs
;;; and strip ";;;XXX"
;;;

;;;XXX;; BBDB PH Filter
;;;XXX(load "bbdb-ph")

;;;XXX(setq bbdb-ph-filename
;;;XXX      (concat "/dos/u/" (user-login-name) "/bb-phone.cdf"))
;;;XXX;;; - to output the *BBDB* buffer in PH tab-delimited-file (.CDF)
;;;XXX;;; format, invoke M-x bbdb-ph-output
;;;XXX;;;
;;;XXX;;; - you may also want to modify default values of the following (use
;;;XXX;;;   M-x describe-variable for details):
;;;XXX;;;     bbdb-ph-output-elide
;;;XXX;;;     bbdb-ph-output-requires
;;;XXX;;;     bbdb-ph-output-no-bare-names


(require 'bbdb-print)
(require 'basic-ext)


(defvar bbdb-ph-filename "~/data.out"
  "*Default file name for bbdb-output-ph printouts of BBDB database.")


(defvar bbdb-ph-output-elide '(creation-date timestamp mail-alias)
  "*List of symbols denoting BBDB fields NOT to be output.
Valid symbols are: name comp net phones addrs.  You can also use the
tags for notes (e.g., creation-date).
  e.g.: '(net creation-date)
See also variable bbdb-ph-output-requires.")


(defvar bbdb-ph-output-requires '(and name net)
  "*A boolean expression of 'and' and 'or' to be evaluated to determine if
the current record should be output.  Valid symbols for use
in the boolean expression are: name comp net phones addrs notes.
  e.g.: (and name (or comp addrs))
See also variable bbdb-ph-output-elide.
")


(defvar bbdb-ph-output-no-bare-names t
  "*A bare name is one with no information other than
that in bbdb-ph-output-requires.  To avoid printing
these set this variable to t")


(defun bbdb-ph-output (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-ph-filename)))
  (setq bbdb-ph-filename (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-ph-filename)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter
	    (boph-maybe-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min))
    (message "PH tag and tab-delimited file %s generated." bbdb-ph-filename)))


(defun boph-maybe-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in Ph format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: formatting deleted record")))

  (let* ((bbdb-elided-display bbdb-ph-output-elide)
	 (first-letter
	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
	 (name   (and (bbdb-field-shown-p 'name)
		      (or (bbdb-record-getprop record 'tex-name)
			  (bbdb-record-name record))))
	 (comp   (and (bbdb-field-shown-p 'company)
		      (bbdb-record-company record)))
	 (net    (and (bbdb-field-shown-p 'net)
		      (bbdb-record-net record)))
	 (phones (and (bbdb-field-shown-p 'phone)
		      (bbdb-record-phones record)))
	 (addrs  (and (bbdb-field-shown-p 'address)
		      (bbdb-record-addresses record)))
	 (notes  (bbdb-record-raw-notes record))
	 (begin (point))
	 (bare t)
	 ph-name ph-email ph-office-phone ph-skypager ph-portable
	 ph-title-notes-part
	 saved-case-fold)


    ;; Section header, if neccessary.

    (if (and current-letter (not (string-equal first-letter current-letter)))
	(message "Now processing \"%s\" entries..." (upcase first-letter)))


    (if (eval bbdb-ph-output-requires)
	(progn

	  ;; =============================================================
	  ;; grovel through BBDB record collecting ph-relevant information
	  ;; =============================================================

	  ;; grovel through name
	  ;;
	  (setq ph-name name)

	  ;; grovel through phone numbers
	  ;;
	  (progn
	    (setq saved-case-fold case-fold-search
		  case-fold-search t)
	    (while phones
	      (let ((place (aref (car phones) 0))
		    (number (bbdb-phone-string (car phones))))
		(cond ((or (string-match place "office")
			   (string-match place "work"))
		       (if (null ph-office-phone)
			   (setq ph-office-phone number)))
		      ((or (string-match place "mobile")
			   (string-match place "cellular"))
		       (if (null ph-portable)
			   (setq ph-portable number)))
		      (t nil)))
	      (setq phones (cdr phones)))


	    (setq case-fold-search saved-case-fold)
	    )

	  ;; grovel through BBDB email-addresses
	  ;;
	  (if net
	      (setq ph-email (car net)))

	  ;; grovel through BBDB Notes
	  ;;
	  (progn 

	    (if (stringp notes)
		(setq notes (list (cons 'notes notes))))

	    (while notes
	      (let ((curr-note (car notes)))
		(if (bbdb-field-shown-p (car curr-note))
		    (cond ((member (car curr-note) '(skypage pager))
			   (setq ph-skypager (boph-mangle-if-multi-line (cdr curr-note))))
			  ((equal (car curr-note) 'mobile)
			   (setq ph-portable (boph-mangle-if-multi-line (cdr curr-note))))
			  ((equal (car curr-note) 'notes)
			   (setq ph-title-notes-part (boph-mangle-if-multi-line (cdr curr-note))))
			  (t nil))
		  ))
	      (setq notes (cdr notes)))
	    )

	  ;; grovel through comp
	  ;;
	  (setq ph-title-coname-part comp)
	  (setq ph-title (concat (or ph-title-coname-part "")
				 (if (and ph-title-coname-part ph-title-notes-part) " " "")
				 (if ph-title-notes-part (concat "[" ph-title-notes-part "]") "")))

	  ;; ====================
	  ;; now output PH record
	  ;; ====================

	  ;; PH 'name' field (maxlen 256)
	  ;;
	  (insert (format "3:%s\t" (boph-maybe-truncate (or name "") 256)))

	  ;; PH 'email' field (maxlen 25) (should be 128?)  ** NOT YET **
	  (if ph-email (setq bare nil))
	  (insert (format "2:%s\t" (boph-maybe-truncate (or ph-email "") 25)))

	  ;; PH 'office_phone' field (max len 60)
	  ;;
	  (if ph-office-phone (setq bare nil))
	  (insert (format "32:%s\t" (boph-maybe-truncate (or ph-office-phone "") 60)))

	  ;; PH 'title' field (maxlen 120) 
	  (insert (format "98:%s\t" (boph-maybe-truncate ph-title 120)))

	  ;; PH 'portable' field (maxlen 60)
	  (if ph-portable (setq bare nil))
	  (insert (format "97:%s\t" (boph-maybe-truncate (or ph-portable "") 60)))

	  ;; PH 'skypager' field (maxlen 64)
	  (if ph-skypager (setq bare nil))
	  (insert (format "27:%s\t" (boph-maybe-truncate (or ph-skypager "") 64)))

	  ;; ==========
	  ;; bare check
	  ;; ==========

	  ;; If record is bare, delete anything we may have inserted.
	  ;; otherwise, mark the end of this record.
	  (if (and bare bbdb-ph-output-no-bare-names)
	      (delete-region begin (point))
	    (insert "\n"))		; PH end of record
	  ))

    ;; return current letter
    current-letter))


(defun boph-maybe-truncate (string maxlen)
  "If STRING is longer than MAXLEN, returns a truncated version."
  (if (> (length string) maxlen)
      (substring string 0 maxlen)
    string))


(defun boph-mangle-if-multi-line (string)
  "If STRING is has multiple lines, mangle it for output to PH"
  (if (string-match "\n" string)
      (string-replace-regexp string "\n" "\t") ; tabs are used to denote new lines in the .cdf file
  string))

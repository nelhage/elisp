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
;;; This is bbdb-hp200lx.el
;;;
;;;
;;; RCS: bbdb-hp200lx.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
;;;
;;; a copy-and-edit job on bbdb-print.el


;;; To use this, add the following to your .emacs
;;; and strip ";;;XXX"
;;;

;;;XXX;; BBDB HP200LX Filter
;;;XXX(load "bbdb-hp200lx")

;;;XXX(setq bbdb-hp200lx-filename
;;;XXX      (concat "/dos/u/" (user-login-name) "/bb-phone.cdf"))
;;;XXX;;; - to output the *BBDB* buffer in HP200LX comma-delimited-file (.CDF)
;;;XXX;;; format, invoke M-x bbdb-hp200lx-output
;;;XXX;;;
;;;XXX;;; - you may also want to modify default values of the following (use
;;;XXX;;;   M-x describe-variable for details):
;;;XXX;;;     bbdb-hp200lx-output-elide
;;;XXX;;;     bbdb-hp200lx-output-requires
;;;XXX;;;     bbdb-hp200lx-output-no-bare-names


(require 'bbdb-print)
(require 'basic-ext)


(defvar bbdb-hp200lx-filename "~/bb-phone.cdf"
  "*Default file name for bbdb-output-hp200lx printouts of BBDB database.")


(defvar bbdb-hp200lx-output-elide '(net creation-date timestamp mail-alias)
  "*List of symbols denoting BBDB fields NOT to be output.
Valid symbols are: name comp net phones addrs.  You can also use the
tags for notes (e.g., creation-date).
  e.g.: '(net creation-date)
See also variable bbdb-hp200lx-output-requires.")


(defvar bbdb-hp200lx-output-requires '(or name comp)
  "*A boolean expression of 'and' and 'or' to be evaluated to determine if
the current record should be output.  Valid symbols for use
in the boolean expression are: name comp net phones addrs notes.
  e.g.: (and name (or comp addrs))
See also variable bbdb-hp200lx-output-elide.
")


(defvar bbdb-hp200lx-output-no-bare-names t
  "*A bare name is one with no information other than
that in bbdb-hp200lx-output-requires.  To avoid printing
these set this variable to t")


(defun bbdb-hp200lx-output (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-hp200lx-filename)))
  (setq bbdb-hp200lx-filename (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-hp200lx-filename)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter
	    (boh-maybe-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min))
    (message "HP 200LX comma-delimited phonebook file %s generated." bbdb-hp200lx-filename)))


(defun boh-maybe-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in Hp200lx format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: formatting deleted record")))


  (let* ((bbdb-elided-display bbdb-hp200lx-output-elide)
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
	 (bare t))


    ;; Section header, if neccessary.


    (if (and current-letter (not (string-equal first-letter current-letter)))
	(message "Now processing \"%s\" entries..." (upcase first-letter)))


    (if (eval bbdb-hp200lx-output-requires)
	(let (more-phones)


	  ;; HP 200LX last name field (maxlen 86 ??) -- used for BBDB name
	  ;;
	  (insert (format "\"%s\"," (boh-maybe-truncate name 86)))


	  ;; HP 200LX first name field (maxlen ??) -- unused
	  (insert ",")


	  ;; HP 200LX middle name field (maxlen ??) -- unused
	  ;;
	  (insert ",")


	  ;; Phone numbers
	  ;;
	  (let (business-phone home-phone fax-phone saved-case-fold)
	    (setq saved-case-fold case-fold-search
		  case-fold-search t)
	    (while phones
	      (let ((place (aref (car phones) 0))
		    (number (bbdb-phone-string (car phones))))
		(cond ((or (string-match place "office")
			   (string-match place "work"))
		       (if (null business-phone)
			   (setq business-phone (list place number))
			 (setq more-phones (cons (list place number) more-phones))))
		      ((string-match place "home")
		       (if (null home-phone)
			   (setq home-phone (list place number))
			 (setq more-phones (cons (list place number) more-phones))))
		      ((or (string-match place "fax")
			   (string-match place "facsimile"))
		       (if (null fax-phone)
			   (setq fax-phone (list place number))
			 (setq more-phones (cons (list place number) more-phones))))
		      (t
		       (setq more-phones (cons (list place number) more-phones)))))
	      (setq phones (cdr phones)))


	    (setq case-fold-search saved-case-fold)


	    ;; HP 200LX business phone field (maxlen 29)
	    (if business-phone
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate
					     (format "%s" (car (cdr business-phone)))
					     29)))
		  (setq bare nil))
	      (insert ","))


	    ;; HP 200LX home phone field (maxlen 29)
	    (if home-phone
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate
					     (format "%s" (car (cdr home-phone)))
					     29)))
		  (setq bare nil))
	      (insert ","))


	    ;; HP 200LX alternate phone field (maxlen 29) -- unused
	    (insert ",")


	    ;; HP 200LX fax phone field (maxlen 29)
	    (if fax-phone
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate
					     (format "%s" (car (cdr fax-phone))) ; the description
					     29)))
		  (setq bare nil))
	      (insert ","))
	    )


	  ;; HP 200LX title field (maxlen 38) -- unused
	  (insert ",")


	  ;; HP 200LX category field (maxlen 127) -- unused
	  (insert ",")


	  ;; HP 200LX company field (maxlen 82) -- used for BBDB company
	  (if comp
	      (insert (format "\"%s\"," (boh-maybe-truncate comp 82)))
	    (insert ","))


	  ;; Addresses
	  ;;
	  (let ((addr (car addrs))	;just take the first bbdb address
		hp-addr1 hp-addr2 hp-city hp-state hp-zip)

	    (if addr
		(progn
		  (setq hp-addr1 (bbdb-address-street1 addr))
		  (setq hp-addr2 (concat (bbdb-address-street2 addr)
 					 (if (and (> (length (bbdb-address-street2 addr)) 0)
 						  (> (length (bbdb-address-street3 addr)) 0))
 					     ", " "")
					 (bbdb-address-street3 addr)))
		  (setq hp-city (bbdb-address-city addr))
		  (setq hp-state (bbdb-address-state addr))
		  (setq hp-zip (bbdb-address-zip-string addr))))

	    ;; HP 200LX address 1 field (maxlen 82)
	    (if hp-addr1
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate hp-addr1 82)))
		  (setq bare nil))
	      (insert ","))

	    ;; HP 200LX address 2 field (maxlen 82)
	    (if hp-addr2
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate hp-addr2 82)))
		  (setq bare nil))
	      (insert ","))

	    ;; HP 200LX city field (maxlen 34)
	    (if hp-city
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate hp-city 34)))
		  (setq bare nil))
	      (insert ","))

	    ;; HP 200LX state field (maxlen 39)
	    (if hp-state
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate hp-state 39)))
		  (setq bare nil))
	      (insert ","))

	    ;; HP 200LX zip field (maxlen 16)
	    (if hp-zip
		(progn
		  (insert (format "\"%s\"," (boh-maybe-truncate hp-zip 16)))
		  (setq bare nil))
	      (insert ","))
	    )

	  ;; BBDB Notes

	  (let (hp-note)
	    (save-excursion
	      (set-buffer (get-buffer-create " *boh-scratch*"))
	      (kill-region (point-min) (point-max))

	      (while more-phones
		(insert (format "%s: %s\t"
				(car (car more-phones)) ; the tag
				(car (cdr (car more-phones)))) ; the number
			)
		(setq bare nil)
		(setq more-phones (cdr more-phones)))

	      ;; output BBDB email-addresses
	      (while net
		(insert (format "%s\t" (car net)))
		(setq bare nil)
		(setq net (cdr net)))

	      (if (stringp notes)
		  (setq notes (list (cons 'notes notes))))

	      (while notes
		(let ((thisnote (car notes)))
		  (if (bbdb-field-shown-p (car thisnote))
		      (progn
			(setq bare nil)
			(if (eq 'notes (car thisnote))
			    (insert (format "Notes: %s\t" (boh-mangle-if-multi-line (cdr thisnote))))
			  (insert (format "Note [%s]: %s\t"
					  (symbol-name (car thisnote))
					  (boh-mangle-if-multi-line (cdr thisnote))))))))
		(setq notes (cdr notes)))

	      (setq hp-note (buffer-string)))

	    ;; HP 200LX notes field (32K for the entire record)
	    (if (> (length hp-note) 0)
		(progn
		  (insert (format "\"%s\"" hp-note))
		  (setq bare nil)))
	    )

	  ;; If record is bare, delete anything we may have inserted.
	  ;; otherwise, mark the end of this record.
	  (if (and bare bbdb-hp200lx-output-no-bare-names)
	      (delete-region begin (point))
	    (insert "\n"))		; HP 200LX end of record
	  ))

    ;; return current letter
    current-letter))


(defun boh-maybe-truncate (string maxlen)
  "If STRING is longer than MAXLEN, returns a truncated version."
  (if (> (length string) maxlen)
      (substring string 0 maxlen)
    string))


(defun boh-mangle-if-multi-line (string)
  "If STRING is has multiple lines, mangle it for output to HP200LX"
  (if (string-match "\n" string)
      (string-replace-regexp string "\n" "\t") ; tabs are used to denote new lines in the .cdf file
  string))

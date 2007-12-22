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
;;; This is bbdb-eudora.el
;;;
;;; 
;;; RCS: bbdb-eudora.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
;;;
;;; a copy-and-edit job on bbdb-print.el

;;; To use this, add the following to your .emacs
;;; and strip ";;;XXX"
;;;

;;;XXX;; BBDB Filters
;;;XXX(load "bbdb-eudora")

;;;XXX(setq bbdb-eudora-nndbase-filename
;;;XXX      (concat "/dos/m/eudora.mai/" (user-login-name) "/nndbase.txt"))
;;;XXX;;; And then
;;;XXX;; (bbdb-eudora-nndbase-output)

;;;XXX(setq bbdb-eudora-rcpdbase-filename
;;;XXX      (concat "/dos/m/eudora.mai/" (user-login-name) "/rcpdbase.txt"))
;;;XXX;;; And then
;;;XXX;; (bbdb-eudora-rcpdbase-output)

(require 'bbdb-print)
(require 'basic-ext)

(defvar bbdb-eudora-nndbase-filename "~/nndbase.txt"
  "*Default file name for bbdb-output-eudora printouts of BBDB database.")

(defun bbdb-eudora-nndbase-output (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-eudora-nndbase-filename)))
  (setq bbdb-eudora-nndbase-filename (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-eudora-nndbase-filename)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter 
	    (boe-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min))
    (message "Eudora nickname file %s generated." bbdb-eudora-nndbase-filename)))

(defsubst boe-print-if-not-blank (string prepend-string &rest more)
  "If STRING is not null, then return it with PREPEND-STRING in front and concatenated
with rest of arguments.  If it is null, then all arguments are 
ignored and the null string is returned."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat prepend-string string more)))

(defun boe-output-this-record-p (name comp net phones addrs notes)
  "Examine NAME COMP NET PHONES ADDRS NOTES and return t if 
the current record is to be output by bbdb-output-eudora."
  ;; if name is non-nil, output it
  (cond ((and name net) t)
	(t nil))
  )


(defun boe-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in Eudora format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: formatting deleted record")))
  
  (let* ((bbdb-elided-display bbdb-print-elide)
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
    
    (if (boe-output-this-record-p name comp net phones addrs notes)
	(progn 

	  ;; Eudora nickname in canonical form (e.g., mohsen.banan)
	  ;;
	  (if name
	      (insert (format "<%s>\n" name)))

	  ;; Email address -- just use their first address.
	  ;;  Make all dots legal line-breaks.
	  ;;
	  ;; output in the following format:  "<Pretty Name>" <email address>
	  (if net
	      (let ((net-addr (car net))
		    (start 0))
		(insert (format ">\"%s\" <%s>\n" name net-addr))))

	  ;; start a Eudora nndbase.txt notes section for this nickname
	  ;; by inserting the nickname again
	  
	  (if name
	      (insert (format "<%s>\n" name)))
	  
	  ;; Company
	  ;;
	  (if comp
	      (insert (format "> Company: %s\n"
			      (boe-mangle-if-multi-line comp))))

	  ;; Phone numbers
	  ;;
	  (while phones
	    (let ((place (aref (car phones) 0))
		  (number (bbdb-phone-string (car phones))))
	      (setq bare nil)
	      (insert (format "> Telephone: %s%s\n" 
			      (boe-print-if-not-blank place "" ": ")
			      number))
	      (setq phones (cdr phones))))

	  ;; Addresses
	  ;;
	  (while addrs
	    (let ((addr (car addrs)))
	      (setq bare nil)
	      (insert
	       (format 
		"> Address: \n%s"
		(concat 
		 (boe-print-if-not-blank (bbdb-address-street1 addr) "> " "\n")
		 (boe-print-if-not-blank (bbdb-address-street2 addr) "> " "\n")
		 (boe-print-if-not-blank (bbdb-address-street3 addr) "> ")
		 (boe-print-if-not-blank (bbdb-address-city addr) "> ") 
		 (if (and (not (equal "" (bbdb-address-city addr)))
			  (not (equal "" (bbdb-address-state addr))))
		     ", ")
		 (boe-print-if-not-blank (bbdb-address-state addr) "" " ")
		 (boe-print-if-not-blank (bbdb-address-zip-string addr) "" "\n")))))
	    (setq addrs (cdr addrs)))

	  ;; BBDB Notes

	  (if (stringp notes)
	      (setq notes (list (cons 'notes notes))))
	  (while notes
	    (let ((thisnote (car notes)))
	      (if (bbdb-field-shown-p (car thisnote))
		  (progn
		    (setq bare nil)
		    (if (eq 'notes (car thisnote))
			(insert (format "> Notes: %s\n"
					(boe-mangle-if-multi-line (cdr thisnote))))
		      (insert (format "> Note [%s]: %s\n" 
				      (symbol-name (car thisnote))
				      (boe-mangle-if-multi-line (cdr thisnote))))))))
	    (setq notes (cdr notes)))

	  ;; If record is bare, delete anything we may have inserted.
	  ;; otherwise, mark the end of this record.

	  (if bare
	      (delete-region begin (point))

	    (setq current-letter first-letter))
	  
	  ))

    ;; return current letter
    current-letter))


(defun boe-mangle-if-multi-line (string)
  "If STRING is has multiple lines, mangle it for output to Eudora" 
  (if (string-match "\n" string)
      (string-replace-regexp string "\n" "\n> ")
    string))


;;;;;;;;;;;; Eudora Receipient DataBase (rcpdbase.txt) ;;;;;;;;;;;

;;;(setq bbdb-eudora-rcpdbase-filename "/dos/m/eudora.mai/mohsen/rcpdbase.txt")
(defvar bbdb-eudora-rcpdbase-filename "~/rcpdbase.txt"
  "*Default file name for bbdb-output-eudora printouts of BBDB database.")

(defun bbdb-eudora-rcpdbase-output (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-eudora-rcpdbase-filename)))
  (setq bbdb-eudora-rcpdbase-filename (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-eudora-rcpdbase-filename)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter 
	    (boe-rcpdbase-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min))
    (message "Eudora rcpt. file %s generated." bbdb-eudora-nndbase-filename)))



(defun boe-rcpdbase-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in Eudora format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: formatting deleted record")))
  
  (let* ((bbdb-elided-display bbdb-print-elide)
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

    (if (and current-letter
	     (not (string-equal first-letter current-letter)))
	(message "Now processing \"%s\" entries..." (upcase first-letter)))
    
    (if (boe-output-this-record-p name comp net phones addrs notes)
	(progn 

	  ;; Eudora nickname in canonical form (e.g., mohsen.banan)
	  ;;
	  (if name
	      (insert (format "%s\n" name)))

	  (setq current-letter first-letter)
	  
	  ))

    ;; return current letter
    current-letter))

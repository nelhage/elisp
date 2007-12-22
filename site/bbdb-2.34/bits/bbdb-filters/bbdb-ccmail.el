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
;;; RCS: bbdb-ccmail.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
;;;
;;; a copy-and-edit job on bbdb-print.el

;;; To use this, add the following to your .emacs
;;; and strip ";;;XXX"
;;;

;;;XXX;; BBDB Filters
;;;XXX(load "bbdb-ccmail")

;;;XXX(setq bbdb-ccmail-filename "~/privdir.ini")
;;;XXX;;; And then
;;;XXX;;; (bbdb-ccmail-output)

;;; TODO
;;; Make the postoffice name optional as an argument
;;;

(require 'bbdb-print)

(defvar bbdb-ccmail-filename "~/privdir.ini"
  "*Default file name for bbdb-output-ccmail printouts of BBDB database.")

(defun bbdb-ccmail-output (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-ccmail-filename)))
  (setq bbdb-ccmail-filename (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-ccmail-filename)
    (delete-region (point-min) (point-max))
    (let* ((ccmail-count 0))
      (while records
	(setq current-letter 
	      (boe-ccmail-format-record (car (car records))
					current-letter))
	(setq records (cdr records)))
      (goto-char (point-min))
      (insert (format "[smtpgate]\nEntryCount=%d\n" ccmail-count))
      (goto-char (point-min)))))

(defun boe-ccmail-output-this-record-p (name net)
  "Examine NAME COMP NET PHONES ADDRS NOTES and return t if 
the current record is to be output by bbdb-output-ccmail."
  ;; if name is non-nil, output it
  (cond ((and name net) t)
	(t nil))
  )


(defun boe-ccmail-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in Ccmail format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: tex formatting deleted record")))
  
  (let* ((bbdb-elided-display bbdb-print-elide)
	 (first-letter 
	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
	 (name   (and (bbdb-field-shown-p 'name)
		      (or (bbdb-record-getprop record 'tex-name)
			  (bbdb-print-tex-quote
			   (bbdb-record-name record)))))
	 (net    (and (bbdb-field-shown-p 'net)
		      (bbdb-record-net record)))
	 (begin (point))
	 )

    (if (and current-letter
	     (not (string-equal first-letter current-letter)))
	(message "Now processing \"%s\" entries..." (upcase first-letter)))
    
    (if (boe-ccmail-output-this-record-p name net)
	(progn 

	  ;; Email address -- just use their first address.
	  ;;  Make all dots legal line-breaks.
	  ;;
	  ;; output in the following format:  "<Pretty Name>" <email address>
	  (if net
	      (let ((net-addr (car net))
		    (start 0))
		(setq ccmail-count (+ ccmail-count 1))
		(insert (format "Entry%d=" ccmail-count))
		(insert (format "\"%s\" <%s>\n" name net-addr))))
	  (setq current-letter first-letter))
      )

    ;; return current letter
    current-letter))


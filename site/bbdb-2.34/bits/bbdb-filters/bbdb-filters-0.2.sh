#! /bin/sh
# This is a shell archive.  Remove anything before this line, then unpack
# it by saving it into a file and typing "sh file".  To overwrite existing
# files, type "sh file -c".  You can also feed this as standard input via
# unshar, or by typing "sh <file", e.g..  If this archive is complete, you
# will see the following message at the end:
#		"End of shell archive."
# Contents:  bbdb-ccmail.el bbdb-eudora.el bbdb-export.el
#   bbdb-hp200lx.el bbdb-ph.el bbdb-passwd.el makefile COPYING.LIB
#   README doc doc/main.texinfo doc/lgpl.tex doc/makefile
#   doc/formatted doc/formatted/bbdb-filters.info
# Wrapped by mohsen@arash on Tue Aug  8 03:09:07 1995
PATH=/bin:/usr/bin:/usr/ucb ; export PATH
if test -f 'bbdb-ccmail.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-ccmail.el'\"
else
echo shar: Extracting \"'bbdb-ccmail.el'\" \(4150 characters\)
sed "s/^X//" >'bbdb-ccmail.el' <<'END_OF_FILE'
X;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;;  collection of input and output filters for BBDB.
X;;; 
X;;;  Copyright (C) 1995 Neda Communications, Inc.
X;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
X;;; 
X;;;  This library is free software; you can redistribute it and/or modify
X;;;  it under the terms of the GNU Library General Public License as
X;;;  published by the Free Software Foundation; either version 2 of the
X;;;  License, or (at your option) any later version.  This library is
X;;;  distributed in the hope that it will be useful, but WITHOUT ANY
X;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;;  License for more details.  You should have received a copy of the GNU
X;;;  Library General Public License along with this library; if not, write
X;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;;  USA.
X;;; 
X;;; This is bbdb-eudora.el
X;;;
X;;; 
X;;; RCS: bbdb-ccmail.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
X;;;
X;;; a copy-and-edit job on bbdb-print.el
X
X;;; To use this, add the following to your .emacs
X;;; and strip ";;;XXX"
X;;;
X
X;;;XXX;; BBDB Filters
X;;;XXX(load "bbdb-ccmail")
X
X;;;XXX(setq bbdb-ccmail-filename "~/privdir.ini")
X;;;XXX;;; And then
X;;;XXX;;; (bbdb-ccmail-output)
X
X;;; TODO
X;;; Make the postoffice name optional as an argument
X;;;
X
X(require 'bbdb-print)
X
X(defvar bbdb-ccmail-filename "~/privdir.ini"
X  "*Default file name for bbdb-output-ccmail printouts of BBDB database.")
X
X(defun bbdb-ccmail-output (to-file)
X  "Print the selected BBDB entries"
X  (interactive (list (read-file-name "Print To File: " bbdb-ccmail-filename)))
X  (setq bbdb-ccmail-filename (expand-file-name to-file))
X  (let ((current-letter t)
X	(records (progn (set-buffer bbdb-buffer-name)
X			bbdb-records)))
X    (find-file bbdb-ccmail-filename)
X    (delete-region (point-min) (point-max))
X    (let* ((ccmail-count 0))
X      (while records
X	(setq current-letter 
X	      (boe-ccmail-format-record (car (car records))
X					current-letter))
X	(setq records (cdr records)))
X      (goto-char (point-min))
X      (insert (format "[smtpgate]\nEntryCount=%d\n" ccmail-count))
X      (goto-char (point-min)))))
X
X(defun boe-ccmail-output-this-record-p (name net)
X  "Examine NAME COMP NET PHONES ADDRS NOTES and return t if 
Xthe current record is to be output by bbdb-output-ccmail."
X  ;; if name is non-nil, output it
X  (cond ((and name net) t)
X	(t nil))
X  )
X
X
X(defun boe-ccmail-format-record (record &optional current-letter brief)
X  "Insert the bbdb RECORD in Ccmail format.
XOptional CURRENT-LETTER is the section we're in -- if this is non-nil and
Xthe first letter of the sortkey of the record differs from it, a new section
Xheading will be output \(an arg of t will always produce a heading).
XThe new current-letter is the return value of this function.
XSomeday, optional third arg BRIEF will produce one-line format."
X  (bbdb-debug (if (bbdb-record-deleted-p record)
X		  (error "plus ungood: tex formatting deleted record")))
X  
X  (let* ((bbdb-elided-display bbdb-print-elide)
X	 (first-letter 
X	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
X	 (name   (and (bbdb-field-shown-p 'name)
X		      (or (bbdb-record-getprop record 'tex-name)
X			  (bbdb-print-tex-quote
X			   (bbdb-record-name record)))))
X	 (net    (and (bbdb-field-shown-p 'net)
X		      (bbdb-record-net record)))
X	 (begin (point))
X	 )
X
X    (if (and current-letter
X	     (not (string-equal first-letter current-letter)))
X	(message "Now processing \"%s\" entries..." (upcase first-letter)))
X    
X    (if (boe-ccmail-output-this-record-p name net)
X	(progn 
X
X	  ;; Email address -- just use their first address.
X	  ;;  Make all dots legal line-breaks.
X	  ;;
X	  ;; output in the following format:  "<Pretty Name>" <email address>
X	  (if net
X	      (let ((net-addr (car net))
X		    (start 0))
X		(setq ccmail-count (+ ccmail-count 1))
X		(insert (format "Entry%d=" ccmail-count))
X		(insert (format "\"%s\" <%s>\n" name net-addr))))
X	  (setq current-letter first-letter))
X      )
X
X    ;; return current letter
X    current-letter))
X
END_OF_FILE
if test 4150 -ne `wc -c <'bbdb-ccmail.el'`; then
    echo shar: \"'bbdb-ccmail.el'\" unpacked with wrong size!
fi
# end of 'bbdb-ccmail.el'
fi
if test -f 'bbdb-eudora.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-eudora.el'\"
else
echo shar: Extracting \"'bbdb-eudora.el'\" \(9916 characters\)
sed "s/^X//" >'bbdb-eudora.el' <<'END_OF_FILE'
X;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;;  collection of input and output filters for BBDB.
X;;; 
X;;;  Copyright (C) 1995 Neda Communications, Inc.
X;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
X;;; 
X;;;  This library is free software; you can redistribute it and/or modify
X;;;  it under the terms of the GNU Library General Public License as
X;;;  published by the Free Software Foundation; either version 2 of the
X;;;  License, or (at your option) any later version.  This library is
X;;;  distributed in the hope that it will be useful, but WITHOUT ANY
X;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;;  License for more details.  You should have received a copy of the GNU
X;;;  Library General Public License along with this library; if not, write
X;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;;  USA.
X;;; 
X;;; This is bbdb-eudora.el
X;;;
X;;; 
X;;; RCS: bbdb-eudora.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
X;;;
X;;; a copy-and-edit job on bbdb-print.el
X
X;;; To use this, add the following to your .emacs
X;;; and strip ";;;XXX"
X;;;
X
X;;;XXX;; BBDB Filters
X;;;XXX(load "bbdb-eudora")
X
X;;;XXX(setq bbdb-eudora-nndbase-filename
X;;;XXX      (concat "/dos/m/eudora.mai/" (user-login-name) "/nndbase.txt"))
X;;;XXX;;; And then
X;;;XXX;; (bbdb-eudora-nndbase-output)
X
X;;;XXX(setq bbdb-eudora-rcpdbase-filename
X;;;XXX      (concat "/dos/m/eudora.mai/" (user-login-name) "/rcpdbase.txt"))
X;;;XXX;;; And then
X;;;XXX;; (bbdb-eudora-rcpdbase-output)
X
X(require 'bbdb-print)
X(require 'basic-ext)
X
X(defvar bbdb-eudora-nndbase-filename "~/nndbase.txt"
X  "*Default file name for bbdb-output-eudora printouts of BBDB database.")
X
X(defun bbdb-eudora-nndbase-output (to-file)
X  "Print the selected BBDB entries"
X  (interactive (list (read-file-name "Print To File: " bbdb-eudora-nndbase-filename)))
X  (setq bbdb-eudora-nndbase-filename (expand-file-name to-file))
X  (let ((current-letter t)
X	(records (progn (set-buffer bbdb-buffer-name)
X			bbdb-records)))
X    (find-file bbdb-eudora-nndbase-filename)
X    (delete-region (point-min) (point-max))
X    (while records
X      (setq current-letter 
X	    (boe-format-record (car (car records)) current-letter))
X      (setq records (cdr records)))
X    (goto-char (point-min))
X    (message "Eudora nickname file %s generated." bbdb-eudora-nndbase-filename)))
X
X(defsubst boe-print-if-not-blank (string prepend-string &rest more)
X  "If STRING is not null, then return it with PREPEND-STRING in front and concatenated
Xwith rest of arguments.  If it is null, then all arguments are 
Xignored and the null string is returned."
X  (if (or (null string) (equal "" string))
X      ""
X    (apply 'concat prepend-string string more)))
X
X(defun boe-output-this-record-p (name comp net phones addrs notes)
X  "Examine NAME COMP NET PHONES ADDRS NOTES and return t if 
Xthe current record is to be output by bbdb-output-eudora."
X  ;; if name is non-nil, output it
X  (cond ((and name net) t)
X	(t nil))
X  )
X
X
X(defun boe-format-record (record &optional current-letter brief)
X  "Insert the bbdb RECORD in Eudora format.
XOptional CURRENT-LETTER is the section we're in -- if this is non-nil and
Xthe first letter of the sortkey of the record differs from it, a new section
Xheading will be output \(an arg of t will always produce a heading).
XThe new current-letter is the return value of this function.
XSomeday, optional third arg BRIEF will produce one-line format."
X  (bbdb-debug (if (bbdb-record-deleted-p record)
X		  (error "plus ungood: formatting deleted record")))
X  
X  (let* ((bbdb-elided-display bbdb-print-elide)
X	 (first-letter 
X	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
X	 (name   (and (bbdb-field-shown-p 'name)
X		      (or (bbdb-record-getprop record 'tex-name)
X			  (bbdb-record-name record))))
X	 (comp   (and (bbdb-field-shown-p 'company)
X		      (bbdb-record-company record)))
X	 (net    (and (bbdb-field-shown-p 'net)
X		      (bbdb-record-net record)))
X	 (phones (and (bbdb-field-shown-p 'phone)
X		      (bbdb-record-phones record)))
X	 (addrs  (and (bbdb-field-shown-p 'address)
X		      (bbdb-record-addresses record)))
X	 (notes  (bbdb-record-raw-notes record))
X	 (begin (point))
X	 (bare t))
X
X    ;; Section header, if neccessary.
X
X    (if (and current-letter (not (string-equal first-letter current-letter)))
X	  (message "Now processing \"%s\" entries..." (upcase first-letter)))
X    
X    (if (boe-output-this-record-p name comp net phones addrs notes)
X	(progn 
X
X	  ;; Eudora nickname in canonical form (e.g., mohsen.banan)
X	  ;;
X	  (if name
X	      (insert (format "<%s>\n" name)))
X
X	  ;; Email address -- just use their first address.
X	  ;;  Make all dots legal line-breaks.
X	  ;;
X	  ;; output in the following format:  "<Pretty Name>" <email address>
X	  (if net
X	      (let ((net-addr (car net))
X		    (start 0))
X		(insert (format ">\"%s\" <%s>\n" name net-addr))))
X
X	  ;; start a Eudora nndbase.txt notes section for this nickname
X	  ;; by inserting the nickname again
X	  
X	  (if name
X	      (insert (format "<%s>\n" name)))
X	  
X	  ;; Company
X	  ;;
X	  (if comp
X	      (insert (format "> Company: %s\n"
X			      (boe-mangle-if-multi-line comp))))
X
X	  ;; Phone numbers
X	  ;;
X	  (while phones
X	    (let ((place (aref (car phones) 0))
X		  (number (bbdb-phone-string (car phones))))
X	      (setq bare nil)
X	      (insert (format "> Telephone: %s%s\n" 
X			      (boe-print-if-not-blank place "" ": ")
X			      number))
X	      (setq phones (cdr phones))))
X
X	  ;; Addresses
X	  ;;
X	  (while addrs
X	    (let ((addr (car addrs)))
X	      (setq bare nil)
X	      (insert
X	       (format 
X		"> Address: \n%s"
X		(concat 
X		 (boe-print-if-not-blank (bbdb-address-street1 addr) "> " "\n")
X		 (boe-print-if-not-blank (bbdb-address-street2 addr) "> " "\n")
X		 (boe-print-if-not-blank (bbdb-address-street3 addr) "> ")
X		 (boe-print-if-not-blank (bbdb-address-city addr) "> ") 
X		 (if (and (not (equal "" (bbdb-address-city addr)))
X			  (not (equal "" (bbdb-address-state addr))))
X		     ", ")
X		 (boe-print-if-not-blank (bbdb-address-state addr) "" " ")
X		 (boe-print-if-not-blank (bbdb-address-zip-string addr) "" "\n")))))
X	    (setq addrs (cdr addrs)))
X
X	  ;; BBDB Notes
X
X	  (if (stringp notes)
X	      (setq notes (list (cons 'notes notes))))
X	  (while notes
X	    (let ((thisnote (car notes)))
X	      (if (bbdb-field-shown-p (car thisnote))
X		  (progn
X		    (setq bare nil)
X		    (if (eq 'notes (car thisnote))
X			(insert (format "> Notes: %s\n"
X					(boe-mangle-if-multi-line (cdr thisnote))))
X		      (insert (format "> Note [%s]: %s\n" 
X				      (symbol-name (car thisnote))
X				      (boe-mangle-if-multi-line (cdr thisnote))))))))
X	    (setq notes (cdr notes)))
X
X	  ;; If record is bare, delete anything we may have inserted.
X	  ;; otherwise, mark the end of this record.
X
X	  (if bare
X	      (delete-region begin (point))
X
X	    (setq current-letter first-letter))
X	  
X	  ))
X
X    ;; return current letter
X    current-letter))
X
X
X(defun boe-mangle-if-multi-line (string)
X  "If STRING is has multiple lines, mangle it for output to Eudora" 
X  (if (string-match "\n" string)
X      (string-replace-regexp string "\n" "\n> ")
X    string))
X
X
X;;;;;;;;;;;; Eudora Receipient DataBase (rcpdbase.txt) ;;;;;;;;;;;
X
X;;;(setq bbdb-eudora-rcpdbase-filename "/dos/m/eudora.mai/mohsen/rcpdbase.txt")
X(defvar bbdb-eudora-rcpdbase-filename "~/rcpdbase.txt"
X  "*Default file name for bbdb-output-eudora printouts of BBDB database.")
X
X(defun bbdb-eudora-rcpdbase-output (to-file)
X  "Print the selected BBDB entries"
X  (interactive (list (read-file-name "Print To File: " bbdb-eudora-rcpdbase-filename)))
X  (setq bbdb-eudora-rcpdbase-filename (expand-file-name to-file))
X  (let ((current-letter t)
X	(records (progn (set-buffer bbdb-buffer-name)
X			bbdb-records)))
X    (find-file bbdb-eudora-rcpdbase-filename)
X    (delete-region (point-min) (point-max))
X    (while records
X      (setq current-letter 
X	    (boe-rcpdbase-format-record (car (car records)) current-letter))
X      (setq records (cdr records)))
X    (goto-char (point-min))
X    (message "Eudora rcpt. file %s generated." bbdb-eudora-nndbase-filename)))
X
X
X
X(defun boe-rcpdbase-format-record (record &optional current-letter brief)
X  "Insert the bbdb RECORD in Eudora format.
XOptional CURRENT-LETTER is the section we're in -- if this is non-nil and
Xthe first letter of the sortkey of the record differs from it, a new section
Xheading will be output \(an arg of t will always produce a heading).
XThe new current-letter is the return value of this function.
XSomeday, optional third arg BRIEF will produce one-line format."
X  (bbdb-debug (if (bbdb-record-deleted-p record)
X		  (error "plus ungood: formatting deleted record")))
X  
X  (let* ((bbdb-elided-display bbdb-print-elide)
X	 (first-letter 
X	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
X	 (name   (and (bbdb-field-shown-p 'name)
X		      (or (bbdb-record-getprop record 'tex-name)
X			  (bbdb-record-name record))))
X	 (comp   (and (bbdb-field-shown-p 'company)
X		      (bbdb-record-company record)))
X	 (net    (and (bbdb-field-shown-p 'net)
X		      (bbdb-record-net record)))
X	 (phones (and (bbdb-field-shown-p 'phone)
X		      (bbdb-record-phones record)))
X	 (addrs  (and (bbdb-field-shown-p 'address)
X		      (bbdb-record-addresses record)))
X	 (notes  (bbdb-record-raw-notes record))
X	 (begin (point))
X	 (bare t))
X
X    ;; Section header, if neccessary.
X
X    (if (and current-letter
X	     (not (string-equal first-letter current-letter)))
X	(message "Now processing \"%s\" entries..." (upcase first-letter)))
X    
X    (if (boe-output-this-record-p name comp net phones addrs notes)
X	(progn 
X
X	  ;; Eudora nickname in canonical form (e.g., mohsen.banan)
X	  ;;
X	  (if name
X	      (insert (format "%s\n" name)))
X
X	  (setq current-letter first-letter)
X	  
X	  ))
X
X    ;; return current letter
X    current-letter))
END_OF_FILE
if test 9916 -ne `wc -c <'bbdb-eudora.el'`; then
    echo shar: \"'bbdb-eudora.el'\" unpacked with wrong size!
fi
# end of 'bbdb-eudora.el'
fi
if test -f 'bbdb-export.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-export.el'\"
else
echo shar: Extracting \"'bbdb-export.el'\" \(4676 characters\)
sed "s/^X//" >'bbdb-export.el' <<'END_OF_FILE'
X;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;;  collection of input and output filters for BBDB.
X;;; 
X;;;  Copyright (C) 1995 Neda Communications, Inc.
X;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
X;;; 
X;;;  This library is free software; you can redistribute it and/or modify
X;;;  it under the terms of the GNU Library General Public License as
X;;;  published by the Free Software Foundation; either version 2 of the
X;;;  License, or (at your option) any later version.  This library is
X;;;  distributed in the hope that it will be useful, but WITHOUT ANY
X;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;;  License for more details.  You should have received a copy of the GNU
X;;;  Library General Public License along with this library; if not, write
X;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;;  USA.
X;;; 
X;;; This is bbdb-export.el
X;;;
X
X(defvar bbdb-export-buffer-name "*BBDB* Export"
X  "*Default buffer name for exporting the contents of the *BBDB* buffer.")
X
X
X(defvar bbdb-export-compactly nil 
X  "If nil, the exported records are compactly printed.  
XOtherwise the exported forms are indented for human-readability (at a
Xcost of somewhat longer processing time for exporting records.  
XThe default value is nil.")
X
X
X(defun bbdb-export ()
X  "Print the selected BBDB entries"
X  (interactive)
X  (save-excursion
X    (let ((to-buffer (get-buffer-create bbdb-export-buffer-name))
X	  (records (progn (set-buffer bbdb-buffer-name)
X			  bbdb-records))
X	  (current-letter ""))
X      ;; wipe to-buffer
X      (switch-to-buffer to-buffer)
X      (delete-region (point-min) (point-max))
X
X      ;; insert header, records, trailer
X      (bexp-buffer-insert-header)
X      (while records
X	(setq current-letter (bexp-do-record (car (car records)) current-letter))
X	(setq records (cdr records)))
X      (bexp-buffer-insert-trailer)
X      
X      (goto-char (point-min))
X      (search-forward "(progn")
X      (search-backward "(progn")
X      (indent-sexp)
X      ))
X  (message "BBDB export buffer %s generated." bbdb-export-buffer-name))
X
X
X(defun bexp-do-record (record current-letter)
X  "Insert the bbdb RECORD in export format."
X  (let* ((name   (bbdb-record-name record))
X	 (comp   (bbdb-record-company record))
X	 (net    (bbdb-record-net record))
X	 (phones (bbdb-record-phones record))
X	 (addrs  (bbdb-record-addresses record))
X	 (notes  (bbdb-record-raw-notes record))
X	 (first-letter (upcase (substring (concat (bbdb-record-sortkey record) "?") 0 1))))
X
X    (if (not (string-equal first-letter current-letter))
X	(progn (message "Now processing \"%s\" entries..." first-letter)
X	       (sleep-for 1)))
X    (bexp-buffer-insert-record name comp net addrs phones notes)
X    first-letter))
X
X
X(defun bexp-buffer-insert-header()
X  (insert ";;; ======= Start of Exported BBDB Records =======\n")
X  (insert "(progn  
X(require 'bbdb-com)
X(defun bbdb-maybe-create (name company net &optional addrs phones notes)
X  \"Try to add a record to BBDB if it does not already exist.\"
X  (condition-case err
X      (progn
X	(bbdb-create-internal name company net addrs phones notes)
X	(message \"%s %s added.\" name (if net (concat \"<\" net \">\") \"\"))
X	(sleep-for 1))    
X    (error (ding)
X	   (message \"%s %s skipped. (%s)\"
X		    name
X		    (if net (concat \"<\" net \">\") \"\")
X		    (car (cdr err)))
X	   (sleep-for 1))))\n\n")
X  (normal-mode))
X
X
X(defun bexp-buffer-insert-trailer()
X  (insert ")\n")
X  (insert ";;; ======= End of Exported BBDB Records =======\n"))
X
X
X(defun bexp-buffer-insert-record (name comp net addrs phones notes)
X  (let ((begin (point))
X	end)
X    (message "Exporting %s" name)
X    (insert (format "(bbdb-maybe-create %s %s '%s '%s '%s '%s)\n"
X		    (prin1-to-string (concat name "--IMPORTED"))
X		    (prin1-to-string comp)
X		    (prin1-to-string net)
X		    (prin1-to-string addrs)
X		    (prin1-to-string phones)
X		    (prin1-to-string notes)
X		    ))
X    (setq end (point))
X    (if (not bbdb-export-compactly) 
X	(progn
X	  ;; format region
X	  (narrow-to-region begin end)
X	  (goto-char begin)
X	  (replace-string " '(" "\n'(")
X	  (goto-char begin)
X	  (replace-string "\" \"" "\"\n\"")
X	  (goto-char begin)
X	  (replace-string "((" "(\n(")
X	  (goto-char begin)
X	  (replace-string "))" ")\n)")
X	  (goto-char begin)
X	  (replace-string "([" "(\n[")
X	  (goto-char begin)
X	  (replace-string "])" "]\n)")
X	  (goto-char begin)
X	  (replace-string ") (" ")\n(")
X	  (goto-char begin)
X	  (replace-string "] [" "]\n[")
X	  (goto-char (point-max))
X	  (lisp-indent-region begin (point))
X	  (widen)))
X    ))
X
X(provide 'bbdb-export)
END_OF_FILE
if test 4676 -ne `wc -c <'bbdb-export.el'`; then
    echo shar: \"'bbdb-export.el'\" unpacked with wrong size!
fi
# end of 'bbdb-export.el'
fi
if test -f 'bbdb-hp200lx.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-hp200lx.el'\"
else
echo shar: Extracting \"'bbdb-hp200lx.el'\" \(10757 characters\)
sed "s/^X//" >'bbdb-hp200lx.el' <<'END_OF_FILE'
X;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;;  collection of input and output filters for BBDB.
X;;; 
X;;;  Copyright (C) 1995 Neda Communications, Inc.
X;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
X;;; 
X;;;  This library is free software; you can redistribute it and/or modify
X;;;  it under the terms of the GNU Library General Public License as
X;;;  published by the Free Software Foundation; either version 2 of the
X;;;  License, or (at your option) any later version.  This library is
X;;;  distributed in the hope that it will be useful, but WITHOUT ANY
X;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;;  License for more details.  You should have received a copy of the GNU
X;;;  Library General Public License along with this library; if not, write
X;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;;  USA.
X;;; 
X;;; This is bbdb-hp200lx.el
X;;;
X;;;
X;;; RCS: bbdb-hp200lx.el,v 1.1.1.1 1995/08/07 08:43:09 mohsen Exp
X;;;
X;;; a copy-and-edit job on bbdb-print.el
X
X
X;;; To use this, add the following to your .emacs
X;;; and strip ";;;XXX"
X;;;
X
X;;;XXX;; BBDB HP200LX Filter
X;;;XXX(load "bbdb-hp200lx")
X
X;;;XXX(setq bbdb-hp200lx-filename
X;;;XXX      (concat "/dos/u/" (user-login-name) "/bb-phone.cdf"))
X;;;XXX;;; - to output the *BBDB* buffer in HP200LX comma-delimited-file (.CDF)
X;;;XXX;;; format, invoke M-x bbdb-hp200lx-output
X;;;XXX;;;
X;;;XXX;;; - you may also want to modify default values of the following (use
X;;;XXX;;;   M-x describe-variable for details):
X;;;XXX;;;     bbdb-hp200lx-output-elide
X;;;XXX;;;     bbdb-hp200lx-output-requires
X;;;XXX;;;     bbdb-hp200lx-output-no-bare-names
X
X
X(require 'bbdb-print)
X(require 'basic-ext)
X
X
X(defvar bbdb-hp200lx-filename "~/bb-phone.cdf"
X  "*Default file name for bbdb-output-hp200lx printouts of BBDB database.")
X
X
X(defvar bbdb-hp200lx-output-elide '(net creation-date timestamp mail-alias)
X  "*List of symbols denoting BBDB fields NOT to be output.
XValid symbols are: name comp net phones addrs.  You can also use the
Xtags for notes (e.g., creation-date).
X  e.g.: '(net creation-date)
XSee also variable bbdb-hp200lx-output-requires.")
X
X
X(defvar bbdb-hp200lx-output-requires '(or name comp)
X  "*A boolean expression of 'and' and 'or' to be evaluated to determine if
Xthe current record should be output.  Valid symbols for use
Xin the boolean expression are: name comp net phones addrs notes.
X  e.g.: (and name (or comp addrs))
XSee also variable bbdb-hp200lx-output-elide.
X")
X
X
X(defvar bbdb-hp200lx-output-no-bare-names t
X  "*A bare name is one with no information other than
Xthat in bbdb-hp200lx-output-requires.  To avoid printing
Xthese set this variable to t")
X
X
X(defun bbdb-hp200lx-output (to-file)
X  "Print the selected BBDB entries"
X  (interactive (list (read-file-name "Print To File: " bbdb-hp200lx-filename)))
X  (setq bbdb-hp200lx-filename (expand-file-name to-file))
X  (let ((current-letter t)
X	(records (progn (set-buffer bbdb-buffer-name)
X			bbdb-records)))
X    (find-file bbdb-hp200lx-filename)
X    (delete-region (point-min) (point-max))
X    (while records
X      (setq current-letter
X	    (boh-maybe-format-record (car (car records)) current-letter))
X      (setq records (cdr records)))
X    (goto-char (point-min))
X    (message "HP 200LX comma-delimited phonebook file %s generated." bbdb-hp200lx-filename)))
X
X
X(defun boh-maybe-format-record (record &optional current-letter brief)
X  "Insert the bbdb RECORD in Hp200lx format.
XOptional CURRENT-LETTER is the section we're in -- if this is non-nil and
Xthe first letter of the sortkey of the record differs from it, a new section
Xheading will be output \(an arg of t will always produce a heading).
XThe new current-letter is the return value of this function.
XSomeday, optional third arg BRIEF will produce one-line format."
X  (bbdb-debug (if (bbdb-record-deleted-p record)
X		  (error "plus ungood: formatting deleted record")))
X
X
X  (let* ((bbdb-elided-display bbdb-hp200lx-output-elide)
X	 (first-letter
X	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
X	 (name   (and (bbdb-field-shown-p 'name)
X		      (or (bbdb-record-getprop record 'tex-name)
X			  (bbdb-record-name record))))
X	 (comp   (and (bbdb-field-shown-p 'company)
X		      (bbdb-record-company record)))
X	 (net    (and (bbdb-field-shown-p 'net)
X		      (bbdb-record-net record)))
X	 (phones (and (bbdb-field-shown-p 'phone)
X		      (bbdb-record-phones record)))
X	 (addrs  (and (bbdb-field-shown-p 'address)
X		      (bbdb-record-addresses record)))
X	 (notes  (bbdb-record-raw-notes record))
X	 (begin (point))
X	 (bare t))
X
X
X    ;; Section header, if neccessary.
X
X
X    (if (and current-letter (not (string-equal first-letter current-letter)))
X	(message "Now processing \"%s\" entries..." (upcase first-letter)))
X
X
X    (if (eval bbdb-hp200lx-output-requires)
X	(let (more-phones)
X
X
X	  ;; HP 200LX last name field (maxlen 86 ??) -- used for BBDB name
X	  ;;
X	  (insert (format "\"%s\"," (boh-maybe-truncate name 86)))
X
X
X	  ;; HP 200LX first name field (maxlen ??) -- unused
X	  (insert ",")
X
X
X	  ;; HP 200LX middle name field (maxlen ??) -- unused
X	  ;;
X	  (insert ",")
X
X
X	  ;; Phone numbers
X	  ;;
X	  (let (business-phone home-phone fax-phone saved-case-fold)
X	    (setq saved-case-fold case-fold-search
X		  case-fold-search t)
X	    (while phones
X	      (let ((place (aref (car phones) 0))
X		    (number (bbdb-phone-string (car phones))))
X		(cond ((or (string-match place "office")
X			   (string-match place "work"))
X		       (if (null business-phone)
X			   (setq business-phone (list place number))
X			 (setq more-phones (cons (list place number) more-phones))))
X		      ((string-match place "home")
X		       (if (null home-phone)
X			   (setq home-phone (list place number))
X			 (setq more-phones (cons (list place number) more-phones))))
X		      ((or (string-match place "fax")
X			   (string-match place "facsimile"))
X		       (if (null fax-phone)
X			   (setq fax-phone (list place number))
X			 (setq more-phones (cons (list place number) more-phones))))
X		      (t
X		       (setq more-phones (cons (list place number) more-phones)))))
X	      (setq phones (cdr phones)))
X
X
X	    (setq case-fold-search saved-case-fold)
X
X
X	    ;; HP 200LX business phone field (maxlen 29)
X	    (if business-phone
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate
X					     (format "%s" (car (cdr business-phone)))
X					     29)))
X		  (setq bare nil))
X	      (insert ","))
X
X
X	    ;; HP 200LX home phone field (maxlen 29)
X	    (if home-phone
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate
X					     (format "%s" (car (cdr home-phone)))
X					     29)))
X		  (setq bare nil))
X	      (insert ","))
X
X
X	    ;; HP 200LX alternate phone field (maxlen 29) -- unused
X	    (insert ",")
X
X
X	    ;; HP 200LX fax phone field (maxlen 29)
X	    (if fax-phone
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate
X					     (format "%s" (car (cdr fax-phone))) ; the description
X					     29)))
X		  (setq bare nil))
X	      (insert ","))
X	    )
X
X
X	  ;; HP 200LX title field (maxlen 38) -- unused
X	  (insert ",")
X
X
X	  ;; HP 200LX category field (maxlen 127) -- unused
X	  (insert ",")
X
X
X	  ;; HP 200LX company field (maxlen 82) -- used for BBDB company
X	  (if comp
X	      (insert (format "\"%s\"," (boh-maybe-truncate comp 82)))
X	    (insert ","))
X
X
X	  ;; Addresses
X	  ;;
X	  (let ((addr (car addrs))	;just take the first bbdb address
X		hp-addr1 hp-addr2 hp-city hp-state hp-zip)
X
X	    (if addr
X		(progn
X		  (setq hp-addr1 (bbdb-address-street1 addr))
X		  (setq hp-addr2 (concat (bbdb-address-street2 addr)
X 					 (if (and (> (length (bbdb-address-street2 addr)) 0)
X 						  (> (length (bbdb-address-street3 addr)) 0))
X 					     ", " "")
X					 (bbdb-address-street3 addr)))
X		  (setq hp-city (bbdb-address-city addr))
X		  (setq hp-state (bbdb-address-state addr))
X		  (setq hp-zip (bbdb-address-zip-string addr))))
X
X	    ;; HP 200LX address 1 field (maxlen 82)
X	    (if hp-addr1
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate hp-addr1 82)))
X		  (setq bare nil))
X	      (insert ","))
X
X	    ;; HP 200LX address 2 field (maxlen 82)
X	    (if hp-addr2
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate hp-addr2 82)))
X		  (setq bare nil))
X	      (insert ","))
X
X	    ;; HP 200LX city field (maxlen 34)
X	    (if hp-city
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate hp-city 34)))
X		  (setq bare nil))
X	      (insert ","))
X
X	    ;; HP 200LX state field (maxlen 39)
X	    (if hp-state
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate hp-state 39)))
X		  (setq bare nil))
X	      (insert ","))
X
X	    ;; HP 200LX zip field (maxlen 16)
X	    (if hp-zip
X		(progn
X		  (insert (format "\"%s\"," (boh-maybe-truncate hp-zip 16)))
X		  (setq bare nil))
X	      (insert ","))
X	    )
X
X	  ;; BBDB Notes
X
X	  (let (hp-note)
X	    (save-excursion
X	      (set-buffer (get-buffer-create " *boh-scratch*"))
X	      (kill-region (point-min) (point-max))
X
X	      (while more-phones
X		(insert (format "%s: %s\t"
X				(car (car more-phones)) ; the tag
X				(car (cdr (car more-phones)))) ; the number
X			)
X		(setq bare nil)
X		(setq more-phones (cdr more-phones)))
X
X	      ;; output BBDB email-addresses
X	      (while net
X		(insert (format "%s\t" (car net)))
X		(setq bare nil)
X		(setq net (cdr net)))
X
X	      (if (stringp notes)
X		  (setq notes (list (cons 'notes notes))))
X
X	      (while notes
X		(let ((thisnote (car notes)))
X		  (if (bbdb-field-shown-p (car thisnote))
X		      (progn
X			(setq bare nil)
X			(if (eq 'notes (car thisnote))
X			    (insert (format "Notes: %s\t" (boh-mangle-if-multi-line (cdr thisnote))))
X			  (insert (format "Note [%s]: %s\t"
X					  (symbol-name (car thisnote))
X					  (boh-mangle-if-multi-line (cdr thisnote))))))))
X		(setq notes (cdr notes)))
X
X	      (setq hp-note (buffer-string)))
X
X	    ;; HP 200LX notes field (32K for the entire record)
X	    (if (> (length hp-note) 0)
X		(progn
X		  (insert (format "\"%s\"" hp-note))
X		  (setq bare nil)))
X	    )
X
X	  ;; If record is bare, delete anything we may have inserted.
X	  ;; otherwise, mark the end of this record.
X	  (if (and bare bbdb-hp200lx-output-no-bare-names)
X	      (delete-region begin (point))
X	    (insert "\n"))		; HP 200LX end of record
X	  ))
X
X    ;; return current letter
X    current-letter))
X
X
X(defun boh-maybe-truncate (string maxlen)
X  "If STRING is longer than MAXLEN, returns a truncated version."
X  (if (> (length string) maxlen)
X      (substring string 0 maxlen)
X    string))
X
X
X(defun boh-mangle-if-multi-line (string)
X  "If STRING is has multiple lines, mangle it for output to HP200LX"
X  (if (string-match "\n" string)
X      (string-replace-regexp string "\n" "\t") ; tabs are used to denote new lines in the .cdf file
X  string))
END_OF_FILE
if test 10757 -ne `wc -c <'bbdb-hp200lx.el'`; then
    echo shar: \"'bbdb-hp200lx.el'\" unpacked with wrong size!
fi
# end of 'bbdb-hp200lx.el'
fi
if test -f 'bbdb-ph.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-ph.el'\"
else
echo shar: Extracting \"'bbdb-ph.el'\" \(8467 characters\)
sed "s/^X//" >'bbdb-ph.el' <<'END_OF_FILE'
X;;;  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;;  collection of input and output filters for BBDB.
X;;; 
X;;;  Copyright (C) 1995 Neda Communications, Inc.
X;;; 	Prepared by Mohsen Banan (mohsen@neda.com)
X;;; 
X;;;  This library is free software; you can redistribute it and/or modify
X;;;  it under the terms of the GNU Library General Public License as
X;;;  published by the Free Software Foundation; either version 2 of the
X;;;  License, or (at your option) any later version.  This library is
X;;;  distributed in the hope that it will be useful, but WITHOUT ANY
X;;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;;  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;;  License for more details.  You should have received a copy of the GNU
X;;;  Library General Public License along with this library; if not, write
X;;;  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;;  USA.
X;;; 
X;;; This is bbdb-ph.el
X;;;
X;;;
X;;; RCS: bbdb-ph.el,v 1.1.1.1 1995/08/07 08:43:08 mohsen Exp
X;;;
X;;; a copy-and-edit job on bbdb-print.el
X
X
X;;; To use this, add the following to your .emacs
X;;; and strip ";;;XXX"
X;;;
X
X;;;XXX;; BBDB PH Filter
X;;;XXX(load "bbdb-ph")
X
X;;;XXX(setq bbdb-ph-filename
X;;;XXX      (concat "/dos/u/" (user-login-name) "/bb-phone.cdf"))
X;;;XXX;;; - to output the *BBDB* buffer in PH tab-delimited-file (.CDF)
X;;;XXX;;; format, invoke M-x bbdb-ph-output
X;;;XXX;;;
X;;;XXX;;; - you may also want to modify default values of the following (use
X;;;XXX;;;   M-x describe-variable for details):
X;;;XXX;;;     bbdb-ph-output-elide
X;;;XXX;;;     bbdb-ph-output-requires
X;;;XXX;;;     bbdb-ph-output-no-bare-names
X
X
X(require 'bbdb-print)
X(require 'basic-ext)
X
X
X(defvar bbdb-ph-filename "~/data.out"
X  "*Default file name for bbdb-output-ph printouts of BBDB database.")
X
X
X(defvar bbdb-ph-output-elide '(creation-date timestamp mail-alias)
X  "*List of symbols denoting BBDB fields NOT to be output.
XValid symbols are: name comp net phones addrs.  You can also use the
Xtags for notes (e.g., creation-date).
X  e.g.: '(net creation-date)
XSee also variable bbdb-ph-output-requires.")
X
X
X(defvar bbdb-ph-output-requires '(and name net)
X  "*A boolean expression of 'and' and 'or' to be evaluated to determine if
Xthe current record should be output.  Valid symbols for use
Xin the boolean expression are: name comp net phones addrs notes.
X  e.g.: (and name (or comp addrs))
XSee also variable bbdb-ph-output-elide.
X")
X
X
X(defvar bbdb-ph-output-no-bare-names t
X  "*A bare name is one with no information other than
Xthat in bbdb-ph-output-requires.  To avoid printing
Xthese set this variable to t")
X
X
X(defun bbdb-ph-output (to-file)
X  "Print the selected BBDB entries"
X  (interactive (list (read-file-name "Print To File: " bbdb-ph-filename)))
X  (setq bbdb-ph-filename (expand-file-name to-file))
X  (let ((current-letter t)
X	(records (progn (set-buffer bbdb-buffer-name)
X			bbdb-records)))
X    (find-file bbdb-ph-filename)
X    (delete-region (point-min) (point-max))
X    (while records
X      (setq current-letter
X	    (boph-maybe-format-record (car (car records)) current-letter))
X      (setq records (cdr records)))
X    (goto-char (point-min))
X    (message "PH tag and tab-delimited file %s generated." bbdb-ph-filename)))
X
X
X(defun boph-maybe-format-record (record &optional current-letter brief)
X  "Insert the bbdb RECORD in Ph format.
XOptional CURRENT-LETTER is the section we're in -- if this is non-nil and
Xthe first letter of the sortkey of the record differs from it, a new section
Xheading will be output \(an arg of t will always produce a heading).
XThe new current-letter is the return value of this function.
XSomeday, optional third arg BRIEF will produce one-line format."
X  (bbdb-debug (if (bbdb-record-deleted-p record)
X		  (error "plus ungood: formatting deleted record")))
X
X  (let* ((bbdb-elided-display bbdb-ph-output-elide)
X	 (first-letter
X	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
X	 (name   (and (bbdb-field-shown-p 'name)
X		      (or (bbdb-record-getprop record 'tex-name)
X			  (bbdb-record-name record))))
X	 (comp   (and (bbdb-field-shown-p 'company)
X		      (bbdb-record-company record)))
X	 (net    (and (bbdb-field-shown-p 'net)
X		      (bbdb-record-net record)))
X	 (phones (and (bbdb-field-shown-p 'phone)
X		      (bbdb-record-phones record)))
X	 (addrs  (and (bbdb-field-shown-p 'address)
X		      (bbdb-record-addresses record)))
X	 (notes  (bbdb-record-raw-notes record))
X	 (begin (point))
X	 (bare t)
X	 ph-name ph-email ph-office-phone ph-skypager ph-portable
X	 ph-title-notes-part
X	 saved-case-fold)
X
X
X    ;; Section header, if neccessary.
X
X    (if (and current-letter (not (string-equal first-letter current-letter)))
X	(message "Now processing \"%s\" entries..." (upcase first-letter)))
X
X
X    (if (eval bbdb-ph-output-requires)
X	(progn
X
X	  ;; =============================================================
X	  ;; grovel through BBDB record collecting ph-relevant information
X	  ;; =============================================================
X
X	  ;; grovel through name
X	  ;;
X	  (setq ph-name name)
X
X	  ;; grovel through phone numbers
X	  ;;
X	  (progn
X	    (setq saved-case-fold case-fold-search
X		  case-fold-search t)
X	    (while phones
X	      (let ((place (aref (car phones) 0))
X		    (number (bbdb-phone-string (car phones))))
X		(cond ((or (string-match place "office")
X			   (string-match place "work"))
X		       (if (null ph-office-phone)
X			   (setq ph-office-phone number)))
X		      ((or (string-match place "mobile")
X			   (string-match place "cellular"))
X		       (if (null ph-portable)
X			   (setq ph-portable number)))
X		      (t nil)))
X	      (setq phones (cdr phones)))
X
X
X	    (setq case-fold-search saved-case-fold)
X	    )
X
X	  ;; grovel through BBDB email-addresses
X	  ;;
X	  (if net
X	      (setq ph-email (car net)))
X
X	  ;; grovel through BBDB Notes
X	  ;;
X	  (progn 
X
X	    (if (stringp notes)
X		(setq notes (list (cons 'notes notes))))
X
X	    (while notes
X	      (let ((curr-note (car notes)))
X		(if (bbdb-field-shown-p (car curr-note))
X		    (cond ((member (car curr-note) '(skypage pager))
X			   (setq ph-skypager (boph-mangle-if-multi-line (cdr curr-note))))
X			  ((equal (car curr-note) 'mobile)
X			   (setq ph-portable (boph-mangle-if-multi-line (cdr curr-note))))
X			  ((equal (car curr-note) 'notes)
X			   (setq ph-title-notes-part (boph-mangle-if-multi-line (cdr curr-note))))
X			  (t nil))
X		  ))
X	      (setq notes (cdr notes)))
X	    )
X
X	  ;; grovel through comp
X	  ;;
X	  (setq ph-title-coname-part comp)
X	  (setq ph-title (concat (or ph-title-coname-part "")
X				 (if (and ph-title-coname-part ph-title-notes-part) " " "")
X				 (if ph-title-notes-part (concat "[" ph-title-notes-part "]") "")))
X
X	  ;; ====================
X	  ;; now output PH record
X	  ;; ====================
X
X	  ;; PH 'name' field (maxlen 256)
X	  ;;
X	  (insert (format "3:%s\t" (boph-maybe-truncate (or name "") 256)))
X
X	  ;; PH 'email' field (maxlen 25) (should be 128?)  ** NOT YET **
X	  (if ph-email (setq bare nil))
X	  (insert (format "2:%s\t" (boph-maybe-truncate (or ph-email "") 25)))
X
X	  ;; PH 'office_phone' field (max len 60)
X	  ;;
X	  (if ph-office-phone (setq bare nil))
X	  (insert (format "32:%s\t" (boph-maybe-truncate (or ph-office-phone "") 60)))
X
X	  ;; PH 'title' field (maxlen 120) 
X	  (insert (format "98:%s\t" (boph-maybe-truncate ph-title 120)))
X
X	  ;; PH 'portable' field (maxlen 60)
X	  (if ph-portable (setq bare nil))
X	  (insert (format "97:%s\t" (boph-maybe-truncate (or ph-portable "") 60)))
X
X	  ;; PH 'skypager' field (maxlen 64)
X	  (if ph-skypager (setq bare nil))
X	  (insert (format "27:%s\t" (boph-maybe-truncate (or ph-skypager "") 64)))
X
X	  ;; ==========
X	  ;; bare check
X	  ;; ==========
X
X	  ;; If record is bare, delete anything we may have inserted.
X	  ;; otherwise, mark the end of this record.
X	  (if (and bare bbdb-ph-output-no-bare-names)
X	      (delete-region begin (point))
X	    (insert "\n"))		; PH end of record
X	  ))
X
X    ;; return current letter
X    current-letter))
X
X
X(defun boph-maybe-truncate (string maxlen)
X  "If STRING is longer than MAXLEN, returns a truncated version."
X  (if (> (length string) maxlen)
X      (substring string 0 maxlen)
X    string))
X
X
X(defun boph-mangle-if-multi-line (string)
X  "If STRING is has multiple lines, mangle it for output to PH"
X  (if (string-match "\n" string)
X      (string-replace-regexp string "\n" "\t") ; tabs are used to denote new lines in the .cdf file
X  string))
END_OF_FILE
if test 8467 -ne `wc -c <'bbdb-ph.el'`; then
    echo shar: \"'bbdb-ph.el'\" unpacked with wrong size!
fi
# end of 'bbdb-ph.el'
fi
if test -f 'bbdb-passwd.el' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'bbdb-passwd.el'\"
else
echo shar: Extracting \"'bbdb-passwd.el'\" \(6613 characters\)
sed "s/^X//" >'bbdb-passwd.el' <<'END_OF_FILE'
X;;; This file is part of the BBDB Filters Package. BBDB Filters Package is a
X;;; collection of input and output filters for BBDB.
X;;;
X;;; Copyright (C) 1995 Neda Communications, Inc.
X;;;        Prepared by Mohsen Banan (mohsen@neda.com)
X;;;
X;;; This library is free software; you can redistribute it and/or modify
X;;; it under the terms of the GNU Library General Public License as
X;;; published by the Free Software Foundation; either version 2 of the
X;;; License, or (at your option) any later version.  This library is
X;;; distributed in the hope that it will be useful, but WITHOUT ANY
X;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
X;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X;;; License for more details.  You should have received a copy of the GNU
X;;; Library General Public License along with this library; if not, write
X;;; to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X;;; USA.
X
X;;; This is bbdb-passwd.el
X
X;;; This file is a bbdb filter.  It converts passwd files to the
X;;; canonical bbdb input filter format (i.e., a file of
X;;; bif-create-record expressions
X
X
X(defvar bpf-default-bif-file "~/passwd-bif.el"
X  "*Default file name for bbdb-passwd-input.")
X
X
X(defvar bpf-default-domain-name (if (boundp '*eoe-site-name*) *eoe-site-name*)
X  "*Default domain name for bbdb-passwd-input.")
X
X
X(defvar bpf-default-org-name (if (boundp 'gnus-local-organization) gnus-local-organization
X			       bpf-default-domain-name)
X  "*Default organization name for bbdb-passwd-input.")
X
X
X(defvar bpf-omit-uid-limit 100
X  "Skip UIDs below this value.  Default is 100.")
X
X(defvar bpf-omit-user-name-regexp "\\(sl-\\\|guest\\)"
X  "Skip usernames that match this regular expression.
XE.g., \"\\\\(sl-\\\\\\|guest\\\\)\"
X")
X
X(defvar bpf-omit-user-name-list '("nobody" "noaccess")
X  "Skip usernames in this list.
XE.g., '(\"noaccess\" \"nobody\")
X")
X
X(defvar bpf-omit-pretty-name-regexp "\\(Slip \\\|Listserv\\\|PPP\\)"
X  "Skip pretty names that match this regular expression.
XE.g., \"\\\\(Slip \\\\\\|Listserv\\\\\\|PPP\\\\)\"
X")
X
X(defvar bpf-omit-pretty-name-list '()
X  "Skip pretty names that match this regular expression.
XE.g., '(\"John Q. Public\")
X")
X
X
X(defun bbdb-passwd-input (domain-name org-name to-file)
X  "Parse current buffer which contains a UNIX passwd file to generate a .bif format file"
X  (interactive (list (setq bpf-default-domain-name (read-string "Domain name: "
X								bpf-default-domain-name))
X		     (setq bpf-default-org-name (read-string "Organization name: "
X							     bpf-default-org-name))
X		     (setq bpf-default-bif-file
X			   (read-file-name "Output To File: "
X					   (concat
X					    (file-name-directory bpf-default-bif-file)
X					    (concat "bif-" bpf-default-domain-name ".el"))
X					   (concat
X					    (file-name-directory bpf-default-bif-file)
X					    (concat "bif-" bpf-default-domain-name ".el"))))))
X  (let (to-buffer)
X    (save-excursion
X      (message (expand-file-name to-file))
X      (set-buffer (find-file (expand-file-name to-file)))
X      (delete-region (point-min) (point-max))
X      (bif-buffer-insert-header)
X      (setq to-buffer (current-buffer)))
X
X    ;; walk the passwd file in the current buffer
X    (goto-char (point-min))
X    (while (not (eobp))
X      (beginning-of-line)
X      (bpf-parse-line domain-name org-name to-buffer)
X      (forward-line 1))
X
X    (message "Done.")
X    (set-buffer to-buffer)
X    ))
X
X
X(defun bif-buffer-insert-header ()
X  (insert "(require 'bbdb-passwd)\n\n"))
X
X
X(defun bif-buffer-insert-record (pretty-name org-name email)
X  (insert (format "(bif-create-record"))
X
X  (insert (format " \"%s\"" pretty-name)) ; NAME string
X
X  (insert (format " \"%s\"" org-name))	; COMPANY is a string or nil
X
X  (insert (format " \"%s\"" email))	; NET is a comma-separated list of email address,
X					;  or a list of strings
X
X  ;; (insert " nil")			 ; ADDRS is a list of address objects.
X					; An address is a vector of the form
X					; ["location" "line1" "line2" "line3" "City" "State" zip]
X
X  ;; (insert " nil")                         ; PHONES is a list of phone-number objects.
X					;  A phone-number is a vector of the form
X					;  ["location" areacode prefix suffix extension-or-nil]
X					;  or
X					;  ["location" "phone-number"]
X
X  ;; (insert " nil")                         ; NOTES is a string, or an alist associating symbols with
X					;  strings.
X
X  (insert ")\n")
X  )
X
X(defun bpf-parse-line (domain-name org-name to-buffer)
X  "Parse the passwd file line.  Point is assumed to be at the beginning of line."
X  (let (record-string uid user-name pretty-name email)
X    (setq record-string (buffer-substring (point)
X					  (progn (end-of-line) (point))))
X
X    (message "Processing record: %s" record-string)
X
X    ;; (setq record-string "mohsen:x:100:10:Mohsen Banan:/home/arash/mohsen:/bin/csh")
X
X    ;; check for a valid and qualifying uid on line, else skip
X    (cond ((and
X	    ;;
X	    ;; extract and test uid
X	    ;;
X	    (string-match "^\\w*:\\w*:\\([0-9]+\\):" record-string)
X	    (setq uid (read (substring record-string
X				       (match-beginning 1)
X				       (match-end 1))))
X	    (>= uid bpf-omit-uid-limit)
X	    ;;
X	    ;; extract and test user name
X	    ;;
X	    (string-match "^\\([^:]+\\):" record-string)
X	    (setq user-name (substring record-string (match-beginning 1) (match-end 1)))
X	    (or (null bpf-omit-user-name-regexp)
X		(not (string-match bpf-omit-user-name-regexp user-name)))
X	    (or (null bpf-omit-user-name-list)
X		(not (member user-name bpf-omit-user-name-list)))
X	    ;;
X	    ;; extract and test pretty name
X	    ;;
X	    (string-match "^[^:]*:[^:]*:[^:]*:[^:]*:\\([^:]+\\):" record-string)
X	    (setq pretty-name (substring record-string (match-beginning 1) (match-end 1)))
X	    (or (null bpf-omit-pretty-name-regexp)
X		(not (string-match bpf-omit-pretty-name-regexp pretty-name)))
X	    (or (null bpf-omit-pretty-name-list)
X		(not (member pretty-name bpf-omit-pretty-name-list)))
X	    )
X
X	   ;; synthesize email address
X	   (setq email (concat user-name "@" domain-name))
X
X	   ;; output bif record
X	   (save-excursion
X	     (set-buffer to-buffer)
X	     (bif-buffer-insert-record pretty-name org-name email)
X	     )
X	   )
X	  (t
X	   ;; not a valid line, skip
X	   nil))
X    ))
X
X(defun bif-create-record (name company net &optional addrs phones notes)
X  "Try to add a record to BBDB; if one does not already exist."
X  (condition-case err
X      (progn
X	(bbdb-create-internal name company net addrs phones notes)
X	(message "%s <%s> added." name net))
X    (error (message "%s" (car (cdr err)))
X	   (sleep-for 1))))
X
X
X(provide 'bbdb-passwd)
X
END_OF_FILE
if test 6613 -ne `wc -c <'bbdb-passwd.el'`; then
    echo shar: \"'bbdb-passwd.el'\" unpacked with wrong size!
fi
# end of 'bbdb-passwd.el'
fi
if test -f 'makefile' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'makefile'\"
else
echo shar: Extracting \"'makefile'\" \(1977 characters\)
sed "s/^X//" >'makefile' <<'END_OF_FILE'
X#  This file is part of the BBDB Filters Package. BBDB Filters Package is a
X#  collection of input and output filters for BBDB.
X# 
X#  Copyright (C) 1995 Neda Communications, Inc.
X# 	Prepared by Mohsen Banan (mohsen@neda.com)
X# 
X#  This library is free software; you can redistribute it and/or modify
X#  it under the terms of the GNU Library General Public License as
X#  published by the Free Software Foundation; either version 2 of the
X#  License, or (at your option) any later version.  This library is
X#  distributed in the hope that it will be useful, but WITHOUT ANY
X#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
X#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
X#  License for more details.  You should have received a copy of the GNU
X#  Library General Public License along with this library; if not, write
X#  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
X#  USA.
X# 
X#
X
X# Makefile for the Insidious Big Brother Database -- Input and Output Filters
X#
X# RCS makefile,v 1.2 1995/08/08 01:20:32 mohsen Exp
X#
X
XEOEBASE	= /usr/public/eoe/lisp/public/bbdbPlus
XEOEINFO	= /usr/public/eoe/info
XEMACS	= xemacs
X
X
X# You shouldn't need to change anything after this point.
X
XSRCS = bbdb-ccmail.el bbdb-eudora.el bbdb-export.el bbdb-hp200lx.el bbdb-ph.el bbdb-passwd.el
X
XSHELL=/bin/sh
X
X.SUFFIXES:
X.SUFFIXES: .elc .el 
X
X.el.elc: 
X	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)
X
Xdefault:
X	@echo Targets: install clean shar	
X
Xinstall: 
X	cp $(SRCS) $(EOEBASE)
X	cd doc; make EOEBASE=$(EOEBASE) EOEINFO=$(EOEINFO) install
X
Xclean:
X	-/bin/rm *.elc package.shar
X	cd doc; make EOEBASE=$(EOEBASE) EOEINFO=$(EOEINFO) clean
X
X
Xshar: $(SRCS) makefile
X	shar -o package.shar $(SRCS) makefile COPYING.LIB README \
X	doc doc/main.texinfo doc/lgpl.tex doc/makefile \
X	doc/formatted  doc/formatted/bbdb-filters.info 
X
X
XFORFTPING	= /h8/var/ftp/pub/eoe/bbdbPlus/bbdb-filters-0.2.tar 
X
Xtar: $(SRCS) makefile
X	tar cvf $(FORFTPING) .
X
X
X
END_OF_FILE
if test 1977 -ne `wc -c <'makefile'`; then
    echo shar: \"'makefile'\" unpacked with wrong size!
fi
# end of 'makefile'
fi
if test -f 'COPYING.LIB' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'COPYING.LIB'\"
else
echo shar: Extracting \"'COPYING.LIB'\" \(25265 characters\)
sed "s/^X//" >'COPYING.LIB' <<'END_OF_FILE'
X		  GNU LIBRARY GENERAL PUBLIC LICENSE
X		       Version 2, June 1991
X
X Copyright (C) 1991 Free Software Foundation, Inc.
X                    675 Mass Ave, Cambridge, MA 02139, USA
X Everyone is permitted to copy and distribute verbatim copies
X of this license document, but changing it is not allowed.
X
X[This is the first released version of the library GPL.  It is
X numbered 2 because it goes with version 2 of the ordinary GPL.]
X
X			    Preamble
X
X  The licenses for most software are designed to take away your
Xfreedom to share and change it.  By contrast, the GNU General Public
XLicenses are intended to guarantee your freedom to share and change
Xfree software--to make sure the software is free for all its users.
X
X  This license, the Library General Public License, applies to some
Xspecially designated Free Software Foundation software, and to any
Xother libraries whose authors decide to use it.  You can use it for
Xyour libraries, too.
X
X  When we speak of free software, we are referring to freedom, not
Xprice.  Our General Public Licenses are designed to make sure that you
Xhave the freedom to distribute copies of free software (and charge for
Xthis service if you wish), that you receive source code or can get it
Xif you want it, that you can change the software or use pieces of it
Xin new free programs; and that you know you can do these things.
X
X  To protect your rights, we need to make restrictions that forbid
Xanyone to deny you these rights or to ask you to surrender the rights.
XThese restrictions translate to certain responsibilities for you if
Xyou distribute copies of the library, or if you modify it.
X
X  For example, if you distribute copies of the library, whether gratis
Xor for a fee, you must give the recipients all the rights that we gave
Xyou.  You must make sure that they, too, receive or can get the source
Xcode.  If you link a program with the library, you must provide
Xcomplete object files to the recipients so that they can relink them
Xwith the library, after making changes to the library and recompiling
Xit.  And you must show them these terms so they know their rights.
X
X  Our method of protecting your rights has two steps: (1) copyright
Xthe library, and (2) offer you this license which gives you legal
Xpermission to copy, distribute and/or modify the library.
X
X  Also, for each distributor's protection, we want to make certain
Xthat everyone understands that there is no warranty for this free
Xlibrary.  If the library is modified by someone else and passed on, we
Xwant its recipients to know that what they have is not the original
Xversion, so that any problems introduced by others will not reflect on
Xthe original authors' reputations.
X
X  Finally, any free program is threatened constantly by software
Xpatents.  We wish to avoid the danger that companies distributing free
Xsoftware will individually obtain patent licenses, thus in effect
Xtransforming the program into proprietary software.  To prevent this,
Xwe have made it clear that any patent must be licensed for everyone's
Xfree use or not licensed at all.
X
X  Most GNU software, including some libraries, is covered by the ordinary
XGNU General Public License, which was designed for utility programs.  This
Xlicense, the GNU Library General Public License, applies to certain
Xdesignated libraries.  This license is quite different from the ordinary
Xone; be sure to read it in full, and don't assume that anything in it is
Xthe same as in the ordinary license.
X
X  The reason we have a separate public license for some libraries is that
Xthey blur the distinction we usually make between modifying or adding to a
Xprogram and simply using it.  Linking a program with a library, without
Xchanging the library, is in some sense simply using the library, and is
Xanalogous to running a utility program or application program.  However, in
Xa textual and legal sense, the linked executable is a combined work, a
Xderivative of the original library, and the ordinary General Public License
Xtreats it as such.
X
X  Because of this blurred distinction, using the ordinary General
XPublic License for libraries did not effectively promote software
Xsharing, because most developers did not use the libraries.  We
Xconcluded that weaker conditions might promote sharing better.
X
X  However, unrestricted linking of non-free programs would deprive the
Xusers of those programs of all benefit from the free status of the
Xlibraries themselves.  This Library General Public License is intended to
Xpermit developers of non-free programs to use free libraries, while
Xpreserving your freedom as a user of such programs to change the free
Xlibraries that are incorporated in them.  (We have not seen how to achieve
Xthis as regards changes in header files, but we have achieved it as regards
Xchanges in the actual functions of the Library.)  The hope is that this
Xwill lead to faster development of free libraries.
X
X  The precise terms and conditions for copying, distribution and
Xmodification follow.  Pay close attention to the difference between a
X"work based on the library" and a "work that uses the library".  The
Xformer contains code derived from the library, while the latter only
Xworks together with the library.
X
X  Note that it is possible for a library to be covered by the ordinary
XGeneral Public License rather than by this special one.
X
X		  GNU LIBRARY GENERAL PUBLIC LICENSE
X   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
X
X  0. This License Agreement applies to any software library which
Xcontains a notice placed by the copyright holder or other authorized
Xparty saying it may be distributed under the terms of this Library
XGeneral Public License (also called "this License").  Each licensee is
Xaddressed as "you".
X
X  A "library" means a collection of software functions and/or data
Xprepared so as to be conveniently linked with application programs
X(which use some of those functions and data) to form executables.
X
X  The "Library", below, refers to any such software library or work
Xwhich has been distributed under these terms.  A "work based on the
XLibrary" means either the Library or any derivative work under
Xcopyright law: that is to say, a work containing the Library or a
Xportion of it, either verbatim or with modifications and/or translated
Xstraightforwardly into another language.  (Hereinafter, translation is
Xincluded without limitation in the term "modification".)
X
X  "Source code" for a work means the preferred form of the work for
Xmaking modifications to it.  For a library, complete source code means
Xall the source code for all modules it contains, plus any associated
Xinterface definition files, plus the scripts used to control compilation
Xand installation of the library.
X
X  Activities other than copying, distribution and modification are not
Xcovered by this License; they are outside its scope.  The act of
Xrunning a program using the Library is not restricted, and output from
Xsuch a program is covered only if its contents constitute a work based
Xon the Library (independent of the use of the Library in a tool for
Xwriting it).  Whether that is true depends on what the Library does
Xand what the program that uses the Library does.
X  
X  1. You may copy and distribute verbatim copies of the Library's
Xcomplete source code as you receive it, in any medium, provided that
Xyou conspicuously and appropriately publish on each copy an
Xappropriate copyright notice and disclaimer of warranty; keep intact
Xall the notices that refer to this License and to the absence of any
Xwarranty; and distribute a copy of this License along with the
XLibrary.
X
X  You may charge a fee for the physical act of transferring a copy,
Xand you may at your option offer warranty protection in exchange for a
Xfee.
X
X  2. You may modify your copy or copies of the Library or any portion
Xof it, thus forming a work based on the Library, and copy and
Xdistribute such modifications or work under the terms of Section 1
Xabove, provided that you also meet all of these conditions:
X
X    a) The modified work must itself be a software library.
X
X    b) You must cause the files modified to carry prominent notices
X    stating that you changed the files and the date of any change.
X
X    c) You must cause the whole of the work to be licensed at no
X    charge to all third parties under the terms of this License.
X
X    d) If a facility in the modified Library refers to a function or a
X    table of data to be supplied by an application program that uses
X    the facility, other than as an argument passed when the facility
X    is invoked, then you must make a good faith effort to ensure that,
X    in the event an application does not supply such function or
X    table, the facility still operates, and performs whatever part of
X    its purpose remains meaningful.
X
X    (For example, a function in a library to compute square roots has
X    a purpose that is entirely well-defined independent of the
X    application.  Therefore, Subsection 2d requires that any
X    application-supplied function or table used by this function must
X    be optional: if the application does not supply it, the square
X    root function must still compute square roots.)
X
XThese requirements apply to the modified work as a whole.  If
Xidentifiable sections of that work are not derived from the Library,
Xand can be reasonably considered independent and separate works in
Xthemselves, then this License, and its terms, do not apply to those
Xsections when you distribute them as separate works.  But when you
Xdistribute the same sections as part of a whole which is a work based
Xon the Library, the distribution of the whole must be on the terms of
Xthis License, whose permissions for other licensees extend to the
Xentire whole, and thus to each and every part regardless of who wrote
Xit.
X
XThus, it is not the intent of this section to claim rights or contest
Xyour rights to work written entirely by you; rather, the intent is to
Xexercise the right to control the distribution of derivative or
Xcollective works based on the Library.
X
XIn addition, mere aggregation of another work not based on the Library
Xwith the Library (or with a work based on the Library) on a volume of
Xa storage or distribution medium does not bring the other work under
Xthe scope of this License.
X
X  3. You may opt to apply the terms of the ordinary GNU General Public
XLicense instead of this License to a given copy of the Library.  To do
Xthis, you must alter all the notices that refer to this License, so
Xthat they refer to the ordinary GNU General Public License, version 2,
Xinstead of to this License.  (If a newer version than version 2 of the
Xordinary GNU General Public License has appeared, then you can specify
Xthat version instead if you wish.)  Do not make any other change in
Xthese notices.
X
X  Once this change is made in a given copy, it is irreversible for
Xthat copy, so the ordinary GNU General Public License applies to all
Xsubsequent copies and derivative works made from that copy.
X
X  This option is useful when you wish to copy part of the code of
Xthe Library into a program that is not a library.
X
X  4. You may copy and distribute the Library (or a portion or
Xderivative of it, under Section 2) in object code or executable form
Xunder the terms of Sections 1 and 2 above provided that you accompany
Xit with the complete corresponding machine-readable source code, which
Xmust be distributed under the terms of Sections 1 and 2 above on a
Xmedium customarily used for software interchange.
X
X  If distribution of object code is made by offering access to copy
Xfrom a designated place, then offering equivalent access to copy the
Xsource code from the same place satisfies the requirement to
Xdistribute the source code, even though third parties are not
Xcompelled to copy the source along with the object code.
X
X  5. A program that contains no derivative of any portion of the
XLibrary, but is designed to work with the Library by being compiled or
Xlinked with it, is called a "work that uses the Library".  Such a
Xwork, in isolation, is not a derivative work of the Library, and
Xtherefore falls outside the scope of this License.
X
X  However, linking a "work that uses the Library" with the Library
Xcreates an executable that is a derivative of the Library (because it
Xcontains portions of the Library), rather than a "work that uses the
Xlibrary".  The executable is therefore covered by this License.
XSection 6 states terms for distribution of such executables.
X
X  When a "work that uses the Library" uses material from a header file
Xthat is part of the Library, the object code for the work may be a
Xderivative work of the Library even though the source code is not.
XWhether this is true is especially significant if the work can be
Xlinked without the Library, or if the work is itself a library.  The
Xthreshold for this to be true is not precisely defined by law.
X
X  If such an object file uses only numerical parameters, data
Xstructure layouts and accessors, and small macros and small inline
Xfunctions (ten lines or less in length), then the use of the object
Xfile is unrestricted, regardless of whether it is legally a derivative
Xwork.  (Executables containing this object code plus portions of the
XLibrary will still fall under Section 6.)
X
X  Otherwise, if the work is a derivative of the Library, you may
Xdistribute the object code for the work under the terms of Section 6.
XAny executables containing that work also fall under Section 6,
Xwhether or not they are linked directly with the Library itself.
X
X  6. As an exception to the Sections above, you may also compile or
Xlink a "work that uses the Library" with the Library to produce a
Xwork containing portions of the Library, and distribute that work
Xunder terms of your choice, provided that the terms permit
Xmodification of the work for the customer's own use and reverse
Xengineering for debugging such modifications.
X
X  You must give prominent notice with each copy of the work that the
XLibrary is used in it and that the Library and its use are covered by
Xthis License.  You must supply a copy of this License.  If the work
Xduring execution displays copyright notices, you must include the
Xcopyright notice for the Library among them, as well as a reference
Xdirecting the user to the copy of this License.  Also, you must do one
Xof these things:
X
X    a) Accompany the work with the complete corresponding
X    machine-readable source code for the Library including whatever
X    changes were used in the work (which must be distributed under
X    Sections 1 and 2 above); and, if the work is an executable linked
X    with the Library, with the complete machine-readable "work that
X    uses the Library", as object code and/or source code, so that the
X    user can modify the Library and then relink to produce a modified
X    executable containing the modified Library.  (It is understood
X    that the user who changes the contents of definitions files in the
X    Library will not necessarily be able to recompile the application
X    to use the modified definitions.)
X
X    b) Accompany the work with a written offer, valid for at
X    least three years, to give the same user the materials
X    specified in Subsection 6a, above, for a charge no more
X    than the cost of performing this distribution.
X
X    c) If distribution of the work is made by offering access to copy
X    from a designated place, offer equivalent access to copy the above
X    specified materials from the same place.
X
X    d) Verify that the user has already received a copy of these
X    materials or that you have already sent this user a copy.
X
X  For an executable, the required form of the "work that uses the
XLibrary" must include any data and utility programs needed for
Xreproducing the executable from it.  However, as a special exception,
Xthe source code distributed need not include anything that is normally
Xdistributed (in either source or binary form) with the major
Xcomponents (compiler, kernel, and so on) of the operating system on
Xwhich the executable runs, unless that component itself accompanies
Xthe executable.
X
X  It may happen that this requirement contradicts the license
Xrestrictions of other proprietary libraries that do not normally
Xaccompany the operating system.  Such a contradiction means you cannot
Xuse both them and the Library together in an executable that you
Xdistribute.
X
X  7. You may place library facilities that are a work based on the
XLibrary side-by-side in a single library together with other library
Xfacilities not covered by this License, and distribute such a combined
Xlibrary, provided that the separate distribution of the work based on
Xthe Library and of the other library facilities is otherwise
Xpermitted, and provided that you do these two things:
X
X    a) Accompany the combined library with a copy of the same work
X    based on the Library, uncombined with any other library
X    facilities.  This must be distributed under the terms of the
X    Sections above.
X
X    b) Give prominent notice with the combined library of the fact
X    that part of it is a work based on the Library, and explaining
X    where to find the accompanying uncombined form of the same work.
X
X  8. You may not copy, modify, sublicense, link with, or distribute
Xthe Library except as expressly provided under this License.  Any
Xattempt otherwise to copy, modify, sublicense, link with, or
Xdistribute the Library is void, and will automatically terminate your
Xrights under this License.  However, parties who have received copies,
Xor rights, from you under this License will not have their licenses
Xterminated so long as such parties remain in full compliance.
X
X  9. You are not required to accept this License, since you have not
Xsigned it.  However, nothing else grants you permission to modify or
Xdistribute the Library or its derivative works.  These actions are
Xprohibited by law if you do not accept this License.  Therefore, by
Xmodifying or distributing the Library (or any work based on the
XLibrary), you indicate your acceptance of this License to do so, and
Xall its terms and conditions for copying, distributing or modifying
Xthe Library or works based on it.
X
X  10. Each time you redistribute the Library (or any work based on the
XLibrary), the recipient automatically receives a license from the
Xoriginal licensor to copy, distribute, link with or modify the Library
Xsubject to these terms and conditions.  You may not impose any further
Xrestrictions on the recipients' exercise of the rights granted herein.
XYou are not responsible for enforcing compliance by third parties to
Xthis License.
X
X  11. If, as a consequence of a court judgment or allegation of patent
Xinfringement or for any other reason (not limited to patent issues),
Xconditions are imposed on you (whether by court order, agreement or
Xotherwise) that contradict the conditions of this License, they do not
Xexcuse you from the conditions of this License.  If you cannot
Xdistribute so as to satisfy simultaneously your obligations under this
XLicense and any other pertinent obligations, then as a consequence you
Xmay not distribute the Library at all.  For example, if a patent
Xlicense would not permit royalty-free redistribution of the Library by
Xall those who receive copies directly or indirectly through you, then
Xthe only way you could satisfy both it and this License would be to
Xrefrain entirely from distribution of the Library.
X
XIf any portion of this section is held invalid or unenforceable under any
Xparticular circumstance, the balance of the section is intended to apply,
Xand the section as a whole is intended to apply in other circumstances.
X
XIt is not the purpose of this section to induce you to infringe any
Xpatents or other property right claims or to contest validity of any
Xsuch claims; this section has the sole purpose of protecting the
Xintegrity of the free software distribution system which is
Ximplemented by public license practices.  Many people have made
Xgenerous contributions to the wide range of software distributed
Xthrough that system in reliance on consistent application of that
Xsystem; it is up to the author/donor to decide if he or she is willing
Xto distribute software through any other system and a licensee cannot
Ximpose that choice.
X
XThis section is intended to make thoroughly clear what is believed to
Xbe a consequence of the rest of this License.
X
X  12. If the distribution and/or use of the Library is restricted in
Xcertain countries either by patents or by copyrighted interfaces, the
Xoriginal copyright holder who places the Library under this License may add
Xan explicit geographical distribution limitation excluding those countries,
Xso that distribution is permitted only in or among countries not thus
Xexcluded.  In such case, this License incorporates the limitation as if
Xwritten in the body of this License.
X
X  13. The Free Software Foundation may publish revised and/or new
Xversions of the Library General Public License from time to time.
XSuch new versions will be similar in spirit to the present version,
Xbut may differ in detail to address new problems or concerns.
X
XEach version is given a distinguishing version number.  If the Library
Xspecifies a version number of this License which applies to it and
X"any later version", you have the option of following the terms and
Xconditions either of that version or of any later version published by
Xthe Free Software Foundation.  If the Library does not specify a
Xlicense version number, you may choose any version ever published by
Xthe Free Software Foundation.
X
X  14. If you wish to incorporate parts of the Library into other free
Xprograms whose distribution conditions are incompatible with these,
Xwrite to the author to ask for permission.  For software which is
Xcopyrighted by the Free Software Foundation, write to the Free
XSoftware Foundation; we sometimes make exceptions for this.  Our
Xdecision will be guided by the two goals of preserving the free status
Xof all derivatives of our free software and of promoting the sharing
Xand reuse of software generally.
X
X			    NO WARRANTY
X
X  15. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
XWARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
XEXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
XOTHER PARTIES PROVIDE THE LIBRARY "AS IS" WITHOUT WARRANTY OF ANY
XKIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
XIMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
XPURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
XLIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
XTHE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
X
X  16. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
XWRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
XAND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
XFOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
XCONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
XLIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
XRENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
XFAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
XSUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
XDAMAGES.
X
X		     END OF TERMS AND CONDITIONS
X
X     Appendix: How to Apply These Terms to Your New Libraries
X
X  If you develop a new library, and you want it to be of the greatest
Xpossible use to the public, we recommend making it free software that
Xeveryone can redistribute and change.  You can do so by permitting
Xredistribution under these terms (or, alternatively, under the terms of the
Xordinary General Public License).
X
X  To apply these terms, attach the following notices to the library.  It is
Xsafest to attach them to the start of each source file to most effectively
Xconvey the exclusion of warranty; and each file should have at least the
X"copyright" line and a pointer to where the full notice is found.
X
X    <one line to give the library's name and a brief idea of what it does.>
X    Copyright (C) <year>  <name of author>
X
X    This library is free software; you can redistribute it and/or
X    modify it under the terms of the GNU Library General Public
X    License as published by the Free Software Foundation; either
X    version 2 of the License, or (at your option) any later version.
X
X    This library is distributed in the hope that it will be useful,
X    but WITHOUT ANY WARRANTY; without even the implied warranty of
X    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
X    Library General Public License for more details.
X
X    You should have received a copy of the GNU Library General Public
X    License along with this library; if not, write to the Free
X    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
X
XAlso add information on how to contact you by electronic and paper mail.
X
XYou should also get your employer (if you work as a programmer) or your
Xschool, if any, to sign a "copyright disclaimer" for the library, if
Xnecessary.  Here is a sample; alter the names:
X
X  Yoyodyne, Inc., hereby disclaims all copyright interest in the
X  library `Frob' (a library for tweaking knobs) written by James Random Hacker.
X
X  <signature of Ty Coon>, 1 April 1990
X  Ty Coon, President of Vice
X
XThat's all there is to it!
END_OF_FILE
if test 25265 -ne `wc -c <'COPYING.LIB'`; then
    echo shar: \"'COPYING.LIB'\" unpacked with wrong size!
fi
# end of 'COPYING.LIB'
fi
if test -f 'README' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'README'\"
else
echo shar: Extracting \"'README'\" \(2011 characters\)
sed "s/^X//" >'README' <<'END_OF_FILE'
X
XBBDB is a rolodex-like database program for GNU Emacs.
XBBDB stands for Insidious Big Brother Database. BBDB is written by:
XJamie Zawinski <jwz@mcom.com>. My current version is 1.50.
X
XWe have prepared a family of filters for BBDB. Currently the output
Xfilters include:
X
X     - bbdb --> emacs lisp exporting (for exchanging business cards)
X     - bbdb --> HP100/200 LX Phone Book
X     - bbdb --> PC Eudora Nicknames
X     - bbdb --> CC Mail Nicknames 
X     - bbdb --> PH/QI
X
XThere is presently only one input filter:
X
X     - bbdb <-- UNIX passwd files
X
XWe hope that over time a variety of other input and output filters
Xwill be added to this collection.
X
X
Xbbdb-export in particular, can be very useful over the net.
XIt provides a convenient way for exchanging business cards.
X
X
XThis is a preliminary release. This stuff has not been tested much
Xoutside of our office. We do use most of these filters on an going basis
Xand they work fine for us.
X
XTo install, just edit the makefile and run "make install".
X
XTo run them, read the comments on top of each filter file.
X
XThere is very skimpy documentation in latexinfo format. It is just
Xmeant to be a starting point.
X
XIn addition to the attached shar file,
Xyou can also ftp this package from:
X   //anonymous@ftp.neda.com:/pub/eoe/bbdbPlus/bbdb-filters-0.2.tar 
X   URL =  ftp://ftp.neda.com/pub/eoe/bbdbPlus/bbdb-filters-0.2.tar 
X
XMany of the filters require bbdb-tex-print package by:
XBoris Goldowsky <boris@prodigal.psych.rochester.edu>.
X
XThe one that we use can be found in:
X  //anonymous@ftp.neda.com:/pub/eoe/bbdbPlus/bbdb-tex-3.0.tar 
X  URL =  ftp://ftp.neda.com/pub/eoe/bbdbPlus/bbdb-tex-3.0.tar 
X
XYou can checkout the overview of this package by 
Xbrowsing the manual (latex/info/html) at:
X  URL = http://www.neda.com/eoe/bbdbFilters/bbdbFilters.html
X
X
XSend  bug-reports, comments and suggestions to:
X		  Mohsen Banan-neda <mohsen@neda.com>
Xand refer to:
X	bbdb-filters RCS: README,v 1.2 1995/08/08 02:59:15 mohsen Exp
X
X
XHope you find this helpful.
X
X...Mohsen.
X
END_OF_FILE
if test 2011 -ne `wc -c <'README'`; then
    echo shar: \"'README'\" unpacked with wrong size!
fi
# end of 'README'
fi
if test ! -d 'doc' ; then
    echo shar: Creating directory \"'doc'\"
    mkdir 'doc'
fi
if test -f 'doc/main.texinfo' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'doc/main.texinfo'\"
else
echo shar: Extracting \"'doc/main.texinfo'\" \(15557 characters\)
sed "s/^X//" >'doc/main.texinfo' <<'END_OF_FILE'
X% This is really LaTeXInfo, but some time LaTeX mode is more useful -*- LaTeX  -*-
X% This is really LaTeXInfo, but some time LaTeX mode is more useful -*- Latexinfo  -*-
X%
X%  Revision:  main.texinfo,v 1.1.1.1 1995/08/07 08:43:10 mohsen Exp
X%
X%\documentstyle[12pt,latexinfo,format,smallverb,tabular]{book}
X%\documentstyle[12pt,latexinfo,format]{book}
X\documentstyle[12pt,format,hyperlatex,latexinfo]{book}
X%\documentstyle[12pt,times,latexinfo,format]{book}
X%\documentstyle[12pt,avantgarde,latexinfo,format]{book}
X%\documentstyle[12pt,palatino,latexinfo,format]{book}
X%\documentstyle[10pt,avantgarde,latexinfo,format]{book}
X
X\pagestyle{empty}
X
X\c \input{transfig}  \c Used with eepic -- not needed when using psfig.
X\input{epsf}
X
X\begin{document}
X
X\c \bibliographystyle{alpha}  \c [banan92]
X\c \bibliographystyle{plain}   \c Numbers [1]
X
X\c \textwidth 5.2in \c for .tty generation
X
X\htmldirectory{bbdbFilters}
X\htmlname{bbdbFilters}
X\htmltitle{BBDB Filters}
X\htmlmathitalics
X\htmladdress{\htmlrule{}info@neda.com}
X
X\c Declare which indices you want to make use of.
X\newindex{cp}
X\newindex{fn}
X
X\title{BBDB Input and Output Filters\\
X        \vspace{0.25in} {\large DRAFT}\\
X        {\normalsize Version 0.2}}  
X
X\author{{\normalsize Prepared by}\\
X        Mohsen Banan \\
X        \code{mohsen@neda.com}\\
X        Neda Communications, Inc.\\
X        17005 SE 31st Place\\
X        Bellevue, WA 98008}
X
X\c (current-time-string)
X\date{July 26, 1995}
X\c \date{\today}
X
X\maketitle
X
X\c The following commands start the copyright page for the printed manual.
X\clearpage
X\vspace*{0pt plus 1filll}
X
X
X\bigskip
X\bigskip
X\bigskip
X
X
XThis document describes the ``BBDB Input and Output Filters'' package,
Xa utility which translates BBDB information to and from various other
Xformats.
X
X\begin{display}
X
XCopyright \copyright 1995 Neda Communications, Inc.
X
XPublished by:
XNeda Communications, Inc.
X17005 SE 31st Place, 
XBellevue, WA 98008 USA
X
X\end{display}
X
X
XPermission is granted to make and distribute verbatim copies of this
Xmanual provided the copyright notice and this permission notice are
Xpreserved on all copies.
X
XPermission is granted to copy and distribute modified versions of this
Xmanual under the conditions for verbatim copying, provided that the
Xentire resulting derived work is distributed under the terms of a
Xpermission notice identical to this one.
X
XPermission is granted to copy and distribute translations of this
Xmanual into another language, under the above conditions for modified
Xversions, except that this permission notice may be stated in a
Xtranslation approved by the Foundation.
X
X\bigskip
X\bigskip
X
X\clearpage
X\pagestyle{headings}
X
X\c Use roman numerals for the page numbers and Insert the Table of Contents.
X\pagenumbering{roman}
X\tableofcontents
X
X\c \listoftables
X\c \listoffigures
X
X\c End the Table of Contents and start numbering from 1 with Arabic numbers
X
X\clearpage
X\pagenumbering{arabic}
X
X\c Anything before the setfilename will not appear in the Info file.
X\setfilename{INFOFILE}
X
X\topnode{BBDB Filters}
X
X\htmlmenu{6}
X
X\begin{ifinfo}
XCopyright \copyright \var{1995} \var{Neda Communications, Inc.}
X\end{ifinfo}
X
X\c The Top node contains the master menu for the Info file.
X\c This appears only in the Info file, not the printed manual.
X
X\chapter{Introduction}
X
XOver time much valuable data has been gathered in BBDB database files.
XMany wish to share parts or all of this information with others.  They
Xalso wish to have access to this same information from other systems
X(like personal digital assistants) lacking straightforward BBDB
Xaccess.
X
XFor these reasons, we have prepared a family of filters that convert
Xthe information in BBDB to and from a variety of other
Xformats. ``Output filters'' export BBDB information to other formats
Xwhile ``input filters'' import information from other formats into
XBBDB.
X
XOur hope is that over time this collection of BBDB filters will grow
Xthrough contributed code.
X
X\section{About This Package}
X
XThis package is a collection of filters and is called ``BBDB Input and
XOutput Filters''.  It has been somewhat tested with BBDB version 1.50.
XThe present state of the software is still preliminary although it has
Xproved useful.
X
X\section{About This Manual}
X
XThis documentation applies to Version 0.2 of the ``BBDB Input and
XOutput Filters'' package.  The documentation is presently skeletal and
Xvery preliminary.  It mostly provides the user with instructions for
Xuse, and very little background is included.  Familiarity with Emacs
XLisp is assumed for some sections.
X
X\chapter{Output Filters}
X
X``Output filters'' are used to export BBDB information into formats 
Xused by other systems.
X
XIn general, an output filter uses the contents of your
X\code{*BBDB*} buffer as input.  Note that output filters do not use
XBBDB files (typically `\code{~/.bbdb}') directly.
X
XAn output filter is invoked by executing its associated lisp function.
XThe name of the function is conventionally named \code{bbdb-<system>-output} 
X(e.g., \code{M-x bbdb-hp200lx-output}).
X
XThe result of running an output filter is to create a new buffer that
Xcontains the \code{*BBDB*} information appropriately transformed into a
Xformat suitable for use by the target system.  The new buffer is given
Xa file name that you specify.
X
X\section{HP 200LX Phone Book}
X
X\cindex{HP 200LX Connectivity Pack}
XThis package has only been tested on HP 200LX palmtop systems.  It
Xalso requires the ``HP 200LX Connectivity Pack'' for converting
Xcomma-delimited ASCII files into binary .PDB files which are read by
Xthe HP 200LX Phone Book application.  Version 1.00 of the ``HP 200LX
XConnectivty Pack'' was used for testing.
X
XThe HP 200LX output filter is in file \code{bbdb-hp200lx.el}.
X
X\begin{enumerate}
X
X\findex{bbdb-hp200lx-output}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\item Invoke \code{ bbdb-hp200lx-output} to create an ASCII .CDF 
X(Comma Delimited File). \cindex{.CDF file, HP 200LX Phone Book}
X
X\item Using Xlate/Merge option of HP Connectivity Pack convert the
X.CDF file into a binary .PDB file used by the Phone Book program.
X\cindex{.PDF file, HP 200LX Phone Book}
X
X\item Download the .PDB file to your palmtop's internal disk and
Xensure that the Phone Book program is set use the newly downloaded
X.PDB file.
X
X\end{enumerate}
X
X\section{PC Eudora}
X
XBBDB information can be exported to PC Eudora in two formats--as a
Xnickname database file and as a recipients database file.
X
XThe PC Eudora output filter is in file \code{bbdb-eudora.el}.
X
X\subsection{PC Eudora Nickname Database}
X
X\begin{enumerate}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\findex{bbdb-eudora-nndbase-output}
X\item Invoke \code{bbdb-eudora-nndbase-output} to create a PC Eudora
XNickname database file.
X
X\item Make the file accessible to PC Eudora.
X
X\end{enumerate}
X
X\subsection{PC Eudora Recipient Database}
X
X\begin{enumerate}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\findex{bbdb-eudora-rcpdbase-output}
X\item Invoke \code{bbdb-eudora-rcpdbase-output} to create a PC Eudora 
Xrecipient's database file.
X
X\item Make the file accessible to PC Eudora.
X
X\end{enumerate}
X
X\section{Lotus cc:Mail Nicknames}
X
XThe Lotus cc:Mail output filter is in file \code{bbdb-ccmail.el}.
X
X\begin{enumerate}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\findex{bbdb-ccmail-output}
X\item Invoke \code{ bbdb-ccmail-output} to create a cc:Mail Nicknames file.
X
X\item Make the file accessible to cc:Mail. 
X
X\end{enumerate}
X
X\section{PH}
X
XThe PH output filter is in file \code{bbdb-ph.el}.
X
X\begin{enumerate}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\findex{bbdb-ph-output}
X\item Invoke \code{bbdb-ph-output} to create a \code{ph} data file for 
Xuse with the  \code{maked} program.
X
X\item Make the file accessible to \code{ph}.
X
X\end{enumerate}
X
X\section{Emacs Lisp Export}
X
XThe Emacs Lisp Export output filter is in file \code{bbdb-export.el}.
X
XThis output filter uses the current contents of your
X\code{*BBDB*} buffer to generate a new buffer (\code{*BBDB* Export}) 
Xthat contains a single lisp \code{(progn ...)} expression.  For
Xexample, a \code{*BBDB*} buffer containing two records would result in
Xthe following \code{*BBDB* Export} buffer:
X
X\begin{example}
X;;; ======= Start of Exported BBDB Records =======
X(progn  
X  (require 'bbdb-com)
X  (defun bbdb-maybe-create (name company net &optional addrs phones notes)
X    "Try to add a record to BBDB if it does not already exist."
X    (condition-case err
X        (progn
X          (bbdb-create-internal name company net addrs phones notes)
X          (message "%s %s added." name (if net (concat "<" net ">") ""))
X          (sleep-for 1))    
X      (error (ding)
X             (message "%s %s skipped. (%s)"
X                      name
X                      (if net (concat "<" net ">") "")
X                      (car (cdr err)))
X             (sleep-for 1))))
X
X  (bbdb-maybe-create "Jill Doe--IMPORTED"
X                     "CBS Corporation"
X                     '("jilld@cbs.com")
X                     '(
X                       ["Home"
X                        "368 222ND PL"
X                        ""
X                        ""
X                        "Springfield"
X                        "MA" 2117]
X                       )
X                     '(
X                       ["Office" 617 555 9983 0]
X                       ) '"Movie Mogul")
X  (bbdb-maybe-create "John Doe--IMPORTED"
X                     "ABC Incorporated"
X                     '("jdoe@abc.com")
X                     '(
X                       ["Office"
X                        "123 Any Street"
X                        ""
X                        ""
X                        "Any Town"
X                        "WA" (98027 7758)]
X                       )
X                     '(
X                       ["Office" 206 555 1234 0]
X                       ) '"TV Producer")
X  )
X;;; ======= End of Exported BBDB Records =======
X\end{example}
X
X\cindex{Sending BBDB records via email}
XThis lisp expression can then be sent via email or some other
Xtext-based messaging facility to another user who can then evaluate
Xthe expression which will add the \code{BBDB} records to the
Xrecipient's
X\code{BBDB} database.  
X
XOnly new records are added.  A record with the same name or net
Xaddress as one already existing in the \code{BBDB} is skipped
Xentirely.
X
XIn the sample contents of a \code{*BBDB* Export} buffer presented, two
Xrecords are being exported--one for ``John Doe'' and the other for
X``Jill Doe''.  Notice that their names have been appended with
X\code{--IMPORTED}.  This string can be used to quick locate each record
Xthat is added to the database using this mechanism.
X
XThe following steps are for exporting BBDB records into Emacs Lisp:
X
X\begin{enumerate}
X
X\item Invoke \code{M-x bbdb} to populate the \code{*BBDB*} buffer
Xwith the contents you wish to export.
X
X\findex{bbdb-export}
X\item Invoke \code{bbdb-export} to create a \code{*BBDB* Export} buffer which contains a
Xsingle \code{(progn ...)} can be evaluated to add the records to the
Xexisting \code{BBDB} database (if the records do not already exist).
X
X\item Use the contents of \code{*BBDB* Export} in email and other messaging systems.
X
X\end{enumerate}
X
XThe following steps are for a user wishing to import the contents of a
X\code{*BBDB* Export} buffer's expression into his or her own database:
X
X\begin{enumerate}
X
X\item Evaluate the region bounded by the lines \\
X \code{;;; ======= Start of Exported BBDB Records =======} \\
Xand \\
X \code{;;; ======= End of Exported BBDB Records =======}. \\
XYou can use such commands as
X\code{M-x eval-region} or \code{M-x eval-last-sexp}.
X
X\item Review the newly imported entries.  To see them, invoke \code{M-x
Xbbdb} and specify \code{--IMPORTED} at the \code{Regular Expression}
Xprompt.  
X
X\item After reviewing the contents of the imported records, you may
Xwish to remove the \code{--IMPORTED} that is appended to the name by
X\code{bbdb-export}.
X
X\end{enumerate}
X
X\chapter{Input Filters}
X
X``Input filters'' are used to import into BBDB information from a
Xforeign system's data file.
X
XThe name of the function is conventionally named
X\code{bbdb-<system>-input} (e.g., \code{bbdb-passwd-input} is the name
Xof the Emacs Lisp function for the UNIX password file input filter).
X
XIn general, an ``input filter'' expects the foreign system's data to
Xbe in the current buffer.  The contents of the current buffer are used
Xto create an Emacs Lisp file which when loaded will add new records
Xinto your BBDB database if they don't yet exist--existing BBDB records
Xwill not be modified.
X
X\section{General Facilities for Input Filtering}
X
XThe result of running an input filter is to produce a new buffer a
Xseries of \code{bif-create-record} \findex{bif-create-record}
Xexpressions, each corresponding to a single user's record.  Notice
Xthat input filters do not directly modify the contents of the BBDB
Xfiles (typically `\code{~/.bbdb}').
X
XTo actually modify the contents of the BBDB database, you must
Xevaluated the expressions in the resultant buffer created by the input
Xfilter.  One way to do so is simply to invoke \code{M-x eval-buffer}.
XAnother way is to simply save the buffer to disk and load its contents
Xinto Emacs Lisp using \code{M-x load-file}.
X
X\section{UNIX Password Files}
X
XThe UNIX password file input filter is in file \code{bbdb-passwd.el}.
X
X\begin{enumerate}
X
X\item Use \code{M-x find-file} to visit the UNIX password file you wish to import.
X
X\findex{bbdb-passwd-input}
X\item With the password file in the current buffer, invoke the input 
Xfilter \code{M-x bbdb-passwd-input}.  You will be prompted for the
Xdomain name associated with that host's password file; an organization
Xname; as well as the file name to be associated with the buffer of
X\code{bif-create-record} expressions.
X
X\item Evaluate the contents of the input filter's buffer to add records 
Xinto your BBDB database file.
X
X\end{enumerate}
X
X\chapter{Miscellany}
X
X\section{TODO List}
X
X\begin{itemize}
X
X\item Move generic input filter functionality out of
X\code{bbdb-passwd.el} and into, say, \code{bbdb-ifilt.el}.  
XThe generic functionality code has names typically prefixed with \code{bif-}.
X
X\item Add support for \code{gdbload} (as an alternative to the 
XXlate/Merge application provided in the HP 200LX Connectivity Pack)
Xinto the HP 200LX output filter.  This is based on input from Robert
XNicholson \code{<robert@steffi.dircon.co.uk>}.
X
X\item Add documentation for variables in the various input and output filters.
X
X\item Check and document all dependencies on other packages.
X
X\end{itemize}
X
X\section{Credits}
X
XPean Lim \code{<pean@neda.com>} wrote most of this package.  Mohsen
XBanan \code{<mohsen@neda.com>} put it all together and guided the
Xwork.  Neda Communications, Inc. sponsored the work.  The output
Xfilters code is based on \code{bbdb-print} by Boris Goldowsky\\
X\code{<boris@prodigal.psych.rochester.edu>}.
X
X\c ;;;;;;;;;;;;;;;; Appendix Starts Here ;;;;;;;;;;;;;
X\appendix
X
X\mbinput{lgpl.tex}
X
X\begin{tex}
X%\bibliography{/usr/local/lib/bib/gnu,/usr/local/lib/bib/networking,/usr/local/lib/bib/directory,/usr/local/lib/bib/rfcs}
X\end{tex}
X
X\c \twocolumn
X\node Concept Index, Top, First Chapter, Top
X\unnumbered{Concept Index}
X
X\printindex{cp}
X
X\H \htmlprintindex
X
X\node Command Index, Top, First Chapter, Top
X\unnumbered{Command Index}
X
X\printindex{fn}
X
X\end{document}
X
END_OF_FILE
if test 15557 -ne `wc -c <'doc/main.texinfo'`; then
    echo shar: \"'doc/main.texinfo'\" unpacked with wrong size!
fi
# end of 'doc/main.texinfo'
fi
if test -f 'doc/lgpl.tex' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'doc/lgpl.tex'\"
else
echo shar: Extracting \"'doc/lgpl.tex'\" \(25800 characters\)
sed "s/^X//" >'doc/lgpl.tex' <<'END_OF_FILE'
X\c This LGPL is meant to be included from other files.
X\c To format a standalone LGPL, use liblic.texi.
X
X\chapter{GNU LIBRARY GENERAL PUBLIC LICENSE}
X
X\begin{center}
XVersion 2, June 1991
X\end{center}
X
X\begin{example}
XCopyright \copyright{} 1991 Free Software Foundation, Inc.
X675 Mass Ave, Cambridge, MA 02139, USA
XEveryone is permitted to copy and distribute verbatim copies
Xof this license document, but changing it is not allowed.
X
X[This is the first released version of the library GPL.  It is
X numbered 2 because it goes with version 2 of the ordinary GPL.]
X\end{example}
X
X\section*{Preamble}
X
X  The licenses for most software are designed to take away your
Xfreedom to share and change it.  By contrast, the GNU General Public
XLicenses are intended to guarantee your freedom to share and change
Xfree software---to make sure the software is free for all its users.
X
X  This license, the Library General Public License, applies to some
Xspecially designated Free Software Foundation software, and to any
Xother libraries whose authors decide to use it.  You can use it for
Xyour libraries, too.
X
X  When we speak of free software, we are referring to freedom, not
Xprice.  Our General Public Licenses are designed to make sure that you
Xhave the freedom to distribute copies of free software (and charge for
Xthis service if you wish), that you receive source code or can get it
Xif you want it, that you can change the software or use pieces of it
Xin new free programs; and that you know you can do these things.
X
X  To protect your rights, we need to make restrictions that forbid
Xanyone to deny you these rights or to ask you to surrender the rights.
XThese restrictions translate to certain responsibilities for you if
Xyou distribute copies of the library, or if you modify it.
X
X  For example, if you distribute copies of the library, whether gratis
Xor for a fee, you must give the recipients all the rights that we gave
Xyou.  You must make sure that they, too, receive or can get the source
Xcode.  If you link a program with the library, you must provide
Xcomplete object files to the recipients so that they can relink them
Xwith the library, after making changes to the library and recompiling
Xit.  And you must show them these terms so they know their rights.
X
X  Our method of protecting your rights has two steps: (1) copyright
Xthe library, and (2) offer you this license which gives you legal
Xpermission to copy, distribute and/or modify the library.
X
X  Also, for each distributor's protection, we want to make certain
Xthat everyone understands that there is no warranty for this free
Xlibrary.  If the library is modified by someone else and passed on, we
Xwant its recipients to know that what they have is not the original
Xversion, so that any problems introduced by others will not reflect on
Xthe original authors' reputations.
X
X  Finally, any free program is threatened constantly by software
Xpatents.  We wish to avoid the danger that companies distributing free
Xsoftware will individually obtain patent licenses, thus in effect
Xtransforming the program into proprietary software.  To prevent this,
Xwe have made it clear that any patent must be licensed for everyone's
Xfree use or not licensed at all.
X
X  Most GNU software, including some libraries, is covered by the ordinary
XGNU General Public License, which was designed for utility programs.  This
Xlicense, the GNU Library General Public License, applies to certain
Xdesignated libraries.  This license is quite different from the ordinary
Xone; be sure to read it in full, and don't assume that anything in it is
Xthe same as in the ordinary license.
X
X  The reason we have a separate public license for some libraries is that
Xthey blur the distinction we usually make between modifying or adding to a
Xprogram and simply using it.  Linking a program with a library, without
Xchanging the library, is in some sense simply using the library, and is
Xanalogous to running a utility program or application program.  However, in
Xa textual and legal sense, the linked executable is a combined work, a
Xderivative of the original library, and the ordinary General Public License
Xtreats it as such.
X
X  Because of this blurred distinction, using the ordinary General
XPublic License for libraries did not effectively promote software
Xsharing, because most developers did not use the libraries.  We
Xconcluded that weaker conditions might promote sharing better.
X
X  However, unrestricted linking of non-free programs would deprive the
Xusers of those programs of all benefit from the free status of the
Xlibraries themselves.  This Library General Public License is intended to
Xpermit developers of non-free programs to use free libraries, while
Xpreserving your freedom as a user of such programs to change the free
Xlibraries that are incorporated in them.  (We have not seen how to achieve
Xthis as regards changes in header files, but we have achieved it as regards
Xchanges in the actual functions of the Library.)  The hope is that this
Xwill lead to faster development of free libraries.
X
X  The precise terms and conditions for copying, distribution and
Xmodification follow.  Pay close attention to the difference between a
X``work based on the library'' and a ``work that uses the library''.  The
Xformer contains code derived from the library, while the latter only
Xworks together with the library.
X
X  Note that it is possible for a library to be covered by the ordinary
XGeneral Public License rather than by this special one.
X
X\begin{iftex}
X\section*{TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION}
X\end{iftex}
X\begin{ifinfo}
X\begin{center}
XTERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
X\end{center}
X\end{ifinfo}
X
X\begin{enumerate}
X\item
XThis License Agreement applies to any software library which
Xcontains a notice placed by the copyright holder or other authorized
Xparty saying it may be distributed under the terms of this Library
XGeneral Public License (also called ``this License'').  Each licensee is
Xaddressed as ``you''.
X
X  A ``library'' means a collection of software functions and/or data
Xprepared so as to be conveniently linked with application programs
X(which use some of those functions and data) to form executables.
X
X  The ``Library'', below, refers to any such software library or work
Xwhich has been distributed under these terms.  A ``work based on the
XLibrary'' means either the Library or any derivative work under
Xcopyright law: that is to say, a work containing the Library or a
Xportion of it, either verbatim or with modifications and/or translated
Xstraightforwardly into another language.  (Hereinafter, translation is
Xincluded without limitation in the term ``modification''.)
X
X  ``Source code'' for a work means the preferred form of the work for
Xmaking modifications to it.  For a library, complete source code means
Xall the source code for all modules it contains, plus any associated
Xinterface definition files, plus the scripts used to control compilation
Xand installation of the library.
X
X  Activities other than copying, distribution and modification are not
Xcovered by this License; they are outside its scope.  The act of
Xrunning a program using the Library is not restricted, and output from
Xsuch a program is covered only if its contents constitute a work based
Xon the Library (independent of the use of the Library in a tool for
Xwriting it).  Whether that is true depends on what the Library does
Xand what the program that uses the Library does.
X  
X\item
XYou may copy and distribute verbatim copies of the Library's
Xcomplete source code as you receive it, in any medium, provided that
Xyou conspicuously and appropriately publish on each copy an
Xappropriate copyright notice and disclaimer of warranty; keep intact
Xall the notices that refer to this License and to the absence of any
Xwarranty; and distribute a copy of this License along with the
XLibrary.
X
X  You may charge a fee for the physical act of transferring a copy,
Xand you may at your option offer warranty protection in exchange for a
Xfee.
X
X\item
XYou may modify your copy or copies of the Library or any portion
Xof it, thus forming a work based on the Library, and copy and
Xdistribute such modifications or work under the terms of Section 1
Xabove, provided that you also meet all of these conditions:
X
X\begin{enumerate}
X\item
XThe modified work must itself be a software library.
X
X\item
XYou must cause the files modified to carry prominent notices
Xstating that you changed the files and the date of any change.
X
X\item
XYou must cause the whole of the work to be licensed at no
Xcharge to all third parties under the terms of this License.
X
X\item
XIf a facility in the modified Library refers to a function or a
Xtable of data to be supplied by an application program that uses
Xthe facility, other than as an argument passed when the facility
Xis invoked, then you must make a good faith effort to ensure that,
Xin the event an application does not supply such function or
Xtable, the facility still operates, and performs whatever part of
Xits purpose remains meaningful.
X
X(For example, a function in a library to compute square roots has
Xa purpose that is entirely well-defined independent of the
Xapplication.  Therefore, Subsection 2d requires that any
Xapplication-supplied function or table used by this function must
Xbe optional: if the application does not supply it, the square
Xroot function must still compute square roots.)
X\end{enumerate}
X
XThese requirements apply to the modified work as a whole.  If
Xidentifiable sections of that work are not derived from the Library,
Xand can be reasonably considered independent and separate works in
Xthemselves, then this License, and its terms, do not apply to those
Xsections when you distribute them as separate works.  But when you
Xdistribute the same sections as part of a whole which is a work based
Xon the Library, the distribution of the whole must be on the terms of
Xthis License, whose permissions for other licensees extend to the
Xentire whole, and thus to each and every part regardless of who wrote
Xit.
X
XThus, it is not the intent of this section to claim rights or contest
Xyour rights to work written entirely by you; rather, the intent is to
Xexercise the right to control the distribution of derivative or
Xcollective works based on the Library.
X
XIn addition, mere aggregation of another work not based on the Library
Xwith the Library (or with a work based on the Library) on a volume of
Xa storage or distribution medium does not bring the other work under
Xthe scope of this License.
X
X\item
XYou may opt to apply the terms of the ordinary GNU General Public
XLicense instead of this License to a given copy of the Library.  To do
Xthis, you must alter all the notices that refer to this License, so
Xthat they refer to the ordinary GNU General Public License, version 2,
Xinstead of to this License.  (If a newer version than version 2 of the
Xordinary GNU General Public License has appeared, then you can specify
Xthat version instead if you wish.)  Do not make any other change in
Xthese notices.
X
X  Once this change is made in a given copy, it is irreversible for
Xthat copy, so the ordinary GNU General Public License applies to all
Xsubsequent copies and derivative works made from that copy.
X
X  This option is useful when you wish to copy part of the code of
Xthe Library into a program that is not a library.
X
X\item
XYou may copy and distribute the Library (or a portion or
Xderivative of it, under Section 2) in object code or executable form
Xunder the terms of Sections 1 and 2 above provided that you accompany
Xit with the complete corresponding machine-readable source code, which
Xmust be distributed under the terms of Sections 1 and 2 above on a
Xmedium customarily used for software interchange.
X
X  If distribution of object code is made by offering access to copy
Xfrom a designated place, then offering equivalent access to copy the
Xsource code from the same place satisfies the requirement to
Xdistribute the source code, even though third parties are not
Xcompelled to copy the source along with the object code.
X
X\item
XA program that contains no derivative of any portion of the
XLibrary, but is designed to work with the Library by being compiled or
Xlinked with it, is called a ``work that uses the Library''.  Such a
Xwork, in isolation, is not a derivative work of the Library, and
Xtherefore falls outside the scope of this License.
X
X  However, linking a ``work that uses the Library'' with the Library
Xcreates an executable that is a derivative of the Library (because it
Xcontains portions of the Library), rather than a ``work that uses the
Xlibrary''.  The executable is therefore covered by this License.
XSection 6 states terms for distribution of such executables.
X
X  When a ``work that uses the Library'' uses material from a header file
Xthat is part of the Library, the object code for the work may be a
Xderivative work of the Library even though the source code is not.
XWhether this is true is especially significant if the work can be
Xlinked without the Library, or if the work is itself a library.  The
Xthreshold for this to be true is not precisely defined by law.
X
X  If such an object file uses only numerical parameters, data
Xstructure layouts and accessors, and small macros and small inline
Xfunctions (ten lines or less in length), then the use of the object
Xfile is unrestricted, regardless of whether it is legally a derivative
Xwork.  (Executables containing this object code plus portions of the
XLibrary will still fall under Section 6.)
X
X  Otherwise, if the work is a derivative of the Library, you may
Xdistribute the object code for the work under the terms of Section 6.
XAny executables containing that work also fall under Section 6,
Xwhether or not they are linked directly with the Library itself.
X
X\item
XAs an exception to the Sections above, you may also compile or
Xlink a ``work that uses the Library'' with the Library to produce a
Xwork containing portions of the Library, and distribute that work
Xunder terms of your choice, provided that the terms permit
Xmodification of the work for the customer's own use and reverse
Xengineering for debugging such modifications.
X
X  You must give prominent notice with each copy of the work that the
XLibrary is used in it and that the Library and its use are covered by
Xthis License.  You must supply a copy of this License.  If the work
Xduring execution displays copyright notices, you must include the
Xcopyright notice for the Library among them, as well as a reference
Xdirecting the user to the copy of this License.  Also, you must do one
Xof these things:
X
X\begin{enumerate}
X\item
XAccompany the work with the complete corresponding
Xmachine-readable source code for the Library including whatever
Xchanges were used in the work (which must be distributed under
XSections 1 and 2 above); and, if the work is an executable linked
Xwith the Library, with the complete machine-readable ``work that
Xuses the Library'', as object code and/or source code, so that the
Xuser can modify the Library and then relink to produce a modified
Xexecutable containing the modified Library.  (It is understood
Xthat the user who changes the contents of definitions files in the
XLibrary will not necessarily be able to recompile the application
Xto use the modified definitions.)
X
X\item
XAccompany the work with a written offer, valid for at
Xleast three years, to give the same user the materials
Xspecified in Subsection 6a, above, for a charge no more
Xthan the cost of performing this distribution.
X
X\item
XIf distribution of the work is made by offering access to copy
Xfrom a designated place, offer equivalent access to copy the above
Xspecified materials from the same place.
X
X\item
XVerify that the user has already received a copy of these
Xmaterials or that you have already sent this user a copy.
X\end{enumerate}
X
X  For an executable, the required form of the ``work that uses the
XLibrary'' must include any data and utility programs needed for
Xreproducing the executable from it.  However, as a special exception,
Xthe source code distributed need not include anything that is normally
Xdistributed (in either source or binary form) with the major
Xcomponents (compiler, kernel, and so on) of the operating system on
Xwhich the executable runs, unless that component itself accompanies
Xthe executable.
X
X  It may happen that this requirement contradicts the license
Xrestrictions of other proprietary libraries that do not normally
Xaccompany the operating system.  Such a contradiction means you cannot
Xuse both them and the Library together in an executable that you
Xdistribute.
X
X\item
XYou may place library facilities that are a work based on the
XLibrary side-by-side in a single library together with other library
Xfacilities not covered by this License, and distribute such a combined
Xlibrary, provided that the separate distribution of the work based on
Xthe Library and of the other library facilities is otherwise
Xpermitted, and provided that you do these two things:
X
X\begin{enumerate}
X\item
XAccompany the combined library with a copy of the same work
Xbased on the Library, uncombined with any other library
Xfacilities.  This must be distributed under the terms of the
XSections above.
X
X\item
XGive prominent notice with the combined library of the fact
Xthat part of it is a work based on the Library, and explaining
Xwhere to find the accompanying uncombined form of the same work.
X\end{enumerate}
X
X\item
XYou may not copy, modify, sublicense, link with, or distribute
Xthe Library except as expressly provided under this License.  Any
Xattempt otherwise to copy, modify, sublicense, link with, or
Xdistribute the Library is void, and will automatically terminate your
Xrights under this License.  However, parties who have received copies,
Xor rights, from you under this License will not have their licenses
Xterminated so long as such parties remain in full compliance.
X
X\item
XYou are not required to accept this License, since you have not
Xsigned it.  However, nothing else grants you permission to modify or
Xdistribute the Library or its derivative works.  These actions are
Xprohibited by law if you do not accept this License.  Therefore, by
Xmodifying or distributing the Library (or any work based on the
XLibrary), you indicate your acceptance of this License to do so, and
Xall its terms and conditions for copying, distributing or modifying
Xthe Library or works based on it.
X
X\item
XEach time you redistribute the Library (or any work based on the
XLibrary), the recipient automatically receives a license from the
Xoriginal licensor to copy, distribute, link with or modify the Library
Xsubject to these terms and conditions.  You may not impose any further
Xrestrictions on the recipients' exercise of the rights granted herein.
XYou are not responsible for enforcing compliance by third parties to
Xthis License.
X
X\item
XIf, as a consequence of a court judgment or allegation of patent
Xinfringement or for any other reason (not limited to patent issues),
Xconditions are imposed on you (whether by court order, agreement or
Xotherwise) that contradict the conditions of this License, they do not
Xexcuse you from the conditions of this License.  If you cannot
Xdistribute so as to satisfy simultaneously your obligations under this
XLicense and any other pertinent obligations, then as a consequence you
Xmay not distribute the Library at all.  For example, if a patent
Xlicense would not permit royalty-free redistribution of the Library by
Xall those who receive copies directly or indirectly through you, then
Xthe only way you could satisfy both it and this License would be to
Xrefrain entirely from distribution of the Library.
X
XIf any portion of this section is held invalid or unenforceable under any
Xparticular circumstance, the balance of the section is intended to apply,
Xand the section as a whole is intended to apply in other circumstances.
X
XIt is not the purpose of this section to induce you to infringe any
Xpatents or other property right claims or to contest validity of any
Xsuch claims; this section has the sole purpose of protecting the
Xintegrity of the free software distribution system which is
Ximplemented by public license practices.  Many people have made
Xgenerous contributions to the wide range of software distributed
Xthrough that system in reliance on consistent application of that
Xsystem; it is up to the author/donor to decide if he or she is willing
Xto distribute software through any other system and a licensee cannot
Ximpose that choice.
X
XThis section is intended to make thoroughly clear what is believed to
Xbe a consequence of the rest of this License.
X
X\item
XIf the distribution and/or use of the Library is restricted in
Xcertain countries either by patents or by copyrighted interfaces, the
Xoriginal copyright holder who places the Library under this License may add
Xan explicit geographical distribution limitation excluding those countries,
Xso that distribution is permitted only in or among countries not thus
Xexcluded.  In such case, this License incorporates the limitation as if
Xwritten in the body of this License.
X
X\item
XThe Free Software Foundation may publish revised and/or new
Xversions of the Library General Public License from time to time.
XSuch new versions will be similar in spirit to the present version,
Xbut may differ in detail to address new problems or concerns.
X
XEach version is given a distinguishing version number.  If the Library
Xspecifies a version number of this License which applies to it and
X``any later version'', you have the option of following the terms and
Xconditions either of that version or of any later version published by
Xthe Free Software Foundation.  If the Library does not specify a
Xlicense version number, you may choose any version ever published by
Xthe Free Software Foundation.
X
X\item
XIf you wish to incorporate parts of the Library into other free
Xprograms whose distribution conditions are incompatible with these,
Xwrite to the author to ask for permission.  For software which is
Xcopyrighted by the Free Software Foundation, write to the Free
XSoftware Foundation; we sometimes make exceptions for this.  Our
Xdecision will be guided by the two goals of preserving the free status
Xof all derivatives of our free software and of promoting the sharing
Xand reuse of software generally.
X
X\begin{iftex}
X\section*{NO WARRANTY}
X\end{iftex}
X\begin{ifinfo}
X\begin{center}
XNO WARRANTY
X\end{center}
X\end{ifinfo}
X
X\item
XBECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
XWARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
XEXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
XOTHER PARTIES PROVIDE THE LIBRARY ``AS IS'' WITHOUT WARRANTY OF ANY
XKIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
XIMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
XPURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
XLIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
XTHE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
X
X\item
XIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
XWRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
XAND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
XFOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
XCONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
XLIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
XRENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
XFAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
XSUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
XDAMAGES.
X\end{enumerate}
X
X\begin{iftex}
X\section*{END OF TERMS AND CONDITIONS}
X\end{iftex}
X\begin{ifinfo}
X\begin{center}
XEND OF TERMS AND CONDITIONS
X\end{center}
X\end{ifinfo}
X
X\clearpage
X
X\section*{How to Apply These Terms to Your New Libraries}
X
X  If you develop a new library, and you want it to be of the greatest
Xpossible use to the public, we recommend making it free software that
Xeveryone can redistribute and change.  You can do so by permitting
Xredistribution under these terms (or, alternatively, under the terms of the
Xordinary General Public License).
X
X  To apply these terms, attach the following notices to the library.  It is
Xsafest to attach them to the start of each source file to most effectively
Xconvey the exclusion of warranty; and each file should have at least the
X``copyright'' line and a pointer to where the full notice is found.
X
X\begin{smallexample}
X\var{one line to give the library's name and an idea of what it does.}
XCopyright (C) \var{year}  \var{name of author}
X
XThis library is free software; you can redistribute it and/or
Xmodify it under the terms of the GNU Library General Public
XLicense as published by the Free Software Foundation; either
Xversion 2 of the License, or (at your option) any later version.
X
XThis library is distributed in the hope that it will be useful,
Xbut WITHOUT ANY WARRANTY; without even the implied warranty of
XMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
XLibrary General Public License for more details.
X
XYou should have received a copy of the GNU Library General Public
XLicense along with this library; if not, write to the
XFree Software Foundation, Inc., 675 Mass Ave, Cambridge,
XMA 02139, USA.
X\end{smallexample}
X
XAlso add information on how to contact you by electronic and paper mail.
X
XYou should also get your employer (if you work as a programmer) or your
Xschool, if any, to sign a ``copyright disclaimer'' for the library, if
Xnecessary.  Here is a sample; alter the names:
X
X\begin{example}
XYoyodyne, Inc., hereby disclaims all copyright interest in
Xthe library `Frob' (a library for tweaking knobs) written
Xby James Random Hacker.
X
X\var{signature of Ty Coon}, 1 April 1990
XTy Coon, President of Vice
X\end{example}
X
XThat's all there is to it!
END_OF_FILE
if test 25800 -ne `wc -c <'doc/lgpl.tex'`; then
    echo shar: \"'doc/lgpl.tex'\" unpacked with wrong size!
fi
# end of 'doc/lgpl.tex'
fi
if test -f 'doc/makefile' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'doc/makefile'\"
else
echo shar: Extracting \"'doc/makefile'\" \(3859 characters\)
sed "s/^X//" >'doc/makefile' <<'END_OF_FILE'
X#
X# RCS makefile,v 1.1.1.1 1995/08/07 08:43:10 mohsen Exp
X#
X
X# The name of the file
XMANUAL=main
XINFOFILE= bbdb-filters.info
X
XTEXPARTS = 
X
XEPSFIGS = 
X
XTGRINDS = 
X
XEOEBASE		= /usr/public/eoe/lisp/public/bbdbPlus
XEOEINFO		= /usr/public/eoe/info
X
X# The name of your DVI to PS filter
XDVIPS=dvips -f
X
X# The name of your GNU Emacs
XEMACS=	xemacs
X
XLATEXINFO= /usr/public/tex/latexinfo1.7
X
X###
X###  SHOUL NOT HAVE TO TOUCH ANYTHING BELOW HERE
X###
XSHELL=/bin/sh
X
X.SUFFIXES:
X.SUFFIXES:	.lpr .ps .tty .xdvi .dvi .tex  .ptex .eps .fig .c
X
X.fig.eps:
X	fig2dev -L ps $< > $@
X
X.c.tex: 
X	tgrind -f $< > $@
X
X
X# DEFAULT TARGET
X#all:	$(INFOFILE) $(MANUAL).ps
Xall:	fast.ps
X
X$(INFOFILE):	$(MANUAL).tex $(TEXPARTS)
X	rm -f makeinfo.el
X	sed -e 	"s+MANUAL+$(MANUAL)+" \
X	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/makeinfo.tmplt > makeinfo.el
X	$(EMACS) -batch -q -l makeinfo.el
X	#cp $(INFOFILE) /usr/public/eoe/info
X
X$(MANUAL).tex:	$(MANUAL).texinfo $(TEXPARTS)
X	sed -e 	"s+INFOFILE+$(INFOFILE)+" $(MANUAL).texinfo | expand > $(MANUAL).tex
X	-rm -f maketex.el
X	sed -e 	"s+MANUAL+$(MANUAL)+" \
X	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/maketex.tmplt > maketex.el
X	$(EMACS) -batch -q -l maketex.el
X
X$(MANUAL).hyperlatex:	$(MANUAL).texinfo $(TEXPARTS)
X	sed -e 	"s+INFOFILE+$(INFOFILE)+" $(MANUAL).texinfo | expand > $(MANUAL).hyperlatex
X	-rm -f makehyperlatex.el
X	sed -e 	"s+MANUAL+$(MANUAL)+" \
X	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/makehyperlatex.tmplt > makehyperlatex.el
X	$(EMACS) -batch -q -l makehyperlatex.el
X
X$(MANUAL).dvi:	$(MANUAL).tex $(EPSFIGS) $(TGRINDS)
X	latex2dvi $(MANUAL).tex
X
X$(MANUAL).bbl:	
X	latex $(MANUAL)
X	-bibtex $(MANUAL)
X	latex $(MANUAL)
X
X$(MANUAL).xdvi:	$(MANUAL).dvi
X	xdvi $(MANUAL).dvi &
X
X$(MANUAL).ps:	$(MANUAL).dvi
X	$(DVIPS) $(MANUAL) > $(MANUAL).ps
X
X$(MANUAL).lpr:	$(MANUAL).ps
X	lpr $(MANUAL).ps
X
Xinfo:   $(INFOFILE)
X	-echo Built $(INFOFILE)
X
X$(MANUAL).html: $(MANUAL)/$(MANUAL).html
X	-echo Building $(MANUAL)/$(MANUAL).html
X
X$(MANUAL)/$(MANUAL).html: $(MANUAL).dvi $(MANUAL).htmlTex
X	/usr/public/src/Sol-2/networking/www/latex2html-95.1/latex2html $(MANUAL).tex
X
XEMACSBASE	= /opt/public/networking/www/hyperlatex-1.3/emacs
X
Xhtml:	 $(MANUAL).hyperlatex # $(MANUAL).dvi
X	-mkdir bbdbFilters
X	$(EMACS) -batch -no-init-file -no-site-file \
X	-l $(EMACSBASE)/hyperlatex1.el -funcall batch-hyperlatex-format $(MANUAL).hyperlatex
X	echo latex \'\\def\\makegifs{}\\input{$(MANUAL).hyperlatex}\' > dolatex.sh
X	#sh dolatex.sh ; /bin/rm dolatex.sh
X	#sh $(MANUAL).makegif 
X
Xinstall: $(INFOFILE)
X	cp $(INFOFILE) $(EOEINFO)/$(INFOFILE)
X	
X#
X# Fast Processing
X#
X
Xfast.tex:	$(MANUAL).texinfo $(TEXPARTS)
X	sed -e "s+INFOFILE+$(INFOFILE)+" -e "s+mbinput+input+" $(MANUAL).texinfo | expand > fast.tex
X
Xfast.dvi:	fast.tex $(EPSFIGS) $(TGRINDS)
X	latex fast.tex
X
Xfast.xdvi:	fast.dvi
X	xdvi fast.dvi &
X
Xfast.ps:	fast.dvi
X	$(DVIPS) fast > fast.ps
X
Xfast.xps:	fast.ps
X	pageview fast.ps &
X
Xfast.lpr:	fast.ps
X	lpr fast.ps
X
X
X# TeX Figures for when dvi files are needed. Just an example
X#XX.tex YY.tex: XX.fig YY.fig 
X#	transfig -m 1.00 -L eepic -M fig.make XX.fig YY.fig
X#	make -f fig.make
X
X# Encapsulated PostScript figures -- Done by the Suffix rules
X#XX.eps: XX.fig 
X#	fig2dev -L ps -m 1.0 $< > $@
X
X# Src Code
X#cot-calling.tex: cot-calling.c 
X#	tgrind -f $< > $@
X
X
Xshar::
X	split $(MANUAL).tex $(MANUAL)-
X
Xclean:	
X	rm -f $(MANUAL).log $(MANUAL).blg makeinfo.el maketex.el *~ #~
X
Xveryclean: clean
X	rm -f $(MANUAL).ps $(MANUAL).dvi $(MANUAL).dlog $(MANUAL).info
X
Xrealclean: veryclean
X	rm -f $(MANUAL).aux $(MANUAL).bbl $(MANUAL).blg $(MANUAL).cp \
X	$(MANUAL).toc $(MANUAL).cps $(MANUAL).lot $(MANUAL).lof fig.make \
X	$(MANUAL).auxO $(MANUAL).fn $(MANUAL).fns \
X	transfig.tex $(MANUAL).tex $(INFOFILE) \
X	$(MANUAL).hyperlatex makehyperlatex.el dolatex.sh \
X	fast.aux fast.dvi fast.log fast.ps fast.tex fast.toc fast.cp fast.fn \
X	$(EPSFIGS) $(TGRINDS)
X
END_OF_FILE
if test 3859 -ne `wc -c <'doc/makefile'`; then
    echo shar: \"'doc/makefile'\" unpacked with wrong size!
fi
# end of 'doc/makefile'
fi
if test ! -d 'doc/formatted' ; then
    echo shar: Creating directory \"'doc/formatted'\"
    mkdir 'doc/formatted'
fi
if test -f 'doc/formatted/bbdb-filters.info' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'doc/formatted/bbdb-filters.info'\"
else
echo shar: Extracting \"'doc/formatted/bbdb-filters.info'\" \(44122 characters\)
sed "s/^X//" >'doc/formatted/bbdb-filters.info' <<'END_OF_FILE'
XInfo file: bbdb-filters.info,    -*-Text-*-
Xproduced by latexinfo-format-buffer
Xfrom file: main.tex
X
X
X
XFile: bbdb-filters.info  Node: Top, Prev: (dir), Up: (dir), Next: Introduction
X
X{BBDB Filters}
X
X{6}
X
XCopyright (C)1995 NEDA COMMUNICATIONS, INC.
X
X
X* Menu:
X
X* Introduction::		
X* Output Filters::		
X* Input Filters::		
X* Miscellany::			
X* GNU LIBRARY GENERAL PUBLIC LICENSE::	
X* Concept Index::
X* Command Index::
X
X --- The Detailed Node Listing ---
X
XIntroduction
X
X* About This Package::		
X* About This Manual::		
X
XOutput Filters
X
X* HP 200LX Phone Book::		
X* PC Eudora::			
X* Lotus cc:Mail Nicknames::	
X* PH::				
X* Emacs Lisp Export::		
X
XPC Eudora
X
X* PC Eudora Nickname Database::
X* PC Eudora Recipient Database::
X
XInput Filters
X
X* General Facilities for Input Filtering::  
X* UNIX Password Files::		
X
XMiscellany
X
X* TODO List::			
X* Credits::			
X
XGNU LIBRARY GENERAL PUBLIC LICENSE
X
X* Preamble::			
X* TERMS AND CONDITIONS FOR COPYING::  *
X* NO WARRANTY::			
X* END OF TERMS AND CONDITIONS::	 
X* How to Apply These Terms to Your New Libraries::  
X
X
X
XFile: bbdb-filters.info  Node: Introduction, Prev: Top, Up: Top, Next: Output Filters
X
XIntroduction
X************
X
X
XOver time much valuable data has been gathered in BBDB database files.
XMany wish to share parts or all of this information with others.  They
Xalso wish to have access to this same information from other systems
X(like personal digital assistants) lacking straightforward BBDB
Xaccess.
X
XFor these reasons, we have prepared a family of filters that convert
Xthe information in BBDB to and from a variety of other
Xformats. "Output filters" export BBDB information to other formats
Xwhile "input filters" import information from other formats into
XBBDB.
X
XOur hope is that over time this collection of BBDB filters will grow
Xthrough contributed code.
X
X
X* Menu:
X
X* About This Package::		
X* About This Manual::		
X
X
X
XFile: bbdb-filters.info  Node: About This Package, Prev: Introduction, Up: Introduction, Next: About This Manual
X
XAbout This Package
X==================
X
X
XThis package is a collection of filters and is called "BBDB Input and
XOutput Filters".  It has been somewhat tested with BBDB version 1.50.
XThe present state of the software is still preliminary although it has
Xproved useful.
X
X
XFile: bbdb-filters.info  Node: About This Manual, Prev: About This Package, Up: Introduction
X
XAbout This Manual
X=================
X
X
XThis documentation applies to Version 0.2 of the "BBDB Input and
XOutput Filters" package.  The documentation is presently skeletal and
Xvery preliminary.  It mostly provides the user with instructions for
Xuse, and very little background is included.  Familiarity with Emacs
XLisp is assumed for some sections.
X
X
XFile: bbdb-filters.info  Node: Output Filters, Prev: Introduction, Up: Top, Next: Input Filters
X
XOutput Filters
X**************
X
X
X"Output filters" are used to export BBDB information into formats 
Xused by other systems.
X
XIn general, an output filter uses the contents of your
X`*BBDB*' buffer as input.  Note that output filters do not use
XBBDB files (typically ``~/.bbdb'') directly.
X
XAn output filter is invoked by executing its associated lisp function.
XThe name of the function is conventionally named `bbdb-<system>-output' 
X(e.g., `M-x bbdb-hp200lx-output').
X
XThe result of running an output filter is to create a new buffer that
Xcontains the `*BBDB*' information appropriately transformed into a
Xformat suitable for use by the target system.  The new buffer is given
Xa file name that you specify.
X
X
X* Menu:
X
X* HP 200LX Phone Book::		
X* PC Eudora::			
X* Lotus cc:Mail Nicknames::	
X* PH::				
X* Emacs Lisp Export::		
X
X
X
XFile: bbdb-filters.info  Node: HP 200LX Phone Book, Prev: Output Filters, Up: Output Filters, Next: PC Eudora
X
XHP 200LX Phone Book
X===================
X
X
XThis package has only been tested on HP 200LX palmtop systems.  It
Xalso requires the "HP 200LX Connectivity Pack" for converting
Xcomma-delimited ASCII files into binary .PDB files which are read by
Xthe HP 200LX Phone Book application.  Version 1.00 of the "HP 200LX
XConnectivty Pack" was used for testing.
X
XThe HP 200LX output filter is in file `bbdb-hp200lx.el'.
X
X     
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke ` bbdb-hp200lx-output' to create an ASCII .CDF 
X     (Comma Delimited File). 
X     
X  3. Using Xlate/Merge option of HP Connectivity Pack convert the
X     .CDF file into a binary .PDB file used by the Phone Book program.
X     
X  4. Download the .PDB file to your palmtop's internal disk and
X     ensure that the Phone Book program is set use the newly downloaded
X     .PDB file.
X     
X
X
X
XFile: bbdb-filters.info  Node: PC Eudora, Prev: HP 200LX Phone Book, Up: Output Filters, Next: Lotus cc:Mail Nicknames
X
XPC Eudora
X=========
X
X
XBBDB information can be exported to PC Eudora in two formats--as a
Xnickname database file and as a recipients database file.
X
XThe PC Eudora output filter is in file `bbdb-eudora.el'.
X
X* Menu:
X
X* PC Eudora Nickname Database::
X* PC Eudora Recipient Database::
X
X
X
XFile: bbdb-filters.info  Node: PC Eudora Nickname Database, Prev: PC Eudora, Up: PC Eudora, Next: PC Eudora Recipient Database
X
XPC Eudora Nickname Database
X---------------------------
X
X
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke `bbdb-eudora-nndbase-output' to create a PC Eudora
X     Nickname database file.
X     
X  3. Make the file accessible to PC Eudora.
X     
X
X
X
XFile: bbdb-filters.info  Node: PC Eudora Recipient Database, Prev: PC Eudora Nickname Database, Up: PC Eudora
X
XPC Eudora Recipient Database
X----------------------------
X
X
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke `bbdb-eudora-rcpdbase-output' to create a PC Eudora 
X     recipient's database file.
X     
X  3. Make the file accessible to PC Eudora.
X     
X
X
X
XFile: bbdb-filters.info  Node: Lotus cc:Mail Nicknames, Prev: PC Eudora, Up: Output Filters, Next: PH
X
XLotus cc:Mail Nicknames
X=======================
X
X
XThe Lotus cc:Mail output filter is in file `bbdb-ccmail.el'.
X
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke ` bbdb-ccmail-output' to create a cc:Mail Nicknames file.
X     
X  3. Make the file accessible to cc:Mail. 
X     
X
X
X
XFile: bbdb-filters.info  Node: PH, Prev: Lotus cc:Mail Nicknames, Up: Output Filters, Next: Emacs Lisp Export
X
XPH
X==
X
X
XThe PH output filter is in file `bbdb-ph.el'.
X
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke `bbdb-ph-output' to create a `ph' data file for 
X     use with the  `maked' program.
X     
X  3. Make the file accessible to `ph'.
X     
X
X
X
XFile: bbdb-filters.info  Node: Emacs Lisp Export, Prev: PH, Up: Output Filters
X
XEmacs Lisp Export
X=================
X
X
XThe Emacs Lisp Export output filter is in file `bbdb-export.el'.
X
XThis output filter uses the current contents of your
X`*BBDB*' buffer to generate a new buffer (`*BBDB* Export') 
Xthat contains a single lisp `(progn ...)' expression.  For
Xexample, a `*BBDB*' buffer containing two records would result in
Xthe following `*BBDB* Export' buffer:
X
X     
X     ;;; ======= Start of Exported BBDB Records =======
X     (progn  
X       (require 'bbdb-com)
X       (defun bbdb-maybe-create (name company net &optional addrs phones notes)
X         "Try to add a record to BBDB if it does not already exist."
X         (condition-case err
X             (progn
X               (bbdb-create-internal name company net addrs phones notes)
X               (message "%s %s added." name (if net (concat "<" net ">") ""))
X               (sleep-for 1))    
X           (error (ding)
X                  (message "%s %s skipped. (%s)"
X                           name
X                           (if net (concat "<" net ">") "")
X                           (car (cdr err)))
X                  (sleep-for 1))))
X     
X       (bbdb-maybe-create "Jill Doe--IMPORTED"
X                          "CBS Corporation"
X                          '("jilld@cbs.com")
X                          '(
X                            ["Home"
X                             "368 222ND PL"
X                             ""
X                             ""
X                             "Springfield"
X                             "MA" 2117]
X                            )
X                          '(
X                            ["Office" 617 555 9983 0]
X                            ) '"Movie Mogul")
X       (bbdb-maybe-create "John Doe--IMPORTED"
X                          "ABC Incorporated"
X                          '("jdoe@abc.com")
X                          '(
X                            ["Office"
X                             "123 Any Street"
X                             ""
X                             ""
X                             "Any Town"
X                             "WA" (98027 7758)]
X                            )
X                          '(
X                            ["Office" 206 555 1234 0]
X                            ) '"TV Producer")
X       )
X     ;;; ======= End of Exported BBDB Records =======
X
X
XThis lisp expression can then be sent via email or some other
Xtext-based messaging facility to another user who can then evaluate
Xthe expression which will add the `BBDB' records to the
Xrecipient's
X`BBDB' database.  
X
XOnly new records are added.  A record with the same name or net
Xaddress as one already existing in the `BBDB' is skipped
Xentirely.
X
XIn the sample contents of a `*BBDB* Export' buffer presented, two
Xrecords are being exported--one for "John Doe" and the other for
X"Jill Doe".  Notice that their names have been appended with
X`--IMPORTED'.  This string can be used to quick locate each record
Xthat is added to the database using this mechanism.
X
XThe following steps are for exporting BBDB records into Emacs Lisp:
X
X     
X  1. Invoke `M-x bbdb' to populate the `*BBDB*' buffer
X     with the contents you wish to export.
X     
X  2. Invoke `bbdb-export' to create a `*BBDB* Export' buffer which contains a
X     single `(progn ...)' can be evaluated to add the records to the
X     existing `BBDB' database (if the records do not already exist).
X     
X  3. Use the contents of `*BBDB* Export' in email and other messaging systems.
X     
X
X
XThe following steps are for a user wishing to import the contents of a
X`*BBDB* Export' buffer's expression into his or her own database:
X
X     
X  1. Evaluate the region bounded by the lines 
X      `;;; ======= Start of Exported BBDB Records =======' 
X     and 
X      `;;; ======= End of Exported BBDB Records ======='. 
X     You can use such commands as
X     `M-x eval-region' or `M-x eval-last-sexp'.
X     
X  2. Review the newly imported entries.  To see them, invoke `M-x
X     bbdb' and specify `--IMPORTED' at the `Regular Expression'
X     prompt.  
X     
X  3. After reviewing the contents of the imported records, you may
X     wish to remove the `--IMPORTED' that is appended to the name by
X     `bbdb-export'.
X     
X
X
X
XFile: bbdb-filters.info  Node: Input Filters, Prev: Output Filters, Up: Top, Next: Miscellany
X
XInput Filters
X*************
X
X
X"Input filters" are used to import into BBDB information from a
Xforeign system's data file.
X
XThe name of the function is conventionally named
X`bbdb-<system>-input' (e.g., `bbdb-passwd-input' is the name
Xof the Emacs Lisp function for the UNIX password file input filter).
X
XIn general, an "input filter" expects the foreign system's data to
Xbe in the current buffer.  The contents of the current buffer are used
Xto create an Emacs Lisp file which when loaded will add new records
Xinto your BBDB database if they don't yet exist--existing BBDB records
Xwill not be modified.
X
X
X* Menu:
X
X* General Facilities for Input Filtering::  
X* UNIX Password Files::		
X
X
X
XFile: bbdb-filters.info  Node: General Facilities for Input Filtering, Prev: Input Filters, Up: Input Filters, Next: UNIX Password Files
X
XGeneral Facilities for Input Filtering
X======================================
X
X
XThe result of running an input filter is to produce a new buffer a
Xseries of `bif-create-record' 
Xexpressions, each corresponding to a single user's record.  Notice
Xthat input filters do not directly modify the contents of the BBDB
Xfiles (typically ``~/.bbdb'').
X
XTo actually modify the contents of the BBDB database, you must
Xevaluated the expressions in the resultant buffer created by the input
Xfilter.  One way to do so is simply to invoke `M-x eval-buffer'.
XAnother way is to simply save the buffer to disk and load its contents
Xinto Emacs Lisp using `M-x load-file'.
X
X
XFile: bbdb-filters.info  Node: UNIX Password Files, Prev: General Facilities for Input Filtering, Up: Input Filters
X
XUNIX Password Files
X===================
X
X
XThe UNIX password file input filter is in file `bbdb-passwd.el'.
X
X     
X  1. Use `M-x find-file' to visit the UNIX password file you wish to import.
X     
X  2. With the password file in the current buffer, invoke the input 
X     filter `M-x bbdb-passwd-input'.  You will be prompted for the
X     domain name associated with that host's password file; an organization
X     name; as well as the file name to be associated with the buffer of
X     `bif-create-record' expressions.
X     
X  3. Evaluate the contents of the input filter's buffer to add records 
X     into your BBDB database file.
X     
X
X
X
XFile: bbdb-filters.info  Node: Miscellany, Prev: Input Filters, Up: Top, Next: GNU LIBRARY GENERAL PUBLIC LICENSE
X
XMiscellany
X**********
X
X
X
X* Menu:
X
X* TODO List::			
X* Credits::			
X
X
X
XFile: bbdb-filters.info  Node: TODO List, Prev: Miscellany, Up: Miscellany, Next: Credits
X
XTODO List
X=========
X
X
X     
X   * Move generic input filter functionality out of
X     `bbdb-passwd.el' and into, say, `bbdb-ifilt.el'.  
X     The generic functionality code has names typically prefixed with `bif-'.
X     
X   * Add support for `gdbload' (as an alternative to the 
X     Xlate/Merge application provided in the HP 200LX Connectivity Pack)
X     into the HP 200LX output filter.  This is based on input from Robert
X     Nicholson `<robert@steffi.dircon.co.uk>'.
X     
X   * Add documentation for variables in the various input and output filters.
X     
X   * Check and document all dependencies on other packages.
X     
X
X
X
XFile: bbdb-filters.info  Node: Credits, Prev: TODO List, Up: Miscellany
X
XCredits
X=======
X
X
XPean Lim `<pean@neda.com>' wrote most of this package.  Mohsen
XBanan `<mohsen@neda.com>' put it all together and guided the
Xwork.  Neda Communications, Inc. sponsored the work.  The output
Xfilters code is based on `bbdb-print' by Boris Goldowsky
X`<boris@prodigal.psych.rochester.edu>'.
X
X
X
XFile: bbdb-filters.info  Node: GNU LIBRARY GENERAL PUBLIC LICENSE, Prev: Miscellany, Up: Top, Next: Concept Index
X
XGNU LIBRARY GENERAL PUBLIC LICENSE
X**********************************
X
X
X                          Version 2, June 1991
X
X
X     
X     Copyright (C) 1991 Free Software Foundation, Inc.
X     675 Mass Ave, Cambridge, MA 02139, USA
X     Everyone is permitted to copy and distribute verbatim copies
X     of this license document, but changing it is not allowed.
X     
X     [This is the first released version of the library GPL.  It is
X      numbered 2 because it goes with version 2 of the ordinary GPL.]
X
X
X
X* Menu:
X
X* Preamble::			
X* TERMS AND CONDITIONS FOR COPYING::  *
X* NO WARRANTY::			
X* END OF TERMS AND CONDITIONS::	 
X* How to Apply These Terms to Your New Libraries::  
X
X
X
XFile: bbdb-filters.info  Node: Preamble, Prev: GNU LIBRARY GENERAL PUBLIC LICENSE, Up: GNU LIBRARY GENERAL PUBLIC LICENSE, Next: TERMS AND CONDITIONS FOR COPYING
X
XPreamble
X========
X
X
X  The licenses for most software are designed to take away your
Xfreedom to share and change it.  By contrast, the GNU General Public
XLicenses are intended to guarantee your freedom to share and change
Xfree software---to make sure the software is free for all its users.
X
X  This license, the Library General Public License, applies to some
Xspecially designated Free Software Foundation software, and to any
Xother libraries whose authors decide to use it.  You can use it for
Xyour libraries, too.
X
X  When we speak of free software, we are referring to freedom, not
Xprice.  Our General Public Licenses are designed to make sure that you
Xhave the freedom to distribute copies of free software (and charge for
Xthis service if you wish), that you receive source code or can get it
Xif you want it, that you can change the software or use pieces of it
Xin new free programs; and that you know you can do these things.
X
X  To protect your rights, we need to make restrictions that forbid
Xanyone to deny you these rights or to ask you to surrender the rights.
XThese restrictions translate to certain responsibilities for you if
Xyou distribute copies of the library, or if you modify it.
X
X  For example, if you distribute copies of the library, whether gratis
Xor for a fee, you must give the recipients all the rights that we gave
Xyou.  You must make sure that they, too, receive or can get the source
Xcode.  If you link a program with the library, you must provide
Xcomplete object files to the recipients so that they can relink them
Xwith the library, after making changes to the library and recompiling
Xit.  And you must show them these terms so they know their rights.
X
X  Our method of protecting your rights has two steps: (1) copyright
Xthe library, and (2) offer you this license which gives you legal
Xpermission to copy, distribute and/or modify the library.
X
X  Also, for each distributor's protection, we want to make certain
Xthat everyone understands that there is no warranty for this free
Xlibrary.  If the library is modified by someone else and passed on, we
Xwant its recipients to know that what they have is not the original
Xversion, so that any problems introduced by others will not reflect on
Xthe original authors' reputations.
X
X  Finally, any free program is threatened constantly by software
Xpatents.  We wish to avoid the danger that companies distributing free
Xsoftware will individually obtain patent licenses, thus in effect
Xtransforming the program into proprietary software.  To prevent this,
Xwe have made it clear that any patent must be licensed for everyone's
Xfree use or not licensed at all.
X
X  Most GNU software, including some libraries, is covered by the ordinary
XGNU General Public License, which was designed for utility programs.  This
Xlicense, the GNU Library General Public License, applies to certain
Xdesignated libraries.  This license is quite different from the ordinary
Xone; be sure to read it in full, and don't assume that anything in it is
Xthe same as in the ordinary license.
X
X  The reason we have a separate public license for some libraries is that
Xthey blur the distinction we usually make between modifying or adding to a
Xprogram and simply using it.  Linking a program with a library, without
Xchanging the library, is in some sense simply using the library, and is
Xanalogous to running a utility program or application program.  However, in
Xa textual and legal sense, the linked executable is a combined work, a
Xderivative of the original library, and the ordinary General Public License
Xtreats it as such.
X
X  Because of this blurred distinction, using the ordinary General
XPublic License for libraries did not effectively promote software
Xsharing, because most developers did not use the libraries.  We
Xconcluded that weaker conditions might promote sharing better.
X
X  However, unrestricted linking of non-free programs would deprive the
Xusers of those programs of all benefit from the free status of the
Xlibraries themselves.  This Library General Public License is intended to
Xpermit developers of non-free programs to use free libraries, while
Xpreserving your freedom as a user of such programs to change the free
Xlibraries that are incorporated in them.  (We have not seen how to achieve
Xthis as regards changes in header files, but we have achieved it as regards
Xchanges in the actual functions of the Library.)  The hope is that this
Xwill lead to faster development of free libraries.
X
X  The precise terms and conditions for copying, distribution and
Xmodification follow.  Pay close attention to the difference between a
X"work based on the library" and a "work that uses the library".  The
Xformer contains code derived from the library, while the latter only
Xworks together with the library.
X
X  Note that it is possible for a library to be covered by the ordinary
XGeneral Public License rather than by this special one.
X
X
X    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
X
X
X
X  1. This License Agreement applies to any software library which
X     contains a notice placed by the copyright holder or other authorized
X     party saying it may be distributed under the terms of this Library
X     General Public License (also called "this License").  Each licensee is
X     addressed as "you".
X     
X       A "library" means a collection of software functions and/or data
X     prepared so as to be conveniently linked with application programs
X     (which use some of those functions and data) to form executables.
X     
X       The "Library", below, refers to any such software library or work
X     which has been distributed under these terms.  A "work based on the
X     Library" means either the Library or any derivative work under
X     copyright law: that is to say, a work containing the Library or a
X     portion of it, either verbatim or with modifications and/or translated
X     straightforwardly into another language.  (Hereinafter, translation is
X     included without limitation in the term "modification".)
X     
X       "Source code" for a work means the preferred form of the work for
X     making modifications to it.  For a library, complete source code means
X     all the source code for all modules it contains, plus any associated
X     interface definition files, plus the scripts used to control compilation
X     and installation of the library.
X     
X       Activities other than copying, distribution and modification are not
X     covered by this License; they are outside its scope.  The act of
X     running a program using the Library is not restricted, and output from
X     such a program is covered only if its contents constitute a work based
X     on the Library (independent of the use of the Library in a tool for
X     writing it).  Whether that is true depends on what the Library does
X     and what the program that uses the Library does.
X       
X  2. You may copy and distribute verbatim copies of the Library's
X     complete source code as you receive it, in any medium, provided that
X     you conspicuously and appropriately publish on each copy an
X     appropriate copyright notice and disclaimer of warranty; keep intact
X     all the notices that refer to this License and to the absence of any
X     warranty; and distribute a copy of this License along with the
X     Library.
X     
X       You may charge a fee for the physical act of transferring a copy,
X     and you may at your option offer warranty protection in exchange for a
X     fee.
X     
X  3. You may modify your copy or copies of the Library or any portion
X     of it, thus forming a work based on the Library, and copy and
X     distribute such modifications or work under the terms of Section 1
X     above, provided that you also meet all of these conditions:
X     
X       1. The modified work must itself be a software library.
X          
X       2. You must cause the files modified to carry prominent notices
X          stating that you changed the files and the date of any change.
X          
X       3. You must cause the whole of the work to be licensed at no
X          charge to all third parties under the terms of this License.
X          
X       4. If a facility in the modified Library refers to a function or a
X          table of data to be supplied by an application program that uses
X          the facility, other than as an argument passed when the facility
X          is invoked, then you must make a good faith effort to ensure that,
X          in the event an application does not supply such function or
X          table, the facility still operates, and performs whatever part of
X          its purpose remains meaningful.
X          
X          (For example, a function in a library to compute square roots has
X          a purpose that is entirely well-defined independent of the
X          application.  Therefore, Subsection 2d requires that any
X          application-supplied function or table used by this function must
X          be optional: if the application does not supply it, the square
X          root function must still compute square roots.)
X     
X     
X     These requirements apply to the modified work as a whole.  If
X     identifiable sections of that work are not derived from the Library,
X     and can be reasonably considered independent and separate works in
X     themselves, then this License, and its terms, do not apply to those
X     sections when you distribute them as separate works.  But when you
X     distribute the same sections as part of a whole which is a work based
X     on the Library, the distribution of the whole must be on the terms of
X     this License, whose permissions for other licensees extend to the
X     entire whole, and thus to each and every part regardless of who wrote
X     it.
X     
X     Thus, it is not the intent of this section to claim rights or contest
X     your rights to work written entirely by you; rather, the intent is to
X     exercise the right to control the distribution of derivative or
X     collective works based on the Library.
X     
X     In addition, mere aggregation of another work not based on the Library
X     with the Library (or with a work based on the Library) on a volume of
X     a storage or distribution medium does not bring the other work under
X     the scope of this License.
X     
X  4. You may opt to apply the terms of the ordinary GNU General Public
X     License instead of this License to a given copy of the Library.  To do
X     this, you must alter all the notices that refer to this License, so
X     that they refer to the ordinary GNU General Public License, version 2,
X     instead of to this License.  (If a newer version than version 2 of the
X     ordinary GNU General Public License has appeared, then you can specify
X     that version instead if you wish.)  Do not make any other change in
X     these notices.
X     
X       Once this change is made in a given copy, it is irreversible for
X     that copy, so the ordinary GNU General Public License applies to all
X     subsequent copies and derivative works made from that copy.
X     
X       This option is useful when you wish to copy part of the code of
X     the Library into a program that is not a library.
X     
X  5. You may copy and distribute the Library (or a portion or
X     derivative of it, under Section 2) in object code or executable form
X     under the terms of Sections 1 and 2 above provided that you accompany
X     it with the complete corresponding machine-readable source code, which
X     must be distributed under the terms of Sections 1 and 2 above on a
X     medium customarily used for software interchange.
X     
X       If distribution of object code is made by offering access to copy
X     from a designated place, then offering equivalent access to copy the
X     source code from the same place satisfies the requirement to
X     distribute the source code, even though third parties are not
X     compelled to copy the source along with the object code.
X     
X  6. A program that contains no derivative of any portion of the
X     Library, but is designed to work with the Library by being compiled or
X     linked with it, is called a "work that uses the Library".  Such a
X     work, in isolation, is not a derivative work of the Library, and
X     therefore falls outside the scope of this License.
X     
X       However, linking a "work that uses the Library" with the Library
X     creates an executable that is a derivative of the Library (because it
X     contains portions of the Library), rather than a "work that uses the
X     library".  The executable is therefore covered by this License.
X     Section 6 states terms for distribution of such executables.
X     
X       When a "work that uses the Library" uses material from a header file
X     that is part of the Library, the object code for the work may be a
X     derivative work of the Library even though the source code is not.
X     Whether this is true is especially significant if the work can be
X     linked without the Library, or if the work is itself a library.  The
X     threshold for this to be true is not precisely defined by law.
X     
X       If such an object file uses only numerical parameters, data
X     structure layouts and accessors, and small macros and small inline
X     functions (ten lines or less in length), then the use of the object
X     file is unrestricted, regardless of whether it is legally a derivative
X     work.  (Executables containing this object code plus portions of the
X     Library will still fall under Section 6.)
X     
X       Otherwise, if the work is a derivative of the Library, you may
X     distribute the object code for the work under the terms of Section 6.
X     Any executables containing that work also fall under Section 6,
X     whether or not they are linked directly with the Library itself.
X     
X  7. As an exception to the Sections above, you may also compile or
X     link a "work that uses the Library" with the Library to produce a
X     work containing portions of the Library, and distribute that work
X     under terms of your choice, provided that the terms permit
X     modification of the work for the customer's own use and reverse
X     engineering for debugging such modifications.
X     
X       You must give prominent notice with each copy of the work that the
X     Library is used in it and that the Library and its use are covered by
X     this License.  You must supply a copy of this License.  If the work
X     during execution displays copyright notices, you must include the
X     copyright notice for the Library among them, as well as a reference
X     directing the user to the copy of this License.  Also, you must do one
X     of these things:
X     
X       1. Accompany the work with the complete corresponding
X          machine-readable source code for the Library including whatever
X          changes were used in the work (which must be distributed under
X          Sections 1 and 2 above); and, if the work is an executable linked
X          with the Library, with the complete machine-readable "work that
X          uses the Library", as object code and/or source code, so that the
X          user can modify the Library and then relink to produce a modified
X          executable containing the modified Library.  (It is understood
X          that the user who changes the contents of definitions files in the
X          Library will not necessarily be able to recompile the application
X          to use the modified definitions.)
X          
X       2. Accompany the work with a written offer, valid for at
X          least three years, to give the same user the materials
X          specified in Subsection 6a, above, for a charge no more
X          than the cost of performing this distribution.
X          
X       3. If distribution of the work is made by offering access to copy
X          from a designated place, offer equivalent access to copy the above
X          specified materials from the same place.
X          
X       4. Verify that the user has already received a copy of these
X          materials or that you have already sent this user a copy.
X     
X     
X       For an executable, the required form of the "work that uses the
X     Library" must include any data and utility programs needed for
X     reproducing the executable from it.  However, as a special exception,
X     the source code distributed need not include anything that is normally
X     distributed (in either source or binary form) with the major
X     components (compiler, kernel, and so on) of the operating system on
X     which the executable runs, unless that component itself accompanies
X     the executable.
X     
X       It may happen that this requirement contradicts the license
X     restrictions of other proprietary libraries that do not normally
X     accompany the operating system.  Such a contradiction means you cannot
X     use both them and the Library together in an executable that you
X     distribute.
X     
X  8. You may place library facilities that are a work based on the
X     Library side-by-side in a single library together with other library
X     facilities not covered by this License, and distribute such a combined
X     library, provided that the separate distribution of the work based on
X     the Library and of the other library facilities is otherwise
X     permitted, and provided that you do these two things:
X     
X       1. Accompany the combined library with a copy of the same work
X          based on the Library, uncombined with any other library
X          facilities.  This must be distributed under the terms of the
X          Sections above.
X          
X       2. Give prominent notice with the combined library of the fact
X          that part of it is a work based on the Library, and explaining
X          where to find the accompanying uncombined form of the same work.
X     
X     
X  9. You may not copy, modify, sublicense, link with, or distribute
X     the Library except as expressly provided under this License.  Any
X     attempt otherwise to copy, modify, sublicense, link with, or
X     distribute the Library is void, and will automatically terminate your
X     rights under this License.  However, parties who have received copies,
X     or rights, from you under this License will not have their licenses
X     terminated so long as such parties remain in full compliance.
X     
X 10. You are not required to accept this License, since you have not
X     signed it.  However, nothing else grants you permission to modify or
X     distribute the Library or its derivative works.  These actions are
X     prohibited by law if you do not accept this License.  Therefore, by
X     modifying or distributing the Library (or any work based on the
X     Library), you indicate your acceptance of this License to do so, and
X     all its terms and conditions for copying, distributing or modifying
X     the Library or works based on it.
X     
X 11. Each time you redistribute the Library (or any work based on the
X     Library), the recipient automatically receives a license from the
X     original licensor to copy, distribute, link with or modify the Library
X     subject to these terms and conditions.  You may not impose any further
X     restrictions on the recipients' exercise of the rights granted herein.
X     You are not responsible for enforcing compliance by third parties to
X     this License.
X     
X 12. If, as a consequence of a court judgment or allegation of patent
X     infringement or for any other reason (not limited to patent issues),
X     conditions are imposed on you (whether by court order, agreement or
X     otherwise) that contradict the conditions of this License, they do not
X     excuse you from the conditions of this License.  If you cannot
X     distribute so as to satisfy simultaneously your obligations under this
X     License and any other pertinent obligations, then as a consequence you
X     may not distribute the Library at all.  For example, if a patent
X     license would not permit royalty-free redistribution of the Library by
X     all those who receive copies directly or indirectly through you, then
X     the only way you could satisfy both it and this License would be to
X     refrain entirely from distribution of the Library.
X     
X     If any portion of this section is held invalid or unenforceable under any
X     particular circumstance, the balance of the section is intended to apply,
X     and the section as a whole is intended to apply in other circumstances.
X     
X     It is not the purpose of this section to induce you to infringe any
X     patents or other property right claims or to contest validity of any
X     such claims; this section has the sole purpose of protecting the
X     integrity of the free software distribution system which is
X     implemented by public license practices.  Many people have made
X     generous contributions to the wide range of software distributed
X     through that system in reliance on consistent application of that
X     system; it is up to the author/donor to decide if he or she is willing
X     to distribute software through any other system and a licensee cannot
X     impose that choice.
X     
X     This section is intended to make thoroughly clear what is believed to
X     be a consequence of the rest of this License.
X     
X 13. If the distribution and/or use of the Library is restricted in
X     certain countries either by patents or by copyrighted interfaces, the
X     original copyright holder who places the Library under this License may add
X     an explicit geographical distribution limitation excluding those countries,
X     so that distribution is permitted only in or among countries not thus
X     excluded.  In such case, this License incorporates the limitation as if
X     written in the body of this License.
X     
X 14. The Free Software Foundation may publish revised and/or new
X     versions of the Library General Public License from time to time.
X     Such new versions will be similar in spirit to the present version,
X     but may differ in detail to address new problems or concerns.
X     
X     Each version is given a distinguishing version number.  If the Library
X     specifies a version number of this License which applies to it and
X     "any later version", you have the option of following the terms and
X     conditions either of that version or of any later version published by
X     the Free Software Foundation.  If the Library does not specify a
X     license version number, you may choose any version ever published by
X     the Free Software Foundation.
X     
X 15. If you wish to incorporate parts of the Library into other free
X     programs whose distribution conditions are incompatible with these,
X     write to the author to ask for permission.  For software which is
X     copyrighted by the Free Software Foundation, write to the Free
X     Software Foundation; we sometimes make exceptions for this.  Our
X     decision will be guided by the two goals of preserving the free status
X     of all derivatives of our free software and of promoting the sharing
X     and reuse of software generally.
X     
X     
X                                 NO WARRANTY
X     
X     
X     
X 16. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
X     WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
X     EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
X     OTHER PARTIES PROVIDE THE LIBRARY "AS IS" WITHOUT WARRANTY OF ANY
X     KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
X     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
X     PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
X     LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
X     THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.
X     
X 17. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
X     WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
X     AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
X     FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
X     CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
X     LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
X     RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
X     FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
X     SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
X     DAMAGES.
X
X
X
X                      END OF TERMS AND CONDITIONS
X
X
X
X
X
XFile: bbdb-filters.info  Node: How to Apply These Terms to Your New Libraries, Prev: END OF TERMS AND CONDITIONS, Up: GNU LIBRARY GENERAL PUBLIC LICENSE
X
XHow to Apply These Terms to Your New Libraries
X==============================================
X
X
X  If you develop a new library, and you want it to be of the greatest
Xpossible use to the public, we recommend making it free software that
Xeveryone can redistribute and change.  You can do so by permitting
Xredistribution under these terms (or, alternatively, under the terms of the
Xordinary General Public License).
X
X  To apply these terms, attach the following notices to the library.  It is
Xsafest to attach them to the start of each source file to most effectively
Xconvey the exclusion of warranty; and each file should have at least the
X"copyright" line and a pointer to where the full notice is found.
X
X     
X     ONE LINE TO GIVE THE LIBRARY'S NAME AND AN IDEA OF WHAT IT DOES.
X     Copyright (C) YEAR  NAME OF AUTHOR
X     
X     This library is free software; you can redistribute it and/or
X     modify it under the terms of the GNU Library General Public
X     License as published by the Free Software Foundation; either
X     version 2 of the License, or (at your option) any later version.
X     
X     This library is distributed in the hope that it will be useful,
X     but WITHOUT ANY WARRANTY; without even the implied warranty of
X     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
X     Library General Public License for more details.
X     
X     You should have received a copy of the GNU Library General Public
X     License along with this library; if not, write to the
X     Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
X     MA 02139, USA.
X
X
XAlso add information on how to contact you by electronic and paper mail.
X
XYou should also get your employer (if you work as a programmer) or your
Xschool, if any, to sign a "copyright disclaimer" for the library, if
Xnecessary.  Here is a sample; alter the names:
X
X     
X     Yoyodyne, Inc., hereby disclaims all copyright interest in
X     the library `Frob' (a library for tweaking knobs) written
X     by James Random Hacker.
X     
X     SIGNATURE OF TY COON, 1 April 1990
X     Ty Coon, President of Vice
X
X
XThat's all there is to it!
X
X
X
X
XFile: bbdb-filters.info  Node: Concept Index, Prev: GNU LIBRARY GENERAL PUBLIC LICENSE, Up: Top, Next: Command Index
X
XConcept Index
X*************
X
X
X
X* Menu:
X
X* About This Manual: About This Manual.
X* About This Package: About This Package.
X* .CDF file, HP 200LX Phone Book: HP 200LX Phone Book.
X* Credits: Credits.
X* Emacs Lisp Export: Emacs Lisp Export.
X* General Facilities for Input Filtering: General Facilities for Input Filtering.
X* GNU LIBRARY GENERAL PUBLIC LICENSE: GNU LIBRARY GENERAL PUBLIC LICENSE.
X* How to Apply These Terms to Your New Libraries: How to Apply These Terms to Your New Libraries.
X* HP 200LX Connectivity Pack: HP 200LX Phone Book.
X* HP 200LX Phone Book: HP 200LX Phone Book.
X* Input Filters: Input Filters.
X* Introduction: Introduction.
X* Lotus cc:Mail Nicknames: Lotus cc:Mail Nicknames.
X* Miscellany: Miscellany.
X* Output Filters: Output Filters.
X* PC Eudora Nickname Database: PC Eudora Nickname Database.
X* PC Eudora: PC Eudora.
X* PC Eudora Recipient Database: PC Eudora Recipient Database.
X* .PDF file, HP 200LX Phone Book: HP 200LX Phone Book.
X* PH: PH.
X* Preamble: Preamble.
X* Sending BBDB records via email: Emacs Lisp Export.
X* TODO List: TODO List.
X* UNIX Password Files: UNIX Password Files.
X
X
X
X
XFile: bbdb-filters.info  Node: Command Index, Prev: Concept Index, Up: Top
X
XCommand Index
X*************
X
X
X
X* Menu:
X
X* bbdb-ccmail-output: Lotus cc:Mail Nicknames.
X* bbdb-eudora-nndbase-output: PC Eudora Nickname Database.
X* bbdb-eudora-rcpdbase-output: PC Eudora Recipient Database.
X* bbdb-export: Emacs Lisp Export.
X* bbdb-hp200lx-output: HP 200LX Phone Book.
X* bbdb-passwd-input: UNIX Password Files.
X* bbdb-ph-output: PH.
X* bif-create-record: General Facilities for Input Filtering.
X
X
END_OF_FILE
echo shar: 23 control characters may be missing from \"'doc/formatted/bbdb-filters.info'\"
if test 44122 -ne `wc -c <'doc/formatted/bbdb-filters.info'`; then
    echo shar: \"'doc/formatted/bbdb-filters.info'\" unpacked with wrong size!
fi
# end of 'doc/formatted/bbdb-filters.info'
fi
echo shar: End of shell archive.
exit 0

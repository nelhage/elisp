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
;;; This is bbdb-export.el
;;;

(defvar bbdb-export-buffer-name "*BBDB* Export"
  "*Default buffer name for exporting the contents of the *BBDB* buffer.")


(defvar bbdb-export-compactly nil 
  "If nil, the exported records are compactly printed.  
Otherwise the exported forms are indented for human-readability (at a
cost of somewhat longer processing time for exporting records.  
The default value is nil.")


(defun bbdb-export ()
  "Print the selected BBDB entries"
  (interactive)
  (save-excursion
    (let ((to-buffer (get-buffer-create bbdb-export-buffer-name))
	  (records (progn (set-buffer bbdb-buffer-name)
			  bbdb-records))
	  (current-letter ""))
      ;; wipe to-buffer
      (switch-to-buffer to-buffer)
      (delete-region (point-min) (point-max))

      ;; insert header, records, trailer
      (bexp-buffer-insert-header)
      (while records
	(setq current-letter (bexp-do-record (car (car records)) current-letter))
	(setq records (cdr records)))
      (bexp-buffer-insert-trailer)
      
      (goto-char (point-min))
      (search-forward "(progn")
      (search-backward "(progn")
      (indent-sexp)
      ))
  (message "BBDB export buffer %s generated." bbdb-export-buffer-name))


(defun bexp-do-record (record current-letter)
  "Insert the bbdb RECORD in export format."
  (let* ((name   (bbdb-record-name record))
	 (comp   (bbdb-record-company record))
	 (net    (bbdb-record-net record))
	 (phones (bbdb-record-phones record))
	 (addrs  (bbdb-record-addresses record))
	 (notes  (bbdb-record-raw-notes record))
	 (first-letter (upcase (substring (concat (bbdb-record-sortkey record) "?") 0 1))))

    (if (not (string-equal first-letter current-letter))
	(progn (message "Now processing \"%s\" entries..." first-letter)
	       (sleep-for 1)))
    (bexp-buffer-insert-record name comp net addrs phones notes)
    first-letter))


(defun bexp-buffer-insert-header()
  (insert ";;; ======= Start of Exported BBDB Records =======\n")
  (insert "(progn  
(require 'bbdb-com)
(defun bbdb-maybe-create (name company net &optional addrs phones notes)
  \"Try to add a record to BBDB if it does not already exist.\"
  (condition-case err
      (progn
	(bbdb-create-internal name company net addrs phones notes)
	(message \"%s %s added.\" name (if net (concat \"<\" net \">\") \"\"))
	(sleep-for 1))    
    (error (ding)
	   (message \"%s %s skipped. (%s)\"
		    name
		    (if net (concat \"<\" net \">\") \"\")
		    (car (cdr err)))
	   (sleep-for 1))))\n\n")
  (normal-mode))


(defun bexp-buffer-insert-trailer()
  (insert ")\n")
  (insert ";;; ======= End of Exported BBDB Records =======\n"))


(defun bexp-buffer-insert-record (name comp net addrs phones notes)
  (let ((begin (point))
	end)
    (message "Exporting %s" name)
    (insert (format "(bbdb-maybe-create %s %s '%s '%s '%s '%s)\n"
		    (prin1-to-string (concat name "--IMPORTED"))
		    (prin1-to-string comp)
		    (prin1-to-string net)
		    (prin1-to-string addrs)
		    (prin1-to-string phones)
		    (prin1-to-string notes)
		    ))
    (setq end (point))
    (if (not bbdb-export-compactly) 
	(progn
	  ;; format region
	  (narrow-to-region begin end)
	  (goto-char begin)
	  (replace-string " '(" "\n'(")
	  (goto-char begin)
	  (replace-string "\" \"" "\"\n\"")
	  (goto-char begin)
	  (replace-string "((" "(\n(")
	  (goto-char begin)
	  (replace-string "))" ")\n)")
	  (goto-char begin)
	  (replace-string "([" "(\n[")
	  (goto-char begin)
	  (replace-string "])" "]\n)")
	  (goto-char begin)
	  (replace-string ") (" ")\n(")
	  (goto-char begin)
	  (replace-string "] [" "]\n[")
	  (goto-char (point-max))
	  (lisp-indent-region begin (point))
	  (widen)))
    ))

(provide 'bbdb-export)

;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993, 1995 Jamie Zawinski <jwz@lucid.com>.
;;; Converting a BBDB database to a Netscape Address Book.
;;; last change21-feb-97.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This file attempts to convert a BBDB database to a Netscape Address Book
;;; file.  It doesn't work very well.  If you fix it, let me know.  -- jwz

(require 'bbdb)

(defun bbdb-mozilla-insert-url (string)
  (let ((p (point))
	c)
    (insert string)
    (goto-char (prog1 p (setq p (point))))
    (while (progn
	     (skip-chars-forward "-a-zA-Z0-9.@/_\r\n" p)
	     (< (point) p))
      (setq c (following-char))
      (delete-char 1)
      (insert (format "%%%02X" c))
      (setq p (+ 2 p)))
    (goto-char p)))

(defun bbdb-mozilla-insert-html (string)
  (let ((p (point))
	c)
    (insert string)
    (goto-char (prog1 p (setq p (point))))
    (while (progn
	     (skip-chars-forward "^&<>" p)
	     (< (point) p))
      (setq c (following-char))
      (delete-char 1)
      (cond ((= c ?&) (insert "&amp;") (setq p (+ p 4)))
	    ((= c ?<) (insert "&lt;") (setq p (+ p 3)))
	    (t (insert "&gt;") (setq p (+ p 3)))))
    (goto-char p)))

(defun bbdb-mozilla-emit-record (record aliases)
  (let (addr)
    (cond ((setq addr (car (bbdb-record-net record)))
	   (insert "    <DT><A HREF=\"mailto:")
	   (bbdb-mozilla-insert-url addr)
	   (insert "\"")
	   (let ((nick nil))
	     (cond (nick
		    (insert " NICKNAME=\"")
		    (bbdb-mozilla-insert-html nick)
		    (insert "\"")))
	     (insert ">"))
	   (let ((name (or (bbdb-record-name record)
			   (bbdb-record-company record)
			   "")))
	     (bbdb-mozilla-insert-html name))
	   (insert "</A>\n")
	   (let ((notes nil))
	     (cond (notes
		    )))
	   t)
	  (t nil))))

(defun bbdb-to-netscape ()
  (let* ((target (cons bbdb-define-all-aliases-field
		       "^[a-z, ]+$"))
	 (records1 (bbdb-search (bbdb-records)
				nil			; name
				nil			; company
				nil ;"netscape\\.com"	; net
				target			; notes
				))
	 (records records1)
	 result record aliases match
	 (lists nil)
	 (single-aliases nil)
	 (count 0)
	 )
    (message "%d" (length records1))
    (while records
      (setq record (car records))
      (setq aliases (bbdb-record-getprop record bbdb-define-all-aliases-field))
      (setq aliases (and aliases (bbdb-split aliases ",")))
      (while aliases
	(if (setq match (assoc (car aliases) result))
	    (nconc match (cons record nil))
	  (setq result (cons (list (car aliases) record) result)))
	(setq aliases (cdr aliases)))
      (setq records (cdr records)))
    (while result
      (let ((alias (downcase (car (car result))))
	    (expansion (cdr (car result))))
	(cond
	 ((cdr expansion)
	  (setq lists (cons (cons alias expansion) lists)))
	 (expansion
	  (setq single-aliases (cons (cons (car expansion) alias)
				     single-aliases))))
	(setq result (cdr result))))

;    (setq records (bbdb-records))
    (setq records records1)
    (set-buffer (get-buffer-create "*netscape-address-book*"))
    (erase-buffer)
    (insert "<!DOCTYPE NETSCAPE-Addressbook-file-1>\n"
	    "<!-- This is an automatically generated file.\n"
	    "It will be read and overwritten.\n"
	    "Do Not Edit! -->\n"
	    "<TITLE>" (user-full-name) "'s Address book</TITLE>\n"
	    "<H1>" (user-full-name) "'s Address book</H1>\n"
	    "\n"
	    "<DL><p>\n")
    (while records
      (setq record (car records))
      (insert "    <DT><A HREF=\"mailto:")
      (let ((net (car (bbdb-record-net record))))
	(if net (insert net))
	(insert "\" ALIASID=\"")
	(prin1 count (current-buffer))
	(insert "\"")
	(message "%d..." count)
	(setq count (1+ count))
	(cond ((setq match (cdr (assq record single-aliases)))
	       (insert " NICKNAME=\"")
	       (princ match (current-buffer))
	       (insert "\"")))
	(insert ">")
	(insert (or (bbdb-record-name record)
		    net
		    (bbdb-record-company record)
		    "")))

      (insert "</A>\n")
      (let ((phones (bbdb-record-phones record))
	    (addrs (bbdb-record-addresses record))
	    (aka (bbdb-record-aka record))
	    phone
	    )

	(insert "<DD>")
	(setq match nil)
	(while phones
	  (setq phone (car phones))
	  (setq match t)
	  (insert (format " %14s: " (bbdb-phone-location phone)))
	  (insert (bbdb-phone-string phone) "\n<BR>")
	  (setq phones (cdr phones)))
	(let (addr c s)
	  (while addrs
	    (setq addr (car addrs))
	    (setq match t)
	    (insert (format " %14s: " (bbdb-address-location addr)))
	    (if (= 0 (length (setq s (bbdb-address-street1 addr)))) nil
	      (indent-to 17) (insert s "\n<BR>"))
	    (if (= 0 (length (setq s (bbdb-address-street2 addr)))) nil
	      (indent-to 17) (insert s "\n<BR>"))
	    (if (= 0 (length (setq s (bbdb-address-street3 addr)))) nil
	      (indent-to 17) (insert s "\n<BR>"))
	    (indent-to 17)
	    (insert (setq c (bbdb-address-city addr)))
	    (setq s (bbdb-address-state addr))
	    (if (and (> (length c) 0) (> (length s) 0)) (insert ", "))
	    (insert s "  ")
	    (insert (bbdb-address-zip-string addr) "\n<BR>")
	    (setq addrs (cdr addrs))))
	(cond (aka
	       (setq match t)
	       (insert (format " %14s: %s\n<BR>" "AKA"
			       (mapconcat (function identity) aka ", ")))))
	(let ((notes (bbdb-record-raw-notes record)))
	  (if (stringp notes)
	      (setq notes (list (cons 'notes notes))))
	  (while notes
	    (if (memq (car (car notes))
		      '(mail-alias password bbdb mail-name face mark-char aka))
		nil
	      (setq match t)
	      (insert (format " %14s: " (car (car notes))))
	      (let ((p (point)))
		(insert (cdr (car notes)))
		(save-excursion
		  (save-restriction
		    (narrow-to-region p (1- (point)))
		    (goto-char (1+ p))
		    (while (search-forward "\n" nil t)
		      (forward-char -1)
		      (insert "<BR>")
		      (forward-char 1)
		      (insert (make-string 17 ?\ )))))
		(insert "\n")))
	    (setq notes (cdr notes)))))

      (or match (delete-char -4))

      (setq records (cdr records))
      )
    (insert "</DL><p>\n")
    ))

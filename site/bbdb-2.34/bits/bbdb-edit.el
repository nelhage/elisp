;;; bbdb-edit.el --- BBDB field edit
;; Copyright (C) 1999, 2000, 2001 Shenghuo ZHU

;; Author: Shenghuo ZHU <zsh@cs.rochester.edu>
;; Created: Fri Aug 27 17:45:25 EDT 1999
;; Keywords: BBDB field edit

;; This file is not a part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; bbdb-field-edit-add (`insert') and bbdb-field-edit-del (`delete')
;; add/del a item to/from a certain field of the bbdb record.  These
;; keys also support `*'.

;;; Code:

(require 'bbdb)

(defun bbdb-field-edit-get-values (record field)
  (cond
   ((eq field 'net) (bbdb-record-net record))
   ((eq field 'AKA) (bbdb-record-aka record))
   ((eq field 'address) (bbdb-record-addresses record))
   ((eq field 'phone) (bbdb-record-phones record))
   (t (bbdb-split (or (bbdb-record-getprop record field) "")
		  (or (get field 'field-separator)
		      bbdb-notes-default-separator)))))

(defun bbdb-field-edit-put-values (record field values)
  (if values
      (cond
       ((eq field 'net) (bbdb-record-set-net record values))
       ((eq field 'AKA) (bbdb-record-set-aka record values)) 
       ((eq field 'address) (bbdb-record-set-addresses record values))
       ((eq field 'phone) (bbdb-record-set-phones record values))
       (t (bbdb-record-putprop record field 
			       (bbdb-join values
					  (or (get field 'field-separator)
					      bbdb-notes-default-separator)))))
    (if (memq field '(net AKA address))
	(bbdb-record-store-field-internal record field nil)
      (bbdb-record-putprop record field nil)))
  (bbdb-change-record record t)
  (bbdb-redisplay-one-record record))

;;;###autoload
(defun bbdb-field-edit-add (bbdb-record field value)
  "Add VALUE to FIELD of bbdb-record(s)."
    (interactive (list (if (bbdb-do-all-records-p)
			 (mapcar 'car bbdb-records)
		       (list (bbdb-current-record)))
		     (completing-read 
		      "Field: "
		      (append '(("net")("notes")("AKA"))
			      (bbdb-propnames))
		      nil nil 
		      (symbol-name
		       (let ((on-field (bbdb-current-field t)))
			 (cond ((null on-field) 'mail-alias)
			       ((eq (car on-field) 'property) 
				 (car (nth 1 on-field)))
			       (t  (car on-field))))))
		     (bbdb-read-string "Value: ")))
  (if (stringp field) (setq field (intern field)))
  (if (memq field '(name address phone))
      (error "Use `e' to edit this field."))
  (while bbdb-record
    (let ((values (bbdb-field-edit-get-values (car bbdb-record) field)))
      (if (member value values) nil
	(bbdb-field-edit-put-values (car bbdb-record) field 
				    (cons value values))))
    (setq bbdb-record (cdr bbdb-record))))

;;;###autoload
(defun bbdb-field-edit-del (bbdb-record field value)
  "Delete VALUE to FIELD of bbdb-record(s).
If prefix arg exists, delete all existing field values matching VALUE(regexp)."
  (interactive (list (if (bbdb-do-all-records-p)
			 (mapcar 'car bbdb-records)
		       (list (bbdb-current-record)))
		     (completing-read 
		      "Field: "
		      (append '(("net")("notes")("AKA"))
			      (bbdb-propnames))
		      nil nil (symbol-name
			       (let ((on-field (bbdb-current-field t)))
				 (cond ((null on-field) 'mail-alias)
				       ((eq (car on-field) 'property) 
					(car (nth 1 on-field)))
				       (t (car on-field))))))
		     (bbdb-read-string (if current-prefix-arg
					   "Regexp: "
					   "Value: "))))
  (if (stringp field) (setq field (intern field)))
  (if (memq field '(name address phone))
      (error "Use `e' to edit this field."))
  (while bbdb-record
    (let ((values (bbdb-field-edit-get-values (car bbdb-record) field)))
      (cond
       (current-prefix-arg 
	(let (nvalues found)
	  (while values
	    (if (string-match value (car values))
		(setq found t)
	      (setq nvalues (cons (car values) nvalues)))
	    (setq values (cdr values)))
	  (if found
	      (bbdb-field-edit-put-values (car bbdb-record) field 
					  (nreverse nvalues)))))
       (t 
	(if (member value values)
	    (bbdb-field-edit-put-values (car bbdb-record) field 
					(delete value values))))))
    (setq bbdb-record (cdr bbdb-record))))

;;; The key binding might be moved to somewhere else.

(define-key bbdb-mode-map [(insert)] 'bbdb-field-edit-add)
(define-key bbdb-mode-map [(delete)] 'bbdb-field-edit-del)

(provide 'bbdb-edit)

;; bbdb-edit.el ends here

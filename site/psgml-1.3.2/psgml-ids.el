;;; psgml-ids.el --- Management of ID/IDREFS for PSGML
;; $Id: psgml-ids.el,v 2.1 2005/02/09 15:29:09 lenst Exp $

;; Copyright (C) 1999 Jean-Daniel Fekete

;; Author: Jean-Daniel Fekete <Jean-Daniel.Fekete@emn.fr>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides some extra functions to manage IDs and IDREFs in attibutes


(provide 'psgml-ids)
(require 'psgml)
(require 'psgml-api)


(defvar sgml-record-id-p t
  "Set to non-nil, if you want to record all referenced IDS for completion.")

(defvar sgml-id-list nil
  "List of IDs available for completing IDREFs")
;(make-variable-buffer-local 'sgml-id-list)

(defvar sgml-id-alist nil
  "Alist of IDs available for completing IDREFs")

(defvar sgml-id-list-sorted-p nil
  "Set to T when the sgml-id-list is sorted")

(defvar sgml-edit-idrefs-map
  (let ((map (make-sparse-keymap 'sgml-edit-idrefs-map)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " 'self-insert-command)
    map))


(defun sgml-id-list ()
  (unless sgml-id-list-sorted-p
    (setq sgml-id-list (sort sgml-id-list #'string-lessp)
	  sgml-id-list-sorted-p t
	  sgml-id-alist nil))
  sgml-id-list)

(defun sgml-id-alist ()
  (unless sgml-id-alist
    (setq sgml-id-alist (mapcar #'(lambda (id) (cons id id)) (sgml-id-list))))
  sgml-id-alist)

(defun sgml-add-id (id)
  (unless (or (not sgml-record-id-p) (member id sgml-id-list))
    (push id sgml-id-list)
    (setq sgml-id-list-sorted-p nil)))

(defun sgml-ids-add-from (element)
  "Find of all attributes of type ID in ELEMENT and add their value to the
sgml-id-list."
  (let ((asl (sgml-element-attribute-specification-list element))
	(adl (sgml-element-attlist element)))

    (dolist (as asl)
      (let* ((aname (sgml-attspec-name as))
	     (value (sgml-attspec-attval as))
	     (dcl-value (sgml-attdecl-declared-value
			 (sgml-lookup-attdecl aname adl))))
	(if (and (eq dcl-value 'ID)
		 value)
	    (sgml-add-id value))))))


(defun sgml-ids-add-current ()
  (interactive)
  (sgml-ids-add-from (sgml-find-context-of (point))))

(defun sgml-ids-add-all (&optional element)
  "Find all the ids of elements inside ELEMENT or the top element if not
specified"
  (interactive)
  (let ((el (or element (sgml-top-element))))
    (sgml-map-element-modify (function sgml-ids-add-from) el)))


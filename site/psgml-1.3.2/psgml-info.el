;;;; psgml-info.el
;;; Last edited: 2000-11-09 19:23:50 lenst
;;; $Id: psgml-info.el,v 2.16 2005/02/27 17:12:05 lenst Exp $

;; Copyright (C) 1994, 1995 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

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


;;;; Commentary:

;; This file is an addon to the PSGML package.  

;; This file contains some commands to print out information about the
;; current DTD.

;; sgml-list-elements
;;    Will list all elements and the attributes declared for the element.

;; sgml-list-attributes
;;    Will list all attributes declared and the elements that use them.

;; sgml-list-terminals
;;    Will list all elements that can contain data.

;; sgml-list-occur-in-elements
;;    Will list all element types and where it can occur.

;; sgml-list-content-elements
;;    Will list all element types and the element types that can occur
;;    in its content.

;;;; Code:

(provide 'psgml-info)
(require 'psgml)
(require 'psgml-parse)

(defconst sgml-attr-col 18)

(eval-when-compile (require 'cl))

;;;; Utility functions

(defsubst sgml-add-to-table (row-index elem table)
  (let ((p (assoc row-index table)))
    (cond ((null p)
	   (cons (list row-index elem) table))
	  (t
	   (nconc p (list elem))
	   table))))

(defsubst sgml-add-last-unique (x l)
  (unless (memq x l)
    (nconc l (list x))))

(defun sgml-map-element-types (func)
  (sgml-need-dtd)
  (sgml-map-eltypes func
		    (sgml-pstate-dtd sgml-buffer-parse-state)
		    t))


(defun sgml-set-difference (l1 l2)
  (if (or (null l1) (null l2))
      l1
    (let ((res nil))
      (while l1
	(or (member (car l1)
		   l2)
	    (push (car l1) res))
	(pop l1))
      res)))

(defun sgml-union (l1 l2)
  (cond ((null l1) l2) ((null l2) l1)
	((equal l1 l2) l1)
	(t
	 (or (>= (length l1) (length l2))
	     (setq l1 (prog1 l2 (setq l2 l1))))
	 (while l2
	   (or (member (car l2) l1)
	       (push (car l2) l1))
	   (pop l2))
	 l1)))

(defun sgml-eltype-refrenced-elements (eltype)
  "List of element types referenced in the model of ELTYPE."
  ;; Now with cache. Uses appdata prop re-cache.
  (or
   (sgml-eltype-appdata eltype 're-cache)
   (let* ((res				; result list (eltypes)
	   nil)
	  (states			; list of states
	   (list (sgml-eltype-model eltype)))
	  (agenda			; point into states
	   states))
     (cond
      ((not (sgml-model-group-p (car states)))
       nil)
      (t
       (while agenda
	 (cond
	  ((sgml-normal-state-p (car agenda))
	   (loop for m in (append (sgml-state-opts (car agenda))
				  (sgml-state-reqs (car agenda)))
		 do
		 (add-to-list 'res (sgml-move-token m))
		 (sgml-add-last-unique (sgml-move-dest m) states)))
       
	  (t				; &-node
	   (sgml-add-last-unique (sgml-and-node-next (car agenda)) states)
	   (loop for dfa in (sgml-and-node-dfas (car agenda)) do
		 (sgml-add-last-unique dfa states))))
	 (setq agenda (cdr agenda)))
       (setq res
             (sort (copy-sequence (sgml-set-difference
                                   (sgml-union res (sgml-eltype-includes eltype))
                                   (sgml-eltype-excludes eltype)))
                   (function string-lessp)))
       (setf (sgml-eltype-appdata eltype 're-cache) res)
       res)))))


;;;; List elements

(defun sgml-list-elements ()
  "List the elements and their attributes in the current DTD."
  (interactive)
  (message "Creating table...")
  (sgml-display-table
   (sgml-map-element-types
    (function
     (lambda (eltype)
       (cons (sgml-eltype-name eltype)
	     (mapcar (function sgml-attdecl-name)
		     (sgml-eltype-attlist eltype))))))
   "Elements" "Element" "Attribute"))


;;;; List attributes

(defun sgml-list-attributes ()
  "List the attributes and in which elements they occur."
  (interactive)
  (let ((attributes nil))
    (message "Creating table...")
    (sgml-map-element-types
     (function
      (lambda (eltype)
	(loop for a in (sgml-eltype-attlist eltype) do
	      (setq attributes
		    (sgml-add-to-table (sgml-attdecl-name a)
				       (sgml-eltype-name eltype)
				       attributes))))))
    (sgml-display-table attributes
			"Attributes" "Attribute" "Element")))




;;;; List terminals

(defun sgml-list-terminals ()
  "List the elements that can have data in their content."
  (interactive)
  (message "Creating table...")
  (let ((data-models (list sgml-cdata sgml-rcdata sgml-any)))
    (sgml-display-table
     (delq nil
	   (sgml-map-element-types
	    (function
	     (lambda (eltype)
	       (if (or (sgml-eltype-mixed eltype)
		       (memq (sgml-eltype-model eltype) data-models))
		   (list (sgml-eltype-name eltype)
			 (symbol-name
			  (if (sgml-model-group-p (sgml-eltype-model eltype))
			      'mixed
			    (sgml-eltype-model eltype)))))))))
     "Terminals" "Element" "Content")))


;;;; Element cross reference list

(defun sgml-list-content-elements ()
  "List all element types and the element types that can occur in its content."
  (interactive)
  (message "Creating table...")
  (sgml-display-table
   (sgml-map-element-types
    (function
     (lambda (eltype)
       (cons (sgml-eltype-name eltype)
	     (mapcar (function sgml-eltype-name)
		     (sgml-eltype-refrenced-elements eltype))))))
   "Elements referenced by elements"
   "Element" "Content"))

(defun sgml-list-occur-in-elements ()
  "List all element types and where it can occur."
  (interactive)
  (message "Creating table...")
  (let ((cross nil))
    (sgml-map-element-types
     (function
      (lambda (eltype)
	(loop for ref in (sgml-eltype-refrenced-elements eltype)
	      do (setq cross (sgml-add-to-table ref
						(sgml-eltype-name eltype)
						cross))))))
    (sgml-display-table
     cross
     "Cross referenced element types" "Element" "Can occur in")))


;;;; Display table

(defun sgml-display-table (table title col-title1 col-title2
				 &optional width nosort)
  (or width
      (setq width sgml-attr-col))
  (let ((buf (get-buffer-create (format "*%s*" title))))
    (message "Preparing display...")
    (set-buffer buf)
    (erase-buffer)
    (insert col-title1)
    (indent-to width)
    (insert col-title2 "\n")
    (insert-char ?= (length col-title1))
    (indent-to width)
    (insert-char ?= (length col-title2))
    (insert "\n")
    (unless nosort
      (setq table (sort table (function (lambda (a b)
					  (string< (car a) (car b)))))))
    (loop for e in table do
	  (insert (format "%s " (car e)))
	  (loop for name in (if nosort
				(cdr e)
			      (sort (cdr e) (function string-lessp)))
		do
		(when (> (+ (length name) (current-column))
			 fill-column)
		  (insert "\n"))
		(when (< (current-column) sgml-attr-col)
		  (indent-to width))
		(insert  name " "))
	  (insert "\n"))
    (goto-char (point-min))
    (display-buffer buf)
    (message nil)))


;;;; Describe entity

(defun sgml-describe-entity (name)
  "Describe the properties of an entity as declared in the current DTD."
  (interactive
   (let (default input)
     (sgml-need-dtd)
     (save-excursion
       (sgml-with-parser-syntax
	(unless (sgml-parse-delim "ERO")
	  (skip-chars-backward "^&\"'= \t\n"))
	(setq default (or (sgml-parse-name t) ""))))
     (setq input (completing-read
		  (format "Entity name (%s): " default)
		  (sgml-entity-completion-table
		   (sgml-dtd-entities
		    (sgml-pstate-dtd sgml-buffer-parse-state)))))
     (list
      (if (equal "" input) default input))))
  
  (with-output-to-temp-buffer "*Help*"
    (let ((entity (sgml-lookup-entity name
				      (sgml-dtd-entities
				       (sgml-pstate-dtd
					sgml-buffer-parse-state)))))
      (or entity (error "Undefined entity"))
      (princ (format "Entity %s is %s\n"
		     name
		     (cond ((null entity)
			    "undefined")
			   (t
			    (format "a %s entity"
				    (sgml-entity-type entity))))))
      (when entity
	(let ((text (sgml-entity-text entity))
	      (notation (sgml-entity-notation entity)))
	  (cond ((stringp text)
		 (princ "Defined to be:\n")
		 (princ text))
		(t
		 (princ "With external identifier ")
		 (princ (if (car text) "PUBLIC" "SYSTEM")) 
		 (when (car text)
		   (princ (format " '%s'" (car text))))
		 (when (cdr text)
		   (princ (format " '%s'" (cdr text))))
		 (when notation
		   (princ (format "\nand notation '%s'" notation))))))))))



;;;; Describe element type

(defun sgml-princ-names (names &optional first sep)
  (setq sep (or sep " "))
  (loop with col = 0
	for name in names
        for this-sep = (if first (prog1 first (setq first nil)) sep)
	do
        (princ this-sep)
	(incf col (length this-sep))
	(when (and (> col 0) (> (+ col (length name)) fill-column))
	  (princ "\n ")
	  (setq col 1))
        (princ name)
	(incf col (length name))))

(defun sgml-describe-element-type (et-name)
  "Describe the properties of an element type as declared in the current DTD."
  (interactive
   (let (default input)
     (sgml-need-dtd)
     (save-excursion
       (sgml-with-parser-syntax
	(unless (sgml-parse-delim "STAGO")
	  (skip-syntax-backward "w_"))
	(setq default (sgml-parse-name))
	(unless (and default
		     (sgml-eltype-defined (sgml-lookup-eltype default)))
	  (setq default nil))))
     (setq input (sgml-read-element-type (if default
					     (format "Element type (%s): "
						     default)
					   "Element type: ")
					 sgml-dtd-info
					 default))

     (list
      (sgml-eltype-name input))))

  (sgml-need-dtd)
  (let ((et (sgml-lookup-eltype et-name)))
    (with-output-to-temp-buffer "*Help*"
      (princ (format "ELEMENT: %s\n\n" (sgml-eltype-name et)))
      (princ (format " Start-tag is %s.\n End-tag is %s.\n"
		     (if (sgml-eltype-stag-optional et)
			 "optional" "required")
		     (if (sgml-eltype-etag-optional et)
			 "optional" "required")))
      (princ "\nATTRIBUTES:\n")
      (loop for attdecl in (sgml-eltype-attlist et) do
	    (let ((name (sgml-attdecl-name attdecl))
		  (dval (sgml-attdecl-declared-value attdecl))
		  (defl (sgml-attdecl-default-value attdecl)))
	      (when (listp dval)
		(setq dval (concat (if (eq (first dval)
					   'NOTATION)
				       "#NOTATION (" "(")
				   (mapconcat (function identity)
					      (second dval)
					      "|")
				   ")")))
	      (cond ((sgml-default-value-type-p 'FIXED defl)
		     (setq defl (format "#FIXED '%s'"
					(sgml-default-value-attval defl))))
		    ((symbolp defl)
		     (setq defl (upcase (format "#%s" defl))))
		    (t
		     (setq defl (format "'%s'"
					(sgml-default-value-attval defl)))))
	      (princ (format " %-9s %-30s %s\n" name dval defl))))
      ;; ----
      (let ((s (sgml-eltype-shortmap et)))
	(when s
	  (princ (format "\nUSEMAP: %s\n" s))))
      ;; ----
      (princ "\nCONTENT: ")
      (cond ((symbolp (sgml-eltype-model et)) (princ (sgml-eltype-model et)))
	    (t
	     (princ (if (sgml-eltype-mixed et) "mixed\n\n"
                      "element\n\n"))
	     (sgml-princ-names
	      (mapcar #'symbol-name (sgml-eltype-refrenced-elements et)))))
      (let ((incl (sgml-eltype-includes et))
            (excl (sgml-eltype-excludes et)))
        (when (or incl excl)
          (princ "\n\nEXCEPTIONS:"))
        (when incl
          (princ "\n + ")
          (sgml-princ-names (mapcar #'symbol-name incl)))
        (when excl
          (princ "\n - ")
          (sgml-princ-names (mapcar #'symbol-name excl))))
      ;; ----
      (princ "\n\nOCCURS IN:\n\n")
      (let ((occurs-in ()))
	(sgml-map-eltypes
	 (function (lambda (cand)
		     (when (memq et (sgml-eltype-refrenced-elements cand))
		       (push cand occurs-in))))
	 (sgml-pstate-dtd sgml-buffer-parse-state))
        (sgml-princ-names (mapcar 'sgml-eltype-name
                                  (sort occurs-in (function string-lessp))))))))


;;;; Print general info about the DTD.

(defun sgml-describe-dtd ()
  "Display information about the current DTD."
  (interactive)
  (sgml-need-dtd)
  (let ((elements 0)
	(entities 0)
	(parameters 0)
	(fmt "%20s %s\n")
	(hdr ""))

    (sgml-map-eltypes (function (lambda (e) (incf elements)))
		      sgml-dtd-info)
    (sgml-map-entities (function (lambda (e) (incf entities)))
		       (sgml-dtd-entities sgml-dtd-info))
    (sgml-map-entities (function (lambda (e) (incf parameters)))
		       (sgml-dtd-parameters sgml-dtd-info))

    (with-output-to-temp-buffer "*Help*"
      (princ (format fmt "Doctype:" (sgml-dtd-doctype sgml-dtd-info)))
      (when (sgml-dtd-merged sgml-dtd-info)
	(princ (format fmt "Compiled DTD:"
		       (car (sgml-dtd-merged sgml-dtd-info)))))
      (princ (format fmt "Element types:" (format "%d" elements)))
      (princ (format fmt "Entities:" (format "%d" entities)))
      (princ (format fmt "Parameter entities:" (format "%d" parameters)))

      (setq hdr "Files used:")
      (loop for x in (sgml-dtd-dependencies sgml-dtd-info)
	    if (stringp x)
	    do (princ (format fmt hdr x))
	    (setq hdr ""))

      (setq hdr "Undef parameters:")
      (sgml-map-entities
       (function (lambda (entity)
		   (when (sgml-entity-marked-undefined-p entity)
		     (princ (format fmt hdr (sgml-entity-name entity)))
		     (setq hdr ""))))
       (sgml-dtd-parameters sgml-dtd-info)))))


(defalias 'sgml-general-dtd-info 'sgml-describe-dtd)


;;; psgml-info.el ends here

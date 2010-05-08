;;; psgml-edit.el --- Editing commands for SGML-mode with parsing support
;;
;; $Id: psgml-edit.el,v 2.73 2005/03/02 19:46:31 lenst Exp $

;; Copyright (C) 1994, 1995, 1996 Lennart Staflin

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

;; Part of major mode for editing the SGML document-markup language.


;;;; Code:

(provide 'psgml-edit)
(require 'psgml)
(require 'psgml-parse)
(require 'psgml-ids)
(eval-when-compile (require 'cl))

;; (eval-when-compile
;;   (setq byte-compile-warnings '(free-vars unresolved callargs redefine)))


;;;; Variables

(defvar sgml-split-level nil
  "Used by sgml-split-element")


;;;; SGML mode: structure editing

(defun sgml-last-element ()
  "Return the element where last command left point.
This either uses the save value in `sgml-last-element' or parses the buffer
to find current open element."
  (setq sgml-markup-type nil)
  (if (and (not sgml-xml-p)
           (memq last-command sgml-users-of-last-element)
	   sgml-last-element)		; Don't return nil
      sgml-last-element
    (setq sgml-last-element (sgml-find-context-of (point))))  )

(defun sgml-set-last-element (&optional el)
  (if el (setq sgml-last-element el))
  (sgml-show-context sgml-last-element))

(defun sgml-beginning-of-element ()
  "Move to after the start-tag of the current element.
If the start-tag is implied, move to the start of the element."
  (interactive)
  (goto-char (sgml-element-stag-end (sgml-last-element)))
  (sgml-set-last-element (if (sgml-element-empty sgml-last-element)
			     (sgml-element-parent sgml-last-element))))

(defun sgml-end-of-element ()
  "Move to before the end-tag of the current element."
  (interactive)
  (goto-char (sgml-element-etag-start (sgml-last-element)))
  (sgml-set-last-element (if (sgml-element-empty sgml-last-element)
			     (sgml-element-parent sgml-last-element))))

(defun sgml-backward-up-element ()
  "Move backward out of this element level.
That is move to before the start-tag or where a start-tag is implied."
  (interactive)
  (goto-char (sgml-element-start (sgml-last-element)))
  (sgml-set-last-element (sgml-element-parent sgml-last-element)))

(defun sgml-up-element ()
  "Move forward out of this element level.
That is move to after the end-tag or where an end-tag is implied."
  (interactive)
  (goto-char (sgml-element-end (sgml-last-element)))
  (sgml-set-last-element (sgml-element-parent sgml-last-element)))

(defun sgml-forward-element ()
  "Move forward over next element."
  (interactive)
  (let ((next
	 (sgml-find-element-after (point) (sgml-last-element))))
    (goto-char (sgml-element-end next))
    (sgml-set-last-element (sgml-element-parent next))))

(defun sgml-backward-element ()
  "Move backward over previous element at this level.
With implied tags this is ambiguous."
  (interactive)
  (let ((prev				; previous element
	 (sgml-find-previous-element (point) (sgml-last-element))))
    (goto-char (sgml-element-start prev))
    (sgml-set-last-element (sgml-element-parent prev))))

(defun sgml-down-element ()
  "Move forward and down one level in the element structure."
  (interactive)
  (let ((to
	 (sgml-find-element-after (point) (sgml-last-element))))
    (when (sgml-strict-epos-p (sgml-element-stag-epos to))
      (error "Sub-element in other entity"))
    (goto-char (sgml-element-stag-end to))
    (sgml-set-last-element (if (sgml-element-empty to)
			       (sgml-element-parent to)
			     to))))

(defun sgml-kill-element ()
  "Kill the element following the cursor."
  (interactive "*")
  (sgml-parse-to-here)
  (when sgml-markup-type
    (error "Point is inside markup"))
  (kill-region (point)
	       (sgml-element-end (sgml-find-element-after (point)))))

(defun sgml-transpose-element ()
  "Interchange element before point with element after point, leave point after."
  (interactive "*")
  (let ((pre (sgml-find-previous-element (point)))
	(next (sgml-find-element-after (point)))
	s1 s2 m2)
    (goto-char (sgml-element-start next))
    (setq m2 (point-marker))
    (setq s2 (buffer-substring (point)
			       (sgml-element-end next)))
    (delete-region (point) (sgml-element-end next))
    (goto-char (sgml-element-start pre))
    (setq s1 (buffer-substring (point) (sgml-element-end pre)))
    (delete-region (point) (sgml-element-end pre))
    (insert-before-markers s2)
    (goto-char m2)
    (insert s1)
    (sgml-message "")))

(defun sgml-mark-element ()
  "Set mark after next element."
  (interactive)
  (push-mark (sgml-element-end (sgml-find-element-after (point))) nil t))

(defun sgml-mark-current-element ()
  "Set mark at end of current element, and leave point before current element."
  (interactive)
  (let ((el (sgml-find-element-of (point))))
    (goto-char (sgml-element-start el))
    (push-mark (sgml-element-end el) nil t)))


(defun sgml-change-element-name (gi)
  "Replace the name of the current element with a new name.
Eventual attributes of the current element will be translated if 
possible."
  (interactive
   (list (let ((el (sgml-find-element-of (point))))
	   (goto-char (sgml-element-start el))
	   (sgml-read-element-name
	    (format "Change %s to: " (sgml-element-name el))))))
  (when (or (null gi) (equal gi ""))
    (error "Illegal name"))
  (let* ((element (sgml-find-element-of (point)))
	 (attspec (sgml-element-attribute-specification-list element))
	 (oldattlist (sgml-element-attlist element))
         (tagc (if (and sgml-xml-p (sgml-element-empty element))
                (sgml-delim "XML-TAGCE")
              (sgml-delim "TAGC")))
         (tagc-len (length tagc)))
    (goto-char (sgml-element-end element))
    (unless  (sgml-element-empty element)
      (delete-char (- (sgml-element-etag-len element))))
    (insert (sgml-end-tag-of gi))
    (goto-char (sgml-element-start element))
    (delete-char (sgml-element-stag-len element))
    (insert (sgml-delim "STAGO")
            (sgml-general-insert-case gi)
            tagc)
    (let* ((newel (sgml-find-context-of (point)))
	   (newattlist (sgml-element-attlist newel))
	   (newasl (sgml-translate-attribute-specification-list
		    attspec oldattlist newattlist)))
      (backward-char tagc-len)
      (sgml-insert-attributes newasl newattlist)
      (forward-char tagc-len))))


(defun sgml-translate-attribute-specification-list (values from to)
  "Translate attribute specification from one element type to another.
Input attribute values in VALUES using attlist FROM is translated into
a list using attlist TO."
  (let ((new-values nil)
	(sgml-show-warnings t)
	tem)
    (loop for attspec in values 
	  as from-decl = (sgml-lookup-attdecl (sgml-attspec-name attspec) from)
	  as to-decl   = (sgml-lookup-attdecl (sgml-attspec-name attspec) to)
	  do
	  (cond
	   ;; Special case ID attribute
	   ((and (eq 'ID (sgml-attdecl-declared-value from-decl))
		 (setq tem (sgml-attribute-with-declared-value to 'ID)))
	    (push
	     (sgml-make-attspec (sgml-attdecl-name tem)
				(sgml-attspec-attval attspec))
	     new-values))
	   ;; Use attribute with same name if compatible type
	   ((equal (sgml-attdecl-declared-value from-decl)
		   (sgml-attdecl-declared-value to-decl))
	    (push attspec new-values))
	   (to-decl
	    (sgml-log-warning
	     "Attribute %s has new declared-value"
	     (sgml-attspec-name attspec))
	    (push attspec new-values))
	   (t
	    (sgml-log-warning "Can't translate attribute %s = %s"
			      (sgml-attspec-name attspec)
			      (sgml-attspec-attval attspec)))))
    new-values))

(defun sgml-untag-element ()
  "Remove tags from current element."
  (interactive "*")
  (let ((el (sgml-find-element-of (point))))
    (when (or (sgml-strict-epos-p (sgml-element-stag-epos el))
	      (sgml-strict-epos-p (sgml-element-etag-epos el)))
      (error "Current element has some tag inside an entity reference"))
    (goto-char (sgml-element-etag-start el))
    (delete-char (sgml-element-etag-len el))
    (goto-char (sgml-element-start el))
    (delete-char (sgml-element-stag-len el))))

(defun sgml-kill-markup ()
  "Kill next tag, markup declaration or process instruction."
  (interactive "*")
  (let ((start (point)))
    (sgml-with-parser-syntax
     (sgml-parse-s)
     (setq sgml-markup-start (point))
     (cond ((sgml-parse-markup-declaration 'ignore))
	   ((sgml-parse-processing-instruction))
	   ((sgml-skip-tag)))
     (kill-region start (point)))))


;;;; SGML mode: folding

(defun sgml-fold-region (beg end &optional unhide)
  "Hide (or if prefixarg unhide) region.
If called from a program first two arguments are start and end of
region. And optional third argument true unhides."
  (interactive "r\nP")
  (setq selective-display t)
  (let ((mp (buffer-modified-p))
	(inhibit-read-only t)
        (before-change-functions nil)
	(after-change-functions nil))
    (unwind-protect
        (subst-char-in-region beg end
                              (if unhide ?\r ?\n)
                              (if unhide ?\n ?\r)
                              'noundo)
      (when sgml-buggy-subst-char-in-region
        (set-buffer-modified-p mp)))))

(defun sgml-fold-element ()
  "Fold the lines comprising the current element, leaving the first line visible.
This uses the selective display feature."
  (interactive)
  (sgml-parse-to-here)
  (cond ((and (eq sgml-current-tree sgml-top-tree) ; outside document element
	      sgml-markup-type)
	 (sgml-fold-region sgml-markup-start
			   (save-excursion
			     (sgml-parse-to (point))
			     (point))))
	((and (eq sgml-current-tree sgml-top-tree) ; outside document element
	      (looking-at " *<!"))
	 (sgml-fold-region (point)
			   (save-excursion
			     (skip-chars-forward " \t")
			     (sgml-parse-to (1+ (point)))
			     (point))))

	(t
	 (let ((el (sgml-find-element-of (point))))
	   (when (eq el sgml-top-tree)
	     (error "No element here"))
	   (save-excursion
	     (goto-char (sgml-element-end el))
	     (when (zerop (sgml-element-etag-len el))
	       (skip-chars-backward " \t\n"))
	     (sgml-fold-region (sgml-element-start el)
			       (point)))))))

(defun sgml-fold-subelement ()
  "Fold all elements current elements content, leaving the first lines visible.
This uses the selective display feature."
  (interactive)
  (let* ((el (sgml-find-element-of (point)))
	 (c (sgml-element-content el)))
    (while c
      (sgml-fold-region (sgml-element-start c)
			(sgml-element-end c))
      (setq c (sgml-element-next c)))))

(defun sgml-unfold-line ()
  "Show hidden lines in current line."
  (interactive)
  (let ((op (point)))
    (beginning-of-line)
    (push-mark)
    (end-of-line)
    (exchange-point-and-mark)
    (sgml-fold-region (point) (mark) 'unhide)
    (goto-char op)))

(defun sgml-unfold-element ()
  "Show all hidden lines in current element."
  (interactive)
  (let* ((element (sgml-find-element-of (point))))
    (sgml-fold-region (sgml-element-start element)
		      (sgml-element-end element)
		      'unfold)))

(defun sgml-expand-element ()
  "As sgml-fold-subelement, but unfold first."
  (interactive)
  (sgml-unfold-element)
  (sgml-fold-subelement))

(defun sgml-unfold-all ()
  "Show all hidden lines in buffer."
  (interactive)
  (sgml-fold-region (point-min)
		    (point-max)
		    'unfold))

;;;; SGML mode: indentation and movement


(defun sgml-indent-according-to-level (element)
  (* sgml-indent-step
     (sgml-element-level element)))

(defun sgml-indent-according-to-stag (element)
  (save-excursion
    (goto-char (sgml-element-start element))
    (+ (current-column) sgml-indent-step)))

(defun sgml-indent-according-to-stag-end (element)
  (save-excursion
    (goto-char (sgml-element-start element))
    (+ 
     (current-column)
     (length (sgml-element-gi element))
     2)))


;;(setq sgml-content-indent-function 'sgml-indent-according-to-stag)

(defun sgml-indent-line (&optional col element)
  "Indent line, calling parser to determine level unless COL or ELEMENT
is given.  If COL is given it should be the column to indent to.  If
ELEMENT is given it should be a parse tree node, from which the level
is determined.
Deprecated: ELEMENT"
  (sgml-debug "-> sgml-indent-line %s %s"
              col (if element (sgml-element-gi element)))
  (when sgml-indent-step
    (let ((here (point-marker))
          ;; Where the indentation goes, i.e., will this be data
          element-insert                
          ;; Where we compute indentation, where the thing we indent is.
          ;; Can be different from above if end-tag is omitted.
          element-level)
      (back-to-indentation)
      (unless col
	;; Determine element
	(setq element-insert
	      (let ((sgml-throw-on-error 'parse-error))
		(catch sgml-throw-on-error
                  ;; This used to be (sgml-find-element-of (point))
                  ;; Why? Possibly to handle omitted end-tags
                  (sgml-debug "-- sgml-indent-line find context")
                  (sgml-find-context-of (point)))))
        (setq element-level element-insert)
        (when (and (not (eobp)) element-level)
          (setq element-level (sgml-find-element-of (point)))
          ;; It would be good if sgml-find-element-of would also tell
          ;; us if the character is in the start-tag/end-tag or
          ;; content
          (when (or (= (point) (sgml-element-start element-level))
                    (sgml-with-parser-syntax (sgml-is-end-tag)))
            (setq element-level (sgml-element-parent element-level)))))
      (when (eq element-level sgml-top-tree) ; not in a element at all
	(setq element-level nil)        ; forget element
	(goto-char here))		; insert normal tab insted)
      (when element-level
        (cond ((and (> (point) (sgml-element-start element-insert))
                    (< (point) (sgml-element-stag-end element-insert))
                    (not (sgml-element-data-p
                          (sgml-element-parent element-insert))))
               (setq col
                     (funcall sgml-attribute-indent-function element-insert)))
              ((or sgml-indent-data
                   (not (sgml-element-data-p element-insert)))
               (setq col
                     (funcall sgml-content-indent-function element-level)))))
      (when (and col (/= col (current-column)))
	(beginning-of-line 1)    
	(delete-horizontal-space)
	(indent-to col))
      (when (< (point) here)
	(goto-char here))
      col)))


(defun sgml-next-data-field ()
  "Move forward to next point where data is allowed."
  (interactive)
  (when (eobp)
    (error "End of buffer"))
  (let ((sgml-throw-on-warning 'next-data)
	(avoid-el (sgml-last-element)))
    ;; Avoid stopping in current element, unless point is in the start
    ;; tag of the element
    (when (< (point) (sgml-element-stag-end avoid-el))
      (setq avoid-el nil))
    (catch sgml-throw-on-warning
      (while (progn
	       (sgml-parse-to (1+ (point)))
	       (setq sgml-last-element
		     (if (not (eq ?< (following-char)))
			 (sgml-find-element-of (point))
		       sgml-current-tree))
	       (or (eq sgml-last-element avoid-el)
		   (not (sgml-element-data-p sgml-last-element)))))
      (sgml-set-last-element))))


(defun sgml-next-trouble-spot ()
  "Move forward to next point where something is amiss with the structure."
  (interactive)
  (push-mark)
  (sgml-note-change-at (point))		; Prune the parse tree
  (sgml-parse-to (point))
  (let ((sgml-throw-on-warning 'trouble))
    (or (catch sgml-throw-on-warning
	  (sgml-parse-until-end-of nil t))
	(message "Ok"))))



;;;; SGML mode: information display

(defun sgml-list-valid-tags ()
  "Display a list of the contextually valid tags."
  (interactive)
  (sgml-parse-to-here)
  (let ((model (sgml-element-model sgml-current-tree))
	(smap-name (sgml-lookup-shortref-name
		    (sgml-dtd-shortmaps sgml-dtd-info)
		    sgml-current-shortmap)))
    (with-output-to-temp-buffer "*Tags*"
      (princ (format "Current element: %s  %s\n"
		     (sgml-element-name sgml-current-tree)
		     (if (sgml-eltype-defined
			  (sgml-element-eltype sgml-current-tree))
			 ""
		       "[UNDEFINED]")))
      (princ (format "Element content: %s  %s\n"
		     (cond ((or (sgml-current-mixed-p) (eq model sgml-any))
			    "mixed")
			   ((sgml-model-group-p model)
			    "element")
			   (t
			    model))
		     (if (eq model sgml-any)
			 "[ANY]" "")))
      
      (when smap-name
	(princ (format "Current short reference map: %s\n" smap-name)))
      
      (cond ((sgml-final-p sgml-current-state)
	     (princ "Valid end-tags : ")
	     (loop for e in (sgml-current-list-of-endable-eltypes)
		   do (princ (sgml-end-tag-of e)) (princ " "))
	     (terpri))
	    (t
	     (princ "Current element can not end here\n")))
;;;      (let ((s (sgml-tree-shortmap sgml-current-tree)))
;;;	(when s
;;;	  (princ (format "Current shortref map: %s\n" s))))
      (princ "Valid start-tags\n")
      (sgml-print-valid-tags "In current element:"
			     sgml-current-tree sgml-current-state))))

(defun sgml-print-valid-tags (prompt tree state &optional exclude omitted-stag)
  (if (not (sgml-model-group-p state))
      (princ (format "%s (in %s)\n" prompt state))
    (let* ((req (sgml-required-tokens state))
	   (elems (nconc req
			 (delq sgml-pcdata-token
			       (sgml-optional-tokens state))))
	   (in (sgml-tree-includes tree))
	   (ex (append exclude (sgml-tree-excludes tree))))
      ;; Modify for exceptions
      (while in
	(unless (memq (car in) elems)
	  (setq elems (nconc elems (list (car in)))))
	(setq in (cdr in)))
      (while ex
	(setq elems (delq (car ex) elems))
	(setq ex (cdr ex)))
      ;; 
      (setq elems (sort elems (function string-lessp)))
      (sgml-print-list-of-tags prompt elems)
      ;; Check for omissable start-tags
      (when (and req (null (cdr req)))
	;; *** Assumes tokens are eltypes
	(let ((el (sgml-fake-open-element tree (car req))))
	  (when (sgml-element-stag-optional el)
	    (sgml-print-valid-tags
	     (format "If omitting %s:" (sgml-start-tag-of el))
	     el
	     (sgml-element-model el)
	     (append exclude elems)
	     'omitted-stag))))
      ;; Check for omissable end-tag
      (when (and (not omitted-stag)
		 (sgml-final-p state)
		 (sgml-element-etag-optional tree))
	(sgml-print-valid-tags
	 (format "If omitting %s:" (sgml-end-tag-of tree))
	 (sgml-element-parent tree)
	 (sgml-element-pstate tree)
	 (append exclude elems))))))

(defun sgml-print-list-of-tags (prompt list)
  (when list
    (princ prompt)
    (let ((col (length prompt))
	  (w   (1- (frame-width))))
      (loop for e in list
	    as str = (sgml-start-tag-of e)
	    do
	    (setq col (+ col (length str) 2))
	    (cond ((>= col w)
		   (setq col (+ (length str) 2))
		   (terpri)))
	    (princ "  ")
	    (princ str))
      (terpri))))


(defun sgml-show-context-standard (el &optional markup-type)
  (let* ((model (sgml-element-model el)))
    (format "%s %s"
            (cond (markup-type (format "%s" markup-type))
                  ((sgml-element-mixed el)
                   "#PCDATA")
                  ((not (sgml-model-group-p model))
                   model)
                  (t ""))
            (if (eq el sgml-top-tree)
		      "in empty context"
                      (sgml-element-context-string el)))))


(defun sgml-show-context-backslash (el &optional markup-type)
  (let ((gis nil))
    (while (not (sgml-off-top-p el))
      (push (sgml-element-gi el) gis)
      (setq el (sgml-element-parent el)))
    (mapconcat #'sgml-general-insert-case gis "\\")))


(defun sgml-show-context (&optional element)
  "Display where the cursor is in the element hierarchy."
  (interactive)
  (message "%s" (funcall sgml-show-context-function
                         (or element (sgml-last-element))
                         (if element nil sgml-markup-type))))


(defun sgml-what-element ()
  "Display what element is under the cursor."
  (interactive)
  (let* ((pos (point))
	 (nobol (eq (point) sgml-rs-ignore-pos))
	 (sref (and sgml-current-shortmap
                    (sgml-deref-shortmap sgml-current-shortmap nobol)))
	 (el nil))
    (goto-char pos)
    (setq el (sgml-find-element-of pos))
    (assert (not (null el)))
    (message "%s %s"
	     (cond ((eq el sgml-top-tree)
		    "outside document element")
		   ((< (point) (sgml-element-stag-end el))
		    "start-tag")
		   ((>= (point) (sgml-element-etag-start el))
		    "end-tag")
		   (sref
		    "shortref")
		   (t
		    "content"))
	     (sgml-element-context-string el))))

;;;; SGML mode: keyboard inserting

(defun sgml-coerce-element-type (obj)
  (when (stringp obj)
    (setq obj (sgml-lookup-eltype (sgml-general-case obj))))
  (when nil                             ;FIXME: need predicate
    (setq obj (sgml-tree-eltype obj)))
  obj)

(defun sgml-break-brefore-stag-p (element)
  (sgml-eltype-appdata (sgml-coerce-element-type element)
                       'break-brefore-stag))

(defun sgml-break-after-stag-p (element)
  (sgml-eltype-appdata (sgml-coerce-element-type element)
                       'break-after-stag))

(defun sgml-insert-break ()
  (skip-chars-backward " \t")
  (cond ((bolp)
         (if (looking-at "^\\s-*$")
             (fixup-whitespace)))
        (t
         ;; FIXME: fixup-whitespace ??
         (insert "\n"))))


(defun sgml-insert-tag (tag &optional silent no-nl-after)
  "Insert a tag, reading tag name in minibuffer with completion.
If sgml-leave-point-after-insert is t, the point is left after the
inserted tag(s), unless the element has some required content. If
sgml-leave-point-after-insert is nil the point is left after the first
tag inserted."
  (interactive 
   (list
    (let ((completion-ignore-case sgml-namecase-general))
      (completing-read "Tag: " (sgml-completion-table) nil t "<" ))))
  (sgml-find-context-of (point))
  (assert (null sgml-markup-type))
  ;; Fix white-space before tag
  (unless (sgml-element-data-p (sgml-parse-to-here))
    (skip-chars-backward " \t")
    (cond ((bolp)
	   (if (looking-at "^\\s-*$")
	       (fixup-whitespace)))
	  (t
	   (insert "\n"))))
  (insert tag)
  (sgml-indent-line)  
  (unless no-nl-after
    (save-excursion
      (unless (sgml-element-data-p (sgml-parse-to-here))
	(unless (eolp)
	  (save-excursion (insert "\n"))))))
  (or silent (sgml-show-context)))

(defvar sgml-new-attribute-list-function
  (function sgml-default-asl))

(defun sgml-insert-element (name &optional after silent)
  "Reads element name from minibuffer and inserts start and end tags.
If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has some required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive (list (sgml-read-element-name "Element: ")
		     sgml-leave-point-after-insert))
  (let (newpos				; position to leave cursor at
	element				; inserted element
	(sgml-show-warnings nil))
    (when (and name (not (equal name "")))
      (when (sgml-break-brefore-stag-p name)
        (sgml-insert-break))
      (sgml-insert-tag (sgml-start-tag-of name) 'silent)
      (if (and sgml-xml-p (sgml-check-empty name))
	  (forward-char -2)
	(forward-char -1))
      (setq element (sgml-find-element-of (point)))
      (sgml-insert-attributes (funcall sgml-new-attribute-list-function
				       element)
			      (sgml-element-attlist element))
      ;; Get element with new attributes
      (setq element (sgml-find-context-of (point)))
      (if (and sgml-xml-p (sgml-check-empty name))
	  (forward-char 2)
	(forward-char 1))
      (when (sgml-break-after-stag-p name)
        (sgml-insert-break))
      (when (not (sgml-element-empty element))
	(when (and sgml-auto-insert-required-elements
		   (sgml-model-group-p sgml-current-state))
	  (let (tem)
	    (while (and (setq tem (sgml-required-tokens sgml-current-state))
			(null (cdr tem)))
	      (setq tem (sgml-insert-element (car tem) t t))
	      (setq newpos (or newpos tem))
	      (sgml-parse-to-here))
	    (when tem			; more than one req elem
	      (insert "\n")
	      (when sgml-insert-missing-element-comment
		(insert (format "<!-- one of %s -->" tem))
		(sgml-indent-line)))))
	(setq newpos (or newpos (point)))
	(when sgml-insert-end-tag-on-new-line
	  (insert "\n"))
	(sgml-insert-tag (sgml-end-tag-of name) 'silent)
	(unless after
	  (goto-char newpos))
	(unless silent (sgml-show-context)))
      newpos)))

(defun sgml-default-asl (element)
  (loop for attdecl in (sgml-element-attlist element)
	when (sgml-default-value-type-p (sgml-attdecl-default-value attdecl)
					'REQUIRED)
	collect
	(sgml-make-attspec
	 (sgml-attdecl-name attdecl)
	 (sgml-read-attribute-value attdecl (sgml-element-name element) nil))))

(defun sgml-tag-region (element start end)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive
   (list
    (save-excursion (goto-char (region-beginning))
		    (sgml-read-element-name "Tag region with element: "))
    (region-beginning)
    (region-end)))
  (save-excursion
    (when (and element (not (equal element "")))
      (goto-char end)
      (insert (sgml-end-tag-of element))
      (goto-char start)
      (sgml-insert-tag (sgml-start-tag-of element)))))

(defun sgml-insert-attributes (avl attlist)
  "Insert the attributes with values AVL and declarations ATTLIST.
AVL should be a assoc list mapping symbols to strings."
  (let (name val dcl def)
    (loop for attspec in attlist do
	  (setq name (sgml-attspec-name attspec)
		val (cdr-safe (sgml-lookup-attspec name avl))
		dcl (sgml-attdecl-declared-value attspec)
		def (sgml-attdecl-default-value attspec))
          (setq name (sgml-general-insert-case name))
	  (unless val			; no value given
	    ;; Supply the default value if a value is needed
	    (cond ((sgml-default-value-type-p 'REQUIRED def)
		   (setq val ""))
		  ((and (or (not (or sgml-xml-p sgml-omittag sgml-shorttag))
                            sgml-insert-defaulted-attributes)
			(consp def))
		   (setq val (sgml-default-value-attval def)))))
          (when val
            (cond ((eq dcl 'CDATA))
                  ((eq dcl 'ENTITY) (setq val (sgml-entity-insert-case val)))
                  (t (setq val (sgml-general-insert-case val)))))
	  (cond 
	   ((null val))			; Ignore
	   ;; Ignore attributes with default value
	   ((and (consp def)		
		 (eq sgml-minimize-attributes 'max)
		 (or sgml-omittag sgml-shorttag)
		 (equal val (sgml-default-value-attval def))))
	   ;; No attribute name for token groups
	   ((and sgml-minimize-attributes sgml-shorttag
		 (member (sgml-general-case val)
			 (sgml-declared-value-token-group dcl)))
	    (insert " " val))
	   (t
	    (insert " " name "=" (sgml-quote-attribute-value val)))))
    (when auto-fill-function
      (funcall auto-fill-function))))


(defun sgml-quote-attribute-value (value)
  "Add quotes to the string VALUE unless minimization is on."
  (let ((quote ""))
	(cond ((and (not sgml-always-quote-attributes)
		    sgml-shorttag
		    (string-match "\\`[-.A-Za-z0-9]+\\'" value))
	       ) ; no need to quote
	      ((not (string-match "\"" value)) ; can use "" quotes
	       (setq quote "\""))
	      (t			; use '' quotes
	       (setq quote "'")))
	(concat quote value quote)))

(defun sgml-completion-table (&optional avoid-tags-in-cdata)
  (sgml-parse-to-here)
  (when sgml-markup-type
    (error "No tags allowed"))
  (cond ((or (sgml-model-group-p sgml-current-state)
	     (eq sgml-current-state sgml-any))
	 (append
	  (mapcar (function (lambda (x) (cons (sgml-end-tag-of x) x)))
		  (sgml-current-list-of-endable-eltypes))
	  (mapcar (function (lambda (x) (cons (sgml-start-tag-of x) x)))
		  (sgml-current-list-of-valid-eltypes))))
	(t
	 (sgml-message "%s" sgml-current-state)
	 nil)))

(defun sgml-element-endable-p ()
  (sgml-parse-to-here)
  (and (not (eq sgml-current-tree sgml-top-tree))
       (sgml-final-p sgml-current-state)))

(defun sgml-insert-end-tag ()
  "Insert end-tag for the current open element."
  (interactive "*")
  (sgml-parse-to-here)
  (cond
   ((eq sgml-current-tree sgml-top-tree)
    (sgml-error "No open element"))
   ((not (sgml-final-p sgml-current-state))
    (sgml-error "Can`t end element here"))
   (t
    (when (and sgml-indent-step
	       (not (sgml-element-data-p sgml-current-tree)))
      (delete-horizontal-space)
      (unless (bolp)
	(insert "\n")))
    (when (prog1 (bolp)
	    (insert (if (eq t (sgml-element-net-enabled sgml-current-tree))
			"/"
		      (sgml-end-tag-of sgml-current-tree))))
      (sgml-indent-line)))))

(defun sgml-insert-start-tag (name asl attlist &optional net)
  ;; Insert a start-tag with attributes
  ;; if NET is true end with NESTC unless XML then end with NESTC NET
  ;; (aka XML-TAGCE).
  (insert (sgml-delim "STAGO") (sgml-general-insert-case name))
  (sgml-insert-attributes asl attlist)
  ;; In XML, force net if element is always empty
  (when (and sgml-xml-p (sgml-check-empty name))
    (setq net t))
  (insert (if net (if sgml-xml-p
                      (sgml-delim "XML-TAGCE")
                    (sgml-delim "NESTC"))
            (sgml-delim "TAGC"))))

(defun sgml-change-start-tag (element asl)
  (let ((name (sgml-element-gi element))
	(attlist (sgml-element-attlist element)))
    ;; Concoct an attribute specification list using the names of the
    ;; existing attributes and those ot be changed.
    (when (and (not attlist) sgml-dtd-less)
      (dolist (elt (mapcar 'car asl))
	(unless (assoc elt attlist)	; avoid duplicates
	  (push (sgml-make-attdecl elt 'CDATA 'REQUIRED) attlist)))
      (setq attlist (nreverse attlist)))
    (assert (sgml-bpos-p (sgml-element-stag-epos element)))
    (goto-char (sgml-element-start element))
    (delete-char (sgml-element-stag-len element))
    (sgml-insert-start-tag name asl attlist
                           (if sgml-xml-p
                               (sgml-element-empty element)
                             (eq t (sgml-element-net-enabled element))))))

(defun sgml-read-attribute-value (attdecl element curvalue)
  "Return the attribute value read from user.
ATTDECL is the attribute declaration for the attribute to read.
CURVALUE is nil or a string that will be used as default value."
  (assert attdecl)
  (let* ((name (sgml-attdecl-name attdecl))
	 (dv (sgml-attdecl-declared-value attdecl))
	 (tokens (sgml-declared-value-token-group dv))
	 (notations (sgml-declared-value-notation dv))
	 ; JDF's addition
	 (ids (and (memq dv '(IDREF IDREFS)) (sgml-id-list)))
	 (type (cond (tokens "token")
		     (notations "NOTATION")
		     (t (symbol-name dv))))
	 (prompt
	  (format "Value for %s in %s (%s%s): "
		  name element type 
		  (if (and curvalue (not (eq dv 'IDREFS)))
		      (format " Default: %s" curvalue)
		    "")))
	 value)
    (setq value 
	  (cond ((or tokens notations)
		 (let ((completion-ignore-case sgml-namecase-general))
		   (completing-read prompt
				    (mapcar 'list (or tokens notations))
				    nil t)))
		(ids
		 (let ((completion-ignore-case sgml-namecase-general)
		       (minibuffer-local-completion-map sgml-edit-idrefs-map))
		   (completing-read prompt
				    'sgml-idrefs-completer
				    nil nil
				    (and curvalue
					 (cons curvalue (length curvalue))))))
		(t
		 (read-string prompt))))
    (if (and curvalue (equal value ""))
	curvalue value)))

(defun sgml-idrefs-completer (fullstring pred action)
  (let* ((start (string-match "\\(\\(:?-\\|\\w\\)*\\)$" fullstring))
	 (string (match-string 0 fullstring))
	 (prefix (substring fullstring 0 start)))
    ;(message "prefix: %s string: %s" prefix string)
    (cond ((null action)
	   (let ((completion (try-completion string (sgml-id-alist) pred)))
	     (if (eq completion t)
		 t
	       (concat prefix completion))))
	  ((eq action t)
	   (all-completions string (sgml-id-alist) pred))
	  ((eq action 'lambda)
	   (member string (sgml-id-alist))))))

(defun sgml-non-fixed-attributes (attlist)
  (loop for attdecl in attlist
	unless (sgml-default-value-type-p 'FIXED 
					  (sgml-attdecl-default-value attdecl))
	collect attdecl))

(defun sgml-insert-attribute (name value)
  "Read attribute name and value from minibuffer and insert attribute spec."
  (interactive
   (let* ((el (sgml-find-attribute-element))
	  (name
           (sgml-general-case
            (let ((completion-ignore-case sgml-namecase-general))
              (completing-read
               "Attribute name: "
               (mapcar
		(function (lambda (a) (list (sgml-attdecl-name a))))
		(if sgml-dtd-less
		    (sgml-tree-asl el)
		  (sgml-non-fixed-attributes (sgml-element-attlist el))))
               nil (not sgml-dtd-less))))))
     (list name
	   (sgml-read-attribute-value
	    (if sgml-dtd-less
		(list name)
	      (sgml-lookup-attdecl name (sgml-element-attlist el)))
	    (sgml-element-name el)
	    (sgml-element-attval el name)))))
  ;; Body
  (assert (stringp name))
  (assert (or (null value) (stringp value)))
  (let* ((el (sgml-find-attribute-element))
	 (asl (cons (sgml-make-attspec name value)
		    (sgml-element-attribute-specification-list el)))
	 (in-tag (< (point) (sgml-element-stag-end el))))
    (sgml-change-start-tag el asl)
    (when in-tag (forward-char -1))))

(defun sgml-split-element ()
  "Split the current element at point.
If repeated, the containing element will be split before the beginning
of then current element."
  (interactive "*")
  (setq sgml-split-level
	(if (eq this-command last-command)
	    (1+ sgml-split-level)
	  0))
  (let ((u (sgml-find-context-of (point)))
	(start (point-marker)))
    (loop repeat sgml-split-level do
	  (goto-char (sgml-element-start u))
	  (setq u (sgml-element-parent u)))
    ;; Verify that a new element can be started
    (unless (and (sgml-element-pstate u) ; in case of top element
		 (sgml-get-move (sgml-element-pstate u)
				(sgml-element-name u)))
      
      (sgml-error "The %s element can't be split"
		  (sgml-element-name u)))
    ;; Do the split
    (sgml-insert-end-tag)
    (insert ?\n)
    (sgml-insert-tag (sgml-start-tag-of u) 'silent)
    (skip-chars-forward " \t\n")
    (sgml-indent-line)
    (when (> sgml-split-level 0)
      (goto-char start))
    (or (eq sgml-top-tree
	    (setq u (sgml-element-parent u)))
	(sgml-message
	 "Repeat the command to split the containing %s element"
	 (sgml-element-name u)))))

;;; David Megginson's custom menus for keys

(defun sgml-custom-dtd (doctype)
  "Insert a DTD declaration from the sgml-custom-dtd alist."
  (interactive
   (list (completing-read "Insert DTD: " sgml-custom-dtd nil t)))
  (let ((entry (assoc doctype sgml-custom-dtd)))
    (sgml-doctype-insert (second entry) (cddr entry))))

(defun sgml-custom-markup (markup)
  "Insert markup from the sgml-custom-markup alist."
  (interactive
   (let ((completion-ignore-case sgml-namecase-general))
     (list (completing-read "Insert Markup: " sgml-custom-markup nil t))))
  (sgml-insert-markup (cadr (assoc markup sgml-custom-markup))))


;;;; SGML mode: Menu inserting

(defun sgml-tags-menu (event)
  "Pop up a menu with valid tags and insert the chosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has some required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive "*e")
  (let ((end (sgml-mouse-region)))
    (sgml-parse-to-here)
    (cond
     ((eq sgml-markup-type 'start-tag)
      (sgml-attrib-menu event))
     (t
      (let ((what
	     (sgml-menu-ask event (if (or end sgml-balanced-tag-edit)
                                      'element 'tags))))
	(cond
	 ((null what))
	 (end
	  (sgml-tag-region what (point) end))
	 (sgml-balanced-tag-edit
	  (sgml-insert-element what))
	 (t
	  (sgml-insert-tag what))))))))

(defun sgml-element-menu (event)
  "Pop up a menu with valid elements and insert choice.
If sgml-leave-point-after-insert is nil the point is left after the first 
tag inserted."
  (interactive "*e")
  (let ((what (sgml-menu-ask event 'element)))
    (and what (sgml-insert-element what))))

(defun sgml-add-element-menu (event)
  (interactive "*e")
  (let ((what (sgml-menu-ask event 'add-element)))
    (and what (sgml-add-element-to-element what nil))))

(defun sgml-start-tag-menu (event)
  "Pop up a menu with valid start-tags and insert choice."
  (interactive "*e")
  (let ((what (sgml-menu-ask event 'start-tag)))
    (and what (sgml-insert-tag what))))

(defun sgml-end-tag-menu (event)
  "Pop up a menu with valid end-tags and insert choice."
  (interactive "*e")
  (let ((what (sgml-menu-ask event 'end-tag)))
    (and what (sgml-insert-tag what))))

(defun sgml-tag-region-menu (event)
  "Pop up a menu with valid elements and tag current region with the choice."
  (interactive "*e")
  (let ((what (sgml-menu-ask event 'element)))
    (and what (sgml-tag-region what
			       (region-beginning)
			       (region-end)))))

(defun sgml-menu-ask (event type)
  (sgml-parse-to-here)
  (let (tab
	(title (capitalize (symbol-name type))))
    (cond
     ((eq type 'add-element)
      (setq tab
            (mapcar #'sgml-eltype-name
                    (sgml--all-possible-elements
                     (sgml-find-context-of (point))))))
     (sgml-markup-type)
     ((eq type 'element)
      (setq tab
	    (mapcar (function symbol-name)
		    (sgml-current-list-of-valid-eltypes))))
     (t
      (unless (eq type 'start-tag)
	(setq tab
	      (mapcar (function sgml-end-tag-of)
		      (sgml-current-list-of-endable-eltypes))))
      (unless (eq type 'end-tag)
	(setq tab
	      (nconc tab
		     (mapcar (function sgml-start-tag-of)
			     (sgml-current-list-of-valid-eltypes)))))))
    (if sgml-dtd-less
	;; The best we can do is assemble a list of elements we've
	;; seen so far.
	(dolist (n (append (sgml-dtd-eltypes sgml-dtd-info) '())
		   ;; Space avoids possible clash with valid element.
		   (setq tab (cons "Any " (cons "--" tab))))
	  (when (and (symbolp n) (not (memq n tab)))
	    (push (symbol-name n) tab))))
    (or tab
	(error "No valid %s at this point" type))
    (let ((elt (sgml-popup-menu event
				title
				(mapcar (function (lambda (x) (cons x x)))
					tab))))
      (if (equal elt "Any ")
	  (setq elt (sgml-read-element-name "Element: ")))
      (or elt (message nil)))))

(defun sgml-entities-menu (event)
  (interactive "*e")
  (sgml-need-dtd)
  (let ((menu
	 (mapcar (function (lambda (x) (cons x x)))
		 (sort (sgml-map-entities (function sgml-entity-name)
					  (sgml-dtd-entities sgml-dtd-info)
					  t)
		       (function string-lessp))))
	choice)
    (unless menu
      (error "No entities defined"))
    (setq choice (sgml-popup-menu event "Entities" menu))
    (when choice
      (insert "&" choice ";"))))

(defun sgml-doctype-insert (doctype vars)
  "Insert string DOCTYPE (ignored if nil) and set variables in &rest VARS.
VARS should be a list of variables and values.
For backward compatibility a single string instead of a variable is 
assigned to sgml-default-dtd-file.
All variables are made buffer local and are also added to the
buffers local variables list."
  (when doctype
    (unless (bolp)
      (insert "\n"))
    (unless (eolp)
      (insert "\n")
      (forward-char -1))
    (sgml-insert-markup doctype))
  (while vars
    (cond ((stringp (car vars))
	   (sgml-set-local-variable 'sgml-default-dtd-file (car vars))
	   (setq vars (cdr vars)))
	  ((car vars)			; Avoid nil
	   (sgml-set-local-variable (car vars) (cadr vars))
	   (setq vars (cddr vars)))
          (t
  	   (setq vars (cddr vars)))))
  (setq sgml-top-tree nil))

(defun sgml-attrib-menu (event)
  "Pop up a menu of the attributes of the current element
\(or the element with start-tag before point)."
  (interactive "e")
    (let ((menu (sgml-make-attrib-menu (sgml-find-attribute-element))))
      (sgml-popup-multi-menu event "Attributes" menu)))

(defun sgml-make-attrib-menu (el)
  (let ((attlist (sgml-non-fixed-attributes (sgml-element-attlist el))))
    (if (and (not attlist) sgml-dtd-less)
      (let ((name
	     (sgml-general-case
	      (let ((completion-ignore-case sgml-namecase-general))
		(completing-read
		 "Attribute name: "
		 (mapcar
		  (lambda (a) (list (sgml-attdecl-name a)))
		  (if sgml-dtd-less
		      (sgml-tree-asl el)
		    (sgml-non-fixed-attributes (sgml-element-attlist el))))
		 nil (not sgml-dtd-less))))))
	(if name
	    (setq attlist (list (sgml-make-attdecl name 'CDATA nil))))))
    (or attlist
	(error "No non-fixed attributes for element"))
    (loop for attdecl in attlist
	  for name = (sgml-attdecl-name attdecl)
	  for defval = (sgml-attdecl-default-value attdecl)
	  for tokens = (or (sgml-declared-value-token-group
			    (sgml-attdecl-declared-value attdecl))
			   (sgml-declared-value-notation
			    (sgml-attdecl-declared-value attdecl)))
	  collect
	  (cons
	   (sgml-attdecl-name attdecl)
	   (nconc
	    (if tokens
		(loop for val in tokens collect
		      (list val
			    (list 'sgml-insert-attribute name val)))
	      (list
	       (list "Set attribute value"
		     (list 'sgml-insert-attribute
			   (sgml-attdecl-name attdecl) 
			   (list 'sgml-read-attribute-value
				 (list 'quote attdecl)
				 (list 'quote (sgml-element-name el))
				 (sgml-element-attval el name))))))
	    (if (sgml-default-value-type-p 'REQUIRED defval)
		nil
	      (list "--"
		    (list (if (sgml-default-value-type-p nil defval)
			      (format "Default: %s"
				      (sgml-default-value-attval defval))
			    "#IMPLIED")
			  (list 'sgml-insert-attribute name nil)))))))))


;;;; New Right Button Menu

(defun sgml-right-menu (event)
  "Pop up a menu with valid tags and insert the choosen tag.
If the variable sgml-balanced-tag-edit is t, also inserts the
corresponding end tag. If sgml-leave-point-after-insert is t, the point
is left after the inserted tag(s), unless the element has som required
content.  If sgml-leave-point-after-insert is nil the point is left
after the first tag inserted."
  (interactive "*e")
  (let ((end (sgml-mouse-region)))
    (sgml-parse-to-here)
    (cond
     ((eq sgml-markup-type 'start-tag)
      (sgml-right-stag-menu event))
     (t
      (let ((what
	     (sgml-menu-ask event (if (or end sgml-balanced-tag-edit)
                                      'element 'tags))))
	(cond
	 ((null what))
	 (end
	  (sgml-tag-region what (point) end))
	 (sgml-balanced-tag-edit
	  (sgml-insert-element what))
	 (t
	  (sgml-insert-tag what))))))))


(defun sgml-right-stag-menu (event)
  (let* ((el (sgml-find-attribute-element))
         (attrib-menu (ignore-errors (sgml-make-attrib-menu el))))

    (let* ((alt-gi (mapcar (function sgml-eltype-name)
                           (progn
                             (sgml-find-context-of (sgml-element-start el))
                             (sgml-current-list-of-valid-eltypes))))
           (change-menu
            (cons "Change To"
                  (loop for gi in alt-gi
                        collect `(,gi (sgml-change-element-name ,gi))))))
      (sgml-popup-multi-menu
       event "Start Tag"
       (list* `("Misc"
                ("Edit attributes" (sgml-edit-attributes))
                ("Normalize" (sgml-normalize-element))
                ("Fill" (sgml-fill-element
                         (sgml-find-context-of (point))))
                ("Splice" (sgml-untag-element))
                ("Fold"   (sgml-fold-element)))
              change-menu
              ;;`("--" "--")
              attrib-menu)))))



;;;; SGML mode: Fill 

(defun sgml-element-fillable (element)
  (and (sgml-element-mixed element)
       (not (sgml-element-appdata element 'nofill))))

(defun sgml-fill-element (element)
  "Fill biggest enclosing element with mixed content.
If current element has pure element content, recursively fill the
subelements."
  (interactive (list (sgml-find-element-of (point))))
  ;;
  (message "Filling...")
  (when (sgml-element-fillable element)
    ;; Find biggest enclosing fillable element
    (while (sgml-element-fillable (sgml-element-parent element))
      (setq element (sgml-element-parent element))))
  ;; 
  (sgml-do-fill element)
  (sgml-message "Done"))

(defun sgml-do-fill (element)
  (when sgml-debug
    (goto-char (sgml-element-start element))
    (sit-for 0))
  (save-excursion
    (cond
     ((sgml-element-fillable element)
      (let (last-pos
	    (c (sgml-element-content element))
	    (agenda nil))		; regions to fill later
	(goto-char (sgml-element-stag-end element))
	(when (eolp) (forward-char 1))
	(setq last-pos (point))
	(while c
	  (cond
	   ((sgml-element-fillable c))
	   (t
	    ;; Put region before element on agenda.  Can't fill it now
	    ;; that would mangle the parse tree that is being traversed.
	    (push (cons last-pos (sgml-element-start c))
		  agenda)
	    (goto-char (sgml-element-start c))
	    (sgml-do-fill c)
	    ;; Fill may change parse tree, get a fresh
	    (setq c (sgml-find-element-of (point)))
	    (setq last-pos (sgml-element-end c))))
	  (setq c (sgml-element-next c)))
	;; Fill the last region in content of element,
	;; but get a fresh parse tree, if it has change due to other fills.
        (goto-char last-pos)
        (when (bolp) (sgml-indent-line))
	(sgml-fill-region last-pos
			  (sgml-element-etag-start
			   (sgml-find-element-of
			    (sgml-element-start element))))
	(while agenda
	  (sgml-fill-region (caar agenda) (cdar agenda))
	  (setq agenda (cdr agenda)))))
     (t
      ;; If element is not mixed, fill subelements recursively
      (let ((c (sgml-element-content element)))
	(while c
	  (goto-char (sgml-element-etag-start c))
          (sgml-indent-line)
	  (goto-char (sgml-element-start c))
          (sgml-indent-line)
          (setq c (sgml-find-element-of (point)))
	  (sgml-do-fill c)
	  (setq c (sgml-element-next (sgml-find-element-of (point))))))))))

(defun sgml-fill-region (start end)
  (sgml-message "Filling...")
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t\n")
    (while (progn (beginning-of-line 1)
		  (< start (point)))
      (delete-char -1)
      (delete-horizontal-space)
      (insert " "))
    (end-of-line 1)
    (let (give-up prev-column opoint oopoint)
      (while (and (not give-up) (> (current-column) fill-column))
	(setq prev-column (current-column))
	(setq oopoint (point))
	(move-to-column (1+ fill-column))
	(skip-chars-backward "^ \t\n")
	(setq opoint (point))
	(skip-chars-backward " \t")
	(if (bolp)
            (progn
              (goto-char opoint)
              (if (re-search-forward "[ \t]" oopoint t)
                  (save-excursion
                    (skip-chars-forward " \t")
                    (setq opoint (point)))
                (setq give-up t))))
        (if (not give-up)
            (progn 
              (delete-region (point) opoint)
              (newline)
              (sgml-indent-line)
              (end-of-line 1)
              (setq give-up (>= (current-column) prev-column))))))))

;;;; SGML mode: Attribute editing

(defvar sgml-start-attributes nil)
(defvar sgml-main-buffer nil)
(defvar sgml-attlist nil)

(defun sgml-edit-attributes ()
  "Edit attributes of current element.
Editing is done in a separate window."
  (interactive)
  (let ((element (sgml-find-attribute-element)))
    (unless (sgml-bpos-p (sgml-element-stag-epos element))
      (error "Element's start-tag is not in the buffer"))
    (push-mark)
    (goto-char (sgml-element-start element))
    (let* ((start (point-marker))
	   (asl (sgml-element-attribute-specification-list element))
	   (cb (current-buffer))
	   (quote sgml-always-quote-attributes)
	   (xml-p sgml-xml-p))
      (switch-to-buffer-other-window
       (sgml-attribute-buffer element asl))
      (make-local-variable 'sgml-start-attributes)
      (setq sgml-start-attributes start)
      (make-local-variable 'sgml-always-quote-attributes)
      (setq sgml-always-quote-attributes quote)
      (make-local-variable 'sgml-main-buffer)
      (setq sgml-main-buffer cb)
      (make-local-variable 'sgml-xml-p)
      (setq sgml-xml-p xml-p))))


(defun sgml-effective-attlist (eltype)
  (let ((effective-attlist nil)
        (attlist (sgml-eltype-attlist eltype))
        (attnames (or (sgml-eltype-appdata eltype 'attnames)
                      '(*))))
    (while (and attnames (not (eq '* (car attnames))))
      (let ((attdecl (sgml-lookup-attdecl (car attnames) attlist)))
        (if attdecl 
            (push attdecl effective-attlist)
          (message "Attnames specefication error: no %s attribute in %s"
                   (car attnames) eltype)))
      (setq attnames (cdr attnames)))
    (when (eq '* (car attnames))
      (while attlist
        (let ((attdecl (sgml-lookup-attdecl (sgml-attdecl-name (car attlist))
                                            effective-attlist)))
          (unless attdecl
            (push (car attlist) effective-attlist)))
        (setq attlist (cdr attlist))))
    (nreverse effective-attlist)))


(defun sgml-attribute-buffer (element asl)
  (let ((bname "*Edit attributes*")
	(buf nil)
	(inhibit-read-only t))
    (save-excursion
      (when (setq buf (get-buffer bname))
	(kill-buffer buf))
      (setq buf (get-buffer-create bname))
      (set-buffer buf)
      (erase-buffer)
      (sgml-edit-attrib-mode)
      (make-local-variable 'sgml-attlist)
      (setq sgml-attlist (sgml-effective-attlist
                          (sgml-element-eltype element)))
      (sgml-insert '(read-only t)
                   (substitute-command-keys
                    "<%s  -- Edit values and finish with \
\\[sgml-edit-attrib-finish], abort with \\[sgml-edit-attrib-abort] --\n")
                   (sgml-element-name element))
      (loop
       for attr in sgml-attlist do
       ;; Produce text like
       ;;  name = value
       ;;  -- declaration : default --
       (let* ((aname (sgml-attdecl-name attr))
	      (dcl-value (sgml-attdecl-declared-value attr))
	      (def-value (sgml-attdecl-default-value attr))
	      (cur-value (sgml-lookup-attspec aname asl)))
	 (sgml-insert			; atribute name
	  '(read-only t category sgml-form) " %s =" aname)
	 (cond				; attribute value
	  ((sgml-default-value-type-p 'FIXED def-value)
	   (sgml-insert '(read-only t category sgml-fixed)
			" #FIXED %s"
			(sgml-default-value-attval def-value)))
	  ((and (null cur-value)
		(or (memq def-value '(IMPLIED CONREF CURRENT))
		    (sgml-default-value-attval def-value)))
           (sgml-insert '(read-only t category sgml-form
                                    rear-nonsticky (read-only category))
                        " ")
	   (sgml-insert '(category sgml-default rear-nonsticky (category))
			"#DEFAULT"))
	  (t
           (sgml-insert '(read-only t category sgml-form
                                    rear-nonsticky (read-only category))
                        " ")
           (when (not (null cur-value))
             (sgml-insert nil "%s" (sgml-attspec-attval cur-value)))))
	 (sgml-insert
	  '(read-only 1)
	  "\n\t-- %s: %s --\n"
	  (cond ((sgml-declared-value-token-group dcl-value))
		((sgml-declared-value-notation dcl-value)
		 (format "NOTATION %s"
			 (sgml-declared-value-notation dcl-value)))
		(t
		 dcl-value))
	  (cond ((sgml-default-value-attval def-value))
		(t
		 (concat "#" (upcase (symbol-name def-value))))))))
      (sgml-insert '(read-only t) ">")
      (goto-char (point-min))
      (sgml-edit-attrib-next))
    buf))


(defvar sgml-edit-attrib-mode-map (make-sparse-keymap))

;; used as only for #DEFAULT in attribute editing. Binds all normally inserting
;; keys to a command that will clear the #DEFAULT before doing self-insert.
(defvar sgml-attr-default-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map sgml-edit-attrib-mode-map)
    (substitute-key-definition 'self-insert-command
                               'sgml-attr-clean-and-insert
                               map
                               global-map)
    (put 'sgml-default 'local-map map)))

(define-key sgml-edit-attrib-mode-map "\C-c\C-c" 'sgml-edit-attrib-finish)
(define-key sgml-edit-attrib-mode-map "\C-c\C-d" 'sgml-edit-attrib-default)
(define-key sgml-edit-attrib-mode-map "\C-c\C-k" 'sgml-edit-attrib-abort)

(define-key sgml-edit-attrib-mode-map "\C-a"  'sgml-edit-attrib-field-start)
(define-key sgml-edit-attrib-mode-map "\C-e"  'sgml-edit-attrib-field-end)
(define-key sgml-edit-attrib-mode-map "\t"  'sgml-edit-attrib-next)

(defun sgml-edit-attrib-mode ()
  "Major mode to edit attribute specification list.\\<sgml-edit-attrib-mode-map>
Use \\[sgml-edit-attrib-next] to move between input fields.  Use
\\[sgml-edit-attrib-default] to make an attribute have its default
value.  To abort edit kill buffer (\\[kill-buffer]) and remove window
\(\\[delete-window]).  To finish edit use \\[sgml-edit-attrib-finish].

\\{sgml-edit-attrib-mode-map}"
  (setq mode-name "SGML edit attributes"
	major-mode 'sgml-edit-attrib-mode)
  (use-local-map sgml-edit-attrib-mode-map)
  (run-hooks 'text-mode-hook 'sgml-edit-attrib-mode-hook))

(defun sgml-edit-attrib-abort ()
  "Abort the attribute editor, removing the window."
  (interactive)
  (let ((cb (current-buffer))
	(start sgml-start-attributes))
    (delete-windows-on cb)
    (kill-buffer cb)
    (when (markerp start)
      (switch-to-buffer (marker-buffer start))
      (goto-char start))))

(defun sgml-edit-attrib-finish ()
  "Finish editing and insert attribute values in original buffer."
  (interactive)
  (let ((cb (current-buffer))
	(asl (sgml-edit-attrib-specification-list))
	;; save buffer local variables
	(start sgml-start-attributes))
    (when (markerp start)
      (delete-windows-on cb)
      (switch-to-buffer (marker-buffer start))
      (kill-buffer cb)
      (goto-char start)
      (let ((element (sgml-find-element-of start)))
	;; *** Should the it be verified that this element
	;; is the one edited?
	(sgml-change-start-tag element asl)))))


(defun sgml-edit-attrib-specification-list ()
  (goto-char (point-min))
  (forward-line 1)
  (sgml-with-parser-syntax
   (let ((asl nil)
	 (al sgml-attlist))
     (while (not (eq ?> (following-char)))
       (sgml-parse-s)
       (sgml-check-nametoken)		; attribute name, should match head of al
       (forward-char 3)
       (unless (memq (get-text-property (point) 'category)
		     '(sgml-default sgml-fixed))
	 (push
	  (sgml-make-attspec (sgml-attdecl-name (car al))
			     (sgml-extract-attribute-value
			      (sgml-attdecl-declared-value (car al))))
	  asl))
       (while (progn (beginning-of-line 2)
		     (or (eolp)
			 (not (get-text-property (point) 'read-only)))))

       (forward-line 1)
       (setq al (cdr al)))
     asl)))


(defun sgml-extract-attribute-value (type)
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
			(progn (sgml-edit-attrib-field-end)
			       (point)))
      (goto-char (point-min))
      (while (not (eobp))
        (if (eq 'sgml-default (get-text-property (point) 'category))
            (delete-char 1)
          (forward-char 1)))
      (unless (eq type 'CDATA)
	(subst-char-in-region (point-min) (point-max) ?\n ? )
	(goto-char (point-min))
	(delete-horizontal-space))
      (goto-char (point-min))
      (when (search-forward "\"" nil t)	; don't allow both " and '
	(goto-char (point-min))
	(while (search-forward "'" nil t) ; replace ' with char ref
	  (replace-match "&#39;")))
      (buffer-string))))

(defun sgml-edit-attrib-default ()
  "Set current attribute value to default."
  (interactive)
  (sgml-edit-attrib-clear)
  (save-excursion
    (sgml-insert '(category sgml-default rear-nonsticky (category))
                 "#DEFAULT")))

(defun sgml-edit-attrib-clear ()
  "Kill the value of current attribute."
  (interactive)
  (let ((inhibit-read-only '(sgml-default)))
    (sgml-edit-attrib-field-start)
    (let ((end (save-excursion (sgml-edit-attrib-field-end) (point))))
      (put-text-property (point) end 'read-only nil)
      (let ((inhibit-read-only t))
        (put-text-property (1- (point)) (point)
                           'rear-nonsticky '(read-only category)))
      (kill-region (point) end))))


(defun sgml-attr-clean-and-insert (n)
  "Insert the character you type, after clearing the current attribute."
  (interactive "p")
  (sgml-edit-attrib-clear)
  (self-insert-command n))


(defun sgml-edit-attrib-field-start ()
  "Go to the start of the attribute value field."
  (interactive)
  (let (start)
    (beginning-of-line 1)
    (while (not (eq t (get-text-property (point) 'read-only)))
      (beginning-of-line 0))
    (while (eq 'sgml-form (get-text-property (point) 'category))
      (setq start (next-single-property-change (point) 'category))
      (unless start (error "No attribute value here"))
      (assert (number-or-marker-p start))
      (goto-char start))))

(defun sgml-edit-attrib-field-end ()
  "Go to the end of the attribute value field."
  (interactive)
  (sgml-edit-attrib-field-start)
  (let ((end (if (and (eolp)
		      (get-text-property (1+ (point)) 'read-only))
		 (point)
	       (next-single-property-change (point) 'read-only))))
    (assert (number-or-marker-p end))
    (goto-char end)))

(defun sgml-edit-attrib-next ()
  "Move to next attribute value."
  (interactive)
  (if (eq t (get-text-property (point) 'read-only))
      (beginning-of-line 1))
  (or (search-forward-regexp (if sgml-have-re-char-clases
				 "^ *[-_.:[:alnum:]]+ *= ?"
			       "^ *[-_.:A-Za-z0-9]+ *= ?") nil t)
      (goto-char (point-min))))


;;;; SGML mode: Hiding tags/attributes

(defconst sgml-tag-regexp
  (if sgml-have-re-char-clases
      "\\(</?>\\|</?[_[:alpha:]][-_:[:alnum:].]*\\(\\([^'\"></]\\|'[^']*'\\|\"[^\"]*\"\\)*\\)/?>?\\)"
    "\\(</?>\\|</?[_A-Za-z][-_:A-Za-z0-9.]*\\(\\([^'\"></]\\|'[^']*'\\|\"[^\"]*\"\\)*\\)/?>?\\)"))

(defun sgml-operate-on-tags (action &optional attr-p)
  (let ((buffer-modified-p (buffer-modified-p))
	(inhibit-read-only t)
	(buffer-read-only nil)
	(before-change-functions nil)
	(markup-index			; match-data index in tag regexp
	 (if attr-p 2 1))
	(tagcount			; number tags to give them uniq
					; invisible properties
	 1))
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward sgml-tag-regexp nil t)
	    (cond
	     ((eq action 'hide)
	      (let ((tag (downcase
			  (buffer-substring-no-properties
			   (1+ (match-beginning 0))
			   (match-beginning 2)))))
		(if (or attr-p (not (member tag sgml-exposed-tags)))
		    (add-text-properties
		     (match-beginning markup-index) (match-end markup-index)
		     (list 'invisible tagcount
			   'rear-nonsticky '(invisible face))))))
	     ((eq action 'show)		; ignore markup-index
	      (remove-text-properties (match-beginning 0) (match-end 0)
				      '(invisible nil)))
	     (t (error "Invalid action: %s" action)))
	    (incf tagcount)))
      (sgml-restore-buffer-modified-p buffer-modified-p))))

(defun sgml-hide-tags ()
  "Hide all tags in buffer."
  (interactive)
  (sgml-operate-on-tags 'hide))

(defun sgml-show-tags ()
  "Show hidden tags in buffer."
  (interactive)
  (sgml-operate-on-tags 'show))

(defun sgml-hide-attributes ()
  "Hide all attribute specifications in the buffer."
  (interactive)
  (sgml-operate-on-tags 'hide 'attributes))

(defun sgml-show-attributes ()
  "Show all attribute specifications in the buffer."
  (interactive)
  (sgml-operate-on-tags 'show 'attributes))


;;;; SGML mode: Normalize (and misc manipulations)

(defun sgml-expand-shortref-to-text (name)
  (let (before-change-functions
	(entity (sgml-lookup-entity name (sgml-dtd-entities sgml-dtd-info))))
    (cond
     ((null entity) (sgml-error "Undefined entity %s" name))
     ((sgml-entity-data-p entity)
      (sgml-expand-shortref-to-entity name))
     (t
      (delete-region sgml-markup-start (point))
      (sgml-entity-insert-text entity)
      (setq sgml-goal (point-max))	; May have changed size of buffer
      ;; now parse the entity text
      (setq sgml-rs-ignore-pos sgml-markup-start)
      (goto-char sgml-markup-start)))))

(defun sgml-expand-shortref-to-entity (name)
  (let ((end (point))
	(re-found nil)
	before-change-functions)
    (goto-char sgml-markup-start)
    (setq re-found (search-forward "\n" end t))
    (delete-region sgml-markup-start end)	   
    (insert "&" name (if re-found "\n" ";"))
    (setq sgml-goal (point-max))	; May have changed size of buffer
    (goto-char (setq sgml-rs-ignore-pos sgml-markup-start))))

(defun sgml-expand-all-shortrefs (to-entity)
  "Expand all short references in the buffer.
Short references to text entities are expanded to the replacement text
of the entity; other short references are expanded into general entity
references.  If argument TO-ENTITY is non-nil, or if called
interactively with a numeric prefix argument, all short references are
replaced by general entity references."
  (interactive "*P")
  (sgml-reparse-buffer
   (if to-entity
       (function sgml-expand-shortref-to-entity)
     (function sgml-expand-shortref-to-text))))

(defun sgml-normalize (to-entity &optional element)
  "Normalize buffer by filling in omitted tags and expanding empty tags.
Argument TO-ENTITY controls how short references are expanded as with
`sgml-expand-all-shortrefs'.  An optional argument ELEMENT can be the
element to normalize instead of the whole buffer, if used no short
references will be expanded."
  (interactive "*P")
  (unless element
    (sgml-expand-all-shortrefs to-entity))
  (let ((only-one (not (null element))))
    (setq element (or element (sgml-top-element)))
    (goto-char (sgml-element-end element)) 
    ;; FIXME: actually the sgml-note-change-at called by the
    ;; before-change-functions need to be delayed to after the normalize
    ;; to avoid destroying the tree wile traversing it.
    (let ((before-change-functions nil))
      (sgml-normalize-content element only-one)))
  (sgml-note-change-at (sgml-element-start element))
  (sgml-message "Done"))

(defun sgml-normalize-element ()
  (interactive "*")
  (sgml-normalize nil (sgml-find-element-of (point))))

(defun sgml-normalize-content (element only-first)
  "Normalize all elements in a content where ELEMENT is first element.
If sgml-normalize-trims is non-nil, trim off white space from ends of
elements with omitted end-tags."
  (let ((content nil))
    (while element			; Build list of content elements
      (push element content)
      (setq element (if only-first
			nil
		      (sgml-element-next element))))
    (while content
      (setq element (car content))
      ;; Progress report
      (sgml-lazy-message "Normalizing %d%% left"
			 (/ (point) (/ (+ (point-max) 100) 100)))
      ;; Fix the end-tag
      (sgml-normalize-end-tag element)
      ;; Fix tags of content
      (sgml-normalize-content (sgml-tree-content element) nil)
      ;; Fix the start-tag
      (sgml-normalize-start-tag element)
      ;; Next content element
      (setq content (cdr content)))))

(defun sgml-normalize-start-tag (element)
  (when (sgml-bpos-p (sgml-element-stag-epos element))
    (goto-char (min (point) (sgml-element-start element)))
    (let ((name (sgml-element-gi element))
	  (attlist (sgml-element-attlist element))
	  (asl (sgml-element-attribute-specification-list element)))
      (save-excursion
	(assert (or (zerop (sgml-element-stag-len element))
		    (= (point) (sgml-element-start element))))
	(delete-char (sgml-element-stag-len element))
	(sgml-insert-start-tag name asl attlist nil)))))

(defun sgml-normalize-end-tag (element)
  (unless (sgml-element-empty element)
    (when (sgml-bpos-p (sgml-element-etag-epos element))
      (goto-char (min (point) (sgml-element-etag-start element)))    
      (if (and (zerop (sgml-element-etag-len element))
	       sgml-normalize-trims)
	  (skip-chars-backward " \t\n\r"))
      (delete-char (sgml-tree-etag-len element))
      (save-excursion (insert (sgml-end-tag-of element))))))


(defun sgml-make-character-reference (&optional invert)
  "Convert character after point into a character reference.
If called with a numeric argument, convert a character reference back
to a normal character.  If called from a program, set optional
argument INVERT to non-nil.  If the function `decode-char' is defined,
convert to and from Unicodes.  Otherwise will only work for ASCII or 8-bit
characters in the current coding system."
  (interactive "*P")
  (cond
   (invert
    (or (looking-at "&#\\([0-9]+\\)[;\n]?")
	(error "No character reference after point"))
    (let ((c (string-to-int (buffer-substring (match-beginning 1)
					      (match-end 1)))))
      (delete-region (match-beginning 0)
		     (match-end 0))
      (if (fboundp 'decode-char)	; Emacs 21, Mule-UCS
	  (setq c (decode-char 'ucs c))
	;; Else have to assume 8-bit character.
	(if (fboundp 'unibyte-char-to-multibyte) ; Emacs 20
	    (setq c (unibyte-char-to-multibyte c))))
      (insert c)))
   ;; Convert character to &#nn;
   (t
    (let ((c (following-char)))
      (delete-char 1)
      (if (fboundp 'encode-char)
	  (setq c (encode-char c 'ucs))
	(if (fboundp 'multibyte-char-to-unibyte)
	    (setq c (multibyte-char-to-unibyte c))))
      (insert (format "&#%d;" c))))))

(defun sgml-expand-entity-reference ()
  "Insert the text of the entity referenced at point."
  (interactive)
  (save-excursion
    (sgml-with-parser-syntax
     (setq sgml-markup-start (point))
     (or (sgml-parse-delim "ERO")
	 (progn
	   (skip-syntax-backward "w_")
	   (forward-char -1)		; @@ Really length of ERO
	   (setq sgml-markup-start (point))
	   (sgml-check-delim "ERO")))
     (let* ((ename (sgml-check-name t))
	    (entity (sgml-lookup-entity ename
					(sgml-dtd-entities
					 (sgml-pstate-dtd
					  sgml-buffer-parse-state)))))
       (unless entity
	 (error "Undefined entity %s" ename))
       (or (sgml-parse-delim "REFC")
	   (sgml-parse-RE))
       (delete-region sgml-markup-start (point))
       (sgml-entity-insert-text entity)))))



(defun sgml-trim-and-leave-element ()
  "Remove blanks at end of current element and move point to after element."
  (interactive)
  (goto-char (sgml-element-etag-start (sgml-last-element)))
  (while (progn (forward-char -1)
		(looking-at "\\s-"))
    (delete-char 1))
  (sgml-up-element))


(defvar sgml-notation-handlers 
  '((gif . "xv") 
    (jpeg . "xv"))
  "*An alist mapping notations to programs handling them")

;; Function contributed by Matthias Clasen <clasen@netzservice.de>
(defun sgml-edit-external-entity ()
  "Open	a new window and display the external entity at the point."
  (interactive)
  (sgml-need-dtd)
  (save-excursion                     
    (sgml-with-parser-syntax  
     (setq sgml-markup-start (point))
     (unless (sgml-parse-delim "ERO")
       (search-backward-regexp "[&>;]")
       (setq sgml-markup-start (point))
       (sgml-check-delim "ERO"))
     (sgml-parse-to-here)		; get an up-to-date parse tree
     (let* ( (parent (buffer-file-name)) ; used to be (sgml-file)
	     (ename (sgml-check-name t))
	     (entity (sgml-lookup-entity ename       
					 (sgml-dtd-entities
					  (sgml-pstate-dtd
					   sgml-buffer-parse-state))))
	     (buffer nil)
	     (ppos nil))
       (unless entity
	 (error "Undefined entity %s" ename))

       (let* ((type (sgml-entity-type entity))
	      (notation (sgml-entity-notation entity))
	      (handler (cdr (assoc notation sgml-notation-handlers))))
	 (case type
	   (ndata 
	    (if handler 
		(progn
		  (message (format "Using '%s' to handle notation '%s'."
				   handler notation))
		  (save-excursion
		    (set-buffer (get-buffer-create "*SGML background*"))
		    (erase-buffer)
		    (let* ((file (sgml-external-file 
				  (sgml-entity-text entity)
				  type
				  (sgml-entity-name entity)))
			   (process (start-process 
				     (format "%s background" handler)
				     nil handler file)))
		      (process-kill-without-query process))))
	      (error "Don't know how to handle notation '%s'." notation)))
	   (text (progn
       
	    ;; here I try to construct a useful value for
	    ;; `sgml-parent-element'.
       
	    ;; find sensible values for the HAS-SEEN-ELEMENT part
	    (let ((seen nil)
		  (child (sgml-tree-content sgml-current-tree)))
	      (while (and child
			  (sgml-tree-etag-epos child)
			  (<= (sgml-tree-end child) (point)))
		(push (sgml-element-gi child) seen)
		(setq child (sgml-tree-next child)))
	      (push (nreverse seen) ppos))
	    
	    ;; find ancestors
	    (let ((rover sgml-current-tree))
	      (while (not (eq rover sgml-top-tree))
		(push (sgml-element-gi rover) ppos)
		(setq rover (sgml-tree-parent rover))))
	    
	    (find-file-other-window
	     (sgml-external-file (sgml-entity-text entity)
				 (sgml-entity-type entity)
				 (sgml-entity-name entity)))
	    (goto-char (point-min))
	    (sgml-mode)
	    (setq sgml-parent-document (cons parent ppos))
	    ;; update the live element indicator of the new window
	    (sgml-parse-to-here)))
	   (t (error "Can't edit entities of type '%s'." type))))))))

;;;; SGML mode: TAB completion

(defun sgml-complete ()
  "Complete the word/tag/entity before point.
If it is a tag (starts with < or </) complete with valid tags.
If it is an entity (starts with &) complete with declared entities.
If it is a markup declaration (starts with <!) complete with markup 
declaration names. If it is a reserved word starting with # complete
reserved words.
If it is something else complete with ispell-complete-word."
  (interactive "*")
  (let ((tab				; The completion table
	 nil)
        (ignore-case                    ; If ignore case in matching completion
         sgml-namecase-general)
        (insert-case
         'sgml-general-insert-case)
	(pattern nil)
	(c nil)
	(here (point)))
    (skip-chars-backward "^ \n\t</!&%#")
    (setq pattern (buffer-substring (point) here))
    (setq c (char-after (1- (point))))
    (cond
     ;; entitiy
     ((eq c ?&)
      (sgml-need-dtd)
      (setq insert-case 'sgml-entity-insert-case)
      (setq tab
	    (sgml-entity-completion-table
	     (sgml-dtd-entities (sgml-pstate-dtd sgml-buffer-parse-state)))))
     ;; start-tag
     ((eq c ?<)
      (save-excursion
	(backward-char 1)
	(sgml-parse-to-here)
	(setq tab (sgml-eltype-completion-table
		   (sgml-current-list-of-valid-eltypes)))))
     ;; end-tag
     ((eq c ?/)
      (save-excursion
	(backward-char 2)
	(sgml-parse-to-here)
	(setq tab (sgml-eltype-completion-table
		   (sgml-current-list-of-endable-eltypes)))))
     ;; markup declaration
     ((eq c ?!)
      (setq tab sgml-markup-declaration-table
            ignore-case t))
     ;; Reserved words with '#' prefix
     ((eq c ?#)
      (setq tab '(("PCDATA") ("NOTATION") ("IMPLIED") ("REQUIRED")
                  ("FIXED") ("EMPTY"))
            ignore-case t))
     (t
      (goto-char here)
      (ispell-complete-word)))
    (when tab
      (let* ((completion-ignore-case ignore-case)
             (completion (try-completion pattern tab)))
	(cond ((null completion)
	       (goto-char here)
	       (message "Can't find completion for \"%s\"" pattern)
	       (ding))
	      ((eq completion t)
	       (goto-char here)
	       (message "[Complete]"))
	      ((not (string= pattern completion))
	       (delete-char (length pattern))
	       (insert (funcall insert-case completion)))
	      (t
	       (goto-char here)
	       (message "Making completion list...")
	       (let ((list (all-completions pattern tab)))
		 (with-output-to-temp-buffer " *Completions*"
		   (display-completion-list list)))
	       (message "Making completion list...%s" "done")))))))


;;;; SGML mode: Options menu

(defun sgml-file-options-menu (&optional event)
  (interactive "e")
  (sgml-options-menu event sgml-file-options))

(defun sgml-user-options-menu (&optional event)
  (interactive "e")
  (sgml-options-menu event sgml-user-options))

(defun sgml-options-menu (event vars)
  (let ((var
	 (let ((maxlen 
		(loop for var in vars
		      maximize (length (sgml-variable-description var)))))
	   (sgml-popup-menu
	    event "Options"
	    (loop for var in vars
		  for desc = (sgml-variable-description var)
		  collect
		  (cons
		   (format "%s%s [%s]"
			   desc
			   (make-string (- maxlen (length desc)) ? )
			   (sgml-option-value-indicator var))
		   var))))))
    (when var
      (sgml-do-set-option var event))))

;; Fixme: Use Customize for this.
(defun sgml-do-set-option (var &optional event)
  (let ((type (sgml-variable-type var))
	(val (symbol-value var)))
    (cond
     ((eq 'toggle type)
      (message "%s set to %s" var (not val))
      (set var (not val)))
     ((eq 'string type)
      (describe-variable var)
      (setq val (read-string (concat (sgml-variable-description var) ": ")))
      (when (stringp val)
	(set var val)))
     ((eq 'file-list  type)
      (describe-variable var)
      (sgml-append-to-help-buffer "\
Enter as many filenames as you want. Entering a directory 
or non-existing filename will exit the loop.")
      (setq val nil)
      (while (let ((next
		    (expand-file-name
		     (read-file-name
		      (concat (sgml-variable-description var) ": ")
		      nil "" nil nil))))
	       (if (and (file-exists-p next) (not (file-directory-p next)))
		   (setq val (cons next val)))))
      (set var val))
     ((eq 'file-or-nil type) 
      (describe-variable var)
      (sgml-append-to-help-buffer "\
Entering a directory or non-existing filename here
will reset the variable.")
      (setq val (expand-file-name
		 (read-file-name
		  (concat (sgml-variable-description var) ": ") 
		  nil (if (stringp val) (file-name-nondirectory val)) 
		  nil (if (stringp val) (file-name-nondirectory val)) )))
      (if (and (file-exists-p val) (not (file-directory-p val))) 
	  (set var val) 
	(set var nil)))   
     ((consp type)
      (let ((val
	     (sgml-popup-menu event
			      (sgml-variable-description var)
			      (loop for c in type collect
				    (cons
				     (if (consp c) (car c) (format "%s" c))
				     (if (consp c) (cdr c) c))))))
	(set var val)
	(message "%s set to %s" var val)))
     (t
      (describe-variable var)
      (setq val (read-string (concat (sgml-variable-description var)
				     " (sexp): ")))
      (when (stringp val)
	(set var (car (read-from-string val)))))))
  (force-mode-line-update))

(defun sgml-append-to-help-buffer (string)
  (save-excursion
    (set-buffer "*Help*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n" string))))

;;;; SGML mode: insert element where valid

(defun sgml--add-before-p (tok state child)
  ;; Can TOK be added in STATE followed by CHILD 
  (let ((snext (sgml-get-move state tok))
        (c child))
    (when snext
      (while c
        (setq snext (sgml-get-move snext
                                   (sgml-eltype-token
                                    (sgml-element-eltype c))))
        (setq c (and snext (sgml-element-next c)))))
    ;; If snext is still non nill it can be inserted 
    snext))

(defun sgml--all-possible-elements (el)
  (let ((c (sgml-element-content el))
        (s (sgml-element-model el))
        (found nil))
    (loop do
	  ;; Fixme: this test avoids an error when DTD-less, but it's
	  ;; probably an inappropriate kludge.  -- fx
          (when (not (eq s 'ANY))
	    (dolist (tok (nconc (sgml-optional-tokens s)
				(sgml-required-tokens s)))
	      (unless (memq tok found)
		;; tok is optional here and not already found -- check that
		;; it would not make the content invalid
		(when (sgml--add-before-p tok s c)
                  (push tok found)))))
          while c do
          (setq s (sgml-element-pstate c))
          (setq c (sgml-element-next c)))
    (mapcar #'sgml-token-eltype found)))


(defun sgml-add-element-to-element (gi first)
  "Add an element of type GI to the current element.
The element will be added at the last legal position if FIRST is `nil',
otherwise it will be added at the first legal position."
  (interactive
   (let ((tab
          (mapcar (lambda (et) (cons (sgml-eltype-name et) nil))
                  (sgml--all-possible-elements
                   (sgml-find-context-of (point))))))
     (cond ((null tab)
            (error "No element possible"))
           (t
            (let ((completion-ignore-case sgml-namecase-general))
              (list (completing-read "Element: " tab nil t
                                     (and (null (cdr tab)) (caar tab)))
                    current-prefix-arg))))))
  (let ((el (sgml-find-context-of (point)))
        (et (sgml-lookup-eltype (sgml-general-case gi))))
    ;; First expand empty tag
    (when (and sgml-xml-p (sgml-element-empty el))
      (save-excursion
	(goto-char (sgml-element-stag-end el))
	(delete-char -2)
	(insert ">\n" (sgml-end-tag-of sgml-current-tree))
	(sgml-indent-line))
      (setq el (sgml-find-context-of (point))))
    (let ((c (sgml-element-content el))
          (s (sgml-element-model el))
          (tok (sgml-eltype-token et))
          (last nil))
      ;; Find legal position for new element
      (while (and (not (cond
                        ((sgml--add-before-p tok s c)
                         (setq last (if c (sgml-element-start c)
                                      (sgml-element-etag-start el)))
                         first)))
                  (cond
                   (c (setq s (sgml-element-pstate c))
                      (setq c (sgml-element-next c))
                      t))))
      (cond (last
             (goto-char last)
             (sgml-insert-element gi))
            (t
             (error "A %s element is not valid in current element" gi))))))

;;;; Show current element type
;; Candidate for C-c C-t

(autoload 'sgml-princ-names "psgml-info")
(autoload 'sgml-eltype-refrenced-elements "psgml-info")

(defun sgml-show-current-element-type ()
  "Show information about the current element and its type."
  (interactive)
  (let* ((el (sgml-find-context-of (point)))
         (et (sgml-element-eltype el)))
    (with-output-to-temp-buffer "*Current Element Type*"
      (princ (format "ELEMENT: %s%s\n" (sgml-eltype-name et)
                     (let ((help-text (sgml-eltype-appdata et 'help-text)))
                       (if help-text
                           (format " -- %s" help-text)
                           ""))))
      (when sgml-omittag
        (princ (format "\n Start-tag is %s.\n End-tag is %s.\n"
                       (if (sgml-eltype-stag-optional et)
                           "optional" "required")
                       (if (sgml-eltype-etag-optional et)
                           "optional" "required"))))
      ;; ----
      (princ "\nCONTENT: ")
      (cond ((symbolp (sgml-eltype-model et)) (princ (sgml-eltype-model et)))
	    (t
	     (princ (if (sgml-eltype-mixed et)
                        "mixed\n"
                      "element\n"))
             (sgml-print-position-in-model el et (point) sgml-current-state)
             (princ "\n\n")
	     (sgml-princ-names
	      (mapcar #'symbol-name (sgml-eltype-refrenced-elements et))
              "All: ")))
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
      (princ "\n\nATTRIBUTES:\n")
      (sgml-print-attlist et)
      ;; ----
      (let ((s (sgml-eltype-shortmap et)))
	(when s
	  (princ (format "\nUSEMAP: %s\n" s))))
      ;; ----
      (princ "\nOCCURS IN:\n")
      (let ((occurs-in ()))
	(sgml-map-eltypes
	 (function (lambda (cand)
		     (when (memq et (sgml-eltype-refrenced-elements cand))
		       (push cand occurs-in))))
	 (sgml-pstate-dtd sgml-buffer-parse-state))
        (sgml-princ-names (mapcar 'sgml-eltype-name
                                  (sort occurs-in (function string-lessp))))))))

(defun sgml-print-attlist (et)
  (let ((ob (current-buffer)))
    (set-buffer standard-output)
    (unwind-protect
        (loop
         for attdecl in (sgml-eltype-attlist et) do
         (princ " ")
         (princ (sgml-attdecl-name attdecl))
         (let ((dval (sgml-attdecl-declared-value attdecl))
               (defl (sgml-attdecl-default-value attdecl)))
           (when (listp dval)
             (setq dval (concat (if (eq (first dval)
                                        'NOTATION)
                                    "#NOTATION (" "(")
                                (mapconcat (function identity)
                                           (second dval)
                                           "|")
                                ")")))
           (indent-to 15 1)
           (princ dval)
           (cond ((sgml-default-value-type-p 'FIXED defl)
                  (setq defl (format "#FIXED '%s'"
                                     (sgml-default-value-attval defl))))
                 ((symbolp defl)
                  (setq defl (upcase (format "#%s" defl))))
                 (t
                  (setq defl (format "'%s'"
                                     (sgml-default-value-attval defl)))))

           (indent-to 48 1)
           (princ defl)
           (terpri)))
      (set-buffer ob))))


(defun sgml-print-position-in-model (element element-type buffer-pos parse-state)
  (let ((u (sgml-element-content element))
        (names nil))
    (while (and u (>= buffer-pos (sgml-element-end u)))
      (push (sgml-element-gi u) names)
      (setq u (sgml-element-next u)))
    (when names
      (sgml-princ-names (nreverse names) " " ", ")
      (princ "\n")))
  (princ " ->")
  (let* ((state parse-state)
         (required-seq                  ; the seq of req el following point
          (loop for required = (sgml-required-tokens state)
                while (and required (null (cdr required)))
                collect (sgml-eltype-name (car required))
                do (setq state (sgml-get-move state (car required)))))
         (last-alt
          (mapcar 'sgml-eltype-name
                  (append (sgml-optional-tokens state)
                          (sgml-required-tokens state)))))
    (cond
     (required-seq
      (when last-alt
        (nconc required-seq
               (list (concat "("
                             (mapconcat (lambda (x) x)
                                        last-alt " | ")
                             (if (sgml-final state)
                                 ")?" ")")))))
      (sgml-princ-names required-seq " " ", "))

     (last-alt
      (sgml-princ-names last-alt " (" " | ")
      (princ ")")
      (when (sgml-final state)
        (princ "?"))))))


;;;; Structure Viewing and Navigating


(defun sgml-show-structure ()
  "Show the document structure in a separate buffer."
  (interactive)
  (let ((source (current-buffer))
        (result (get-buffer-create "*Document structure*")))
    (set-buffer result)
    (occur-mode)
    (erase-buffer)
    (let ((structure
           (save-excursion
             (set-buffer source)
             (sgml-structure-elements (sgml-top-element)))))
      (sgml-show-structure-insert structure))
    (goto-char (point-min))
    (display-buffer result)))


(defun sgml-show-structure-insert (structure)
  (loop for (gi level marker title) in structure do
       (let ((start (point)))
         (insert (make-string (* 2 level) ? ))
         (sgml-insert `(face match mouse-face highlight) gi)
         (sgml-insert `(mouse-face highlight) " %s" title)
         (insert "\n")
         (add-text-properties
          start (point)
          `(occur-target ,marker help-echo "mouse-2: go to this occurrence")))))
  

(defun sgml-show-struct-element-p (element)
  (let ((configured (sgml-element-appdata element 'structure)))
    (unless (eql configured 'ignore)
      (or configured
          (and (not (sgml-element-data-p element))
               (not (sgml-element-empty element)))))))


(defun sgml-structure-elements (element)
  (when (sgml-show-struct-element-p element)
    (let ((gi (sgml-element-gi element))
          (level (sgml-element-level element))
          (child1 (sgml-element-content element))
          (marker nil)
          (title ""))
      (goto-char (sgml-element-start element))
      (setq marker (copy-marker (point-marker)))
      (when (and child1
                 (not (sgml-show-struct-element-p child1))
                 (sgml-element-data-p child1))
        (let ((start-epos (sgml-element-stag-epos child1))
              (end-epos (sgml-element-etag-epos child1)))
          (when (and (sgml-bpos-p start-epos)
                     (sgml-bpos-p end-epos))
            (goto-char start-epos)
            (forward-char (sgml-element-stag-len child1))
            (when (looking-at "\\s-*$")
              (forward-line 1))
            (when (< (point) end-epos)
              (setq title
                    (buffer-substring (point)
                                      (min (line-end-position)
                                           end-epos)))))))
      (cons (list (sgml-general-insert-case gi)
                  level marker title)
            (loop for child = child1 then (sgml-element-next child)
               while child
               nconc (sgml-structure-elements child))))))


;;; psgml-edit.el ends here

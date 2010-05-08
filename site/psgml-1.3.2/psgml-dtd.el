;;;; psgml-dtd.el --- DTD parser for SGML-editing mode with parsing support
;; $Id: psgml-dtd.el,v 2.30 2003/03/25 19:46:09 lenst Exp $

;; Copyright (C) 1994 Lennart Staflin

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

(provide 'psgml-dtd)
(require 'psgml)
(require 'psgml-parse)
(eval-when-compile (require 'cl))

;;;; Variables

;; Variables used during doctype parsing and loading
(defvar sgml-used-pcdata nil
  "True if model group built is mixed.")


;;;; Constructing basic

(defun sgml-copy-moves (s1 s2)
  "Copy all moves from S1 to S2, keeping their status."
  (let ((l (sgml-state-opts s1)))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (sgml-state-reqs s1))
    (while l
      (sgml-add-req-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))))

(defun sgml-copy-moves-to-opt (s1 s2)
  "Copy all moves from S1 to S2 as optional moves."
  (let ((l (sgml-state-opts s1)))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))
    (setq l (sgml-state-reqs s1))
    (while l
      (sgml-add-opt-move s2
			 (sgml-move-token (car l))
			 (sgml-move-dest (car l)))
      (setq l (cdr l)))))


(defun sgml-some-states-of (state)
  ;; List of some states reachable from STATE, includes all final states
  (let* ((states (list state))
	 (l states)
	 s ms m)
    (while l
      (setq s (car l)
	    ms (append (sgml-state-opts s) (sgml-state-reqs s)))
      (while ms
	(setq m (sgml-move-dest (car ms))
	      ms (cdr ms))
	(unless (sgml-normal-state-p m)
	  (setq m (sgml-and-node-next m)))
	(unless (memq m states)
	  (nconc states (list m))))
      (setq l (cdr l)))
    states))

(defmacro sgml-for-all-final-states (s dfa &rest forms)
  "For all final states S in DFA do FORMS.
Syntax: var dfa-expr &body forms"
  (` (let ((L-states (sgml-some-states-of (, dfa)))
	   (, s))
       (while L-states
	 (when (sgml-state-final-p (setq (, s) (car L-states)))
	   (,@ forms))
	 (setq L-states (cdr L-states))))))

(put 'sgml-for-all-final-states 'lisp-indent-hook 2)
(put 'sgml-for-all-final-states 'edebug-form-hook '(symbolp &rest form))


;;;; Optimization for the dfa building

(defsubst sgml-empty-state-p (s)
  ;; True if S has no outgoing moves
  (and (sgml-normal-state-p s)
       (null (sgml-state-reqs s))
       (null (sgml-state-opts s)))  )

(defun sgml-one-final-state (s)
  ;; Collapse all states that have no moves
  ;; This is a safe optimization, useful for (..|..|..)
  (sgml-debug "OPT one final: reqs %d opts %d"
	      (length (sgml-state-reqs s))
	      (length (sgml-state-opts s)))
  (let ((final nil)
	dest)
    (loop for m in (append (sgml-state-reqs s)
			   (sgml-state-opts s))
	  do
	  (setq dest (sgml-move-dest m))
	  (when (sgml-empty-state-p dest)
	    (cond ((null final)
		   (setq final dest))
		  (t
		   (setf (sgml-move-dest m) final)))))))

(defun sgml-states-equal (s1 s2)
  (and (= (length (sgml-state-opts s1))
	  (length (sgml-state-opts s2)))
       (= (length (sgml-state-reqs s1))
	  (length (sgml-state-reqs s2)))
       (loop for m in (sgml-state-opts s1)
	     always
	     (eq (sgml-move-dest m)
		 (sgml-move-dest (sgml-moves-lookup (sgml-move-token m)
						    (sgml-state-opts s2)))))
       (loop for m in (sgml-state-reqs s1)
	     always
	     (eq (sgml-move-dest m)
		 (sgml-move-dest (sgml-moves-lookup (sgml-move-token m)
						    (sgml-state-reqs s2)))))))

(defun sgml-remove-redundant-states-1 (s)
  ;; Remove states accessible from s with one move and equivalent to s,
  ;; by changing the moves from s.
  (sgml-debug "OPT redundant-1: reqs %d opts %d"
	      (length (sgml-state-reqs s))
	      (length (sgml-state-opts s)))
  (let ((yes nil)
	(no (list s))
	(l (sgml-state-reqs s))
	(nl (sgml-state-opts s))
	dest)
    (while (or l (setq l (prog1 nl (setq nl nil))))
      (cond
       ((not (sgml-normal-state-p (setq dest (sgml-move-dest (car l))))))
       ((memq dest no))
       ((memq dest yes))
       ((sgml-states-equal s dest)
	(progn (push dest yes))))
      (setq l (cdr l)))
    (setq l (sgml-state-opts s)
	  nl (sgml-state-reqs s))
    (when yes
      (sgml-debug "OPT redundant-1: success %s" (length yes))
      (while (or l (setq l (prog1 nl (setq nl nil))))
	(cond ((memq (sgml-move-dest (car l)) yes)
	       (setf (sgml-move-dest (car l)) s)))
	(setq l (cdr l))))))
	  


;;;; Constructing

(defun sgml-make-opt (s1)
  (when (sgml-state-reqs s1)
    (setf (sgml-state-opts s1)
	  (nconc (sgml-state-opts s1)
		 (sgml-state-reqs s1)))
    (setf (sgml-state-reqs s1) nil))
  s1)

(defun sgml-make-* (s1)
  (setq s1 (sgml-make-+ s1))
  (when (sgml-state-reqs s1)
    (sgml-make-opt s1))
  (sgml-remove-redundant-states-1 s1)
  s1)

(defun sgml-make-+ (s1)
  (sgml-for-all-final-states s s1
    (sgml-copy-moves-to-opt s1 s))
  (sgml-remove-redundant-states-1 s1)	; optimize
  s1)

(defun sgml-make-conc (s1 s2)
  (let ((moves (append (sgml-state-reqs s1) (sgml-state-opts s1))))
    (cond
     (;; optimize the case where all moves from s1 goes to empty states
      (loop for m in moves
	    always (sgml-empty-state-p (sgml-move-dest m)))
      (loop for m in moves do (setf (sgml-move-dest m) s2))
      (when (sgml-state-final-p s1)
	(sgml-copy-moves s2 s1)))
     (t					; general case
      (sgml-for-all-final-states s s1
	(sgml-copy-moves s2 s)
	(sgml-remove-redundant-states-1 s)))))
  s1)

(defun sgml-make-pcdata ()
  (sgml-make-* (sgml-make-primitive-content-token sgml-pcdata-token)))

(defun sgml-reduce-, (l)
  (while (cdr l)
    (setcar (cdr l)
	    (sgml-make-conc (car l) (cadr l)))
    (setq l (cdr l)))
  (car l))

(defun sgml-reduce-| (l)
  (while (cdr l)			; apply the binary make-alt
    (cond ((or (sgml-state-final-p (car l))	; is result optional
	       (sgml-state-final-p (cadr l)))
	   (sgml-make-opt (car l))
	   (sgml-copy-moves-to-opt (cadr l) (car l)))
	  (t
	   (sgml-copy-moves (cadr l) (car l))))
    (setcdr l (cddr l)))
  (sgml-one-final-state (car l))	; optimization
  (car l))

(defun sgml-make-& (dfas)
  (let ((&n (sgml-make-and-node dfas (sgml-make-state)))
	(s (sgml-make-state))
	(l dfas))
    (while l				; For each si:
      ;; For m in opts(si): add optional move from s to &n on token(m).
      (loop for m in (sgml-state-opts (car l))
	    do (sgml-add-opt-move s (sgml-move-token m) &n))
      ;; For m in reqs(si): add required move from s to &n on token(m).
      (loop for m in (sgml-state-reqs (car l))
	    do (sgml-add-req-move s (sgml-move-token m) &n))
      (setq l (cdr l)))
    ;; Return s.
    s))



;(sgml-make-conc (sgml-make-primitive-content-token 'para) (sgml-make-primitive-content-token 'list))
;(sgml-make-conc (sgml-make-& (list (sgml-make-primitive-content-token 'para) (sgml-make-primitive-content-token 'list))) (sgml-make-primitive-content-token 'foo))

;(setq x  (sgml-some-states-of  (sgml-make-primitive-content-token 'para)))
;(sgml-state-final-p (car x) )
;(sgml-state-final-p (cadr x))


;;;; Parse doctype: General

(defun sgml-skip-ts ()
  ;; Skip over ts*
  ;;70  ts   = 5 s | EE | 60+ parameter entity reference
  ;;For simplicity I use ps*
  ;;65  ps   = 5 s | EE | 60+ parameter entity reference | 92 comment
  ;;*** some comments are accepted that shouldn't
  (sgml-skip-ps))

(defun sgml-parse-character-reference (&optional dofunchar)
  ;; *** Actually only numerical character references
  ;; I don't know how to handel the function character references.
  ;; For the shortrefs let's give them numeric values.
  (if (if dofunchar
	  (sgml-parse-delim "CRO" (digit nmstart))
	(sgml-parse-delim "CRO" (digit)))
      (prog1 (if (sgml-is-delim "NULL" digit)
		 (string-to-int (sgml-check-nametoken))
	       (let ((spec (sgml-check-name)))
		 (or (cdr (assoc spec '(("RE" . 10)
					("RS" . 1)
					("TAB" . 9)
					("SPACE" . 32))))
		     ;; *** What to do with other names?
		     127)))
	(or (sgml-parse-delim "REFC")
	    (sgml-parse-RE)))))

(defun sgml-parse-parameter-literal (&optional dofunchar)
  (let* (lita				; flag if lita
	 (value				; accumulates literals value
	  "")
	 (original-buffer		; Buffer (entity) where lit started
	  (current-buffer))
	 temp)
    (cond
     ((or (sgml-parse-delim "LIT")
	  (setq lita (sgml-parse-delim "LITA")))
      (while (not (and (eq (current-buffer) original-buffer)
		       (if lita
			   (sgml-parse-delim "LITA")
			 (sgml-parse-delim "LIT"))))
	(cond ((eobp)
	       (or (sgml-pop-entity)
		   (sgml-error "Parameter literal unterminated")))
	      ((sgml-parse-parameter-entity-ref))
	      ((setq temp (sgml-parse-character-reference dofunchar))
	       (setq value
                     (concat value
                             (cond ((< temp 256)
                                    (if enable-multibyte-characters
                                        (setq temp (unibyte-char-to-multibyte temp)))
                                    (format "%c" temp))
                                   (t
                                    (format "&#%d;" temp))))))
	      (t
	       (setq value
		     (concat value
			     (buffer-substring-no-properties
			      (point)
			      (progn (forward-char 1)
				     (if lita
					 (sgml-skip-upto ("LITA" "PERO" "CRO"))
				       (sgml-skip-upto ("LIT" "PERO" "CRO")))
				     (point))))))))
      value))))

(defun sgml-check-parameter-literal ()
  (or (sgml-parse-parameter-literal)
      (sgml-parse-error "Parameter literal expected")))

(defsubst sgml-parse-connector ()
  (sgml-skip-ps)
  (cond ((sgml-parse-delim "SEQ")
	 (function sgml-reduce-,))
	((sgml-parse-delim "OR")
	 (function sgml-reduce-|))
	((sgml-parse-delim "AND")
	 (if sgml-xml-p
	     (sgml-error "XML forbids AND connector")
	   (function sgml-make-&)))))

(defun sgml-parse-name-group ()
  "Parse a single name or a name group (general name case) .
Returns a list of strings or nil."
  (let (names)
    (cond
     ((sgml-parse-delim "GRPO")
      (sgml-skip-ps)
      (setq names (sgml-parse-name-group)) ; *** Allows more than it should
      (while (sgml-parse-connector)
	(sgml-skip-ps)
	(nconc names (sgml-parse-name-group)))
      (sgml-check-delim "GRPC")
      names)
     ((setq names (sgml-parse-name))
      (list names)))))

(defun sgml-check-name-group ()
  (or (sgml-parse-name-group)
      (sgml-parse-error "Expecting a name or a name group")))

(defun sgml-check-nametoken-group ()
  "Parse a name token group, return a list of strings.
Case transformed for general names."
  (sgml-skip-ps)
  (let ((names nil))
    (cond
     ((sgml-parse-delim GRPO)
      (while (progn
	       (sgml-skip-ps)
	       (push (sgml-general-case (sgml-check-nametoken)) names)
	       (sgml-parse-connector)))
      (sgml-check-delim GRPC)
      (nreverse names))			; store in same order as declared
     (t
      (list (sgml-general-case (sgml-check-nametoken)))))))

(defun sgml-check-element-type ()
  "Parse and check an element type, return list of strings."
;;; 117  element type     =  [[30 generic identifier]]
;;;                      |  [[69 name group]]
;;;                      |  [[118 ranked element]]
;;;                      |  [[119 ranked group]]
  (cond
   ((sgml-parse-delim GRPO)
    (when sgml-xml-p
      (sgml-error "XML forbids name groups for the element type"))
    (sgml-skip-ts)
    (let ((names (list (sgml-check-name))))
      (while (progn (sgml-skip-ts)
		    (sgml-parse-connector))
	(sgml-skip-ts)
	(nconc names (list (sgml-check-name))))
      (sgml-check-delim GRPC)
      ;; A ranked group will have a rank suffix here
      (sgml-skip-ps)
      (if (sgml-is-delim "NULL" digit)
	(let ((suffix (sgml-parse-nametoken)))
	  (loop for n in names
		collect (concat n suffix)))
	names)))
   (t					; gi/ranked element
    (let ((name (sgml-check-name)))
      (sgml-skip-ps)
      (list (if (sgml-is-delim "NULL" digit)
		(concat name (sgml-check-nametoken))
	      name))))))


(defun sgml-check-external (&optional pubid-ok)
  (or (sgml-parse-external pubid-ok)
      (sgml-parse-error "Expecting a PUBLIC or SYSTEM")))

;;;; Parse doctype: notation

(defun sgml-declare-notation ()
  ;;148  notation declaration = MDO, "NOTATION",
  ;;                        65 ps+, 41 notation name,
  ;;                        65 ps+, 149 notation identifier,
  ;;                        65 ps*, MDC
  ;;41   notation name    = 55 name
  ;;149  notation identifier = 73 external identifier
  (sgml-skip-ps)
  (sgml-check-name)
  (sgml-skip-ps)
  (sgml-check-external t))


;;;; Parse doctype: Element

(defun sgml-parse-opt ()
  (sgml-skip-ps)
  (cond ((or (sgml-parse-char ?o)
	     (sgml-parse-char ?O))
	 (if sgml-xml-p
	      (sgml-error "XML forbids omitted tag minimization.")
	   t))
	((sgml-parse-char ?-)
	 (if sgml-xml-p
	     (sgml-error "XML forbids omitted tag minimization")
	   nil))))

(defun sgml-parse-modifier ()
  (cond ((sgml-parse-delim "PLUS")
	 (function sgml-make-+))
	((sgml-parse-delim "REP")
	 (function sgml-make-*))
	((sgml-parse-delim "OPT")
	 (function sgml-make-opt))))

(defun sgml-check-primitive-content-token ()
  (sgml-make-primitive-content-token
   (sgml-eltype-token
    (sgml-lookup-eltype
     (sgml-check-name)))))

(defun sgml-check-model-group ()
  (sgml-skip-ps)
  (let (el mod)
    (cond
     ((sgml-parse-delim "GRPO")
      (let ((subs (list (sgml-check-model-group)))
	    (con1 nil)
	    (con2 nil))
	(while (setq con2 (sgml-parse-connector))
	  (cond ((and con1
		      (not (eq con1 con2)))
		 (sgml-parse-error "Mixed connectors")))
	  (setq con1 con2)
	  (setq subs (nconc subs (list (sgml-check-model-group)))))
	(sgml-check-delim "GRPC")
	(setq el (if con1
		     (funcall con1 subs)
		   (car subs)))))
     ((sgml-parse-rni "PCDATA")         ; #PCDATA (FIXME: when changing case)
      (setq sgml-used-pcdata t)
      (setq el (sgml-make-pcdata)))
     ((sgml-parse-delim "DTGO")			; data tag group
      (when sgml-xml-p
	(sgml-error "XML forbids DATATAG"))
      (sgml-skip-ts)
      (let ((tok (sgml-check-primitive-content-token)))
	(sgml-skip-ts) (sgml-check-delim "SEQ")
	(sgml-skip-ts) (sgml-check-data-tag-pattern)
	(sgml-skip-ts) (sgml-check-delim "DTGC")
	(setq el (sgml-make-conc tok (sgml-make-pcdata)))
	(setq sgml-used-pcdata t)))
     (t
      (setq el (sgml-check-primitive-content-token))))
    (setq mod (sgml-parse-modifier))
    (if mod
	(funcall mod el)
      el)))

(defun sgml-check-data-tag-pattern ()
  ;; 134  data tag pattern
  ;; template | template group
  (cond ((sgml-parse-delim GRPO)
	 (sgml-skip-ts)
	 (sgml-check-parameter-literal)	; data tag template,
	 (while (progn (sgml-skip-ts)
		       (sgml-parse-delim OR))
	   (sgml-skip-ts)
	   (sgml-check-parameter-literal)) ; data tag template
	 (sgml-skip-ts)
	 (sgml-check-delim GRPC))
	(t
	 (sgml-check-parameter-literal))) ; data tag template
  (sgml-skip-ts)
  (when (sgml-parse-delim SEQ)
    (sgml-check-parameter-literal)))	; data tag padding template

(defun sgml-check-content-model ()
  (sgml-check-model-group))

(defun sgml-check-content ()
  (sgml-skip-ps)
  (cond ((sgml-is-delim GRPO)
	 (sgml-check-content-model))
	(t
	 ;; ANY, CDATA, RCDATA or EMPTY
	 (let ((dc (intern (sgml-check-case (sgml-check-name)))))
	   (cond ((eq dc 'ANY)
		  (setq sgml-used-pcdata t))
		 ((eq dc 'CDATA)
		  (when sgml-xml-p
		    (sgml-error "XML forbids CDATA declared content")))
		 ((eq dc 'RCDATA)
		  (when sgml-xml-p
		    (sgml-error "XML forbids RCDATA declared content")))
                 ((eq dc 'EMPTY))
                 (t
                  (sgml-error "Exptected content model group or one of %s"
                              (if sgml-xml-p
                                  "ANY or EMPTY"
                                  "ANY, CDATA, RCDATA or EMPTY"))))
	   dc))))

(defun sgml-parse-exception (type)
  (sgml-skip-ps)
  (if (sgml-parse-char type)
      (if sgml-xml-p
	   (sgml-error "XML forbids inclusion and exclusion exceptions")
	(mapcar (function sgml-lookup-eltype)
		(sgml-check-name-group)))))

(defun sgml-before-eltype-modification ()
;;;  (let ((merged (sgml-dtd-merged sgml-dtd-info)))
;;;    (when (and merged
;;;	       (eq (sgml-dtd-eltypes sgml-dtd-info)
;;;		   (sgml-dtd-eltypes (cdr merged))))
;;;      (setf (sgml-dtd-eltypes sgml-dtd-info)
;;;	    (sgml-merge-eltypes (sgml-make-eltypes-table)
;;;				(sgml-dtd-eltypes sgml-dtd-info)))))
  )

(defun sgml-declare-element ()
  (let* ((names (sgml-check-element-type))
	 (stag-opt (sgml-parse-opt))
	 (etag-opt (sgml-parse-opt))
	 (sgml-used-pcdata nil)
	 (model (sgml-check-content))
	 (exclusions (sgml-parse-exception ?-))
	 (inclusions (sgml-parse-exception ?+)))
    (sgml-before-eltype-modification)
    (while names
      (sgml-debug "Defining element %s" (car names))
      (let ((et (sgml-lookup-eltype (car names))))
	(setf (sgml-eltype-stag-optional et) stag-opt
	      (sgml-eltype-etag-optional et) etag-opt
	      (sgml-eltype-model et) model
	      (sgml-eltype-mixed et) sgml-used-pcdata
	      (sgml-eltype-excludes et) exclusions
	      (sgml-eltype-includes et) inclusions))
      (setq names (cdr names)))
    (sgml-lazy-message "Parsing doctype (%s elements)..."
		       (incf sgml-no-elements))))

;;;; Parse doctype: Entity

(defun sgml-declare-entity ()
  (let (name				; Name of entity
	dest				; Entity table
	(type 'text)			; Type of entity
	(notation nil)                  ; Notation of entity
	text				; Text of entity
	extid				; External id
	)
    (cond
     ((sgml-parse-delim "PERO")		; parameter entity declaration
      (sgml-skip-ps)
      (setq name (sgml-check-name t))
      (setq dest (sgml-dtd-parameters sgml-dtd-info)))
     (t					; normal entity declaration
      (or (sgml-parse-rni "DEFAULT")
	  (setq name (sgml-check-name t)))
      (setq dest (sgml-dtd-entities sgml-dtd-info))))
    (sgml-skip-ps)
    ;;105  entity text  = 66 parameter literal
    ;;                 | 106 data text
    ;;                 | 107 bracketed text
    ;;                 | 108 external entity specification
    (setq extid (sgml-parse-external))
    (setq text
	  (cond
	   (extid			; external entity specification =
					; 73 external identifier,
					; (65 ps+, 109+ entity type)?
	    (sgml-skip-ps)
	    (let ((tn (sgml-parse-entity-type)))
	      (setq type (or (car tn) 'text))
	      (unless (eq (cdr tn) "")
		(setq notation (cdr tn))))
	    extid)
	   ((sgml-startnm-char-next)
	    (let ((token (intern (sgml-check-case (sgml-check-name)))))
	      (sgml-skip-ps)
	      (when (and sgml-xml-p
			 (memq token '(CDATA SDATA PI STARTTAG ENDTAG MS MD)))
		(sgml-error "XML forbids %s entities"
			    (upcase (symbol-name token))))
	      (cond
	       ((memq token '(CDATA SDATA)) ; data text ***
		(setq type token)
		(sgml-check-parameter-literal))
	       ((eq token 'PI)
		(concat "<?" (sgml-check-parameter-literal) ">"))
	       ((eq token 'STARTTAG)
		(sgml-start-tag-of (sgml-check-parameter-literal)))
	       ((eq token 'ENDTAG)
		(sgml-end-tag-of (sgml-check-parameter-literal)))
	       ((eq token 'MS)		; marked section
		(concat "<![" (sgml-check-parameter-literal) "]]>"))
	       ((eq token 'MD)		; Markup declaration
		(concat "<!" (sgml-check-parameter-literal) ">")))))
	   ((sgml-check-parameter-literal))))
    (when dest
      (sgml-entity-declare name dest type text notation))))


(defun sgml-parse-entity-type ()
  ;;109+ entity type      = "SUBDOC"
  ;;                      | (("CDATA" | "NDATA" | "SDATA"),
  ;;                             65 ps+,
  ;;                             41 notation name,
  ;;                             149.2+ data attribute specification?)
  (let ((type (sgml-parse-name))
	(notation nil))
    (when type
      (setq type (intern (sgml-check-case type)))
      (when (and sgml-xml-p (memq type '(SUBDOC CDATA SDATA)))
	(sgml-error "XML forbids %s entities"
		    (upcase (symbol-name type))))
      (cond ((eq type 'SUBDOC))
	    ((memq type '(CDATA NDATA SDATA))
	     (sgml-skip-ps)
	     (setq notation (sgml-parse-name))
	     ;;149.2+ data attribute specification
	     ;;                      = 65 ps+, DSO,
	     ;;                        31 attribute specification list,
	     ;;                        5 s*, DSC
	     (sgml-skip-ps)
	     (when (sgml-parse-delim DSO)
	       (sgml-parse-attribute-specification-list)
	       (sgml-parse-s)
	       (sgml-check-delim DSC)))
	    (t (sgml-error "Illegal entity type: %s" type))))
    (cons type notation)))


;;;; Parse doctype: Attlist

(defun sgml-declare-attlist ()
  (let* ((assnot (cond ((sgml-parse-rni "NOTATION")
			(when sgml-xml-p
			  (sgml-error "XML forbids data attribute declarations"))
			(sgml-skip-ps)
			t)))
	 (assel (sgml-check-name-group))
	 (attlist nil)
	 (attdef nil))
    (when (and sgml-xml-p (> (length assel) 1))
      (sgml-error "XML forbids name groups for an associated element type"))
    (while (setq attdef (sgml-parse-attribute-definition))
      (push attdef attlist))
    (setq attlist (nreverse attlist))
    (unless assnot
      (sgml-before-eltype-modification)
      (loop for elname in assel do
	    (setf (sgml-eltype-attlist (sgml-lookup-eltype elname))
		  (sgml-merge-attlists
		   (sgml-eltype-attlist
		    (sgml-lookup-eltype elname))
		   attlist))))))

(defun sgml-merge-attlists (old new)
  (setq old (nreverse (copy-sequence old)))
  (loop for att in new do
	(unless (assoc (car att) old)
	  (setq old (cons att old))))
  (nreverse old))

(defun sgml-parse-attribute-definition ()
  (sgml-skip-ps)
  (if (sgml-is-delim "MDC") ; End of attlist?
      nil
    (sgml-make-attdecl (sgml-check-name)
		       (sgml-check-declared-value)
		       (sgml-check-default-value))))

(defun sgml-check-declared-value ()
  (sgml-skip-ps)
  (let ((type 'name-token-group)
	(names nil))
    (unless (eq (following-char) ?\()
      (setq type (intern (sgml-check-case (sgml-check-name))))
      (sgml-validate-declared-value type)
      (sgml-skip-ps))
    (when (memq type '(name-token-group NOTATION))
      (setq names (sgml-check-nametoken-group)))
    (sgml-make-declared-value type names)))

(defun sgml-validate-declared-value (type)
  (unless (memq type
		'(CDATA
		  ENTITY
		  ENTITIES
		  ID
		  IDREF
		  IDREFS
		  NAME
		  NAMES
		  NMTOKEN
		  NMTOKENS
		  NOTATION
		  NUMBER
		  NUMBERS
		  NUTOKEN
		  NUTOKENS))
    (sgml-error "Invalid attribute declared value: %s" type))
  (when (and sgml-xml-p (memq type
			      '(NAME NAMES NUMBER NUMBERS NUTOKEN NUTOKENS)))
    (sgml-error "XML forbids %s attributes" (upcase (symbol-name type)))))

(defun sgml-check-default-value ()
  (sgml-skip-ps)
  (let* ((rni (sgml-parse-rni))
	 (key (if rni (intern (sgml-check-case (sgml-check-name))))))
    (if rni (sgml-validate-default-value-rn key))
    (sgml-skip-ps)
    (sgml-make-default-value
     key
     (if (or (not rni) (eq key 'FIXED))
	 (sgml-check-attribute-value-specification)))))

(defun sgml-validate-default-value-rn (rn)
  (unless (memq rn '(REQUIRED FIXED CURRENT CONREF IMPLIED))
    (sgml-error "Unknown reserved name: %s"
		(upcase (symbol-name rn))))
  (when (and sgml-xml-p (memq rn '(CURRENT CONREF)))
    (sgml-error "XML forbids #%s attributes"
		(upcase (symbol-name rn)))))
  


;;;; Parse doctype: Shortref

;;;150  short reference mapping declaration = MDO, "SHORTREF",
;;;                        [[65 ps]]+, [[151 map name]],
;;;                        ([[65 ps]]+, [[66 parameter literal]],
;;;                        [[65 ps]]+, [[55 name]])+,
;;;                        [[65 ps]]*, MDC

(defun sgml-declare-shortref ()
  (let ((mapname (sgml-check-name))
	mappings literal name)
    (while (progn
	     (sgml-skip-ps)
	     (setq literal (sgml-parse-parameter-literal 'dofunchar)))
      (sgml-skip-ps)
      (setq name (sgml-check-name t))
      (push (cons literal name) mappings))
    (sgml-add-shortref-map
     (sgml-dtd-shortmaps sgml-dtd-info)
     mapname
     (sgml-make-shortmap mappings))))

;;;152  short reference use declaration = MDO, "USEMAP",
;;;                        [[65 ps]]+, [[153 map specification]],
;;;                        ([[65 ps]]+, [[72 associated element type]])?,
;;;                        [[65 ps]]*, MDC

(defun sgml-do-usemap-element (mapname)
  ;; This is called from sgml-do-usemap with the mapname
  (sgml-before-eltype-modification)
  (loop for e in (sgml-parse-name-group) do
	(setf (sgml-eltype-shortmap (sgml-lookup-eltype e sgml-dtd-info))
	      (if (null mapname)
		  'empty
		mapname))))


;;;; Parse doctype

(defun sgml-check-dtd-subset ()
  (let ((sgml-parsing-dtd t)
	(eref sgml-current-eref))
    (while
	(progn
	  (setq sgml-markup-start (point))
	  (cond
	   ((and (eobp) (eq sgml-current-eref eref))
	    nil)
	   ((sgml-parse-ds))
	   ((sgml-parse-markup-declaration 'dtd))
	   ((sgml-parse-delim "MS-END")))))))


;;;; Save DTD: compute translation

(defvar sgml-translate-table nil)

(defun sgml-translate-node (node)
  (assert (not (numberp node)))
  (let ((tp (assq node sgml-translate-table)))
    (unless tp
      (setq tp (cons node (length sgml-translate-table)))
      (nconc sgml-translate-table (list tp)))
    (cdr tp)))

(defun sgml-translate-moves (moves)
  (while moves
    (sgml-translate-node (sgml-move-dest (car moves)))
    (setq moves (cdr moves))))

(defun sgml-translate-model (model)
  (let* ((sgml-translate-table (list (cons model 0)))
	 (p sgml-translate-table))
    (while p
      (cond ((sgml-normal-state-p (caar p))
	     (sgml-translate-moves (sgml-state-opts (caar p)))
	     (sgml-translate-moves (sgml-state-reqs (caar p))))
	    (t
	     (sgml-translate-node (sgml-and-node-next (caar p)))))
      (setq p (cdr p)))
    sgml-translate-table))

;;;; Save DTD: binary coding

(defvar sgml-code-token-numbers nil)
(defvar sgml-code-xlate nil)

(defsubst sgml-code-xlate (node)
  ;;(let ((x (cdr (assq node sgml-code-xlate)))) (assert x) x)
  (cdr (assq node sgml-code-xlate)))

(defun sgml-code-number (num)
  (if (> num sgml-max-single-octet-number)
      (insert (+ (lsh (- num sgml-max-single-octet-number) -8)
		 sgml-max-single-octet-number 1)
	      (logand (- num sgml-max-single-octet-number) 255))
    (insert num)))

(defun sgml-code-token-number (token)
  (let ((bp (assq token sgml-code-token-numbers)))
    (unless bp
      (setq sgml-code-token-numbers
	    (nconc sgml-code-token-numbers
		   (list (setq bp (cons token
					(length sgml-code-token-numbers)))))))
    (cdr bp)))

(defun sgml-code-token (token)
  (sgml-code-number (sgml-code-token-number token)))

(defmacro sgml-code-sequence (loop-c &rest body)
  "Produce the binary coding of a counted sequence from a list.
Syntax: (var seq) &body forms
FORMS should produce the binary coding of element in VAR."
  (let ((var (car loop-c))
	(seq (cadr loop-c)))
    (` (let ((seq (, seq)))
	 (sgml-code-number (length seq))
	 (loop for (, var) in seq
	       do (,@ body))))))

(put 'sgml-code-sequence 'lisp-indent-hook 1)
(put 'sgml-code-sequence 'edbug-forms-hook '(sexp &rest form))

(defun sgml-code-sexp (sexp)
  (let ((standard-output (current-buffer)))
    (prin1 sexp)
    (terpri)))

(defun sgml-code-tokens (l)
  (sgml-code-sequence (x l)
    (sgml-code-token x)))

(defsubst sgml-code-move (m)
  (sgml-code-token (sgml-move-token m))
  (insert (sgml-code-xlate (sgml-move-dest m))))

(defun sgml-code-model (m)
  (let ((sgml-code-xlate (sgml-translate-model m)))
    (sgml-code-sequence (s sgml-code-xlate)		; s is (node . number)
      (setq s (car s))			; s is node
      (cond
       ((sgml-normal-state-p s)
	(assert (and (< (length (sgml-state-opts s)) 255)
		     (< (length (sgml-state-reqs s)) 256)))
	(sgml-code-sequence (x (sgml-state-opts s))
	  (sgml-code-move x))
	(sgml-code-sequence (x (sgml-state-reqs s))
	  (sgml-code-move x)))
       (t				; s is a &-node
	(insert 255)			; Tag &-node
	(insert (sgml-code-xlate (sgml-and-node-next s)))
	(sgml-code-sequence (m (sgml-and-node-dfas s))
	  (sgml-code-model m)))))))

(defun sgml-code-element (et)
  (sgml-code-sexp (sgml-eltype-all-miscdata et))
  (cond
   ((not (sgml-eltype-defined et))
    (insert 128))
   (t
    (insert (sgml-eltype-flags et))
    (let ((c (sgml-eltype-model et)))
      (cond ((eq c sgml-cdata) (insert 0))
	    ((eq c sgml-rcdata) (insert 1))
	    ((eq c sgml-empty) (insert 2))
	    ((eq c sgml-any) (insert 3))
	    ((null c) (insert 4))
	    (t
	     (assert (sgml-model-group-p c))
	     (insert 128)
	     (sgml-code-model c))))
    (sgml-code-tokens (sgml-eltype-includes et))
    (sgml-code-tokens (sgml-eltype-excludes et)))))


(defun sgml-code-dtd (dtd)
  "Produce the binary coding of the current DTD into the current buffer."
  (sgml-code-sexp (sgml-dtd-dependencies dtd))
  (sgml-code-sexp (sgml-dtd-parameters dtd))
  (sgml-code-sexp (sgml-dtd-doctype dtd))
  (let ((done 0)			; count written elements
	tot)
    (setq sgml-code-token-numbers nil)
    (sgml-code-token-number sgml-pcdata-token) ; Make #PCDATA token 0
    (sgml-map-eltypes			; Assign numbers to all tokens
     (function (lambda (et)
		 (sgml-code-token-number (sgml-eltype-token et))))
     dtd nil t)
    (setq tot (length sgml-code-token-numbers))
    ;; Produce the counted sequence of element type names
    (sgml-code-sequence (pair (cdr sgml-code-token-numbers))
      (sgml-code-sexp (sgml-eltype-name (car pair))))
    ;; Produce the counted sequence of element types
    (sgml-code-sequence (pair (cdr sgml-code-token-numbers))
      (setq done (1+ done))
      (sgml-code-element (car pair))
      (sgml-lazy-message "Saving DTD %d%% done" (/ (* 100 done) tot)))
    (sgml-code-sexp (sgml-dtd-entities dtd))
    (sgml-code-sexp (sgml-dtd-shortmaps dtd))
    (sgml-code-sexp (sgml-dtd-notations dtd))))


;;;; Save DTD

(defun sgml-save-dtd (file)
  "Save the parsed dtd on FILE."
  (interactive
   (let* ((tem (expand-file-name
		(or sgml-default-dtd-file
		    (sgml-default-dtd-file))))
	  (dir (file-name-directory tem))
	  (nam (file-name-nondirectory tem)))
     (list
      (read-file-name "Save DTD in: " dir tem nil nam))))
  (setq file (expand-file-name file))
  (when (equal file (buffer-file-name))
    (error "Would clobber current file"))
  (sgml-need-dtd)
  (sgml-push-to-entity (sgml-make-entity "#SAVE" nil ""))
  (sgml-write-dtd sgml-dtd-info file)
  (sgml-pop-entity)
  (setq sgml-default-dtd-file
	(if (equal (expand-file-name default-directory)
		   (file-name-directory file))
	    (file-name-nondirectory file)
	  file))
  (setq sgml-loaded-dtd file))

(defun sgml-write-dtd (dtd file)
  "Save the parsed DTD in FILE.
Construct the binary coded DTD (bdtd) in the current buffer."
  (sgml-set-buffer-multibyte nil)
  (insert
   ";;; This file was created by psgml on " (current-time-string)
   " -*-coding:binary-*-\n"
   "(sgml-saved-dtd-version 7)\n")
  (let ((print-escape-multibyte t))
    (sgml-code-dtd dtd))
  (set 'file-type 1)
  (let ((coding-system-for-write 'no-conversion))
    (write-region (point-min) (point-max) file)))


;;; psgml-dtd.el ends here

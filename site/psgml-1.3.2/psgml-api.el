;;; psgml-api.el --- Extra API functions for PSGML
;; $Id: psgml-api.el,v 1.8 2002/04/25 20:50:27 lenst Exp $

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


;;; Commentary:

;; Provides some extra functions for the API to PSGML.


;;; Code:

(provide 'psgml-api)
(require 'psgml)
(require 'psgml-parse)
(eval-when-compile (require 'cl))

;;;; Mapping: map and modify

(defun sgml-map-element-modify (el-fun element)
  "Apply EL-FUN to ELEMENT and the elements in its content.
The EL-FUN may change the buffer.  But if it changes the buffer and
leaves the element with no start-tag some elements may be ignored."
  (let ((level				; level in the element tree
	 0)
	next
	(tick				; change counter
	 (buffer-modified-tick)))
    (while element
      (funcall el-fun element)
      ;; If the function has modified the buffer, a fresh parse is needed
      (when (/= tick (buffer-modified-tick))
	(setq element (sgml-find-element-of (sgml-element-start element)))
	(setq tick (buffer-modified-tick)))
      (cond
       ;; Map content if any
       ((setq next (sgml-element-content element))
	(incf level))
       ;; If in a sub-tree, move to next element
       (t
	(while (and (> level 0)
		    (null (setq next (sgml-element-next element))))
	  (setq element (sgml-element-parent element))
	  (decf level))))
      (setq element next))))

;;;; Map content

(defun sgml-map-content (element element-fun
				 &optional data-fun pi-fun entity-fun)
  "Map content of ELEMENT, calling ELEMENT-FUN for every element.
Also calling DATA-FUN, if non-nil, with data in content."
  (sgml-pop-all-entities)
  (sgml-need-dtd)
  (sgml-element-end element)		; Make sure all content is parsed
  (unless (sgml-element-empty element)
    (let ((main-buffer-max (point-max)))
      (save-excursion
        (sgml-with-parser-syntax-ro
         (sgml-set-parse-state element 'start)
         (when (eobp) (sgml-pop-entity))
         (when (eolp) (forward-char 1))
         (sgml-parse-data main-buffer-max data-fun pi-fun entity-fun)
         (let ((c (sgml-tree-content element)))
           (while c
             (sgml-pop-all-entities)
             (funcall element-fun c)
             (sgml-set-parse-state c 'after)
             (sgml-parse-data main-buffer-max data-fun pi-fun entity-fun)
             (setq c (sgml-tree-next c)))))))))

(defun sgml-parse-data (sgml-goal sgml-data-function sgml-pi-function
				  sgml-entity-function)
  (let ((sgml-throw-on-element-change 'el-done))
    (catch sgml-throw-on-element-change
      (sgml-parse-continue sgml-goal nil t))))


;;;; Entity management

(defun sgml-push-to-string (string)
  "Create an entity from STRING and push it on the top of the entity stack.
After this the current buffer will be a scratch buffer containing the text
of the new entity with point at the first character.
    Use `sgml-pop-entity' to exit from this buffer."
  (sgml-push-to-entity (sgml-make-entity "#STRING" 'text string)))



;;; psgml-api.el ends here

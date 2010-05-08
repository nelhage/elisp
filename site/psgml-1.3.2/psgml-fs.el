;;; psgml-fs.el --- Format a SGML-file according to a style file
;; Copyright (C) 1995, 2000 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: psgml-fs.el,v 1.13 2002/07/14 10:03:26 lenst Exp $
;; Keywords:

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to lenst@lysator.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Commentary:

;; The function `style-format' formats the SGML-file in the current buffer
;; according to the style defined in the file `style.fs' (or the file given
;; by the variable `fs-style').

;; To try it load this file and open the test file example.sgml. Then
;; run the emacs command `M-x style-format'.

;; The style file should contain a single Lisp list.  The elements of
;; this list, themsleves lists, describe the style for an element type.
;; The sublists begin with the generic identifier for the element types and
;; the rest of the lists are characteristic/value pairs.

;; E.g.  ("p"  block t  left 4  top 2)

;; Defines the style for p-elements to be blocks with left margin 4 and
;; at least two blank lines before the block.


;;; Code:
(require 'psgml-api)
(eval-when-compile (require 'cl)
		   (require 'ps-print))

;;;; Formatting parameters

(defvar fs-char
  '((left . 0)
    (first . nil)
    (default-top . 0)
    (default-bottom . 0)
    (ignore-empty-para . nil)
    (literal . nil)))

(defvar fs-special-styles
  '(top bottom before after hang-from text sub-style title)
  "Style attributes that should not be entered in the characteristics table.")


;;; Dynamic variables

(defvar fs-current-element nil)
(defvar fs-buffer)


;;;; Formatting engine

(defun fs-char (p)
  (cdr (assq p fs-char)))

(defun fs-set-char (p val)
  (setcdr (assq p fs-char) val))

(defsetf fs-char fs-set-char)

(defvar fs-para-acc ""
  "Accumulate text of paragraph")

(defvar fs-hang-from nil
  "Hanging indent of current paragraph")

(defvar fs-first-indent nil)
(defvar fs-left-indent nil)

(defvar fs-vspace 0
  "Vertical space after last paragraph")

(defvar fs-filename)
(defvar fs-title)

(defun fs-add-output (str &optional just)
  (save-excursion
    (set-buffer fs-buffer)
    (goto-char (point-max))
    (let ((start (point)))
      (insert str)
      (when just
        (set-justification start (point) just)))))


(defun fs-addvspace (n)
  (when (> n fs-vspace)
    (fs-add-output (make-string (- n fs-vspace) ?\n))
    (setq fs-vspace n)))


(defun fs-para ()
  (when (if (fs-char 'ignore-empty-para)
	    (string-match "[^\t\n ]" fs-para-acc)
	  fs-left-indent)
    (assert fs-left-indent)
    (fs-output-para fs-para-acc fs-first-indent fs-left-indent
		    fs-hang-from
		    (fs-char 'literal))
    (setq fs-vspace 0
	  fs-hang-from nil))
  (setq fs-para-acc ""
	fs-first-indent nil
	fs-left-indent nil))

(defun fs-paraform-data (data)
  (unless fs-left-indent
    (setq fs-left-indent (fs-char 'left)
	  fs-first-indent (fs-char 'first)))
  (let ((face (fs-char 'face)))
    (when face
      (setq data (copy-sequence data))
      (put-text-property 0 (length data)
                         'face face data))
    (setq fs-para-acc (concat fs-para-acc data))))


(defun fs-output-para (text first-indent indent hang-from literal)
  (sgml-push-to-string text)
  (let ((indent-tabs-mode nil)
	(fill-prefix (make-string indent ? )))
    (cond
     (literal
      (goto-char (point-max))
      (unless (bolp)
	(insert ?\n))
      (goto-char (point-min))
      (while (not (eobp))
	(insert fill-prefix)
	(beginning-of-line 2)))
     (t
      (while (re-search-forward "[ \t\n\r]+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (delete-horizontal-space)
      (insert
       (if hang-from
	   hang-from
	 (make-string (or first-indent indent) ? )))
      (fill-region-as-paragraph (point-min) (point-max))
      (goto-char (point-max))
      (unless (bolp)
        (insert ?\n))))
    (fs-add-output (buffer-string) (fs-char 'justification)))
  (sgml-pop-entity)
  (sit-for 0))

(defun fs-paraform-phrase (e)
  (sgml-map-content e
		    (function fs-paraform-phrase)
		    (function fs-paraform-data)
		    nil
		    (function fs-paraform-entity)))

(defun fs-paraform-entity (entity)
  (let ((entity-map (fs-char 'entity-map))
	(text nil))
    (when entity-map
      (setq text
	    (loop for (name val) on entity-map by 'cddr
		  thereis (if (equal name (sgml-entity-name entity))
			      val))))
    (unless text
      (setq text (sgml-entity-text entity)))
    (fs-paraform-data text)))

;;;; Style driven engine

(defvar fs-style "style.fs"
  "*Style sheet to use for `style-format'.
The value can be the style-sheet list, or it can be a file name
\(string) of a file containing the style sheet or it can be the name
\(symbol) of a variable containing the style sheet.")

(put 'fs-style 'variable-interactive
     "fStyle file: ")

(defvar fs-cached-styles nil)

(defun fs-get-style (style)
  (cond ((stringp style)
	 (sgml-cache-catalog style
			     'fs-cached-styles
			     (function (lambda ()
					 (read (current-buffer))))))
	((symbolp style)
	 (fs-get-style (symbol-value style)))
	((listp style)
	 style)
	(t
	 (error "Illegal style value: %s" style))))

(defun fs-engine (e)
  (fs-do-style e
	       (cdr (or (assoc (sgml-element-gi e) fs-style)
			(assq t fs-style)))))

(defun fs-do-style (fs-current-element style)
  (let ((hang-from (eval (plist-get style 'hang-from))))
    (when hang-from
      (setq fs-hang-from
	    (format "%s%s "
		    (make-string
		     (or (fs-char 'hang-left) (fs-char 'left))
		     ? )
                    hang-from))))
  (let ((fs-char (nconc
		  (loop for st on style by 'cddr
			unless (memq (car st) fs-special-styles)
			collect (cons (car st)
				      (eval (cadr st))))
		  fs-char)))
    (when (plist-get style 'block)
      (fs-para)
      (fs-addvspace (or (plist-get style 'top)
			(fs-char 'default-top))))
    (let ((before (plist-get style 'before)))
      (when before
	(fs-do-style e before)))
    (let ((fs-style
           (append (plist-get style 'sub-style)
                   fs-style)))
      (cond ((plist-get style 'text)
             (let ((text (eval (plist-get style 'text))))
               (when (stringp text)
                 (fs-paraform-data text))))
            (t
             (sgml-map-content e
                               (function fs-engine)
                               (function fs-paraform-data)
                               nil
                               (function fs-paraform-entity)))))
    (let ((title (plist-get style 'title)))
      (when title
        (setq title (eval title))
        (save-excursion
          (set-buffer fs-buffer)
          (setq fs-title title))))
    (let ((after (plist-get style 'after)))
      (when after
	(fs-do-style e after)))
    (when (plist-get style 'block)
      (fs-para)
      (fs-addvspace (or (plist-get style 'bottom)
			(fs-char 'default-bottom))))))


(defun fs-clear ()
  (setq fs-para-acc ""
        fs-hang-from nil
        fs-first-indent nil
        fs-left-indent nil
        fs-vspace 0)  )


(defun fs-setup-buffer ()
  (save-excursion
    (let ((orig-filename (buffer-file-name (current-buffer))))
      (set-buffer fs-buffer)
      (erase-buffer)
      (setq ps-left-header
            '(fs-title fs-filename))
      (make-local-variable 'fs-filename)
      (setq fs-filename (file-name-nondirectory orig-filename))
      (make-local-variable 'fs-title)
      (setq fs-title ""))))

(defun fs-wrapper (buffer-name thunk)
  (fs-clear)
  (let ((fs-style (fs-get-style fs-style))
        (fs-buffer (get-buffer-create buffer-name)))
    (fs-setup-buffer)
    (funcall thunk)
    (fs-para)
    (save-excursion
      (set-buffer fs-buffer)
      (goto-char (point-min)))
    fs-buffer))


;;;###autoload
(defun style-format ()
  (interactive)
  (fs-wrapper  "*Formatted*"
               (lambda ()
                 (display-buffer fs-buffer)
                 (fs-engine (sgml-top-element)))))


;;;; Helper functions for use in style sheet

(defun fs-element (&rest moves)
  "Find current or related element."
  (let ((element fs-current-element))
    (while moves
      (case (pop moves)
        (parent (setq element (sgml-element-parent element)))
        (next   (setq element (sgml-element-next element)))
        (child  (setq element (sgml-element-content element)))))
    element))

(defun fs-element-content (&optional e)
  (unless e (setq e (fs-element)))
  (let ((fs-para-acc "") fs-first-indent fs-left-indent)
    (sgml-map-content e
		      (function fs-paraform-phrase)
		      (function fs-paraform-data)
		      nil
		      (function fs-paraform-entity))
    fs-para-acc))

(defun fs-attval (name &optional element)
  (sgml-element-attval (if element element (fs-element))
                       name))

(defun fs-child-number (&optional element)
  (let* ((element (or element (fs-element)))
         (parent (sgml-element-parent element))
         (child  (sgml-element-content parent))
         (number 0))
    (while (and child (not (eq child element)))
      (incf number)
      (setq child (sgml-element-next child)))
    number))


(defun fs-element-with-id (id)
  (block func
    (let ((element (sgml-top-element)))
      (while (not (sgml-off-top-p element))
        (let ((attlist (sgml-element-attlist element)))
          (loop for attdecl in attlist
                if (eq 'ID (sgml-attdecl-declared-value attdecl))
                do (if (compare-strings id nil nil
					(sgml-element-attval
					 element (sgml-attdecl-name attdecl))
					nil nil)
                       (return-from func element))))
        ;; Next element
        (if (sgml-element-content element)
            (setq element (sgml-element-content element))
          (while (null (sgml-element-next element))
            (setq element (sgml-element-parent element))
            (if (sgml-off-top-p element)
                (return-from func nil)))
          (setq element (sgml-element-next element)))))
    nil))


(defun fs-split-tokens (s)
  "Split a string S into a list of tokens."
  (let ((result nil))
    (sgml-push-to-string s)
    (while (not (eobp))
      (skip-syntax-forward "-")
      (let ((start (point)))
        (skip-syntax-forward "^-")
        (when (/= start (point))
          (push (buffer-substring-no-properties start (point))
                result))))
    (sgml-pop-entity)
    (nreverse result)))


;;; fs.el ends here

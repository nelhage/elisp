;;;; psgml-charent.el
;;; Last edited: 1999-12-18 18:54:53 lenst
;;; $Id: psgml-charent.el,v 1.7 2002/04/25 20:50:27 lenst Exp $

;; Copyright (C) 1994 Lennart Staflin

;; Author: Steinar Bang, Falch Hurtigtrykk as., Oslo, 940711
;;	Lennart Staflin <lenst@lysator.liu.se>
;;
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

;;  Functions to convert character entities into displayable characters
;;  and displayable characters back into character entities.

;; This should either use iso-cvt or do better with a multilingual set of entities 


;;;; Code:

(provide 'psgml-charent)
(require 'psgml-parse)
(eval-when-compile (require 'cl))


;;;; Variable declarations

(defvar sgml-display-char-list-filename
  (expand-file-name "iso88591.map"
                    (file-name-directory (locate-library "psgml")))
  "*Name of file holding relations between character codes and character
names of displayable characters")

(defvar sgml-display-char-alist-cache nil)


;;;; Function declarations

(defun sgml-display-char-alist ()
  "Return the current display character alist.
Alist with entity name as key and display character as content."
  (unless (file-exists-p sgml-display-char-list-filename)
    (error "No display char file: %s"
	   sgml-display-char-list-filename))
  (sgml-cache-catalog sgml-display-char-list-filename 
		      'sgml-display-char-alist-cache
		      (function sgml-read-display-char-alist)))

(defun sgml-read-display-char-alist ()
  (let (key disp-char alist)
    (while (re-search-forward "^\\([0-9]+\\)[ \t]+\\(.+\\)$" nil t)
      (setq key (buffer-substring (match-beginning 2) (match-end 2)))
      (setq disp-char (string-to-number (buffer-substring (match-beginning 1)
							  (match-end 1))))
      (if (fboundp 'unibyte-char-to-multibyte)
	  (setq disp-char (unibyte-char-to-multibyte disp-char)))
      (setq disp-char (char-to-string disp-char))
      (push (cons key disp-char)
	    alist))
    alist))

(defun sgml-charent-to-dispchar-alist ()
  "Association list to hold relations of the type
     (CHARACTER-NAME . CHARACTER)
    where 
     CHARACTER-NAME is a string holding a character name
     CHARACTER      is a string holding a single displayable character"
  (sgml-need-dtd)
  (let ((display-chars (sgml-display-char-alist))
	(alist nil))
    (sgml-map-entities
     (function
      (lambda (entity)
	(let ((char (cdr (assoc (sgml-entity-text entity)
				display-chars))))
	  (when char
	    (push (cons (sgml-entity-name entity) char) alist)))))
     (sgml-dtd-entities sgml-dtd-info))
    
    alist))


(defun sgml-charent-to-display-char ()
  "Replace character entities with their display character equivalents"
  (interactive)
  (let ((charent-to-char
	 (sgml-charent-to-dispchar-alist))
	charent replacement)
    (save-excursion
      (goto-char (point-min))
      (sgml-with-parser-syntax
       (while (re-search-forward "&\\(\\w\\(\\w\\|\\s_\\)*\\);?" nil t)
	 (setq charent (buffer-substring-no-properties
                        (match-beginning 1) (match-end 1)))
	 (if (setq replacement (cdr (assoc charent charent-to-char)))
	     (replace-match replacement t t)))))))

(defun sgml-display-char-to-charent ()
  "Replace displayable characters with their character entity equivalents"
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (loop for pair in (sgml-charent-to-dispchar-alist)
	    do (goto-char (point-min))
	    (while (search-forward (cdr pair) nil t)
	      (replace-match (concat "&" (car pair) ";") t t))))))



;;; psgml-charent.el ends here

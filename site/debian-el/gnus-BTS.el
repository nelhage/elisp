;;; gnus-BTS.el --- access the Debian Bug Tracking System from Gnus

;; Copyright (C) 2001 Andreas Fuchs <asf@acm.org>

;; Author: Andreas Fuchs
;; Keywords: gnus, Debian, Bug
;; Status: Works in XEmacs (I think >=21)
;; Created: 2001-02-07

;; $Id: gnus-BTS.el,v 1.1.1.1 2003-04-04 20:16:01 lolando Exp $

;; This file is not part of GNU Emacs.

;; gnus-BTS.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; gnus-BTS.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use this program if you read a lot of debian lists and see many
;; references to the Bug Tracking system in them. It expects to see
;; Bug references in the form of (for example): "#48273", "closes:
;; 238742" or similar.
;;
;; Use `M-x' `gnus-dbts-browse-debpkg-or-bug' over the bug number.

;;; Change log:
;; 2005-08-20  Jari Aalto  <jari aalto A T cante net>
;; 
;; 	* gnus-BTS.el:
;; 	(top level): Changed all variable and function names to use common
;; 	prefix `gnus-dbts-'. This makes package namespace clean. Converted
;; 	all lambda forms to real functions. Cleaned up
;; 	`gnus-select-article-hook' setting.
;; 	Changed all 'setq' to 'defvar'.
;; 	(gnus-dbts-gnus-install): New.
;; 	(gnus-dbts-gnus-select-article-hook): New.
;; 	(gnus-dbts-buttonize): New.
;; 	(eval-after-load): New. Install at point when Gnus is being loaded.
;; 
;; 2005-09-19 Peter S Galbraith <psg@debian.org>
;;
;;      Minor bug fix: gnus-dbts-gnus-install missing brackets.
;;
;; 2007-09-17 Peter S Galbraith <psg@debian.org>
;;
;;      Wrong regexp part of gnus-dbts-debian-bug-regexp called by
;;      gnus-dbts-buttonize-debian (Closes #363161, #442438).
;;      
;; 2007-09-24 intrigeri <intrigeri@boum.org>
;;            Peter S Galbraith <psg@debian.org>
;;
;;      Bug#218286: [Fwd: Re: [gnus-BTS] please make bug numbers in mail
;;      clickable to read them as email.
;;      Introduce `gnus-dbts-read-bugs-as-email'
;;
;;; Code:


;; gnus-dbts = Gnus inerface to Debian Bug Tracking System

(autoload 'thing-at-point "thingatpt")

(defcustom gnus-dbts-read-bugs-as-email nil
  "If t, highlighted Debian bug numbers' buttons call
  `debian-bug-get-bug-as-email'; else, `browse-url' is used."
  :type 'boolean
  :group 'gnus-BTS)

(defvar gnus-dbts-in-debian-group-p nil)

(defvar gnus-dbts-in-debian-devel-announce-group-p nil)

(defvar gnus-dbts-bug-special-keywords "reassign\\|merge")

(defvar gnus-dbts-bug-keywords
  (concat
   "tags\\|severity\\|retitle\\|close\\|closes:\\|Merged\\|reopen\\|Bug\\|"
   gnus-dbts-bug-special-keywords))

(defvar gnus-dbts-bug-prefix " *#?\\|Bugs?\\|#")
(defvar gnus-dbts-bug-number " *\\([0-9]+\\)")
(defvar gnus-dbts-bug-special " +\\([0-9]+\\|[-.A-Za-z0-9]+\\)")

(defvar gnus-dbts-debian-bug-regexp
  (concat
   "\\("
   "\\("
   gnus-dbts-bug-keywords
   "\\)"
   gnus-dbts-bug-prefix
   "\\)"
   gnus-dbts-bug-number))

(defvar gnus-dbts-debian-reassign-or-merge-regexp
  (concat
   "\\("
   gnus-dbts-bug-special-keywords
   "\\)"
   gnus-dbts-bug-number
   gnus-dbts-bug-special))

(defvar gnus-dbts-debian-reassign-regexp
  "reassigned from package `\\([^']*\\)' to `\\([^']*\\)'")

;; debian-bug-get-bug-as-email autoload
(require 'debian-el-loaddefs)

(defun gnus-dbts-browse-debpkg-or-bug (thing)
  (interactive "i")
  (let* ((the-thing (if (null thing)
			(thing-at-point 'sexp)
		      thing))
	 (bugp (string-match "[0-9]+$" the-thing))
	 (bug-or-feature (if bugp
			     (progn
			       (string-match "^[^0-9]*\\([0-9]+\\)$" the-thing)
			       (match-string 1 the-thing))
			   the-thing))
	 (url (if bugp
		  "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug="
		(concat
		 "http://cgi.debian.org/cgi-bin/search_packages.pl"
		 "?&searchon=names&version=all&release=all&keywords="))))
    (if (and bugp gnus-dbts-read-bugs-as-email)
	(debian-bug-get-bug-as-email bug-or-feature)
      (browse-url (concat url bug-or-feature)))))

(defun gnus-dbts-buttonize-debian (regexp num predicate)
  (add-to-list 'gnus-button-alist
	       (list regexp
		     num
		     predicate
		     'gnus-dbts-browse-debpkg-or-bug
		     num)))

(defun gnus-dbts-buttonize ()
  (gnus-dbts-buttonize-debian gnus-dbts-debian-bug-regexp 3
			      'gnus-dbts-in-debian-group-p)
  (gnus-dbts-buttonize-debian gnus-dbts-debian-reassign-or-merge-regexp 3
			      'gnus-dbts-in-debian-group-p)
  (gnus-dbts-buttonize-debian gnus-dbts-debian-bug-regexp 3
			      'gnus-dbts-in-debian-devel-announce-group-p)
  (gnus-dbts-buttonize-debian gnus-dbts-debian-reassign-regexp 1
			      'gnus-dbts-in-debian-group-p)
  (gnus-dbts-buttonize-debian gnus-dbts-debian-reassign-regexp 2
				 'gnus-dbts-in-debian-group-p))

(defun gnus-dbts-gnus-select-article-hook ()
  (setq gnus-dbts-in-debian-group-p
	(string-match "debian"
		      (gnus-group-real-name
		       gnus-newsgroup-name)))
  (setq gnus-dbts-in-debian-devel-announce-group-p
	(string-match "debian.devel.announce"
		      (gnus-group-real-name
		       gnus-newsgroup-name))))

(defun gnus-dbts-gnus-install ()
  (add-hook 'gnus-select-article-hook 'gnus-dbts-gnus-select-article-hook)
   ;; only run once, as soon as the article buffer has been created.
  (add-hook 'gnus-article-mode-hook 'gnus-dbts-buttonize))

(eval-after-load "gnus" '(progn (gnus-dbts-gnus-install)))

(provide 'gnus-BTS)

;; End of file

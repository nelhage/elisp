;;; bbdb-anniv.el --- Get anniversaries from BBDB

;; Copyright (C) 1998 Ivar Rummelhoff

;; Author: Ivar Rummelhoff <ivarru@math.uio.no>
;; Maintainer: Ivar Rummelhoff <ivarru@math.uio.no>
;; Created: 11 March 1998
;; Time-stamp: <00/08/07 10:52:12 ivarru>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an electronic
;; mail to this program's maintainer or by writing to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; (require 'bbdb-anniv)
;; (add-hook 'list-diary-entries-hook #'bbdb-include-anniversaries)
;;
;; will include BBDB-anniversaries when the diary is displayed
;; (fancy).  The anniversaries are stored in the field `anniversary'
;; in the format
;;
;;     [YYYY-MM-DD CLASS-OR-FORMAT-STRING]
;;     {\nYYYY-MM-DD CLASS-OR-FORMAT-STRING}*
;;
;; CLASS-OR-FORMAT-STRING is one of two things:
;;
;;  * an identifier for a class of anniversaries (eg. birthday or
;;    wedding) from `bbdb-anniversary-format-alist'.
;;  * the (format) string displayed in the diary.
;;
;; It defaults to the value of `bbdb-default-anniversary-format'
;; ("birthday" by default).
;;
;; The substitutions in the format string are (in order):
;;  * the name of the record containing this anniversary
;;  * the number of years
;;  * an ordinal suffix (st, nd, rd, th) for the year
;;
;; See the documentation of `bbdb-anniversary-format-alist' for
;; further options.
;;
;; Example (my own record):
;;
;;       1973-06-22 
;;       20??-??-?? wedding
;;       1998-03-12 %s created bbdb-anniv.el %d years ago
;;
;; If you use the hook `sort-diary-entries', you should make sure that
;; it is executed after `bbdb-include-anniversaries'.
;;

(require 'bbdb)
(require 'diary-lib)
(eval-when-compile (require 'cl))

;;;###autoload
(defgroup bbdb-utilities-anniversaries nil
  "Customizations for including diary anniversaries from BBDB."
  :link '(emacs-library-link :tag "Lisp File" "bbdb-anniv.el")
  :group 'bbdb-utilities)

;;;###autoload
(defcustom bbdb-anniversaries nil
  "Should BBDB anniversaries be included when the diary is displayed (fancy)?
You must modify via \\[customize] for this variable to have an effect."
  :set #'(lambda (symbol value)
	   (if value
	       (add-hook 'list-diary-entries-hook
			 #'bbdb-include-anniversaries)
	     (remove-hook 'list-diary-entries-hook
			  #'bbdb-include-anniversaries)))
  :type 'boolean
  :group 'bbdb-utilities-anniversaries
  :require 'bbdb-anniv)

(defcustom bbdb-default-anniversary-format "birthday"
  "Default anniversary class"
  :type  'string
  :group 'bbdb-utilities-anniversaries
  :require 'bbdb)

(defcustom bbdb-anniversary-format-alist 
  '( ("birthday" . "Birthday: %s (%d%s)")
     ("wedding"  . "%s's %d%s wedding anniversary") )
  "How different types of anniversaries should be formatted.
An alist of elements (STRING . FORMAT) where STRING is the name of an
anniversary class and format is either:
1) A format string with the following substitutions (in order):
    * the name of the record containing this anniversary
    * the number of years
    * an ordinal suffix (st, nd, rd, th) for the year

2) A function to be called with three arguments: NAME YEARS SUFFIX
   (string int string) returning a string for the diary or nil.

3) An emacs lisp form that should evaluate to a string (or nil) in the
   scope of variables NAME, YEARS and SUFFIX (among others)."
  :type 'sexp
  :group 'bbdb-utilities-anniversaries
  :require 'bbdb)

(defcustom bbdb-anniversary-field 'anniversary
  "Which BBDB field contains anniversaries."
  :type    'symbol
  :group   'bbdb-utilities-anniversaries
  :require 'bbdb)

(defcustom bbdb-extract-date-fun 'bbdb-anniv-extract-date
  "How to retrieve `month date year' from the anniversary field."
  :type 'function
  :group 'bbdb-utilities-anniversaries
  :require 'bbdb)

(defcustom bbdb-anniversary-reminder-days 0
  "Number of days warning you are given of an impending anniversary.
Modify this to give yourself a n-day warning of those important
anniversaries. This works in a naive fashion, extending (forwards) the
range of days for which diary entries are being listed. When set to 0, 
the behaviour is to only list anniversaries on the day."
  :type 'integer
  :group 'bbdb-utilities-anniversaries
  :require 'bbdb)

;; YYYY-MM-DD  =>  (month date year)
(defun bbdb-anniv-extract-date (time-str)
  (multiple-value-bind (y m d) (bbdb-split time-str "-")
    (list (string-to-number m)
	  (string-to-number d)
	  (string-to-number y))))

(defun bbdb-anniv-split (str)
  (let ((pos (string-match "[ \t]" str)))
    (if pos (list (substring str 0 pos)
		  (bbdb-string-trim (substring str pos)))
      (list str nil))))


(defvar number)
(defvar original-date)

;;;###autoload
(defun bbdb-include-anniversaries ()
  (let ((dates (loop repeat (+ number bbdb-anniversary-reminder-days)
		     for num from (calendar-absolute-from-gregorian
				   original-date)
		     for date = original-date
		     then (calendar-gregorian-from-absolute num)
		     ;; ((MM . DD) . YYYY)
		     collect (cons (cons (extract-calendar-month date)
					 (extract-calendar-day date))
				   (extract-calendar-year date))))
	annivs date years
	split class form)
    (dolist (rec (bbdb-records))
      (when (setq annivs (bbdb-record-getprop
			  rec bbdb-anniversary-field))
	(setq annivs (bbdb-split annivs "\n"))
	(while annivs
	  (setq split (bbdb-anniv-split (pop annivs)))
	  (multiple-value-bind (m d y)
	      (funcall bbdb-extract-date-fun (car split))

	    (when (and (or (setq date (assoc (cons m d) dates))
			   (and (= d 29)
				(= m 2)
				(setq date (assoc '(3 . 1) dates))
				(not (calendar-leap-year-p (cdr date)))))
		       (< 0 (setq years (-  (cdr date) y))))
	      (let* ((class (or (cadr split)
				bbdb-default-anniversary-format))
		     (form (or (cdr (assoc class
					   bbdb-anniversary-format-alist))
			       class))	; (as format string)
		     (name (bbdb-record-name rec))
		     (suffix (diary-ordinal-suffix years))
		     (text (cond
			    ((functionp form)
			     (funcall form name years suffix))
			    ((listp form) (eval form))
			    (t (format form name years suffix)))))
		(when text
		  (bbdb-anniv-add
		   (list (caar date) (cdar date) (cdr date)) ; MM DD YYYY
		   text))))))))))

(defun bbdb-anniv-add (a b)
  (add-to-diary-list a b ""))

(provide 'bbdb-anniv)

;;; bbdb-anniv.el ends here

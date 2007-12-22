;; mc-setversion.el, Support for multiple versions of PGP.
;; Copyright (C) 1998  Len Budney <lbudney@pobox.com>

;;{{{ Licensing
;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;}}}

(defvar mc-default-scheme 'mc-scheme-pgp
  "*Set the default encryption scheme for Mailcrypt to use. Defaults
to pgp 2.6 for backward compatibility.")

(defun mc-setversion (&optional version)
  "Reset path and argument information for the selected version of PGP.
Possible values of VERSION are 2.6, 5.0, and gpg."
  (interactive)

  (if (null version)
      (let
	  ((oldversion
	    (cond
	     ((eq mc-default-scheme 'mc-scheme-pgp50) "5.0")
	     ((eq mc-default-scheme 'mc-scheme-pgp) "2.6")
	     ((eq mc-default-scheme 'mc-scheme-gpg) "gpg")
	     (t nil))
	    )
	   (completion-ignore-case t))
	(setq version 
	      (completing-read 
	       (format "Select PGP version (currently %s): " oldversion)
	       '(
		 ("2.6" 1) 
		 ("5.0" 2)
		 ("gpg" 3)
		 ) nil 
		   t   ; REQUIRE-MATCH
		   nil ; INITIAL
		   nil ; HIST
		       ))
	(if (equal (length version) 0)
	    (setq version oldversion))))

  (cond
   ((string-equal version "5.0")
    (progn
      (setq mc-default-scheme 'mc-scheme-pgp50)
      (message "PGP version set to 5.0.")))
   ((string-equal version "2.6")
    (progn
      (setq mc-default-scheme 'mc-scheme-pgp)
      (message "PGP version set to 2.6.")))
   ((string-equal version "gpg")
    (progn
      (setq mc-default-scheme 'mc-scheme-gpg)
      (message "PGP version set to GPG.")))
   (t (error "bad version string")) ; cannot happen
))


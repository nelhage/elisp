;;; lj-pcomplete.el --- programmable completion for ljupdate composition buffers

;; Copyright (C) 2002, 2003, 2004, 2005 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is part of ljupdate, a LiveJournal client for Emacs.

;; ljupdate is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; ljupdate is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(require 'pcomplete)

(defun lj-pcomplete-setup ()
  "Configure this buffer for programmable completion."
  (set (make-local-variable 'pcomplete-termination-string) "")
  (set (make-local-variable 'pcomplete-ignore-case) t)
  (set (make-local-variable 'pcomplete-use-paring) nil)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'lj-pcomplete-parse-header-arguments)
  (set (make-local-variable 'pcomplete-command-name-function)
       'lj-this-header))

;; header completion

(defun pcomplete/lj-compose-header-mode/Subject ()
  "Attempt to complete the Subject header."
  (pcomplete-here nil))

(defun pcomplete/lj-compose-header-mode/Mood ()
  "Attempt to complete the Mood header."
  (pcomplete-here (sort (mapcar 'car (lj-server-get (lj-this-server) :moods))
                        'string-lessp)
                  nil nil t))

(defun pcomplete/lj-compose-header-mode/Server ()
  "Attempt to complete the Server header."
  (pcomplete-here (sort (lj-servers) 'string-lessp)
                  nil nil t))

(defun pcomplete/lj-compose-header-mode/User ()
  "Attempt to complete the User header."
  (pcomplete-here (sort (lj-users (lj-this-server)) 'string-lessp)
                  nil nil t))

(defun pcomplete/lj-compose-header-mode/Community ()
  "Attempt to complete the Community header."
  (pcomplete-here (sort (copy-list (lj-user-get (lj-this-server)
                                                (lj-this-user)
                                                :access))
                        'string-lessp)
                  nil nil t))

(defun pcomplete/lj-compose-header-mode/Picture ()
  "Attempt to complete the Picture header."
  (pcomplete-here (sort (copy-list (lj-user-get (lj-this-server)
                                                (lj-this-user)
                                                :pics))
                        'string-lessp)
                  nil nil t))

(defun pcomplete/lj-compose-header-mode/Access ()
  "Attempt to complete the Access header."
  (pcomplete-here (sort
                   (append
                    (list "public" "private" "friends")
                    (mapcar 'car
                            (lj-user-get (lj-this-server)
                                         (lj-this-user)
                                         :friends-groups)))
                   'string-lessp)
                  nil nil t))

;; pcomplete support code

(defun lj-pcomplete-parse-header-arguments ()
  "Return a list of parsed whitespace-separated arguments.
These are the words from the beginning of the line up to where point is
right now."
  (let* ((start (save-excursion (beginning-of-line) (point)))
	 (end (point))
	 args beginnings)
    (save-excursion
      (if (< (skip-chars-backward " \t\n" start) 0)
	  (setq args '("")
		beginnings (list end)))
      (setq end (point))
      (while (< (skip-chars-backward "^ \t\n" start) 0)
	(setq beginnings (cons (point) beginnings)
	      args (cons (buffer-substring-no-properties
			  (point) end)
			 args))
	(skip-chars-backward " \t\n" start)
	(setq end (point))))
    (cons args beginnings)))

(provide 'lj-pcomplete)
;;; lj-pcomplete.el ends here

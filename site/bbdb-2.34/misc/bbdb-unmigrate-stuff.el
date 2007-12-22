;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; Copyright (c) 2000 Alex Schroeder
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun bbdb-unmigrate-stuff (&optional new-version)
  "Create a buffer with unmigrated BBDB data.
Usefull if you fooled around with BBDB file format 4 by Alex and want to
start using the official BBDB 2.00.06 again.  In order to do that, you
have to save your .bbdb in BBDB file format 3 instead of the file format 4
introduced by Alex.  This function will create a *BBDB Version 3* buffer
for you, which you can examine and save as your new .bbdb.  The unmigration
will strip the country fields of all entries in your BBDB as such a field
did not exist in the BBDB file format 3 used in BBDB 2.00.06."
  (interactive "nUnmigrate to version (I recommend version 3): ")
  (if (null new-version)
      (setq new-version 3))
  (if (>= new-version bbdb-file-format)
      (error "Current BBDB file format is version %d" bbdb-file-format)
    (let* ((records (bbdb-records))
           (propnames (bbdb-with-db-buffer bbdb-propnames))
           (rec)
           (bbdb-file-format-migration (cons bbdb-file-format new-version))
           (buf (get-buffer-create (format "*BBDB Version %d*" (cdr bbdb-file-format-migration)))))
      (message "Unconverting the BBDB database...")
      (set-buffer buf)
      (erase-buffer)
      (insert (format (concat ";;; file-version: %d\n"
                              ";;; user-fields: %S\n")
                      new-version (mapcar (function (lambda (x) (intern (car x))))
                                          propnames)))
      (while records
        (setq rec (copy-sequence (car records)))
        (bbdb-unmigrate-record rec)
        (aset rec 8 nil)
        (insert (format "%S\n" rec))
        (setq records (cdr records)))
      (pop-to-buffer buf)
      (message "Unconverting the BBDB database...done"))))

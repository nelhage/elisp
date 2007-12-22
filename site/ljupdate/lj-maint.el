;;; lj-maint.el --- compilation and maintenance hacks for ljupdate

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

;; `load-path' frobbing; used for compiling

(add-to-list 'load-path default-directory)
(mapc (lambda (dir) (add-to-list 'load-path dir))
      (parse-colon-path (getenv "LOAD_PATH")))

;; autoload generation; used to create `ljupdate.el'

(defvar generated-autoload-file)
(defvar command-line-args-left)
(defun lj-generate-autoloads ()
  "Generate autoloads for ljupdate."
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file
        (expand-file-name (car command-line-args-left) default-directory))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

(defun lj-debug-response (response)
  "Dump RESPONSE into a buffer so we can look at it."
  (switch-to-buffer (get-buffer-create "*LJ DEBUG*"))
  (maphash (lambda (k v) (insert (format "%s\n%s\n" k v))) response))

(provide 'lj-maint)

;;; lj-maint.el ends here

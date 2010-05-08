;;; psgml-maint.el --- Help functions to maintain PSGML source

;; Copyright (C) 1996 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: psgml-maint.el,v 1.8 2005/02/09 15:28:58 lenst Exp $
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

;; This file contanins commands used during installation and
;; compilation of psgml.

;; psgml-compile-files	Compiles the source files. The version of
;;			Emacs used for compilation will determine
;;			what files are compiled.


;;; Code:

(require 'bytecomp)

(defconst psgml-common-files
  '("psgml.el" "psgml-parse.el" "psgml-edit.el" "psgml-dtd.el"
    "psgml-info.el" "psgml-charent.el" "psgml-api.el" "psgml-sysdep.el"
    "psgml-ids.el"))

(defconst psgml-emacs-files '("psgml-other.el"))
(defconst psgml-xemacs-files '("psgml-lucid.el"))
(defvar psgml-source-dir nil)

(defconst psgml-elisp-source
  (append psgml-common-files
	  (cond ((or (string-match "Lucid" emacs-version)
		     (string-match "XEmacs" emacs-version))
		 psgml-xemacs-files)
		(t
		 psgml-emacs-files))))


(defun psgml-find-source-dir (&optional ask)
  (if psgml-source-dir
      t
    (let ((cand (list "." "./psgml-1.3.2")))
      (while cand
	(if (file-exists-p (expand-file-name "psgml-maint.el" (car cand)))
	    (progn
	      (setq psgml-source-dir (expand-file-name "." (car cand))
		    cand nil))
	  (setq cand (cdr cand))))
      (if (null psgml-source-dir)
	  (if ask
	      (setq psgml-source-dir
		    (expand-file-name
		     (read-file-name "Where is the psgml source? "
				     nil nil t)))
	    (error "No psgml source in current directory"))))))


(defun psgml-compile-files ()
  "Compile the PSGML source files that needs compilation."
  (interactive)
  (psgml-find-source-dir (interactive-p))
  (let ((default-directory psgml-source-dir)
	(load-path (cons psgml-source-dir load-path)))
    (mapcar (function psgml-byte-compile-file)
	    psgml-elisp-source)
    (message "Done compiling")))


(defun psgml-byte-compile-file (file)
  (let ((dest (byte-compile-dest-file file)))
    (if (file-newer-than-file-p file dest)
	(byte-compile-file file))))

(defun psgml-install-elc ()
  "Print list of elc files to install"
  (let ((destdir (car command-line-args-left)))
    (princ (mapconcat (function byte-compile-dest-file)
		      psgml-elisp-source " "))))


;;; psgml-maint.el ends here

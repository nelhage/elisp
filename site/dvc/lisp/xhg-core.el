;;; xhg-core.el --- Common definitions for mercurial support in DVC

;; Copyright (C) 2005-2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides the low-level functions used by the Xtla interfaces
;; to distributed revison control systems.


;;; History:

;;

;;; Code:

(require 'dvc-core)

;; Settings for Mercurial/hg
(defvar xhg-executable
  "hg"
  "The executable used for the hg commandline client.")

(defvar xhg-log-edit-file-name
  "++xhg-log-edit"
  "The filename, used to store the log message before commiting.
Usually that file is placed in the tree-root of the working tree.")

;;;###autoload
(defun xhg-tree-root (&optional location no-error interactive)
  "Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an .hg/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
mercurial managed tree (but return nil)."
  (dvc-tree-root-helper ".hg/" (or interactive (interactive-p))
                        "%S is not in a mercurial-managed tree!"
                        location no-error))


(defun xhg-read-revision (prompt)
  "Read a revision for the actual mercurial working copy."
  (read-string prompt (xhg-log-revision-at-point)))

(provide 'xhg-core)
;;; xhg-core.el ends here

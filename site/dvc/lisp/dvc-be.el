;;; dvc-be.el --- dvc integration for bugs everywhere

;; Copyright (C) 2006 by all contributors

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

;; For more information on bugs everywhere see:
;;   http://panoramicfeedback.com/opensource/


;; dvc-be should be an interface to bugs everywhere
;; at the moment be exists as standalone tool for arch/bzr
;; or as extension for hg
;; dvc-be should work with both:

;; be commands:
;; be assign    Assign an individual or group to fix a bug
;; be close    Close a bug
;; be comment    Add a comment to a bug
;; be diff    Compare bug reports with older tree
;; be inprogress    Bug fixing in progress
;; be list    List bugs
;; be new    Create a new bug
;; be open    Re-open a bug
;; be set    Change tree settings
;; be set-root    Assign the root directory for bug tracking
;; be severity    Show or change a bug's severity level
;; be show    Show a particular bug
;; be target    Show or change a bug's target for fixing
;; be upgrade    Upgrade the bugs to the latest format

;; hg be extension commands:
;;  bassign       assign a person to fix a bug
;;  bclose        close a given bug
;;  bcomment      add a comment to a given bug
;;  binit         initialize the bug repository
;;  binprogress   mark a bug as 'in-progress'
;;  blist         list bugs
;;  bnew          create a new bug
;;  bopen         re-open a given bug
;;  bset          show or change per-tree settings
;;  bseverity     Show or change a bug's severity level.
;;  bshow         show all information about a given bug
;;  btarget       Show or change a bug's target for fixing.
;;  bversion      print the version number

;; the xhg-be extension is in xhg-be.el
;; the standalone support for be will be in this file

;; The UI for listing/changing bugs will be in this file

(provide 'dvc-be)
;;; dvc-be.el ends here

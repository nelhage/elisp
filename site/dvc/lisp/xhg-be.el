;;; xhg-be.el --- dvc integration for the mercurial bugs everywhere plugin

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

(require 'dvc-be)

(defun xhg-binit (&optional dir)
  "Run hg binit."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for hg binit: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (let ((default-directory dir))
    (dvc-run-dvc-sync 'xhg (list "binit")
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg binit finished")))))

(provide 'xhg-be)
;;; xhg-be.el ends here

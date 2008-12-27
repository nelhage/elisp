;;; xmtn-minimal.el --- Definitions for detecting whether to activate xmtn

;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; The minimal set of definitions needed to allow DVC to detect
;; whether a given file is under monotone version control.  Having
;; them in their own file instead of in xmtn-dvc.el avoids having to
;; load all of xmtn-dvc.el just for this simple check.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'dvc-register)
  (require 'dvc-core))

;;;###autoload
(defun xmtn-tree-root (&optional location no-error)
  (dvc-tree-root-helper "_MTN/" (interactive-p)
                        "%s is not in a monotone-managed tree"
                        location no-error))

(provide 'xmtn-minimal)

;;; xmtn-minimal.el ends here

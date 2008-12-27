;;; baz-dvc.el --- The dvc layer for baz

;; Copyright (C) 2005, 2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributors: Matthieu Moy, <Matthieu.Moy@imag.fr>

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

;; This file provides the common dvc layer for baz support

(require 'baz)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'baz "Bazaar 1")
(defalias 'baz-dvc-tree-root 'baz-tree-root)
(defun baz-dvc-diff (base-rev path dont-switch)
  (baz-changes nil base-rev))
(defalias 'baz-dvc-file-diff 'baz-file-diff)
(defalias 'baz-dvc-log-edit 'tla-dvc-log-edit)
(defun baz-dvc-add (file)
  (baz-add nil file))
(defun baz-dvc-log (arg last-n)
  "Shows the changelog in the current Arch tree."
  (baz-logs))
(defun baz-dvc-search-file-in-diff (file)
  (re-search-forward (concat "^\\+\\+\\+ mod/" file "$")))
(defalias 'baz-dvc-name-construct 'baz--name-construct)
(defun baz-dvc-revision-direct-ancestor (revision)
  `(baz (revision ,(baz-revision-direct-ancestor (cadr (cadr revision))))))
(defun baz-dvc-log-edit-file-name-func ()
  (baz-make-log))

;;;###autoload
(defalias 'baz-dvc-command-version 'baz-command-version)

(provide 'baz-dvc)
;;; baz-dvc.el ends here

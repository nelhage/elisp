;;; bzr-core.el --- Core of support for Bazaar 2 in DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

;; Keywords: tools, vc

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

;;

;;; Code:

;; TODO autoconf stuff.
(defvar bzr-executable (if (eq system-type 'windows-nt) "bzr.bat" "bzr")
  "The executable used for the bzr command line client")

;;;###autoload
(defun bzr-tree-root (&optional location no-error interactive)
  "Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at a .bzr/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
bzr-managed tree (but return nil)."
  (interactive)
  (dvc-tree-root-helper ".bzr/checkout/" (or interactive
                                             (interactive-p))
                        "%S is not a bzr-managed tree"
                        location no-error))

;;;###autoload
(defun bzr-branch-root (&optional location no-error interactive)
  "Return the branch root for LOCATION, nil if not in a branch.

This function allows DVC relevant functions (e.g., log) to work
on bzr branches with no tree."
  (interactive)
  (dvc-tree-root-helper ".bzr/branch/" (or interactive
                                           (interactive-p))
                        "%S is not a bzr-managed branch"
                        location no-error))

;;;###autoload
(defun bzr-tree-id ()
  "Call \"bzr log -r 1\" to get the tree-id.
Does anyone know of a better way to get this info?"
  (interactive)
  (let ((tree-id nil))
    (dvc-run-dvc-sync
     'bzr (list "log" "-r" "1")
     :finished (lambda (output error status arguments)
                 (set-buffer output)
                 (goto-char (point-min))
                 (if (re-search-forward "^branch nick:\\s-*\\(.+\\)$" nil t)
                     (setq tree-id (match-string 1))
                   (setq tree-id "<unknown>")))
     :error (lambda (output error status arguments)
              (setq tree-id "<unknown>")))
    (when (interactive-p)
      (message "tree-id for %s: %s" default-directory tree-id))
    tree-id))

;;;###autoload
(defun bzr-prepare-environment (env)
  "Prepare the environment to run bzr."
  (cons "BZR_PROGRESS_BAR=none" env))

;;;###autoload
(defun bzr-default-global-argument ()
  "Disable aliases."
  '("--no-aliases"))

(defun bzr-read-revision (prompt)
  "Read a revision for the actual bzr working copy."
  (read-string prompt (bzr-get-revision-at-point)))

(provide 'bzr-core)
;;; bzr-core.el ends here

;;; tla-dvc.el --- The dvc layer for xtla

;; Copyright (C) 2005-2008 by all contributors

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

;; This file provides the common dvc layer for tla support


;;; History:

;;

;;; Code:

(require 'tla-core)
(eval-and-compile (require 'dvc-unified))
;; ----------------------------------------------------------------------------
;; The dvc functionality
;; ----------------------------------------------------------------------------

;;;###autoload
(dvc-register-dvc 'tla "GNU Arch")

(defalias 'tla-dvc-tree-root 'tla-tree-root)

(defun tla-dvc-diff (base-rev path dont-switch)
  ;; 09.09.2007: We should use base-rev here, but that
  ;; does not work for tla. So drop base-rev to make dvc-diff work for tla again...
  ;;(tla-changes nil base-rev))
  (tla-changes nil))

(defun tla-dvc-file-diff (file &optional base modified dont-switch)
  ;; FIXME: tla-file-diff expects BASE is a string.
  ;; However, tla-dvc-file-diff receives BASE in a list revision form.
  ;; To fill the gap, nil is passed to. -- Masatake.
  ;; FIXME: only tla overrides dvc-dvc-file-diff; perhaps it doesn't need to?
  (tla-file-diff file nil modified dont-switch))

(defun tla-dvc-status ()
  (tla-changes t nil))

(defalias 'tla-dvc-submit-patch 'tla-submit-patch)

(defun tla-dvc-update ()
  (interactive)
  (tla-update default-directory))

(defun tla-dvc-log-edit (&optional root other-frame no-init)
  (interactive "P")
  (tla-edit-log nil (current-buffer) other-frame))

(defun tla-dvc-add (file)
  (tla-add nil file))

(defun tla-dvc-remove-files (&rest files)
  "Call `tla-remove' to remove a list of files."
  (apply 'tla-remove nil files))

(defun tla-dvc-rename (from-name to-name bookkeep-only)
  (interactive)
  (tla-move from-name to-name bookkeep-only))

(defun tla-dvc-log (arg last-n)
  "Show the log for the current Arch tree."
  (tla-logs))

(defun tla-dvc-changelog ()
  "Show the changelog for the current Arch tree."
  (tla-changelog))

(defun tla-dvc-search-file-in-diff (file)
  (re-search-forward (concat "^\\+\\+\\+ mod/" file "$")))

(defalias 'tla-dvc-name-construct 'tla--name-construct)

(defun tla-dvc-revision-direct-ancestor (revision)
  `(tla (revision ,(tla-revision-direct-ancestor (cadr (cadr revision))))))

(defun tla-dvc-log-edit-file-name-func ()
  (tla-make-log))

(defun tla-dvc-inventory ()
  (interactive)
  (tla-inventory))

(defun tla-dvc-missing (&optional other)
  (interactive)
  ;; eventually move the user input logic from tla-missing-1 to this function...
  (tla-missing-1 (tla-tree-root nil t) (tla-tree-version)))

;;;###autoload
(defalias 'tla-dvc-command-version 'tla-command-version)

(defun tla-dvc-delta (base modified &optional dont-switch)
  (interactive (error "TODO: interactive not implemented"))
  (if (and (eq (dvc-revision-get-type base) 'previous-revision)
           (eq (dvc-revision-get-type modified) 'revision)
           (equal (car (dvc-revision-get-data
                        (car (dvc-revision-get-data base))))
                  (car (dvc-revision-get-data modified))))
      ;; base is the ancestor of modified. Optimization possible
      (tla-get-changeset (car (dvc-revision-get-data
                               (car (dvc-revision-get-data base))))
                         t)
    (tla-delta (tla--name-construct (tla-revision-id-to-list base))
               (tla--name-construct (tla-revision-id-to-list modified))
               nil dont-switch)))

;; TODO: This should be an alias for tla-revert-files in the future.
(defun tla-dvc-revert-files (&rest files)
  "See `tla-inventory-revert-file'"
  (mapcar 'tla-inventory-revert-file files))

;;;###autoload
(defalias 'tla-dvc-file-has-conflict-p 'tla-file-has-conflict-p)

(defalias 'tla-dvc-resolved 'tla-resolved)

(defalias 'tla-dvc-init 'tla-start-project)

(provide 'tla-dvc)
;;; tla-dvc.el ends here

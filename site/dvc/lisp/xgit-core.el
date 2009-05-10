;;; xgit-core.el --- Common definitions for git support in DVC

;; Copyright (C) 2006-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Takuzo O'hara <takuzo.ohara@gmail.com>

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

;; This file provides the low-level functions used by the git interface
;; from DVC.


;;; History:

;;

;;; Code:

(require 'dvc-core)

(defgroup dvc-xgit nil
  "Git support in dvc"
  :group 'dvc)

;; Settings for git
(defcustom xgit-executable "git"
  "The executable used for the git commandline client."
  :type 'string
  :group 'dvc-xgit)

(defcustom xgit-git-dir-mapping nil
  "A mapping from the root of a directory tree to the desired
git metadata directory."
  :type '(repeat (list :tag "Rule"
                       (regexp :tag "Root dir")
                       (directory :tag "Git dir")))
  :group 'dvc-xgit)

(defvar xgit-log-edit-file-name
  "DVC_EDITMSG"
  "The filename used to store the log message before commiting.
Usually that file is placed in the .git directory of the working tree.")

(defun xgit-lookup-external-git-dir (&optional location root)
  "Check to see whether the user has specified a custom git metadata
directory in `xgit-git-dir-mapping'.

If root is non-nil, return the tree root, which is guaranteed to
end with a trailing slash.  Otherwise, return the git metadata
directory.

If no rule from `xgit-git-dir-mapping' matches, return nil."
  (setq location (file-name-as-directory (or location default-directory)))
  (save-match-data
    (catch 'found
      (dolist (rule xgit-git-dir-mapping)
        (when (string-match (concat "^" (directory-file-name (car rule)) "/")
                            location)
          (throw 'found (if root (match-string 0 location)
                          (cadr rule)))))
      nil)))

;;;###autoload
(defun xgit-tree-root (&optional location no-error interactive)
  "Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an .git/
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not a
git managed tree (but return nil)."
  (or (xgit-lookup-external-git-dir location t)
      (dvc-tree-root-helper ".git/" (or interactive (interactive-p))
                            "%S is not in a git tree!"
                            location no-error)))

;; Stefan: 17.05.2007: not sure, if xgit-tree-has-head is still needed/valid
(defun xgit-tree-has-head ()
  "Return t, if the git repository has a valid HEAD entry.
It will be nil before the initial commit."
  (file-readable-p (concat (xgit-tree-root) ".git/HEAD")))

(defun xgit-git-dir (&optional location)
  "Return directory name name for .git git metadata directory for LOCATION."
  (let ((git-dir (xgit-lookup-external-git-dir location)))
    (concat (file-relative-name
             (or git-dir (xgit-tree-root location))
             (file-name-as-directory (or location default-directory)))
            (if git-dir "" ".git"))))

(defun xgit-git-dir-option (&optional location)
  "Utility function to add --git-dir option to git command."
  ;; git barfs when "~/" is in the --git-dir argument, so we cannot
  ;; just concat the result of xgit-tree-root as-is
  (concat "--git-dir=" (xgit-git-dir location)))

(defconst xgit-hash-regexp "[0-9a-f]\\{40\\}")

;;;###autoload
(defun xgit-prepare-environment (env)
  "Prepare the environment to run git."
  ;; git pipes the result of "git log" to the PAGER, so set
  ;; GIT_PAGER=cat to work around that feature
  (let ((git-dir (xgit-lookup-external-git-dir)))
    (nconc (when git-dir (list (concat "GIT_DIR=" git-dir)))
           (list "GIT_PAGER=cat")
           env)))

(provide 'xgit-core)
;;; xgit-core.el ends here

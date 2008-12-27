;;; xdarcs-dvc.el --- The dvc layer for darcs

;; Copyright (C) 2006, 2007, 2008 by all contributors

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

;; This file provides the common dvc layer for darcs


;;; History:

;;

;;; Code:

(require 'xdarcs)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'xdarcs "Darcs")

;;;###autoload
(defalias 'xdarcs-dvc-tree-root 'xdarcs-tree-root)

;;;###autoload
(defalias 'xdarcs-dvc-command-version 'xdarcs-command-version)

;;;###autoload
(defalias 'xdarcs-dvc-status 'xdarcs-whatsnew)

;;;###autoload
(defalias 'xdarcs-dvc-pull 'xdarcs-pull)

(defvar xdarcs-ignore-file "_darcs/prefs/boring"
  "Relative path of the darcs boring file within the xdarcs-tree-root.")

(defun xdarcs-dvc-edit-ignore-files ()
  (interactive)
  (find-file-other-window (concat (xdarcs-tree-root) xdarcs-ignore-file)))

(defun xdarcs-dvc-ignore-files (file-list)
  (interactive (list (dvc-current-file-list)))
  (when (y-or-n-p (format "Ignore %S for %s? " file-list (xdarcs-tree-root)))
    (with-current-buffer
        (find-file-noselect (concat (xdarcs-tree-root) xdarcs-ignore-file))
      (goto-char (point-max))
      (dolist (f-name file-list)
        (insert (format "^%s$\n" (regexp-quote f-name))))
      (save-buffer))))

(defun xdarcs-dvc-backend-ignore-file-extensions (extension-list)
  (with-current-buffer
      (find-file-noselect (concat (xdarcs-tree-root) xdarcs-ignore-file))
    (goto-char (point-max))
    (dolist (ext-name extension-list)
      (insert (format "\\.%s$\n" (regexp-quote ext-name))))
    (save-buffer)))

(provide 'xdarcs-dvc)
;;; xdarcs-dvc.el ends here

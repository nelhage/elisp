;;; xgit-dvc.el --- The dvc layer for git

;; Copyright (C) 2006-2009 by all contributors

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

;; This file provides the common dvc layer for git


;;; History:

;;

;;; Code:

(require 'xgit)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'xgit "git")

;;;###autoload
(defalias 'xgit-dvc-tree-root 'xgit-tree-root)

;;;###autoload
(defalias 'xgit-dvc-command-version 'xgit-command-version)

(defalias 'xgit-dvc-delta 'xgit-delta)

(defun xgit-dvc-log-edit-file-name-func ()
  (concat (file-name-as-directory (xgit-git-dir))
	  xgit-log-edit-file-name))

(defun xgit-dvc-log-edit-done (&optional invert-normal)
  "Finish a commit for git, using git commit.

If the partner buffer has files marked, then the index will
always be used.  Otherwise, the `xgit-use-index' option
determines whether the index will be used in this commit.

If INVERT-NORMAL is non-nil, the behavior opposite of that
specified by `xgit-use-index' will be used in this commit."
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name)))
        (files-to-commit (when (buffer-live-p dvc-partner-buffer)
                           (with-current-buffer dvc-partner-buffer
                             (dvc-current-file-list 'nil-if-none-marked))))
        (use-index (if (or (eq xgit-use-index 'ask)
                           (not invert-normal))
                       (xgit-use-index-p)
                     (not (xgit-use-index-p)))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (message "committing %S in %s" (or files-to-commit "all files")
             (dvc-tree-root))
    (dvc-run-dvc-sync
     'xgit (append (list "commit"
                         (unless (or files-to-commit use-index) "-a")
                         "-F" (dvc-log-edit-file-name))
                   files-to-commit)
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (dvc-show-error-buffer output 'commit)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (with-current-buffer error
                             (buffer-string))))
                 (dvc-log-close (capture buffer))
                 ;; doesn't work at the moment (Stefan, 10.02.2006)
                 ;; (dvc-diff-clear-buffers 'xgit (capture default-directory)
                 ;;  "* Just committed! Please refresh buffer\n")
                 (message "git commit finished")))
    (dvc-tips-popup-maybe)))

;;;###autoload
(defun xgit-dvc-log (arg last-n)
  "Shows the changelog in the current git tree.
ARG is passed as prefix argument"
  (call-interactively 'xgit-log))

(defalias 'xgit-dvc-revlog-get-revision 'xgit-revlog-get-revision)

(defalias 'xgit-dvc-name-construct 'xgit-name-construct)

(defun xgit-dvc-changelog (&optional arg)
  "Shows the changelog in the current git tree.
ARG is passed as prefix argument"
  (call-interactively 'xgit-log))

(defalias 'xgit-dvc-prepare-environment 'xgit-prepare-environment)

(defalias 'xgit-dvc-revision-get-last-revision 'xgit-revision-get-last-revision)

(defalias 'xgit-dvc-last-revision 'xgit-last-revision)

(defalias 'xgit-dvc-annotate-time 'xgit-annotate-time)

(defun xgit-dvc-missing (&optional other)
  "Run 'git fetch origin; git log HEAD..origin'"
  (interactive)
  (xgit-fetch "origin")
  (xgit-changelog "HEAD" "origin" t))

(defun xgit-dvc-pull ()
  "Run 'git pull origin'"
  (interactive)
  (xgit-pull "origin"))

(defun* xgit-dvc-push (url &optional (branch "master"))
  "Run 'git push url'.
with prefix arg ask for branch, default to master."
  (interactive "sGit push to: ")
  (xgit-push url branch))

(defalias 'xgit-dvc-clone 'xgit-clone)

(defalias 'xgit-dvc-create-branch 'xgit-branch)
(defalias 'xgit-dvc-select-branch 'xgit-checkout)
(defalias 'xgit-dvc-list-branches 'xgit-branch-list)

(defalias 'xgit-dvc-send-commit-notification 'xgit-gnus-send-commit-notification)
(defalias 'xgit-dvc-init 'xgit-init)

;;;###autoload
(defalias 'xgit-dvc-add 'xgit-add)

(defun xgit-dvc-edit-ignore-files ()
  "Edit git's ignore file.
TODO: Support per directory ignore file.
	  This only supports exclude file now."
  (interactive)
  (find-file-other-window (xgit-get-root-exclude-file)))

(defun xgit-dvc-ignore-files (file-list)
  "Added FILE-LIST to git's ignore file.
TODO: Support per directory ignore file.
	  This only supports exclude file now."
  (interactive (list (dvc-current-file-list)))

  (when (y-or-n-p (format "Ignore %S for %s? "
			  file-list
			  (xgit-git-dir)))
	(with-current-buffer
		(find-file-noselect (xgit-get-root-exclude-file))
	  (goto-char (point-max))
	  (dolist (f-name file-list)
		(insert (format "%s\n" f-name)))
	  (save-buffer))))

(provide 'xgit-dvc)
;;; xgit-dvc.el ends here

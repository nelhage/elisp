;;; xhg-dvc.el --- The dvc layer for xhg

;; Copyright (C) 2005-2008 by all contributors

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

;; This file provides the common dvc layer for xhg


;;; History:

;;

;;; Code:

(require 'xhg)
(eval-and-compile (require 'dvc-unified))

;;;###autoload
(dvc-register-dvc 'xhg "Mercurial")

;;;###autoload
(defalias 'xhg-dvc-tree-root 'xhg-tree-root)

;;;###autoload
(defalias 'xhg-dvc-merge 'xhg-merge)

;;;###autoload
(defun xhg-dvc-export-via-email ()
  (interactive)
  (call-interactively 'xhg-export-via-mail))

(defvar xhg-dvc-commit-extra-parameters nil "A list of extra parameters for the next hg commit.")

(defvar xhg-commit-done-hook '()
  "*Hooks run after a successful commit via `xhg-dvc-log-edit-done'.")

(defun xhg-select-committer-for-next-commit (committer)
  "Select the committer for the next hg commit.
This is done via setting `xhg-dvc-commit-extra-parameters'."
  (interactive (list (read-string "Committer for next hg commit: " xhg-gnus-patch-from-user)))
  (setq xhg-dvc-commit-extra-parameters `("--user" ,committer)))

;; Base functions that are required for every supported dvc system
(defun xhg-dvc-log-edit-done ()
  "Finish a commit for Mercurial."
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name)))
        (files-to-commit (with-current-buffer dvc-partner-buffer (dvc-current-file-list 'nil-if-none-marked))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (message "committing %S in %s" (or files-to-commit "all files") (dvc-tree-root))
    (dvc-run-dvc-sync
     'xhg (append (list "commit" "-l" (dvc-log-edit-file-name))
                  xhg-dvc-commit-extra-parameters files-to-commit)
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (dvc-show-error-buffer output 'commit)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (with-current-buffer error
                             (buffer-string))))
                 (dvc-log-close (capture buffer))
                 ;; doesn't work at the moment (Stefan, 10.02.2006)
                 ;; (dvc-diff-clear-buffers 'xhg (capture default-directory)
                 ;;  "* Just committed! Please refresh buffer\n")
                 (setq xhg-dvc-commit-extra-parameters nil)
                 (message "Mercurial commit finished")
                 (dvc-tips-popup-maybe)
                 (run-hooks 'xhg-commit-done-hook)))))

;;;###autoload
(defalias 'xhg-dvc-save-diff 'xhg-save-diff)

;;;###autoload
(defalias 'xhg-dvc-command-version 'xhg-command-version)

(defun xhg-dvc-changelog (&optional arg)
  "Shows the changelog in the current Mercurial tree.
ARG is passed as prefix argument"
  (call-interactively 'xhg-log))

;; deactivated at them moment, use dvc-dvc-files-to-commit to allow selecting files to commit
;; (defun xhg-dvc-files-to-commit ()
;;   ;; -mar: modified+added+removed
;;   (dvc-run-dvc-sync 'xhg (list "status" "-mar")
;;                     :finished (dvc-capturing-lambda
;;                                   (output error status arguments)
;;                                 (let ((file-list)
;;                                       (modif)
;;                                       (file-name))
;;                                   (set-buffer output)
;;                                   (goto-char (point-min))
;;                                   (while (> (point-max) (point))
;;                                     (cond ((looking-at "M ")
;;                                            (setq modif 'dvc-modified))
;;                                           ((looking-at "A ")
;;                                            (setq modif 'dvc-added))
;;                                           ((looking-at "R ")
;;                                            (setq modif 'dvc-move))
;;                                           (t
;;                                            (setq modif nil)))
;;                                     (setq file-name (buffer-substring-no-properties (+ (point) 2) (line-end-position)))
;;                                     (add-to-list 'file-list (cons modif file-name))
;;                                     (forward-line 1))
;;                                     file-list))))

(defun xhg-dvc-edit-ignore-files ()
  (interactive)
  (find-file-other-window (concat (xhg-tree-root) ".hgignore")))

(defun xhg-dvc-ignore-files (file-list)
  (interactive (list (dvc-current-file-list)))
  (when (y-or-n-p (format "Ignore %S for %s? " file-list (xhg-tree-root)))
    (with-current-buffer
        (find-file-noselect (concat (xhg-tree-root) ".hgignore"))
      (goto-char (point-max))
      (dolist (f-name file-list)
        (insert (format "^%s$\n" (regexp-quote f-name))))
      (save-buffer))))

(defun xhg-dvc-backend-ignore-file-extensions (extension-list)
  (with-current-buffer
      (find-file-noselect (concat (xhg-tree-root) ".hgignore"))
    (goto-char (point-max))
    (dolist (ext-name extension-list)
      (insert (format "\\.%s$\n" (regexp-quote ext-name))))
    (save-buffer)))

(defun xhg-dvc-missing (&optional other)
  "Run hg incoming to show the missing patches for this tree.
When `last-command' was `dvc-pull', run `xhg-missing'."
  (interactive)
  (if (eq last-command 'dvc-pull)
      (xhg-missing-1)
    (xhg-incoming other t)))

(defun xhg-dvc-update ()
  (interactive)
  (xhg-update))

(defvar xhg-dvc-pull-runs-update t
  "Whether `xhg-dvc-pull' should call hg pull with the --update flag.")

(defun xhg-dvc-pull (&optional other)
  "Run hg pull, when `xhg-dvc-pull-runs-update' is t, use the --update flag."
  (interactive)
  (let ((source-path
         (or other
             (let* ((completions (xhg-paths 'both))
                    (initial-input (car (member "default" completions))))
               (if (string= initial-input "default") initial-input
                 (dvc-completing-read
                  "Pull from hg repository: "
                  completions nil nil initial-input))))))
    (xhg-pull source-path xhg-dvc-pull-runs-update)))

(defalias 'xhg-dvc-revlog-get-revision 'xhg-revlog-get-revision)

(defalias 'xhg-dvc-name-construct 'xhg-name-construct)

(defalias 'xhg-dvc-delta 'xhg-delta)

(defalias 'xhg-dvc-clone 'xhg-clone)

(defalias 'xhg-dvc-init 'xhg-init)

(defalias 'xhg-dvc-push 'xhg-push)

(provide 'xhg-dvc)
;;; xhg-dvc.el ends here

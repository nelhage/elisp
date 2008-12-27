;;; bzr-submit.el --- Patch submission support for Bazaar 2 in DVC

;; Copyright (C) 2006 by all contributors

;; Author: Michael Olson <mwolson@gnu.org>

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

(require 'bzr-core)
(require 'bzr)
(require 'diff-mode)

(defgroup dvc-bzr-submit nil
  "Submitting and applying patches via email for bzr."
  :group 'dvc
  :prefix "bzr-submit-")

(defcustom bzr-apply-patch-mapping nil
  "*Project in which patches should be applied.

An alist of rules to map branch nicknames to target directories.

This is used by the `bzr-gnus-apply-patch' function.
Example setting: '((\"dvc-dev-bzr\" \"~/work/bzr/dvc\"))"
  :type '(repeat (list :tag "Rule"
                       (string :tag "Branch nickname")
                       (string :tag "Target directory")))
  :group 'dvc-bzr-submit)

(defcustom bzr-submit-patch-mapping
  '(("dvc-dev-bzr" ("dvc-dev@gna.org" "dvc")))
  "*Email addresses that should be used to send patches.

An alist of rules to map branch nicknames to target email
addresses and the base name to use in the attached patch.

This is used by the `bzr-submit-patch' function."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Branch nickname")
                       (list :tag "Target"
                             (string :tag "Email address")
                             (string :tag "Base name of patch"))))
  :group 'dvc-bzr-submit)

(defcustom bzr-patch-sent-action 'keep-both
  "*What shall be done, after sending a patch via mail.
The possible values are 'keep-patch, 'keep-changes, 'keep-both, 'keep-none."
  :type '(choice (const keep-patch)
                 (const keep-changes)
                 (const keep-both)
                 (const keep-none))
  :group 'dvc-bzr-submit)

(defvar bzr-patch-data nil)

(defun bzr-changed-files (&optional include-added)
  "Retrieve a list of files in the current repo that have changed.
If INCLUDE-ADDED is specified, include files that are newly-added."
  (let ((default-directory (bzr-tree-root))
        (files nil))
    (dvc-run-dvc-sync
     'bzr (list "status")
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (set-buffer output)
                 (goto-char (point-min))
                 (when (and include-added
                            (re-search-forward "^added:" nil t))
                   (forward-line 1)
                   (while (looking-at "^  \\([^ ].*\\)$")
                     (setq files (cons (match-string 1) files))
                     (forward-line 1)))
                 (goto-char (point-min))
                 (when (re-search-forward "^modified:" nil t)
                   (forward-line 1)
                   (while (looking-at "^  \\([^ ].*\\)$")
                     (setq files (cons (match-string 1) files))
                     (forward-line 1))))
     :error (lambda (output error status arguments)
              (error "An error occurred")))
    files))

(defun dvc-read-several-from-list (prompt items)
  "Read several string ITEMS from list, using PROMPT."
  (let ((chosen nil)
        (table (mapcar #'list items))
        item)
    (while (progn
             (and table
                  (setq item (dvc-completing-read prompt table nil t))
                  (stringp item)
                  (not (string= item ""))))
      (setq chosen (cons item chosen))
      (setq table (delete (list item) table)))
    chosen))

(defun bzr-show-diff-from-file (file)
  "Display the diff contained in FILE with DVC font-locking."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (let ((buffer (dvc-prepare-changes-buffer nil nil 'diff nil 'bzr))
          (output (current-buffer)))
      (when dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer))
      ;; Since we did not search for a tree root, some things may not work from the diff buffer.
      (dvc-show-changes-buffer output 'bzr-parse-diff buffer))))

(defun bzr-changes-save-as-patch (file-name
                                  &optional included-files prompt-files)
  "Run \"bzr diff\" to create a .diff file.
The changes are stored in the patch file 'FILE-NAME.diff'.
INCLUDED-FILES lists the files whose changes will be included.  If
this is nil, include changes to all files.
PROMPT-FILES indicates whether to prompt for the files to include in
the patch.  This is only heeded when the function is not called
interactively."
  (interactive
   (list (read-file-name (concat "File to store the patch in "
                                 "(without an extension): ")
                         nil "")
         (dvc-read-several-from-list
          "Files to include (all by default, RET ends): "
          (bzr-changed-files t))))
  (when (and (not (interactive-p)) prompt-files)
    (setq included-files (dvc-read-several-from-list
                          "Files to include (all by default, RET ends): "
                          (bzr-changed-files t))))
  (let ((patch-file-name (concat (expand-file-name file-name) ".diff"))
        (default-directory (bzr-tree-root))
        (continue t))
    (dvc-run-dvc-sync
     'bzr (nconc (list "diff") included-files)
     :finished (lambda (output error status arguments)
                 (message "No changes occurred"))
     :error (dvc-capturing-lambda
                (output error status arguments)
              (set-buffer output)
              (write-file patch-file-name)))))

(defun bzr-undo-diff-from-file (file root-dir)
  "Undo the changes contained in FILE to the bzr project whose
root is ROOT-DIR."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (diff-mode)
    (goto-char (point-min))
    (let ((default-directory root-dir)
          (diff-advance-after-apply-hunk nil))
      (while (re-search-forward diff-file-header-re nil t)
        (condition-case nil
            (while (progn (diff-apply-hunk t)
                          (re-search-forward diff-hunk-header-re nil t)))
          (error nil))))))

;;;###autoload
(defun bzr-prepare-patch-submission (bzr-tree-root
                                     patch-base-name email version-string
                                     &optional description subject
                                     prompt-files)
  "Submit a patch to a bzr working copy (at BZR-TREE-ROOT) via email.
With this feature it is not necessary to branch a bzr archive.
You simply edit your checked out copy from your project and call this function.
The function will create a patch as a .diff file (based on PATCH-BASE-NAME)
and send it to the given email address EMAIL.
VERSION-STRING should indicate the version of bzr that the patch applies to.
DESCRIPTION is a brief descsription of the patch.
SUBJECT is the subject for the email message.
PROMPT-FILES indicates whether to prompt for the files to include in
the patch.
For an example, how to use this function see: `bzr-submit-patch'."
  (interactive)

  ;; create the patch
  (let* ((default-directory bzr-tree-root)
         (patch-directory (expand-file-name ".tmp-dvc/" bzr-tree-root))
         (patch-full-base-name (expand-file-name patch-base-name
                                                 patch-directory))
         (patch-full-name (concat patch-full-base-name ".diff")))
    (unless (file-exists-p patch-directory)
      (make-directory patch-directory))
    (bzr-changes-save-as-patch patch-full-base-name nil prompt-files)

    (require 'reporter)
    (delete-other-windows)
    (reporter-submit-bug-report email nil nil nil nil description)

    (set (make-local-variable 'bzr-patch-data)
         (list patch-full-name bzr-tree-root patch-full-name))
    (insert "[VERSION] " version-string "\n\n")
    (insert bzr-command-version)
    (goto-char (point-max))
    (mml-attach-file patch-full-name "text/x-patch")
    (bzr-show-diff-from-file patch-full-name)
    (other-window 1)

    (goto-char (point-min))
    (mail-position-on-field "Subject")
    (insert (or subject "[PATCH] "))))

(defun bzr-submit-patch-done ()
  "Clean up after sending a patch via mail.
That function is usually called via `message-sent-hook'.  Its
purpose is to revert the sent changes or to delete sent changeset
patch \(see: `bzr-patch-sent-action')."
  (when bzr-patch-data
    (when (memq bzr-patch-sent-action '(keep-patch keep-none))
      (message "Reverting the sent changes in %s" (car bzr-patch-data))
      (bzr-undo-diff-from-file (car bzr-patch-data) (cadr bzr-patch-data)))
    (when (memq bzr-patch-sent-action '(keep-changes keep-none))
      (message "Deleting the sent patch %s" (car (cddr bzr-patch-data)))
      (delete-file (car (cddr bzr-patch-data))))
    (when (memq bzr-patch-sent-action '(keep-both))
      (message "Keeping the sent changes and the sent patch %s"
               (car (cddr bzr-patch-data))))))
(add-hook 'message-sent-hook 'bzr-submit-patch-done)

;;;###autoload
(defun bzr-submit-patch ()
  "Submit a patch for the current bzr project.
With this feature it is not necessary to tag an arch archive.
You simply edit your checked out copy and call this function.
The function will create a patch as *.tar.gz file and prepare a buffer to
send the patch via email.

The variable `bzr-submit-patch-mapping' allows to specify the
target email address and the base name of the sent tarball.

After the user has sent the message, `bzr-submit-patch-done' is called."
  (interactive)
  (if (string= (dvc-run-dvc-sync 'bzr '("status" "-V")
                                 :finished 'dvc-output-buffer-handler)
               "")
      (message "No changes in this bzr working copy - please apply your patch locally and submit it.")
    (bzr-command-version)
    (let* ((tree-id (bzr-tree-id))
           (submit-patch-info (cadr (assoc tree-id
                                           bzr-submit-patch-mapping)))
           (mail-address (or (nth 0 submit-patch-info) ""))
           (patch-base-file-name (or (nth 1 submit-patch-info) "bzr")))
      (bzr-prepare-patch-submission
       (dvc-uniquify-file-name (bzr-tree-root))
       (concat patch-base-file-name "-patch-"
               (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
       mail-address
       tree-id
       dvc-patch-email-message-body-template
       nil
       (interactive-p)))))

(provide 'bzr-submit)
;;; bzr-submit.el ends here

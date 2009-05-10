;;; dvc-unified.el --- The unification layer for dvc

;; Copyright (C) 2005-2009 by all contributors

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

;; This file provides the functionality that unifies the various dvc layers

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `dvc-init'
;;    Initialize a new repository.
;;  `dvc-add-files'
;;    Add FILES to the currently active dvc. FILES is a list of
;;  `dvc-revert-files'
;;    Revert FILES for the currently active dvc.
;;  `dvc-remove-files'
;;    Remove FILES for the currently active dvc.
;;  `dvc-clone'
;;    Ask for the DVC to use and clone SOURCE-PATH.
;;  `dvc-diff'
;;    Display the changes from BASE-REV to the local tree in PATH.
;;  `dvc-diff-against-url'
;;    Show the diff from the current tree against a remote url
;;  `dvc-status'
;;    Display the status in optional PATH tree.
;;  `dvc-log'
;;    Display the brief log for PATH (a file-name; default current
;;  `dvc-apply-patch'
;;    Apply patch `patch-name' on current-tree.
;;  `dvc-rename'
;;    Rename file FROM-NAME to TO-NAME; TO-NAME may be a directory.
;;  `dvc-command-version'
;;    Returns and/or shows the version identity string of backend command.
;;  `dvc-tree-root'
;;    Get the tree root for PATH or the current `default-directory'.
;;  `dvc-log-edit'
;;    Edit the log before commiting. Optional OTHER_FRAME (default
;;  `dvc-ignore-file-extensions'
;;    Ignore the file extensions of the marked files, in all
;;  `dvc-ignore-file-extensions-in-dir'
;;    Ignore the file extensions of the marked files, only in the
;;  `dvc-missing'
;;    Show revisions missing from PATH (default prompt),
;;  `dvc-push'
;;    Push changes to a remote location.
;;  `dvc-create-branch'
;;    Create a new branch.
;;  `dvc-select-branch'
;;    Select a branch.
;;  `dvc-list-branches'
;;    List available branches.
;;


;;; History:

;;

;;; Code:

(require 'dvc-register)
(require 'dvc-core)
(require 'dvc-defs)
(require 'dvc-tips)
(require 'dvc-utils)

;; --------------------------------------------------------------------------------
;; unified functions
;; --------------------------------------------------------------------------------

;;;###autoload
(defun dvc-init ()
  "Initialize a new repository.
It currently supports the initialization for bzr, xhg, xgit, tla.
Note: this function is only useful when called interactively."
  (interactive)
  (when (interactive-p)
    (let ((supported-variants (map t 'symbol-name dvc-registered-backends))
          (working-dir (dvc-uniquify-file-name default-directory))
          (dvc))
      ;; hide backends that don't provide an init function
      (mapcar '(lambda (elem)
                (setq supported-variants (delete elem supported-variants)))
              '("xdarcs" "xmtn" "baz"))
      (add-to-list 'supported-variants "bzr-repo")
      (setq dvc (intern (dvc-completing-read
                         (format "Init a repository for '%s', using dvc: " working-dir)
                         (sort supported-variants 'string-lessp))))
      (cond ((string= dvc "bzr-repo")
             (call-interactively 'bzr-init-repository))
            (t
             (funcall (dvc-function dvc "dvc-init") working-dir))))))

;;;###autoload
(defun dvc-add-files (&rest files)
  "Add FILES to the currently active dvc. FILES is a list of
strings including path from root; interactive defaults
to (dvc-current-file-list)."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "add" files dvc-confirm-add))
    (dvc-apply "dvc-add-files" files)))

;;;###autoload
(defun dvc-revert-files (&rest files)
  "Revert FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "revert" files t))
    (dvc-apply "dvc-revert-files" files)))

;;;###autoload
(defun dvc-remove-files (&rest files)
  "Remove FILES for the currently active dvc."
  (interactive (dvc-current-file-list))
  (when (setq files (dvc-confirm-file-op "remove" files t))
    (dvc-apply "dvc-remove-files" files)))

(defun dvc-remove-optional-args (spec &rest args)
  "Process ARGS, removing those that come after the &optional keyword
in SPEC if they are nil, returning the result."
  (let ((orig args)
        new)
    (if (not (catch 'found
               (while (and spec args)
                 (if (eq (car spec) '&optional)
                     (throw 'found t)
                   (setq new (cons (car args) new)
                         args (cdr args)
                         spec (cdr spec))))
               nil))
        orig
      ;; an &optional keyword was found: process it
      (let ((acc (reverse args)))
        (while (and acc (null (car acc)))
          (setq acc (cdr acc)))
        (when acc
          (setq new (nconc acc new)))
        (nreverse new)))))

;;;###autoload
(defmacro define-dvc-unified-command (name args comment &optional interactive)
  "Define a DVC unified command.  &optional arguments are permitted, but
not &rest."
  (declare (indent 2)
	   (debug (&define name lambda-list stringp
			   [&optional interactive])))
  `(defun ,name ,args
     ,comment
     ,@(when interactive (list interactive))
     (dvc-apply ,(symbol-name name)
                (dvc-remove-optional-args ',args
                                          ,@(remove '&optional args)))))

;;;###autoload
(defun dvc-clone (&optional dvc source-path dest-path rev)
  "Ask for the DVC to use and clone SOURCE-PATH."
  (interactive "P")
  (when (interactive-p)
    (let* ((ffap-url-regexp
            (concat
             "\\`\\("
             "\\(ftp\\|https?\\|git\\|www\\)://" ; needs host
             "\\)."                              ; require one more character
             ))
           (url-at-point (ffap-url-at-point))
           (all-candidates (map t 'symbol-name dvc-registered-backends))
           (git-is-candidate (re-search-backward "git clone .+" (line-beginning-position) t))
           (hg-is-candidate (re-search-backward "hg clone .+" (line-beginning-position) t))
           (bzr-is-candidate (re-search-backward "bzr get .+" (line-beginning-position) t)))
      (setq dvc (intern (dvc-completing-read
                         "Clone, using dvc: "
                         all-candidates
                         nil t
                         (cond (git-is-candidate "xgit")
                               (bzr-is-candidate "bzr")
                               (hg-is-candidate "xhg")
                               (t nil)))))
      (setq source-path (read-string (format "%S-clone from path: " dvc) url-at-point))
      (setq dest-path (expand-file-name (dvc-read-directory-name
                                         (format "Destination Directory for %S-clone: " dvc)
                                         nil nil nil "<default>")))
      (if current-prefix-arg
          (unless (not (eq dvc 'xhg))
            (setq rev (read-string "FromRevision: ")))
        nil)))
  (let ((default-directory (or (file-name-directory dest-path) default-directory)))
    (when (string= (file-name-nondirectory dest-path) "<default>")
      (setq dest-path nil))
    (if rev
        (funcall (dvc-function dvc "dvc-clone") source-path dest-path rev)
      (funcall (dvc-function dvc "dvc-clone") source-path dest-path))))

;;;###autoload
(defun dvc-diff (&optional base-rev path dont-switch)
  "Display the changes from BASE-REV to the local tree in PATH.

BASE-REV (a revision-id) defaults to base revision of the
tree. Use `dvc-delta' for differencing two revisions.

PATH defaults to `default-directory', that is, the whole working tree.
See also `dvc-file-diff', which defaults to the current buffer file.

The new buffer is always displayed; if DONT-SWITCH is nil, select it."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC diff (directory): "
                                       (when path (expand-file-name path)))))
    (setq base-rev (or base-rev
                       ;; Allow back-ends to override this for e.g. git,
                       ;; which can return either the index or the last
                       ;; revision.
                       (dvc-call "dvc-last-revision" (dvc-tree-root path))))
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-diff" base-rev default-directory dont-switch)))

;;;###autoload
(defun dvc-diff-against-url (path)
  "Show the diff from the current tree against a remote url"
  (interactive)
  (dvc-save-some-buffers default-directory)
  (dvc-call "dvc-diff-against-url" path))

(defun dvc-dvc-last-revision (path)
  (list (dvc-current-active-dvc)
        (list 'last-revision path 1)))

;;;###autoload
(define-dvc-unified-command dvc-delta (base modified &optional dont-switch)
  "Display diff from revision BASE to MODIFIED.

BASE and MODIFIED must be full revision IDs, or strings. If
strings, the meaning is back-end specific; it should be some sort
of revision specifier.

The new buffer is always displayed; if DONT-SWITCH is nil, select it."
  (interactive "Mbase revision: \nMmodified revision: "))

;;;###autoload
(define-dvc-unified-command dvc-file-diff (file &optional base modified dont-switch)
  "Display the changes in FILE (default current buffer file)
between BASE (default last-revision) and MODIFIED (default
workspace version).
If DONT-SWITCH is non-nil, just show the diff buffer, don't select it."
  ;; use dvc-diff-diff to default file to dvc-get-file-info-at-point
  (interactive (list buffer-file-name)))

;;;###autoload
(defun dvc-status (&optional path)
  "Display the status in optional PATH tree."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC status (directory): "
                                       (when path (expand-file-name path)))))
    ;; Since we have bound default-directory, we don't need to pass
    ;; `path' to the back-end.
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-status"))
  nil)

(define-dvc-unified-command dvc-name-construct (back-end-revision)
  "Returns a string representation of BACK-END-REVISION.")

;;;###autoload
(defun dvc-log (&optional path last-n)
  "Display the brief log for PATH (a file-name; default current
buffer file name; nil means entire tree), LAST-N entries (default
`dvc-log-last-n'; all if nil). LAST-N may be specified
interactively. Use `dvc-changelog' for the full log."
  (interactive (list (buffer-file-name)
                     (if current-prefix-arg (prefix-numeric-value current-prefix-arg) dvc-log-last-n)))
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC tree root (directory): "
                                       (when path (expand-file-name path))
                                       t)))
    ;; Since we have bound default-directory, we don't need to pass
    ;; 'root' to the back-end.
    (dvc-call "dvc-log" path last-n))
  nil)

(defun dvc-apply-patch (patch-name)
  "Apply patch `patch-name' on current-tree."
  (interactive "fPatch: ")
  (let ((current-dvc (dvc-current-active-dvc)))
    (case current-dvc
      ('xgit (xgit-apply-patch patch-name))
      ('xhg (xhg-import patch-name))
      ;; TODO ==>Please add here appropriate commands for your backend
      (t
       (if (y-or-n-p (format "[%s] don't know how to apply patch, do you want to run a generic command instead?"
                             current-dvc))
           (shell-command (format "cat %s | patch -p1" patch-name))
           (message "I don't known yet how to patch on %s" current-dvc))))))

;;;###autoload
(define-dvc-unified-command dvc-changelog (&optional arg)
  "Display the full changelog in this tree for the actual dvc.
Use `dvc-log' for the brief log."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-add (file)
  "Adds FILE to the repository."
  (interactive "fFile: "))

(define-dvc-unified-command dvc-revision-direct-ancestor (revision)
  "Computes the direct ancestor of a revision.")

(define-dvc-unified-command dvc-revision-nth-ancestor (revision n)
  "Computes the direct ancestor of a revision.")

(define-dvc-unified-command dvc-resolved (file)
  "Mark FILE as resolved"
  (interactive (list (buffer-file-name))))

;; Look at `xhg-ediff-file-at-rev' and `xhg-dvc-ediff-file-revisions'
;; to build backend functions.
(define-dvc-unified-command dvc-ediff-file-revisions ()
  "Ediff rev1 of file against rev2."
  (interactive))

(defun dvc-rename (from-name to-name)
  "Rename file FROM-NAME to TO-NAME; TO-NAME may be a directory.
When called non-interactively, if from-file-name does not exist,
but to-file-name does, just record the rename in the back-end"
  ;; back-end function <dvc>-dvc-rename (from-name to-name bookkeep-only)
  ;; If bookkeep-only nil, rename file in filesystem and back-end
  ;; If non-nil, rename file in back-end only.
  (interactive
   (let* ((from-name (dvc-confirm-read-file-name "Rename: " t))
          (to-name (dvc-confirm-read-file-name
                    (format "Rename %s to: " from-name)
                    nil "" from-name)))
     (list from-name to-name)))

  (if (file-exists-p from-name)
      (progn
        ;; rename the file in the filesystem and back-end
        (if (and (file-exists-p to-name)
                 (not (file-directory-p to-name)))
            (error "%s exists and is not a directory" to-name))
        (when (file-directory-p to-name)
          (setq to-name (file-name-as-directory to-name)))
        (dvc-call "dvc-rename" from-name to-name nil))

    ;; rename the file in the back-end only
    (progn
      ;; rename the file in the filesystem and back-end
      (if (not (file-exists-p to-name))
          (error "%s does not exist" to-name))
      (when (file-directory-p to-name)
        (setq to-name (file-name-as-directory to-name)))
      (dvc-call "dvc-rename" from-name to-name t))))

(defvar dvc-command-version nil)
;;;###autoload
(defun dvc-command-version ()
  "Returns and/or shows the version identity string of backend command."
  (interactive)
  (setq dvc-command-version (dvc-call "dvc-command-version"))
  (when (interactive-p)
    (message "%s" dvc-command-version))
  dvc-command-version)


;;;###autoload
(defun dvc-tree-root (&optional path no-error)
  "Get the tree root for PATH or the current `default-directory'.

When called interactively, print a message including the tree root and
the current active back-end."
  (interactive)
  (let ((dvc-list (or
                   (when dvc-temp-current-active-dvc (list dvc-temp-current-active-dvc))
                   (when dvc-buffer-current-active-dvc (list dvc-buffer-current-active-dvc))
                   (append dvc-select-priority dvc-registered-backends)))
        (root "/")
        (dvc)
        (tree-root-func)
        (path (or path default-directory)))
    (while dvc-list
      (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
      (when (fboundp tree-root-func)
        (let ((current-root (funcall tree-root-func path t)))
          (when (and current-root (> (length current-root) (length root)))
            (setq root current-root)
            (setq dvc (car dvc-list)))))
      (setq dvc-list (cdr dvc-list)))
    (when (string= root "/")
      (unless no-error (error "Tree %s is not under version control"
                              path))
      (setq root nil))
    (when (interactive-p)
      (message "Root: %s (managed by %s)"
               root (dvc-variable dvc "backend-name")))
    root))

;;;###autoload
(defun dvc-log-edit (&optional other-frame no-init)
  "Edit the log before commiting. Optional OTHER_FRAME (default
user prefix) puts log edit buffer in a separate frame (or in the
same frame if `dvc-log-edit-other-frame' is non-nil). Optional
NO-INIT if non-nil suppresses initialization of buffer if one is
reused. `default-directory' must be the tree root."
  (interactive "P")
  (setq other-frame (dvc-xor other-frame dvc-log-edit-other-frame))
  ;; Reuse an existing log-edit buffer if possible.
  ;;
  ;; If this is invoked from a status or diff buffer,
  ;; dvc-buffer-current-active-dvc is set. If invoked from another
  ;; buffer (ie a source file, either directly or via
  ;; dvc-add-log-entry), dvc-buffer-current-active-dvc is nil, there
  ;; might be two back-ends to choose from, and dvc-current-active-dvc
  ;; might prompt. So we look for an existing log-edit buffer for the
  ;; current tree first, and assume the user wants the back-end
  ;; associated with that buffer (ie, it was the result of a previous
  ;; prompt).
  (let ((log-edit-buffers (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'log-edit default-directory)))
    (case (length log-edit-buffers)
      (0 ;; Need to create a new log-edit buffer. In the log-edit
       ;; buffer, dvc-partner-buffer must be set to a buffer with a
       ;; mode that dvc-current-file-list supports. That is
       ;; currently dvc-diff-mode or dired-mode; we don't have a way
       ;; to find dired-mode buffers, so we ignore those.
       (let ((diff-status-buffers
              (append (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'diff default-directory)
                      (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'status default-directory)
                      (dvc-get-matching-buffers dvc-buffer-current-active-dvc 'conflicts default-directory)))
             (activated-from-bookmark-buffer (eq major-mode 'dvc-bookmarks-mode)))
         (case (length diff-status-buffers)
           (0 (if (not activated-from-bookmark-buffer)
                  (error "Must have a DVC diff or status buffer before calling dvc-log-edit")
                (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil)))
           (1
            (set-buffer (nth 1 (car diff-status-buffers)))
            (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil))

           (t ;; multiple: choose current buffer
            (if (memq (current-buffer)
                      (mapcar #'(lambda (item) (nth 1 item))
                              diff-status-buffers))
                (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame nil)

              ;; give up. IMPROVEME: could prompt
              (if dvc-buffer-current-active-dvc
                  (error "More than one dvc-diff or dvc-status buffer for %s in %s; can't tell which to use. Please close some."
                         dvc-buffer-current-active-dvc default-directory)
                (error "More than one dvc-diff or dvc-status buffer for %s; can't tell which to use. Please close some."
                       default-directory)))))))

      (1 ;; Just reuse the buffer. In this call, we can't use
       ;; dvc-buffer-current-active-dvc from the current buffer,
       ;; because it might be nil (if we are in a source buffer). We
       ;; want to use dvc-buffer-current-active-dvc from that buffer
       ;; for this dvc-call, but we can't switch to it first,
       ;; because dvc-log-edit needs the current buffer to set
       ;; dvc-partner-buffer.
       (let ((dvc-temp-current-active-dvc
              (with-current-buffer (nth 1 (car log-edit-buffers)) dvc-buffer-current-active-dvc)))
         (dvc-call "dvc-log-edit" (dvc-tree-root) other-frame no-init)))

      (t ;; multiple matching buffers
       (if dvc-buffer-current-active-dvc
           (error "More than one log-edit buffer for %s in %s; can't tell which to use. Please close some."
                  dvc-buffer-current-active-dvc default-directory)
         (error "More than one log-edit buffer for %s; can't tell which to use. Please close some."
                default-directory))))))

(defvar dvc-back-end-wrappers
  '(("add-log-entry" (&optional other-frame))
    ("add-files" (&rest files))
    ("diff" (&optional base-rev path dont-switch))
    ("ignore-file-extensions" (file-list))
    ("ignore-file-extensions-in-dir" (file-list))
    ("log-edit" (&optional OTHER-FRAME))
    ("missing" (&optional other path force-prompt))
    ("rename" (from-name to-name))
    ("remove-files" (&rest files))
    ("revert-files" (&rest files))
    ("status" (&optional path)))
  "Alist of descriptions of back-end wrappers to define.

A back-end wrapper is a fuction called <back-end>-<something>, whose
body is a simple wrapper around dvc-<something>. This is usefull for
functions which are totally generic, but will use some back-end
specific stuff in their body.

At this point in the file, we don't have the list of back-ends, which
is why we don't do the (defun ...) here, but leave a description for
use by `dvc-register-dvc'.")

;;;###autoload
(define-dvc-unified-command dvc-log-edit-done (&optional arg)
  "Commit and close the log buffer.  Optional ARG is back-end specific."
  (interactive (list current-prefix-arg)))

;;;###autoload
(define-dvc-unified-command dvc-edit-ignore-files ()
  "Edit the ignored file list."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-ignore-files (file-list)
  "Ignore the marked files."
  (interactive (list (dvc-current-file-list))))

;;;###autoload
(defun dvc-ignore-file-extensions (file-list)
  "Ignore the file extensions of the marked files, in all
directories of the workspace."
  (interactive (list (dvc-current-file-list)))
  (let* ((extensions (delete nil (mapcar 'file-name-extension file-list)))
         ;; FIXME: should also filter duplicates. use delete-duplicates
         (root (dvc-tree-root))
         (msg (case (length extensions)
                (1 (format "extension *.%s" (first extensions)))
                (t (format "%d extensions" (length extensions))))))
    (if extensions
        (when (y-or-n-p (format "Ignore %s in workspace %s? " msg root))
          (dvc-call "dvc-backend-ignore-file-extensions" extensions))
      (error "No files with an extension selected"))))

;;;###autoload
(defun dvc-ignore-file-extensions-in-dir (file-list)
  "Ignore the file extensions of the marked files, only in the
directories containing the files, and recursively below them."
  (interactive (list (dvc-current-file-list)))
  ;; We have to match the extensions to the directories, so reject
  ;; command if either is nil.
  (let* ((extensions (mapcar 'file-name-extension file-list))
         (dirs (mapcar 'file-name-directory file-list))
         (msg (case (length extensions)
                (1 (format "extension *.%s in directory `%s'" (first extensions) (first dirs)))
                (t (format "%d extensions in directories" (length extensions))))))
    (dolist (extension extensions)
      (if (not extension)
          (error "A file with no extension selected")))
    (dolist (dir dirs)
      (if (not dir)
          (error "A file with no directory selected")))
    (when (y-or-n-p (format "Ignore %s? " msg))
      (dvc-call "dvc-backend-ignore-file-extensions-in-dir" file-list))))

;;;###autoload
(defun dvc-missing (&optional other path use-current)
  "Show revisions missing from PATH (default prompt),
relative to OTHER. OTHER defaults to the head revision of the
current branch; for some back-ends, it may also be a remote
repository.

If USE-CURRENT non-nil (default user prefix arg), PATH defaults to current tree."
  (interactive `(nil nil ,current-prefix-arg))
  (let ((default-directory
          (dvc-read-project-tree-maybe "DVC missing (directory): "
                                       (when path (expand-file-name path))
                                       use-current)))
    ;; Since we have bound default-directory, we don't need to pass
    ;; `path' to the back-end.
    (dvc-save-some-buffers default-directory)
    (dvc-call "dvc-missing" other))
  nil)

;;;###autoload
(define-dvc-unified-command dvc-inventory ()
  "Show the inventory for this working copy."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-save-diff (file)
  "Store the diff from the working copy against the repository in a file."
  (interactive (list (read-file-name "Save the diff to: "))))

;;;###autoload
(define-dvc-unified-command dvc-update (&optional revision-id)
  "Update this working copy to REVISION-ID (default head of current branch)."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-pull (&optional other)
  "Pull changes from a remote location.
If OTHER is nil, pull from a default or remembered location as
determined by the back-end.  If OTHER is a string, it identifies
a (local or remote) database or branch to pull into the current
database, branch or workspace."
  (interactive))

;;;###autoload
(defun dvc-push ()
  "Push changes to a remote location."
  (interactive)
  (let ((bookmarked-locations (dvc-bookmarks-current-push-locations)))
    (when bookmarked-locations
      (dolist (location bookmarked-locations)
        (message "pushing to: %s" location)
        (dvc-call "dvc-push" location)))))

;;;###autoload
(define-dvc-unified-command dvc-merge (&optional other)
  "Merge with OTHER.
If OTHER is nil, merge heads in current database, or merge from
remembered database.
If OTHER is a string, it identifies a (local or remote) database or
branch to merge into the current database, branch, or workspace."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-submit-patch ()
  "Submit a patch for the current project under DVC control."
  (interactive))

;;;###autoload
(define-dvc-unified-command dvc-send-commit-notification (&optional to)
  "Send a commit notification for the changeset at point.
If TO is provided, send it to that email address.  If a prefix
argument is given, modify the behavior of this command as
specified by the VCS backend."
  (interactive (list current-prefix-arg)))

;;;###autoload
(define-dvc-unified-command dvc-export-via-email ()
  "Send the changeset at point via email."
  (interactive))

;;;###autoload
(defun dvc-create-branch ()
  "Create a new branch."
  (interactive)
  (call-interactively (dvc-function (dvc-current-active-dvc) "dvc-create-branch")))

;;;###autoload
(defun dvc-select-branch ()
  "Select a branch."
  (interactive)
  (call-interactively (dvc-function (dvc-current-active-dvc) "dvc-select-branch")))

;;;###autoload
(defun dvc-list-branches ()
  "List available branches."
  (interactive)
  (call-interactively (dvc-function (dvc-current-active-dvc) "dvc-list-branches")))


(provide 'dvc-unified)

;;; dvc-unified.el ends here

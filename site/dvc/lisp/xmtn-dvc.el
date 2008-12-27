;;; xmtn-dvc.el --- DVC backend for monotone

;; Copyright (C) 2008 Stephen Leake
;; Copyright (C) 2006, 2007, 2008 Christian M. Ohler

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

;; This file implements a DVC backend for the distributed version
;; control system monotone.  The backend will only work with an
;; appropriate version of the mtn binary installed.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'dvc-unified)
  (require 'xmtn-basic-io)
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-automate)
  (require 'xmtn-conflicts)
  (require 'xmtn-ids)
  (require 'xmtn-match)
  (require 'xmtn-minimal)
  (require 'dvc-log)
  (require 'dvc-diff)
  (require 'dvc-core)
  (require 'ewoc))

;; For debugging.
(defun xmtn--load ()
  (require 'dvc-unified)
  (save-some-buffers)
  (mapc (lambda (file)
          (byte-compile-file file t))
        '("xmtn-minimal.el"
          "xmtn-compat.el"
          "xmtn-match.el"
          "xmtn-base.el"
          "xmtn-run.el"
          "xmtn-automate.el"
          "xmtn-basic-io.el"
          "xmtn-ids.el"
          "xmtn-dvc.el"
          "xmtn-revlist.el")))
;;; (xmtn--load)

;;;###autoload
(dvc-register-dvc 'xmtn "monotone")

(defmacro* xmtn--with-automate-command-output-basic-io-parser
    ((parser root-form command-form &key ((:may-kill-p may-kill-p-form)))
     &body body)
  (declare (indent 1) (debug (sexp body)))
  (let ((parser-tmp (gensym))
        (root (gensym))
        (command (gensym))
        (may-kill-p (gensym))
        (session (gensym))
        (handle (gensym)))
    `(let ((,root ,root-form)
           (,command ,command-form)
           (,may-kill-p ,may-kill-p-form))
       (xmtn-automate-with-session (,session ,root)
         (xmtn-automate-with-command (,handle
                                      ,session ,command
                                      :may-kill-p ,may-kill-p)
           (xmtn-automate-command-check-for-and-report-error ,handle)
           (xmtn-automate-command-wait-until-finished ,handle)
           (xmtn-basic-io-with-stanza-parser (,parser
                                              (xmtn-automate-command-buffer
                                               ,handle))
             ,@body))))))

;;;###autoload
(defun xmtn-dvc-log-edit-file-name-func (&optional root)
  (concat (file-name-as-directory (or root (dvc-tree-root)))
          "_MTN/log"))

(defun xmtn--tree-default-branch (root)
  (xmtn-automate-simple-command-output-line root `("get_option" "branch")))

(defun xmtn--tree-has-changes-p-future (root)
  (lexical-let ((future
                 (xmtn--command-output-lines-future
                  root
                  ;; Isn't there a better solution to this?
                  `("ls" "changed"))))
    (lambda ()
      (not (endp (funcall future))))))

(defun xmtn--toposort (root revision-hash-ids)
  (xmtn-automate-simple-command-output-lines root
                                             `("toposort"
                                               ,@revision-hash-ids)))

(defun xmtn--insert-log-edit-hints (root branch buffer prefix normalized-files)
  (with-current-buffer buffer
    (flet ((insert-line (&optional format-string-or-null &rest format-args)
             (if format-string-or-null
                 (let ((line (apply #'format
                                    format-string-or-null format-args)))
                   (assert (not (position ?\n line)))
                   (insert prefix line ?\n))
               (assert (endp format-args))
               (insert prefix ?\n))))
      (save-excursion
        ;; Launching these mtn processes in parallel is a noticeable
        ;; speedup (~14% on some informal benchmarks).  At least it
        ;; was with the version that I benchmarked, etc.
        (xmtn-automate-with-session (nil root)
          (let* ((unknown-future (xmtn--unknown-files-future root))
                 (missing-future (xmtn--missing-files-future root))
                 (consistent-p-future (xmtn--tree-consistent-p-future root))
                 (heads (xmtn--heads root branch))
                 (inconsistent-p (not (funcall consistent-p-future)))
                 (revision (if inconsistent-p
                               nil
                             (xmtn--get-revision root `(local-tree ,root))))
                 (missing (funcall missing-future)))
            (when inconsistent-p
              (insert-line
               "WARNING: Tree is not consistent.")
              (insert-line "Commit will fail unless you fix this first.")
              (insert-line))
            (when missing
              (insert-line "%s missing file(s):" (length missing))
              (dolist (file missing) (insert-line "%s" file))
              (insert-line)
              (insert-line))
            (insert-line "Committing on branch:")
            (insert-line branch)
            (insert-line)
            (unless
                (let* ((parents (xmtn--revision-old-revision-hash-ids revision))
                       (all-parents-are-heads-p
                        (subsetp parents heads :test #'equal))
                       (all-heads-are-parents-p
                        (subsetp heads parents :test #'equal)))
                  (cond ((and (not all-heads-are-parents-p)
                              (not all-parents-are-heads-p))
                         (insert-line "This commit will create divergence.")
                         (insert-line))
                        ((not all-heads-are-parents-p)
                         (insert-line (concat "Divergence will continue to exist"
                                              " after this commit."))
                         (insert-line))
                        (t
                         (progn)))))
            (case normalized-files
              (all
               (insert-line "All files selected for commit."))
              (t
               (insert-line "File(s) selected for commit:")
               ;; Normalized file names are easier to read when coming
               ;; from dired buffer, since otherwise, they would contain
               ;; the entire path.
               (dolist (file
                        ;; Sort in an attempt to match the order of
                        ;; "patch" lines, below.
                        (sort (copy-list normalized-files) #'string<))
                 (insert-line "%s" file))))
            ;; Due to the possibility of race conditions, this check
            ;; doesn't guarantee the operation will succeed.
            (if inconsistent-p
                ;; FIXME: Since automate get_revision can't deal with
                ;; inconsistent workspaces, we should be using
                ;; automate inventory instead.
                (progn (insert-line)
                       (insert-line
                        (concat "Unable to compute modified files while"
                                " the tree is inconsistent.")))
              (let ((committed-changes (list))
                    (other-changes (list)))
                (flet ((collect (path message)
                                (if (or (eql normalized-files 'all)
                                        (member path normalized-files))
                                    (push message committed-changes)
                                  (push message other-changes))))
                  (loop
                   for (path) in (xmtn--revision-delete revision)
                   do (collect path (format "delete    %s" path)))
                  (loop
                   for (from to) in (xmtn--revision-rename revision)
                   ;; FIXME: collect from or collect to?  Monotone
                   ;; doesn't specify how restrictions work for
                   ;; renamings.
                   do (collect to   (format "rename %s to %s" from to)))
                  (loop
                   for (path) in (xmtn--revision-add-dir revision)
                   do (collect path (format "add_dir   %s" path)))
                  (loop
                   for (path contents)
                   in (xmtn--revision-add-file revision)
                   do (collect path (format "add_file  %s" path)))
                  (loop
                   for (path from-contents to-contents)
                   in (xmtn--revision-patch-file revision)
                   do (collect path (format "patch     %s" path)))
                  (loop
                   for (path attr-name)
                   in (xmtn--revision-clear-attr revision)
                   do (collect path (format "clear %s %s"
                                            path attr-name)))
                  (loop
                   for (path attr-name attr-value)
                   in (xmtn--revision-set-attr revision)
                   do (collect path (format "set %s %s %s"
                                            path attr-name attr-value))))
                (setq committed-changes (nreverse committed-changes))
                (setq other-changes (nreverse other-changes))
                (loop
                 for (lines heading-if heading-if-not) in
                 `((,committed-changes
                    ,(format "%s change(s) in selected files:"
                             (length committed-changes))
                    "No changes in selected files.")
                   (,other-changes
                    ,(format
                      "%s change(s) in files not selected for commit:"
                      (length other-changes))
                    "No changes in files not selected for commit."))
                 do
                 (insert-line)
                 (insert-line "%s" (if lines heading-if heading-if-not))
                 (dolist (line lines) (insert-line "%s" line)))))
            (let ((unknown (funcall unknown-future)))
              (insert-line)
              (if (endp unknown)
                  (insert-line "No unknown files.")
                (insert-line "%s unknown file(s):" (length unknown))
                (dolist (file unknown) (insert-line "%s" file))))))))
    (cond ((eql (point) (point-min))
           ;; We take this as an indicator that there is no log message
           ;; yet.  So insert a blank line.
           (insert "\n")
           (goto-char (point-min)))
          (t
           ;; Moving up onto the last line of the log message seems to
           ;; be better than having the cursor sit at the ## prefix of
           ;; the first line of our hints.
           (forward-line -1))))
  nil)

(add-to-list 'format-alist
             '(xmtn--log-file
               "This format automatically removes xmtn's log edit hints from
the file before saving."
               nil
               xmtn--log-file-format-from-fn
               xmtn--log-file-format-to-fn
               t
               nil
               nil))

(defun xmtn--log-file-format-from-fn (begin end)
  (xmtn--assert-nil))

(defun xmtn--log-file-format-to-fn (begin end buffer)
  (dvc-log-flush-commit-file-list))

;;;###autoload
(defun xmtn-dvc-log-edit (root other-frame no-init)
  (if no-init
      (dvc-dvc-log-edit root other-frame no-init)
    (progn
      (dvc-dvc-log-edit root other-frame nil)
      (setq buffer-file-coding-system 'xmtn--monotone-normal-form) ;; FIXME: move this into dvc-get-buffer-create?
      (add-to-list 'buffer-file-format 'xmtn--log-file) ;; FIXME: generalize to dvc--log-file
      )))

;;;###autoload
(defun xmtn-dvc-log-edit-done ()
  (let* ((root default-directory)
         (files (or (with-current-buffer dvc-partner-buffer
                      (dvc-current-file-list 'nil-if-none-marked))
                    'all))
         (normalized-files
          (case files
            (all 'all)
            (t
             ;; Need to normalize in original buffer, since
             ;; switching buffers changes default-directory and
             ;; therefore the semantics of relative file names.
             (with-current-buffer dvc-partner-buffer
               (xmtn--normalize-file-names root files)))))
         (excluded-files
          (with-current-buffer dvc-partner-buffer
            (xmtn--normalize-file-names root (dvc-fileinfo-excluded-files))))
         (branch (xmtn--tree-default-branch root)))
    ;; Saving the buffer will automatically delete any log edit hints.
    (save-buffer)
    (dvc-save-some-buffers root)
    ;; We used to check for things that would make commit fail;
    ;; missing files, nothing to commit. But that just slows things
    ;; down in the typical case; better to just handle the error
    ;; message, which is way more informative anyway.
    (lexical-let* ((progress-message
                    (case normalized-files
                      (all (format "Committing all files in %s" root))
                      (t (case (length normalized-files)
                           (0 (assert nil))
                           (1 (format "Committing file %s in %s"
                                      (first normalized-files) root))
                           (t
                            (format "Committing %s files in %s"
                                    (length normalized-files)
                                    root))))))
                   (log-edit-buffer (current-buffer))
                   (log-edit-file (buffer-file-name))
                   (commit-message-file
                    (xmtn--make-temp-file
                     (concat (expand-file-name log-edit-file) "-xmtn")
                     nil ".tmp")))
      ;; Monotone's rule that _MTN/log must not exist when committing
      ;; non-interactively is really a pain to deal with.
      (rename-file log-edit-file commit-message-file t)
      (xmtn--run-command-async
       root
       `("commit" ,(concat "--message-file=" commit-message-file)
         ,(concat "--branch=" branch)
         ,@(case normalized-files
             (all
              (if excluded-files
                  (mapcar (lambda (file) (concat "--exclude=" file)) excluded-files)
                '()))
             (t (list*
                 ;; Since we are specifying files explicitly, don't
                 ;; recurse into specified directories. Also commit
                 ;; normally excluded files if they are selected.
                 "--depth=0"
                 "--" normalized-files))))
       :error (lambda (output error status arguments)
                (rename-file commit-message-file log-edit-file)
                (dvc-default-error-function output error
                                            status arguments))
       :killed (lambda (output error status arguments)
                 (rename-file commit-message-file log-edit-file)
                 (dvc-default-killed-function output error
                                              status arguments))
       :finished (lambda (output error status arguments)
                   (message "%s... done" progress-message)
                   ;; Monotone creates an empty log file when the
                   ;; commit was successful.  Let's not interfere with
                   ;; that.  (Calling `dvc-log-close' would.)
                   (delete-file commit-message-file)
                   (kill-buffer log-edit-buffer)
                   (dvc-diff-clear-buffers 'xmtn
                                           default-directory
                                           "* Just committed! Please refresh buffer"
                                           (xmtn--status-header
                                            default-directory
                                            (xmtn--get-base-revision-hash-id-or-null default-directory)))))
      ;; Show message _after_ spawning command to override DVC's
      ;; debugging message.
      (message "%s... " progress-message))
    (set-window-configuration dvc-pre-commit-window-configuration)))

;; The term "normalization" here has nothing to do with Unicode
;; normalization.
(defun xmtn--normalize-file-name (root file-name)
  (assert root)
  (let ((normalized-name (file-relative-name file-name root)))
    normalized-name))

(defun xmtn--normalize-file-names (root file-names)
  (check-type file-names list)
  (mapcar (lambda (file-name) (xmtn--normalize-file-name root file-name))
          file-names))

(defun xmtn--display-buffer-maybe (buffer dont-switch)
  (let ((orig-buffer (current-buffer)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (when dont-switch (pop-to-buffer orig-buffer)))
  nil)

(defun xmtn--status-header (root base-revision)
  (let* ((branch (xmtn--tree-default-branch root))
         (head-revisions (xmtn--heads root branch))
         (head-count (length head-revisions)))

    (concat
      (format "Status for %s:\n" root)
      (if base-revision
          (format "  base revision %s\n" base-revision)
        "  tree has no base revision\n")
      (format "  branch %s\n" branch)
      (case head-count
        (0 "  branch is empty\n")
        (1 "  branch is merged\n")
        (t (dvc-face-add (format "  branch has %s heads; need merge\n" head-count) 'dvc-conflict)))
      (if (member base-revision head-revisions)
          "  base revision is a head revision\n"
        (dvc-face-add "  base revision is not a head revision; need update\n" 'dvc-conflict)))))

(defun xmtn--refresh-status-header (status-buffer)
  (with-current-buffer status-buffer
    ;; different modes use different names for the ewoc
    ;; FIXME: should have a separate function for each mode
    (if dvc-fileinfo-ewoc
      (ewoc-set-hf
       dvc-fileinfo-ewoc
       (xmtn--status-header default-directory (xmtn--get-base-revision-hash-id-or-null default-directory))
       ""))))

(defun xmtn--parse-diff-for-dvc (changes-buffer)
  (let ((excluded-files (dvc-default-excluded-files))
        matched)
    (flet ((add-entry
            (path status dir &optional orig-path)
            (with-current-buffer changes-buffer
              (ewoc-enter-last
               dvc-fileinfo-ewoc
               (if dir
                   (make-dvc-fileinfo-dir
                    :mark nil
                    :exclude (dvc-match-excluded excluded-files path)
                    :dir (file-name-directory path)
                    :file (file-name-nondirectory path)
                    :status status
                    :more-status "")
                 (make-dvc-fileinfo-file
                  :mark nil
                  :exclude (dvc-match-excluded excluded-files path)
                  :dir (file-name-directory path)
                  :file (file-name-nondirectory path)
                  :status status
                  :more-status (if orig-path
                                   (if (eq status 'rename-target)
                                       (concat "from " orig-path)
                                     (concat "to " orig-path))
                                 ""))))))
           (likely-dir-p (path) (string-match "/\\'" path)))

      ;; First parse the basic_io contained in dvc-header, if any.
      (let ((revision
             (with-temp-buffer
               (insert dvc-header)
               (goto-char (point-min))
               (while (re-search-forward "^# ?" nil t)
                 (replace-match ""))
               (goto-char (point-min))
               (xmtn-basic-io-skip-blank-lines)
               (delete-region (point-min) (point))
               (xmtn-basic-io-with-stanza-parser
                   (parser (current-buffer))
                 (xmtn--parse-partial-revision parser)))))
        (loop
         for (path) in (xmtn--revision-delete revision)
         do (add-entry path 'deleted (likely-dir-p path)))
        (loop
         for (from to) in (xmtn--revision-rename revision)
         do (assert (eql (not (likely-dir-p from))
                         (not (likely-dir-p to))))
         do (add-entry to 'rename-target (likely-dir-p to) from)
         do (add-entry from 'rename-source (likely-dir-p from) to))
        (loop
         for (path) in (xmtn--revision-add-dir revision)
         do (add-entry path 'added t))
        (loop
         for (path contents)
         in (xmtn--revision-add-file revision)
         do (add-entry path 'added nil))
        (loop
         for (path from-contents to-contents)
         in (xmtn--revision-patch-file revision)
         do (add-entry path 'modified nil))
        ;; Do nothing about clear-attr and set-attr.
        ))

    (setq dvc-header
          (with-current-buffer changes-buffer
            (xmtn--status-header default-directory (xmtn--revision-hash-id dvc-diff-base))))
    nil))

;;;###autoload
(defun xmtn-show-base-revision ()
  "Show the base revision of the current monotone tree in the minibuffer."
  (interactive)
  (let* ((root (dvc-tree-root))
         (hash-id-or-null (xmtn--get-base-revision-hash-id-or-null root)))
    (if hash-id-or-null
        (message "Base revision of tree %s is %s" root hash-id-or-null)
      (message "Tree %s has no base revision" root))))


;;;###autoload
(defun xmtn-dvc-search-file-in-diff (file)
  (re-search-forward
   (let ((quoted-file (regexp-quote file)))
     (concat "^\\(\\("
             "\\+\\+\\+ " quoted-file
             "\\)\\|\\("
             ;; FIXME: What `dvc-diff-diff-or-list' does doesn't work
             ;; for this case, since `diff-hunk-next' doesn't recognize
             ;; mtn's output for this case as a diff hunk.
             "# " quoted-file " is binary"
             "\\)\\)$"))))


;;;###autoload
(defun xmtn-dvc-diff (&optional base-rev path dont-switch)
  (xmtn-dvc-delta base-rev (list 'xmtn (list 'local-tree (xmtn-tree-root path))) dont-switch))

(defvar xmtn-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "MH" 'xmtn-view-heads-revlist)
    (define-key map "MC" 'xmtn-conflicts-propagate)
    (define-key map "MR" 'xmtn-conflicts-review)
    (define-key map "MP" 'xmtn-propagate-from)
    (define-key map "Mx" 'xmtn-conflicts-clean)
    map))

;; items added here should probably also be added to xmtn-revlist-mode-menu, -map in xmtn-revlist.el
(easy-menu-define xmtn-diff-mode-menu xmtn-diff-mode-map
  "Mtn specific diff menu."
  `("DVC-Mtn"
    ["View Heads" xmtn-view-heads-revlist t]
    ["Show propagate conflicts" xmtn-conflicts-propagate t]
    ["Review conflicts" xmtn-conflicts-review t]
    ["Propagate branch" xmtn-propagate-from t]
    ["Clean conflicts resolutions" xmtn-conflicts-clean t]
    ))

(define-derived-mode xmtn-diff-mode dvc-diff-mode "xmtn-diff"
  "Add back-end-specific commands for dvc-diff.")

(dvc-add-uniquify-directory-mode 'xmtn-diff-mode)

;;;###autoload
(defun xmtn-dvc-delta (from-revision-id to-revision-id &optional dont-switch)
  ;; See dvc-unified.el dvc-delta for doc string. That says that
  ;; neither id can be local-tree. However, we also use this as the
  ;; implementation of xmtn-dvc-diff, so we need to handle local-tree.
  (let* ((root (dvc-tree-root))
         (from-resolved (xmtn--resolve-revision-id root from-revision-id))
         (to-resolved (xmtn--resolve-revision-id root to-revision-id)))
    (lexical-let ((buffer
                   (dvc-prepare-changes-buffer `(xmtn ,from-resolved) `(xmtn ,to-resolved) 'diff root 'xmtn))
                  (dont-switch dont-switch))
      (buffer-disable-undo buffer)
      (dvc-save-some-buffers root)
      (let ((rev-specs
             `(,(xmtn-match
                    from-resolved
                  ((local-tree $path)
                   ;; FROM-REVISION-ID is not a committed revision, but the
                   ;; workspace.  mtn diff can't directly handle
                   ;; this case.
                   (error "not implemented"))

                  ((revision $hash-id)
                   (concat "--revision=" hash-id)))

               ,@(xmtn-match
                     to-resolved
                   ((local-tree $path)
                    (assert (xmtn--same-tree-p root path))

                    ;; mtn diff will abort if there are missing
                    ;; files. But checking for that is a slow
                    ;; operation, so allow user to bypass it. We use
                    ;; dvc-confirm-update rather than a separate
                    ;; option, because dvc-confirm-update will be
                    ;; set t for the same reason this would.
                    (if dvc-confirm-update
                        (unless (funcall (xmtn--tree-consistent-p-future root))
                          (error "There are missing files in local tree; unable to diff. Try dvc-status.")))
                    `())

                   ((revision $hash-id)
                    `(,(concat "--revision=" hash-id)))))))

        ;; IMPROVEME: Could use automate content_diff and get_revision.
        (xmtn--run-command-async
         root `("diff" ,@rev-specs)
         :related-buffer buffer
         :finished
         (lambda (output error status arguments)
           (with-current-buffer output
             (xmtn--remove-content-hashes-from-diff))
           (dvc-show-changes-buffer output 'xmtn--parse-diff-for-dvc
                                    buffer dont-switch "^="))))

      (xmtn--display-buffer-maybe buffer dont-switch)

      ;; The call site in `dvc-revlist-diff' needs this return value.
      buffer)))

(defun xmtn--remove-content-hashes-from-diff ()
  ;; Hack: Remove mtn's file content hashes from diff headings since
  ;; `dvc-diff-diff-or-list' and `dvc-diff-find-file-name' gets
  ;; confused by them.
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward
         "^\\(\\+\\+\\+\\|---\\) \\(.*\\)\\(\t[0-9a-z]\\{40\\}\\)$"
         nil t)
      (replace-match "" t nil nil 3))))


(defun xmtn--simple-finished-notification (buffer)
  (lexical-let ((buffer buffer))
    (lambda (output error status arguments)
      (message "Process %s finished" buffer))))

;;;###autoload
(defun xmtn-dvc-command-version ()
  (fourth (xmtn--command-version xmtn-executable)))

(defvar xmtn-dvc-automate-version nil
  "Cached value of mtn automate interface version.")

(defun xmtn-dvc-automate-version ()
  "Return mtn automate version as a number."
  (if xmtn-dvc-automate-version
      xmtn-dvc-automate-version
    (setq xmtn-dvc-automate-version
          (string-to-number (xmtn--command-output-line nil '("automate" "interface_version"))))))

(defun xmtn--unknown-files-future (root)
  (xmtn--command-output-lines-future root '("ls" "unknown")))

(defun xmtn--missing-files-future (root)
  (xmtn--command-output-lines-future root '("ls" "missing")))

(defun xmtn--tree-consistent-p-future (root)
  ;; FIXME: Should also check for file/dir mismatches.
  (lexical-let ((missing-files-future (xmtn--missing-files-future root)))
    (lambda ()
      (null (funcall missing-files-future)))))

(defun xmtn--changes-image (change)
  (ecase change
    (content "content")
    (attrs   "attrs  ")))

(defun xmtn--status-process-entry (ewoc path status changes old-path new-path
                                        old-type new-type fs-type
                                        excluded-files)
  "Create a file entry in ewoc."
  ;; Don't display root directory (""); if requested, don't
  ;; display known or ignored files.
  (if (and (or (not (equal '(known) status))
               (member 'content changes)
               dvc-status-display-known)
           (or (not (equal '(ignored) status))
               dvc-status-display-ignored)
           (not (equal path "")))
      (let ((main-status
             (or
              (if (member 'added status) 'added)
              (if (member 'deleted status) 'deleted)
              (if (member 'ignored status) 'ignored)
              (if (member 'invalid status) 'invalid)
              (if (member 'missing status) 'missing)
              (if (member 'rename-source status) 'rename-source)
              (if (member 'rename-target status) 'rename-target)
              (if (member 'unknown status) 'unknown)
              ;; check for known last; almost everything is known
              (if (member 'known status)
                  (if (member 'content changes)
                      'modified
                    'known))))

            (indexed (not (eq status 'missing))) ;; in terse mode, missing is represented as "D?"
            (more-status "")
            basic-need-more-status)

        (setq basic-need-more-status
              (or (not (equal status (list main-status)))
                  (not (eq changes nil))))

        (case main-status
          (added
           ;; if the file has been modified since is was marked
           ;; 'added', that's still just 'added', so we never need to
           ;; do anything here.
           nil)

          ((deleted missing)
           (if basic-need-more-status
               (setq more-status
                     (concat
                      (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                      (mapconcat 'xmtn--changes-image changes " ")))))

          ((ignored invalid) nil)


          (rename-source
           (setq more-status
                 (concat "to " new-path)))

          (rename-target
           (setq more-status
                 (concat "from " old-path)))

          (modified
           (if (and (equal status '(known))
                    (equal changes '(content)))
               ;; just modified, nothing else
               nil
             (if basic-need-more-status
                 (setq more-status
                       (concat
                        (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                        (mapconcat 'xmtn--changes-image changes " "))))))

          (known
           (if basic-need-more-status
               (setq more-status
                     (concat
                      (mapconcat 'dvc-fileinfo-status-image-full (delq main-status status) " ")
                      (mapconcat 'xmtn--changes-image changes " ")))))
          )

        (case (if (equal fs-type 'none)
                  (if (equal old-type 'none)
                      new-type
                    old-type)
                fs-type)
          (directory
           (ewoc-enter-last ewoc
                            (make-dvc-fileinfo-dir
                             :mark nil
                             :exclude (dvc-match-excluded excluded-files path)
                             :dir (file-name-directory path)
                             :file (file-name-nondirectory path)
                             :status main-status
                             :indexed indexed
                             :more-status more-status)))
          ((file none)
           ;; 'none' indicates a dropped (deleted) file
           (ewoc-enter-last ewoc
                            (make-dvc-fileinfo-file
                             :mark nil
                             :exclude (dvc-match-excluded excluded-files path)
                             :dir (file-name-directory path)
                             :file (file-name-nondirectory path)
                             :status main-status
                             :indexed indexed
                             :more-status more-status)))
          (t
           (error "path %s fs-type %s old-type %s new-type %s" path fs-type old-type new-type))
          ))))

(defun xmtn--parse-inventory (stanza-parser fn)
  (loop for stanza = (funcall stanza-parser)
        while stanza do
        (xmtn-match stanza
          ((("path" (string $path))
            . $rest)
           (let* ((status (loop for entry in (cdr (assoc "status" rest))
                                collect
                                (xmtn-match entry
                                  ((string "added") 'added)
                                  ((string "dropped") 'deleted)
                                  ((string "invalid") 'invalid)
                                  ((string "known") 'known)
                                  ((string "missing") 'missing)
                                  ((string "ignored") 'ignored)
                                  ((string "unknown") 'unknown)
                                  ((string "rename_target") 'rename-target)
                                  ((string "rename_source") 'rename-source))))
                  (fs-type (xmtn-match (cdr (assoc "fs_type" rest))
                             (((string "file")) 'file)
                             (((string "directory")) 'directory)
                             (((string "none")) 'none)))
                  (old-type (xmtn-match (cdr (assoc "new_type" rest))
                              (((string "file")) 'file)
                              (((string "directory")) 'directory)
                              (nil 'none)))
                  (new-type (xmtn-match (cdr (assoc "new_type" rest))
                              (((string "file")) 'file)
                              (((string "directory")) 'directory)
                              (nil 'none)))
                  (changes (loop for entry in (cdr (assoc "changes" rest))
                                 collect
                                 (xmtn-match entry
                                   ((string "content") 'content)
                                   ((string "attrs") 'attrs))))
                  (old-path-or-null (xmtn-match (cdr (assoc "old_path" rest))
                                      (((string $old-path)) old-path)
                                      (nil nil)))
                  (new-path-or-null (xmtn-match (cdr (assoc "new_path" rest))
                                      (((string $new-path)) new-path)
                                      (nil nil)))
                  )
             (funcall fn
                      path
                      status
                      changes
                      old-path-or-null
                      new-path-or-null
                      old-type
                      new-type
                      fs-type))))))

(defun xmtn--status-using-inventory (root)
  ;; We don't run automate inventory through xmtn-automate here as
  ;; that would block.  xmtn-automate doesn't support asynchronous
  ;; command execution yet.
  (let*
      ((base-revision (xmtn--get-base-revision-hash-id-or-null root))
       (status-buffer
        (dvc-prepare-changes-buffer
         `(xmtn (revision ,base-revision))
         `(xmtn (local-tree ,root))
         'status
         root
         'xmtn)))
    (dvc-save-some-buffers root)
    (dvc-switch-to-buffer-maybe status-buffer)
    (dvc-kill-process-maybe status-buffer)
    ;; Attempt to make sure the sentinels have a chance to run.
    (accept-process-output)
    (let ((processes (dvc-processes-related-to-buffer status-buffer)))
      (when processes
        (error "Process still running in buffer %s" status-buffer)))

    (with-current-buffer status-buffer
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (setq dvc-buffer-refresh-function 'xmtn-dvc-status)
      (ewoc-set-hf dvc-fileinfo-ewoc
                   (xmtn--status-header root base-revision)
                   "")
      (ewoc-enter-last dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text "Running monotone..."))
      (ewoc-refresh dvc-fileinfo-ewoc))

    (lexical-let* ((status-buffer status-buffer))
      (xmtn--run-command-async
       root `("automate" "inventory"
              ,@(and (xmtn--have-no-ignore)
                     (not dvc-status-display-known)
                     '("--no-unchanged"))
              ,@(and (xmtn--have-no-ignore)
                     (not dvc-status-display-ignored)
                     '("--no-ignored")))
       :finished (lambda (output error status arguments)
                   ;; Don't use `dvc-show-changes-buffer' here because
                   ;; it attempts to do some regexp stuff for us that we
                   ;; don't need to be done.
                   (with-current-buffer status-buffer
                     (ewoc-enter-last dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text "Parsing inventory..."))
                     (ewoc-refresh dvc-fileinfo-ewoc)
                     (dvc-redisplay t)
                     (dvc-fileinfo-delete-messages)
                     (let ((excluded-files (dvc-default-excluded-files)))
                       (xmtn-basic-io-with-stanza-parser
                           (parser output)
                         (xmtn--parse-inventory
                          parser
                          (lambda (path status changes old-path new-path
                                        old-type new-type fs-type)
                            (xmtn--status-process-entry dvc-fileinfo-ewoc
                                                        path status
                                                        changes
                                                        old-path new-path
                                                        old-type new-type
                                                        fs-type
                                                        excluded-files))))
                       (when (not (ewoc-locate dvc-fileinfo-ewoc))
                         (ewoc-enter-last dvc-fileinfo-ewoc
                                          (make-dvc-fileinfo-message
                                           :text (concat " no changes in workspace")))
                         (ewoc-refresh dvc-fileinfo-ewoc)))))
       :error (lambda (output error status arguments)
                (dvc-diff-error-in-process
                 status-buffer
                 (format "Error running mtn with arguments %S" arguments)
                 output error))
       :killed (lambda (output error status arguments)
                 ;; Create an empty buffer as a fake output buffer to
                 ;; avoid printing all the output so far.
                 (with-temp-buffer
                   (dvc-diff-error-in-process
                    status-buffer
                    (format "Received signal running mtn with arguments %S"
                            arguments)
                    (current-buffer) error)))))))

;;;###autoload
(defun xmtn-dvc-status ()
  "Display status of monotone tree at `default-directory'."
  (xmtn--status-using-inventory default-directory))

;;;###autoload
(defun xmtn-dvc-revision-direct-ancestor (revision-id)
  (let* ((root (dvc-tree-root))
         (resolved-id (xmtn--resolve-revision-id root revision-id)))
    `(xmtn ,(xmtn--resolve-backend-id root
                                      `(previous-revision ,resolved-id 1)))))

;;;###autoload
(defun xmtn-dvc-name-construct (backend-revision)
  (check-type backend-revision xmtn--hash-id)
  backend-revision)

(defun xmtn--mtnignore-file-name (root)
  (concat (file-name-as-directory root) ".mtn-ignore"))

;;;###autoload
(defun xmtn-dvc-edit-ignore-files ()
  (find-file-other-window (xmtn--mtnignore-file-name (dvc-tree-root))))

(defun xmtn--quote-string-as-partial-perl-regexp (string)
  ;; The set of file names/patterns to be ignored by monotone is
  ;; customizable by the user through a hook.  So we can't guarantee
  ;; that writing something to .mtn-ignore really has the desired
  ;; effect.  However, we implement the correct behavior for the
  ;; default hook.
  ;;
  ;; The default hook uses the function regex.search, which is defined
  ;; in lua.cc, which, as of monotone revision
  ;; 341e4a18c594cec49896fa97bd4e74de7bee5827, uses Boost.Regex with
  ;; the default settings (Perl syntax).
  ;;
  ;; http://www.boost.org/libs/regex/doc/syntax_perl.html describes
  ;; this syntax.  This implementation is based on that description.
  (let ((special-chars ".[{()\*+?|^$"))
    (with-output-to-string
      (loop for char across string
            do
            (when (position char special-chars) (write-char ?\\))
            (write-char char)))))

(defun xmtn--perl-regexp-for-extension (extension)
  (format "\\.%s$" (xmtn--quote-string-as-partial-perl-regexp extension)))

(defun xmtn--perl-regexp-for-file-name (file-name)
  (format "^%s$" (xmtn--quote-string-as-partial-perl-regexp file-name)))

(defun xmtn--perl-regexp-for-files-in-directory (directory-file-name)
  (format "^%s" (xmtn--quote-string-as-partial-perl-regexp
                 (file-name-as-directory directory-file-name))))

(defun xmtn--perl-regexp-for-extension-in-dir (file-name)
  (format "^%s.*\\.%s$"
          (xmtn--quote-string-as-partial-perl-regexp
           (file-name-directory file-name))
          (xmtn--quote-string-as-partial-perl-regexp
           (file-name-extension file-name))))

(defun xmtn--add-patterns-to-mtnignore (root patterns interactive-p)
    (save-window-excursion
      ;; use 'find-file-other-window' to preserve current state if
      ;; user is already visiting the ignore file.
      (find-file-other-window (xmtn--mtnignore-file-name root))
      (save-excursion
        (let ((modified-p nil))
          (loop for pattern in patterns
                do
                (goto-char (point-min))
                (unless (re-search-forward (concat "^" (regexp-quote pattern)
                                                   "$")
                                           nil t)
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert pattern "\n")
                  (setq modified-p t)))
          (when modified-p
            ;; 'sort-lines' moves all markers, which defeats save-excursion. Oh well!
            (sort-lines nil (point-min) (point-max))
            (if (and interactive-p
                     dvc-confirm-ignore)
                (lexical-let ((buffer (current-buffer)))
                  (save-some-buffers nil (lambda ()
                                           (eql (current-buffer) buffer))))
              (save-buffer))))))
    nil)

;;;###autoload
(defun xmtn-dvc-ignore-files (file-names)
  (assert (not (endp file-names)))
  (let* ((root (dvc-tree-root))
         (normalized-file-names (xmtn--normalize-file-names root file-names))
         (msg (case (length file-names)
                (1 (format "%s" (first normalized-file-names)))
                (t (format "%s files/directories"
                           (length normalized-file-names))))))
    ;; FIXME: confirm should be in upper level DVC code.
    (when (or (not dvc-confirm-ignore)
              (y-or-n-p (format "Ignore %s in monotone tree %s? " msg root)))
      (xmtn--add-patterns-to-mtnignore
       root
       (let ((default-directory root))
         (mapcan (lambda (file-name)
                   (if (or (file-symlink-p file-name)
                           (xmtn--have-no-ignore)
                           (not (file-directory-p file-name)))
                       (list (xmtn--perl-regexp-for-file-name file-name))

                     ;; If mtn automate inventory doesn't support
                     ;; --no-ignore, it also recurses into unknown
                     ;; directories, so we need to ignore files in
                     ;; this directory as well as the directory
                     ;; itself.
                     (setq file-name (directory-file-name file-name))
                     (list
                      (xmtn--perl-regexp-for-file-name file-name)
                      (xmtn--perl-regexp-for-files-in-directory file-name))))
                 normalized-file-names))
       t))))

;;;###autoload
(defun xmtn-dvc-backend-ignore-file-extensions (extensions)
  (xmtn--add-patterns-to-mtnignore
   (dvc-tree-root)
   (mapcar #'xmtn--perl-regexp-for-extension extensions)
   t))

;;;###autoload
(defun xmtn-dvc-backend-ignore-file-extensions-in-dir (file-list)
  (xmtn--add-patterns-to-mtnignore
   (dvc-tree-root)
   (mapcar #'xmtn--perl-regexp-for-extension-in-dir file-list)
   t))

(defun xmtn--add-files (root file-names)
  (dolist (file-name file-names)
    ;; I don't know how mtn handles symlinks (and symlinks to
    ;; directories), so forbid them for now.
    (assert (not (file-symlink-p file-name))))
  (setq file-names (xmtn--normalize-file-names root file-names))
  (xmtn--run-command-sync root
                          `("add" "--" ,@file-names)))

(defun xmtn--file-registered-p (root file-name)
  ;; FIXME: need a better way to implement this
  (let ((normalized-file-name (xmtn--normalize-file-name root file-name)))
    (block parse
      (xmtn--with-automate-command-output-basic-io-parser
          (parser root `("inventory"))
        (xmtn--parse-inventory parser
                               (lambda (path status changes old-path new-path
                                             old-type new-type fs-type)
                                 (when (equal normalized-file-name path)
                                   (return-from parse
                                     t)))))
      nil)))

;;;###autoload
(defun xmtn-dvc-add-files (&rest files)
  (xmtn--add-files (dvc-tree-root) files))

;; Appears redundant, given that there is `xmtn-dvc-add-files'.  But
;; it's part of the DVC API.  FIXME.
;;;###autoload
(defun xmtn-dvc-add (file)
  (xmtn--add-files (dvc-tree-root) (list file)))

(defun xmtn--do-remove (root file-names do-not-execute)
  (xmtn--run-command-sync
   root `("drop"
          ,@(if do-not-execute `("--bookkeep-only") `())
          "--" ,@(xmtn--normalize-file-names root file-names)))
  ;; return t to indicate we succeeded
  t)

;;;###autoload
(defun xmtn-dvc-remove-files (&rest files)
  (xmtn--do-remove (dvc-tree-root) files nil))

;;;###autoload
(defun xmtn-dvc-rename (from-name to-name bookkeep-only)
  ;; See `dvc-rename' for doc string.
  (let ((root (dvc-tree-root)))
    (let ((to-normalized-name (xmtn--normalize-file-name root to-name))
          (from-normalized-name (xmtn--normalize-file-name root from-name)))
      (xmtn--run-command-sync
       root `("rename"
              ,@(if bookkeep-only `("--bookkeep-only") `())
              "--" ,from-normalized-name ,to-normalized-name))))
  ;; FIXME: We should do something analogous to
  ;; `dvc-revert-some-buffers' (but for renaming) here.  But DVC
  ;; doesn't provide a function for that.
  )

(defun xmtn--insert-hint-into-process-buffer (string)
  (let ((inhibit-read-only t)
        deactivate-mark)
    (save-excursion
      (let ((start (point)))
        (insert string)
        (let ((end (1- (point))))
          (add-text-properties start end '(face (:slant italic))))))))

(defun xmtn--run-command-that-might-invoke-merger (root command post-process)
  ;; Run async, not sync; it might recursively invoke emacsclient for
  ;; merging; and we might need to send an enter keystroke when
  ;; finished.
  (lexical-let ((post-process post-process))
    (xmtn--run-command-async
     root command
     :finished
     (lambda (output error status arguments)
       (with-current-buffer output
         (save-excursion
           (goto-char (point-max))
           (xmtn--insert-hint-into-process-buffer "[process finished]\n")))
       (if post-process
           (funcall post-process)))
     :error
     (lambda (output error status arguments)
       (with-current-buffer output
         (save-excursion
           (goto-char (point-max))
           (xmtn--insert-hint-into-process-buffer
            "[process terminated with an error]\n")
           (dvc-show-error-buffer error))))))
  ;; Show process buffer.  Monotone might spawn an external merger and
  ;; ask the user to hit enter when finished.
  (dvc-show-process-buffer)
  (goto-char (point-min))
  (xmtn--insert-hint-into-process-buffer
   (substitute-command-keys
    (concat
     "This buffer will show the output of the mtn subprocess, if any."
     "\nTo send an \"enter\" keystroke to mtn, use"
     " \\[xmtn-send-enter-to-subprocess]"
     "\nin this buffer.  This might be necessary"
     " if mtn launches an external merger."
     "\nWhen mtn has finished, just bury this buffer, or kill it."
     "\n")))
  (goto-char (point-max))
  ;; I don't think DVC's process filter can deal with read-only
  ;; buffers yet.
  ;;(setq buffer-read-only t)
  )

;;;###autoload
(defun xmtn-send-enter-to-subprocess ()
  "Send an \"enter\" keystroke to a monotone subprocess.

To be used in an xmtn process buffer.  Useful when monotone
spawns an external merger and asks you to hit enter when
finished."
  (interactive)
  (let ((process (loop for (process nil) in dvc-process-running
                       when (eql (current-buffer) (process-buffer process))
                       return process)))
    (unless process
      (error "No active process for buffer %s found" (current-buffer)))
    (process-send-string process "\n")
    (save-excursion
      (goto-char (point-max))
      (xmtn--insert-hint-into-process-buffer "[sent enter keystroke]\n"))))

;;; It's kind of a wart that these "xmtn--do-<operation>" functions
;;; don't have the same contract with respect to
;;; synchronousness/asynchronousness, progress messages and return
;;; value.

(defun xmtn--do-explicit-merge (root left-revision-hash-id right-revision-hash-id
                                     destination-branch-name)
  (check-type root string)
  (check-type left-revision-hash-id xmtn--hash-id)
  (check-type right-revision-hash-id xmtn--hash-id)
  (check-type destination-branch-name string)
  (xmtn--run-command-that-might-invoke-merger root
                                              `("explicit_merge"
                                                "--"
                                                ,left-revision-hash-id
                                                ,right-revision-hash-id
                                                ,destination-branch-name)
                                              nil)
  nil)

(defun xmtn--do-disapprove-future (root revision-hash-id)
  ;; Returns a future so the calling code can block on its completion
  ;; if it wants to.
  (check-type root string)
  (check-type revision-hash-id xmtn--hash-id)
  (xmtn--command-output-lines-future root `("disapprove" ,revision-hash-id)))

(defun xmtn--do-update (root target-revision-hash-id changes-p)
  (check-type root string)
  (check-type target-revision-hash-id xmtn--hash-id)
  (lexical-let ((progress-message (format "Updating tree %s to revision %s"
                                          root target-revision-hash-id)))
    (let ((command `("update" ,(concat "--revision=" target-revision-hash-id)))
          (post-process
           (lambda ()
             (message "%s... done" progress-message)
             (dvc-revert-some-buffers default-directory)
             (dvc-diff-clear-buffers 'xmtn
                                     default-directory
                                     "* Just updated; please refresh buffer"
                                     (xmtn--status-header
                                      default-directory
                                      (xmtn--get-base-revision-hash-id-or-null default-directory)))))
          )

      (message "%s..." progress-message)
      (if changes-p
          (xmtn--run-command-that-might-invoke-merger root command post-process)
        (xmtn--run-command-sync root command)
        (funcall post-process)))
    nil))

(defun xmtn--update-after-confirmation (root target-revision-hash-id)
  ;; mtn will just give an innocuous message if already updated, which
  ;; the user won't see. So check that here - it's fast.
  (when (equal (xmtn--get-base-revision-hash-id root) target-revision-hash-id)
    (error "Tree %s is already based on target revision %s"
           root target-revision-hash-id))
  (dvc-save-some-buffers root)
  (let (changes-p)
    (if dvc-confirm-update
        (progn
          ;; tree-has-changes-p and update will break if tree is
          ;; inconsistent, so check that first. But it's a slow check,
          ;; so don't bother if not confirming; error message from
          ;; update is clear.
          (unless (funcall (xmtn--tree-consistent-p-future root))
            (error "Tree is inconsistent, unable to update"))
          (unless (y-or-n-p
                   (format (concat "Update tree %s to revision %s? ")
                           root target-revision-hash-id))
            (error "Aborted update"))

          ;; has-changes-p is also a slow check. xmtn--do-update will
          ;; show a "possible merger" window if changes-p is true;
          ;; experienced users who set dvc-confirm-update don't need
          ;; that.
          (setq changes-p (funcall (xmtn--tree-has-changes-p-future root)))
          (when changes-p
            (unless (yes-or-no-p
                     (format (concat
                              "Tree %s contains uncommitted changes.  Update anyway? ")
                             root))
              (error "Aborted update")))))
    (xmtn--do-update root target-revision-hash-id changes-p))
  nil)

;;;###autoload
(defun xmtn-dvc-update (&optional revision-id)
  (let ((root (dvc-tree-root)))
    (xmtn-automate-with-session (nil root)
      (if revision-id
          (xmtn--update-after-confirmation root (xmtn--revision-hash-id revision-id))

        (let* ((branch (xmtn--tree-default-branch root))
               (heads (xmtn--heads root branch)))
          (case (length heads)
            (0 (assert nil))
            (1
             (xmtn--update-after-confirmation root (first heads)))

            (t
             ;; User can choose one head from a revlist, or merge them.
             (error (substitute-command-keys
                     (concat "Branch %s is unmerged (%s heads)."
                             "  Try \\[xmtn-view-heads-revlist] and \\[dvc-merge] or \\[dvc-revlist-update]"))
                    branch (length heads))))))))
  nil)

(defun xmtn-propagate-from (other)
  "Propagate from OTHER branch to local tree branch."
  (interactive "MPropagate from branch: ")
  (let*
      ((root (dvc-tree-root))
       (local-branch (xmtn--tree-default-branch root))
       (resolve-conflicts
        (if (file-exists-p (concat root "/_MTN/conflicts"))
            (progn
              (xmtn-conflicts-check-mtn-version)
              "--resolve-conflicts-file=_MTN/conflicts")))
       (cmd (list "propagate" other local-branch resolve-conflicts))
       (prompt
        (if resolve-conflicts
            (concat "Propagate from " other " to " local-branch " resolving conflicts? ")
          (concat "Propagate from " other " to " local-branch "? "))))

    (save-some-buffers t); conflicts file may be open.

    (if (not (yes-or-no-p prompt))
        (error "user abort"))

    (lexical-let
        ((display-buffer (current-buffer)))
      (message "%s..." (mapconcat (lambda (item) item) cmd " "))
      (xmtn--run-command-that-might-invoke-merger
       root cmd
       (lambda () (xmtn--refresh-status-header display-buffer))))))

;;;###autoload
(defun xmtn-dvc-merge (&optional other)
  (if other
      (xmtn-propagate-from other)
    ;; else merge heads
    (let ((root (dvc-tree-root)))
      (lexical-let
          ((display-buffer (current-buffer)))
        (xmtn-automate-with-session
            (nil root)
          (let* ((branch (xmtn--tree-default-branch root))
                 (heads (xmtn--heads root branch)))
            (case (length heads)
              (0 (assert nil))
              (1
               (message "already merged"))
              (t
               (xmtn--run-command-that-might-invoke-merger
                root
                '("merge")
                (lambda () (xmtn--refresh-status-header display-buffer))))))))))
  nil)

;;;###autoload
(defun xmtn-dvc-pull (&optional other)
  "Implement `dvc-pull' for xmtn."
  (lexical-let*
      ((root (dvc-tree-root))
       (name (concat "mtn pull " root)))
    (message "%s..." name)
    ;; mtn progress messages are put to stderr, and there is typically
    ;; nothing written to stdout from this command, so put both in the
    ;; same buffer.
    ;; FIXME: this output is not useful; need to use automation
    (xmtn--run-command-async root `("pull" ,other)
                             :output-buffer name
                             :error-buffer name
                             :finished
                             (lambda (output error status arguments)
                               (pop-to-buffer output)
                               (message "%s... done" name)))))

;;;###autoload
(defun xmtn-dvc-revert-files (&rest file-names)
  (when (stringp file-names) (setq file-names (list file-names)))
  (let ((root (dvc-tree-root)))
    (assert (not (endp file-names)))
    (dvc-save-some-buffers root)
    (let ((normalized-file-names (xmtn--normalize-file-names root file-names)))
      (lexical-let
          ((root root)
           (progress-message
            (if (eql (length file-names) 1)
                (format "Reverting file %s" (first file-names))
              (format "Reverting %s files" (length file-names)))))
        (message "%s..." progress-message)
        (xmtn--run-command-sync root `("revert" "--"
                                       ,@normalized-file-names)
                                :finished
                                (lambda (output error status arguments)
                                  (message "%s... done" progress-message)))
        (dvc-revert-some-buffers root))))
  nil)

;;;###autoload
(defun xmtn-revision-get-previous-revision (file revision-id)
  (xmtn--revision-get-file-helper file (list 'previous-revision (cadr revision-id))))

;;;###autoload
(defun xmtn-revision-get-last-revision (file stuff)
  (xmtn--revision-get-file-helper file `(last-revision ,@stuff)))

;;;###autoload
(defun xmtn-revision-get-file-revision (file stuff)
  (xmtn--revision-get-file-helper file `(revision ,@stuff)))

(defun xmtn--revision-get-file-helper (file backend-id)
  "Fill current buffer with the contents of FILE revision BACKEND-ID."
  (let ((root (dvc-tree-root)))
    (xmtn-automate-with-session (nil root)
      (let* ((normalized-file (xmtn--normalize-file-name root file))
             (corresponding-file
              (xmtn--get-corresponding-path root normalized-file
                                            `(local-tree ,root) backend-id)))
        (if (null corresponding-file)
            ;; File doesn't exist.  Since this function is (as far
            ;; as I know) only called from diff-like functions, a
            ;; missing file is not an error but just means the diff
            ;; should be computed against an empty file.  So just
            ;; leave the buffer empty.
            (progn)
          (let ((temp-dir nil))
            (unwind-protect
                (progn
                  (setq temp-dir (xmtn--make-temp-file
                                  "xmtn--revision-get-file-" t))
                  ;; Going through a temporary file and using
                  ;; `insert-file-contents' in conjunction with as
                  ;; much of the original file name as possible seems
                  ;; to be the best way to make sure that Emacs'
                  ;; entire file coding system detection logic is
                  ;; applied.  Functions like
                  ;; `find-operation-coding-system' and
                  ;; `find-file-name-handler' are not a complete
                  ;; replacement since they don't look at the contents
                  ;; at all.
                  (let ((temp-file (concat temp-dir "/" corresponding-file)))
                    (make-directory (file-name-directory temp-file) t)
                    (with-temp-file temp-file
                      (xmtn--set-buffer-multibyte nil)
                      (setq buffer-file-coding-system 'binary)
                      (xmtn--insert-file-contents-by-name root backend-id corresponding-file (current-buffer)))
                    (let ((output-buffer (current-buffer)))
                      (with-temp-buffer
                        (insert-file-contents temp-file)
                        (let ((input-buffer (current-buffer)))
                          (with-current-buffer output-buffer
                            (insert-buffer-substring input-buffer)))))))
              (when temp-dir
                (dvc-delete-recursively temp-dir)))))))))

(defun xmtn--get-file-by-id (root file-id save-as)
  "Store contents of FILE-ID in file SAVE-AS."
  (xmtn-automate-with-session
   (nil root)
   (with-temp-file save-as
     (xmtn--set-buffer-multibyte nil)
     (setq buffer-file-coding-system 'binary)
     (xmtn--insert-file-contents root file-id (current-buffer)))))

(defun xmtn--revision-parents (root revision-hash-id)
  (xmtn-automate-simple-command-output-lines root
                                             `("parents" ,revision-hash-id)))

(defun xmtn--get-content-changed (root backend-id normalized-file)
  (xmtn-automate-with-session (nil root)
    (xmtn-match (xmtn--resolve-backend-id root backend-id)
      ((local-tree $path) (error "Not implemented"))
      ((revision $revision-hash-id)
       (xmtn--with-automate-command-output-basic-io-parser
           (parser root `("get_content_changed" ,revision-hash-id
                          ,normalized-file))
         (loop for stanza = (funcall parser)
               while stanza
               collect (xmtn-match stanza
                         ((("content_mark" (id $previous-id)))
                          previous-id))))))))

(defun xmtn--limit-length (list n)
  (or (null n) (<= (length list) n)))

(defun xmtn--close-set (fn initial-set last-n)
  (let ((new-elements initial-set)
        (current-set nil))
    (while (and new-elements (xmtn--limit-length current-set last-n))
      (let ((temp-elements nil)
            (next-elements nil)
            (new-element nil))
        (while new-elements
          (setq new-element (car new-elements))
          (setq temp-elements (funcall fn new-element))
          (setq current-set (append (set-difference temp-elements current-set :test #'equal) current-set))
          (setq next-elements (append temp-elements next-elements))
          (setq new-elements (cdr new-elements)))
        (setq new-elements next-elements)))
    current-set))

(defun xmtn--get-content-changed-closure (root backend-id normalized-file last-n)
  (xmtn-automate-with-session (nil root)
    (lexical-let ((root root))
      (labels ((changed-self-or-ancestors (entry)
                (destructuring-bind (hash-id file-name) entry
                  (check-type file-name string)
                  ;; get-content-changed can return one or two revisions
                  (loop for next-change-id in (xmtn--get-content-changed
                                               root `(revision ,hash-id)
                                               file-name)
                        for corresponding-path =
                        (xmtn--get-corresponding-path-raw root file-name
                                                          hash-id next-change-id)
                        when corresponding-path
                        collect `(,next-change-id ,corresponding-path))))
               (changed-proper-ancestors (entry)
                                         (destructuring-bind (hash-id file-name) entry
                                           (check-type file-name string)
                                           ;; revision-parents can return one or two revisions
                                           (loop for parent-id in (xmtn--revision-parents root hash-id)
                                                 for path-in-parent =
                                                 (xmtn--get-corresponding-path-raw root file-name
                                                                                   hash-id parent-id)
                                                 when path-in-parent
                                                 append (changed-self-or-ancestors
                                                         `(,parent-id ,path-in-parent))))))
        (xmtn--close-set
         #'changed-proper-ancestors
         (xmtn-match (xmtn--resolve-backend-id root backend-id)
           ((local-tree $path) (error "Not implemented"))
           ((revision $id) (changed-self-or-ancestors
                            `(,id ,normalized-file))))
         last-n)))))


(defun xmtn--get-corresponding-path-raw (root normalized-file-name
                                              source-revision-hash-id
                                              target-revision-hash-id)
  (check-type normalized-file-name string)
  (xmtn--with-automate-command-output-basic-io-parser
      (next-stanza root `("get_corresponding_path"
                          ,source-revision-hash-id
                          ,normalized-file-name
                          ,target-revision-hash-id))
    (xmtn-match (funcall next-stanza)
      (nil nil)
      ((("file" (string $result)))
       (assert (null (funcall next-stanza)))
       result))))


(defun xmtn--get-corresponding-path (root normalized-file-name
                                          source-revision-backend-id
                                          target-revision-backend-id)
  (block get-corresponding-path
    (xmtn-automate-with-session (nil root)
      (let (source-revision-hash-id
            target-revision-hash-id
            (file-name-postprocessor #'identity))
        (let ((resolved-source-revision
               (xmtn--resolve-backend-id root source-revision-backend-id))
              (resolved-target-revision
               (xmtn--resolve-backend-id root target-revision-backend-id)))
          (xmtn-match resolved-source-revision
            ((revision $hash-id)
             (setq source-revision-hash-id hash-id))
            ((local-tree $path)
             (assert (xmtn--same-tree-p root path))
             (let ((base-revision-hash-id
                    (xmtn--get-base-revision-hash-id-or-null path)))
               (if (null base-revision-hash-id)
                   (xmtn-match resolved-target-revision
                     ((revision $hash-id)
                      (return-from get-corresponding-path nil))
                     ((local-tree $target-path)
                      (assert (xmtn--same-tree-p path target-path))
                      (return-from get-corresponding-path normalized-file-name)))
                 (setq normalized-file-name (xmtn--get-rename-in-workspace-to
                                             path normalized-file-name))
                 (setq source-revision-hash-id base-revision-hash-id)))))
          (xmtn-match resolved-target-revision
            ((revision $hash-id)
             (setq target-revision-hash-id hash-id))
            ((local-tree $path)
             (assert (xmtn--same-tree-p root path))
             (let ((base-revision-hash-id
                    (xmtn--get-base-revision-hash-id-or-null path)))
               (if (null base-revision-hash-id)
                   (return-from get-corresponding-path nil)
                 (setq target-revision-hash-id base-revision-hash-id
                       file-name-postprocessor
                       (lexical-let ((path path))
                         (lambda (file-name)
                           (xmtn--get-rename-in-workspace-from path
                                                               file-name)))))))))
        (let ((result
               (xmtn--get-corresponding-path-raw root normalized-file-name
                                                 source-revision-hash-id
                                                 target-revision-hash-id)))
          (if (null result)
              nil
            (funcall file-name-postprocessor result)))))))

(defun xmtn--get-rename-in-workspace-from (root normalized-source-file-name)
  ;; FIXME: need a better way to implement this
  (check-type normalized-source-file-name string)
  (block parse
    (xmtn--with-automate-command-output-basic-io-parser
        (parser root `("inventory"))
      (xmtn--parse-inventory parser
                             (lambda (path status changes old-path new-path
                                           old-type new-type fs-type)
                               (when (equal normalized-source-file-name
                                            old-path)
                                 (return-from parse
                                   path)))))
    normalized-source-file-name))

(defun xmtn--get-rename-in-workspace-to (root normalized-target-file-name)
  ;; FIXME: need a better way to implement this
  (check-type normalized-target-file-name string)
  (block parse
    (xmtn--with-automate-command-output-basic-io-parser
        (parser root `("inventory" ,normalized-target-file-name))
      (xmtn--parse-inventory parser
                             (lambda (path status changes old-path new-path
                                           old-type new-type fs-type)
                               (when (and old-path
                                          (equal normalized-target-file-name
                                                 path))
                                 (return-from parse
                                   old-path)))))
    normalized-target-file-name))

(defun xmtn--manifest-find-file (root manifest normalized-file-name)
  (let ((matches (remove* normalized-file-name
                          (remove* 'file manifest :key #'first :test-not #'equal)
                          :key #'second :test-not #'equal)))
    (xmtn--assert-optional (member (length matches) '(0 1)))
    (first matches)))

(defun xmtn--revision-manifest-file-entry (root backend-id
                                                normalized-file-name)
  (let ((manifest (xmtn--get-manifest root backend-id)))
    (xmtn--manifest-find-file root manifest normalized-file-name)))

(defun xmtn--revision-file-contents-hash (root backend-id normalized-file-name)
  (xmtn-match (xmtn--revision-manifest-file-entry root backend-id
                                                  normalized-file-name)
    ((file $relative-path $file-contents-hash $attrs)
     (assert (equal relative-path normalized-file-name))
     file-contents-hash)))

(defun xmtn--file-contents-as-string (root content-hash-id)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-simple-command-output-string
   root `("get_file" ,content-hash-id)))

(defun xmtn--insert-file-contents (root content-hash-id buffer)
  (check-type content-hash-id xmtn--hash-id)
  (xmtn-automate-simple-command-output-insert-into-buffer
   root buffer `("get_file" ,content-hash-id)))

(defun xmtn--insert-file-contents-by-name (root backend-id normalized-file-name buffer)
  (let* ((resolved-id (xmtn--resolve-backend-id root backend-id))
         (hash-id (case (car resolved-id)
                    (local-tree nil)
                    (revision (cadr resolved-id))))
         (cmd (if hash-id
                  (cons (list "revision" hash-id) (list "get_file_of" normalized-file-name))
                (list "get_file_of" normalized-file-name))))
    (xmtn-automate-simple-command-output-insert-into-buffer root buffer cmd)))

(defun xmtn--same-tree-p (a b)
  (equal (file-truename a) (file-truename b)))

(defun xmtn--get-manifest (root backend-id)
  (xmtn-automate-with-session (nil root)
    (let ((resolved-id (xmtn--resolve-backend-id root backend-id)))
      (xmtn--with-automate-command-output-basic-io-parser
          (parser root `("get_manifest_of"
                         ,@(xmtn-match resolved-id
                             ((local-tree $path)
                              ;; FIXME: I don't really know what to do if
                              ;; PATH is not the same as ROOT.  Maybe
                              ;; revision id resolution needs to return
                              ;; the proper root, too.
                              (assert (xmtn--same-tree-p root path))
                              (unless (funcall
                                       (xmtn--tree-consistent-p-future root))
                                (error "Tree is inconsistent, unable to get manifest"))
                              '())
                             ((revision $hash-id)
                              `(,hash-id)))))
        (assert (equal (funcall parser) '(("format_version" (string "1")))))
        (loop for stanza = (funcall parser)
              while stanza
              collect (xmtn-match stanza
                        ((("dir" (string $normalized-path)))
                         (let ((dir (decode-coding-string
                                     normalized-path
                                     'xmtn--monotone-normal-form)))
                           (xmtn--assert-optional
                            (or (equal dir "")
                                (not (eql (aref dir (1- (length dir))) ?/))))
                           `(dir ,dir)))
                        ((("file" (string $normalized-path))
                          ("content" (id $hash-id))
                          . $attrs)
                         `(file
                           ,(decode-coding-string
                             normalized-path 'xmtn--monotone-normal-form)
                           ,hash-id
                           ,(mapcar (lambda (attr-entry)
                                      (xmtn-match attr-entry
                                        (("attr"
                                          (string $attr-name)
                                          (string $attr-value))
                                         (list attr-name attr-value))))
                                    attrs)))))))))

(defstruct (xmtn--revision (:constructor xmtn--make-revision))
  ;; matches data output by 'mtn diff'
  new-manifest-hash-id
  old-revision-hash-ids
  delete
  rename
  add-dir
  add-file
  patch-file
  clear-attr
  set-attr
  )


(defun xmtn--get-revision (root backend-id)
  (xmtn-automate-with-session (nil root)
    (let ((resolved-id (xmtn--resolve-backend-id root backend-id)))
      (xmtn--with-automate-command-output-basic-io-parser
          (parser root `("get_revision"
                         ,@(xmtn-match resolved-id
                             ((local-tree $path)
                              ;; FIXME: I don't really know what to do if
                              ;; PATH is not the same as ROOT.  Maybe
                              ;; revision id resolution needs to return
                              ;; the proper root, too.
                              (assert (xmtn--same-tree-p root path))
                              (unless (funcall
                                       (xmtn--tree-consistent-p-future root))
                                (error (concat "Tree is inconsistent,"
                                               " unable to compute revision")))
                              '())
                             ((revision $hash-id)
                              `(,hash-id)))))
        (assert (equal (funcall parser) '(("format_version" (string "1")))))
        (let ((new-manifest-hash-id (xmtn-match (funcall parser)
                                      ((("new_manifest" (id $hash-id)))
                                       hash-id))))
          (let ((proto-revision (xmtn--parse-partial-revision parser)))
            (setf (xmtn--revision-new-manifest-hash-id proto-revision)
                  new-manifest-hash-id)
            proto-revision))))))

(defun xmtn--parse-partial-revision (parser)
  "Parse basic_io output from get_revision, starting with the old_revision stanzas."
  (let ((old-revision-hash-ids (list))
        (delete (list))
        (rename (list))
        (add-dir (list))
        (add-file (list))
        (patch-file (list))
        (clear-attr (list))
        (set-attr (list)))
    (flet ((decode-path (path)
             (decode-coding-string path 'xmtn--monotone-normal-form)))
      (loop for stanza = (funcall parser)
            while stanza
            do
            (xmtn-match stanza
              ;; Most common case, "patch", first.
              ((("patch" (string $filename))
                ("from" (id $from-id))
                ("to" (id $to-id)))
               (push `(,(decode-path filename) ,from-id ,to-id)
                     patch-file))
              ((("old_revision" (null-id)))
               ;; Why doesn't mtn just skip this stanza?
               )
              ((("old_revision" (id $hash-id)))
               (push hash-id old-revision-hash-ids))
              ((("delete" (string $path)))
               (push `(,(decode-path path)) delete))
              ((("rename" (string $from-path))
                ("to" (string $to-path)))
               (push `(,(decode-path from-path) ,(decode-path to-path))
                     rename))
              ((("add_dir" (string $path)))
               (push `(,(decode-path path)) add-dir))
              ((("add_file" (string $path))
                ("content" (id $file-id)))
               (push `(,(decode-path path) ,file-id)
                     add-file))
              ;; "patch": See above.
              ((("clear" (string $path))
                ("attr" (string $attr-name)))
               (push `(,(decode-path path) ,attr-name)
                     clear-attr))
              ((("set" (string $path))
                ("attr" (string $attr-name))
                ("value" (string $attr-value)))
               (push `(,(decode-path path) ,attr-name ,attr-value)
                     set-attr)))))
    (setq old-revision-hash-ids (nreverse old-revision-hash-ids)
          delete (nreverse delete)
          rename (nreverse rename)
          add-dir (nreverse add-dir)
          add-file (nreverse add-file)
          patch-file (nreverse patch-file)
          clear-attr (nreverse clear-attr)
          set-attr (nreverse set-attr))
    (xmtn--make-revision
     :old-revision-hash-ids old-revision-hash-ids
     :delete delete
     :rename rename
     :add-dir add-dir
     :add-file add-file
     :patch-file patch-file
     :clear-attr clear-attr
     :set-attr set-attr
     )))


;;;###autoload
(defun xmtn-dvc-revision-nth-ancestor (&rest args)
  ;; There is a reasonable default implementation to fall back on.  It
  ;; will just call `xmtn-dvc-revision-direct-ancestor' N times.  We
  ;; can't do any better than linear-time anyway, since we have to
  ;; chase the ancestry links (and check the uniqueness at each step).
  (apply #'dvc-dvc-revision-nth-ancestor args))

(defalias 'xmtn-dvc-revlist 'xmtn-view-heads-revlist)

(provide 'xmtn-dvc)

;;; xmtn-dvc.el ends here

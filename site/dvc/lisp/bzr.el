;;; bzr.el --- Support for Bazaar 2 in DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

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
(require 'dvc-diff)
(require 'dvc-core)
(require 'dvc-defs)
(require 'dvc-revlist)
(require 'dvc-annotate)
(eval-and-compile (require 'dvc-lisp))

(eval-when-compile (require 'cl))

(defvar bzr-default-init-repository-directory "~/"
  "The default directory that is suggested when calling `bzr-init-repository'.
This setting is useful, if you'd like to create a bunch of repositories in
a common base directory.")

(defvar bzr-command-version nil
  "Version of bzr that we are using.")

;;example:
;;(setq bzr-mail-notification-destination
;;      '(("dvc-dev-bzr" ("[commit][dvc] " "dvc-dev@gna.org" "http://xsteve.nit.at/dvc/"))))
(defcustom bzr-mail-notification-destination nil
  "*Preset some useful values for commit emails.

An alist of rules to map branch names to target
email addresses and the prefix string for the subject line.

This is used by the `bzr-send-commit-notification' function."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Bzr branch nick")
                       (list :tag "Target"
                             (string :tag "Email subject prefix")
                             (string :tag "Email address")
                             (string :tag "Bzr branch location"))))
  :group 'dvc)


(defvar bzr-pull-done-hook '()
  "*Hooks run after a bzr pull has finished.
Each hook function is called with these parameters:
repo-path: The pull source.
working-copy-dir: The working directory.
pulled-something: If something was pulled.")

(defun bzr-init (&optional dir)
  "Run bzr init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for bzr init: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (dvc-run-dvc-sync 'bzr (list "init" dir)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "bzr init %s finished" dir))))

(defun bzr-init-repository (&optional dir)
  "Run bzr init-repository.
When called interactively, `bzr-default-init-repository-directory' is used as
starting point to enter the new repository directory. That directory is created
via bzr init-repository."
  (interactive
   (list (expand-file-name (dvc-read-directory-name
                            "Directory for bzr init-repository: "
                            (or
                             bzr-default-init-repository-directory
                             default-directory
                             (getenv "HOME"))))))
  (dvc-run-dvc-sync 'bzr (list "init-repository" dir)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "bzr init-repository '%s' finished" dir)))
  dir)

;;;###autoload
(defun bzr-checkout (branch-location to-location &optional lightweight revision)
  "Run bzr checkout."
  (interactive
   (let* ((branch-loc (read-string "bzr checkout branch location: " nil nil bzr-default-init-repository-directory))
          (co-dir (or default-directory (getenv "HOME")))
          (to-loc (expand-file-name
                   (dvc-read-directory-name
                    "bzr checkout to: "
                    co-dir
                    (concat co-dir (file-name-nondirectory
                                    (replace-regexp-in-string
                                     "/trunk/?$" "" branch-loc))))))
          (lw (y-or-n-p "Do a lightweight checkout? "))
          (rev nil))
     (list branch-loc to-loc lw rev)))
  (if current-prefix-arg
      (setq revision (read-string "FromRevision: "))
    (setq revision nil))
  (dvc-run-dvc-sync 'bzr (list "checkout"
                               (when lightweight "--lightweight")
                               branch-location
                               to-location
                               (when revision "-r")
                               revision)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "bzr checkout%s %s at rev %s -> %s finished"
                                         (if lightweight " --lightweight" "")
                                         branch-location revision to-location)
                                (dired to-location))))


;;;###autoload
(defun bzr-pull (&optional repo-path)
  "Run bzr pull."
  (interactive "sPull from bzr repository: ")
  (when (string= repo-path "")
    (setq repo-path nil))
  (dvc-run-dvc-async 'bzr (list "pull" repo-path)
                     :finished
                     (dvc-capturing-lambda
                         (output error status arguments)
                       (dvc-revert-some-buffers)
                       (message "bzr pull finished => %s"
                                (concat (dvc-buffer-content error) (dvc-buffer-content output)))
                       (let ((pulled-something))
                         (with-current-buffer output
                           (goto-char (point-min))
                           (setq pulled-something (not (search-forward "No revisions to pull" nil t)))
                           (run-hook-with-args 'bzr-pull-done-hook (capture repo-path) (capture default-directory) pulled-something))))))

;;;###autoload
(defun bzr-push (&optional repo-path)
  "Run bzr push.
When called with a prefix argument, add the --remember option"
  (interactive (list (read-string (format "Push %sto bzr repository: " (if current-prefix-arg "--remember " "")))))
  (when (string= repo-path "")
    (setq repo-path nil))
  (dvc-run-dvc-async 'bzr (list "push" repo-path (when current-prefix-arg "--remember"))
                     :finished
                     (dvc-capturing-lambda
                         (output error status arguments)
                       (message "bzr push finished => %s"
                                (concat (dvc-buffer-content error) (dvc-buffer-content output))))))

;;;###autoload
(defun bzr-merge (&optional repo-path)
  "Run bzr merge."
  (interactive "sMerge from bzr repository: ")
  (when (string= repo-path "")
    (setq repo-path nil))
  (dvc-run-dvc-async 'bzr (list "merge" repo-path)
                     :finished
                     (dvc-capturing-lambda
                         (output error status arguments)
                       (message "bzr merge finished => %s"
                                (concat (dvc-buffer-content error) (dvc-buffer-content output))))))

(defun bzr-merge-bundle (bundle-file)
  "Run bzr merge from BUNDLE-FILE."
  (interactive "sMerge bzr bundle: ")
  (message "bzr-merge-bundle: %s (%s)" bundle-file default-directory)
  (dvc-run-dvc-sync 'bzr (list "merge" bundle-file)
                    :finished
                    (dvc-capturing-lambda
                        (output error status arguments)
                      (message "bzr merge finished => %s"
                               (concat (dvc-buffer-content error) (dvc-buffer-content output))))))

(defvar bzr-merge-or-pull-from-url-rules nil
  "An alist that maps repository urls to working copies. This rule is used by
`bzr-merge-from-url'.

An example setting is:
 (setq bzr-merge-from-url-rules '((\"http://bzr.xsteve.at/dvc/\" . (pull \"~/site-lisp/dvc/\"))
                                  (\"http://www-verimag.imag.fr/~moy/bzr/dvc/moy/\" . (merge \"/home/stefan/work/myprg/dvc-dev-bzr/\"))))
")
(defun bzr-merge-or-pull-from-url (url)
  "Merge or pull from a given url, autodetect the working directory via
`bzr-merge-or-pull-from-url-rules'."
  (interactive "sMerge from url: ")
  ;; (message "bzr-merge-or-pull-from-url %s" url)
  (let* ((dest (cdr (assoc url bzr-merge-or-pull-from-url-rules)))
         (merge-or-pull (car dest))
         (path (cadr dest))
         (doit t))
    (when (and merge-or-pull path)
      (setq doit (y-or-n-p (format "%s from %s to %s? " (if (eq merge-or-pull 'merge) "Merge" "Pull") url path))))
    (when doit
      (unless merge-or-pull
        (setq merge-or-pull (cdr (assoc
                                  (dvc-completing-read
                                   (format "Merge or pull from %s: " url)
                                   '("Merge" "Pull"))
                                  '(("Merge" . merge) ("Pull" . pull))))))
      (unless path
        (setq path (dvc-read-directory-name (format "%s from %s to: " (if (eq merge-or-pull 'merge) "Merge" "Pull") url))))
      (let ((default-directory path))
        (if (eq merge-or-pull 'merge)
            (progn
              (message "merging from %s to %s" url path)
              (bzr-merge url))
          (message "pulling from  %s to %s" url path)
          (bzr-pull url))))))

;;;###autoload
(defun bzr-update (&optional path)
  "Run bzr update."
  (interactive)
  (unless path
    (setq path default-directory))
  (dvc-run-dvc-async 'bzr (list "update" path)
                     :finished
                     (dvc-capturing-lambda
                         (output error status arguments)
                       (message "bzr update finished => %s"
                                (concat (dvc-buffer-content error) (dvc-buffer-content output))))))


;; bzr-start-project implements the following idea:
;;  bzr init-repo repo
;;  bzr init repo/trunk
;;  bzr checkout --lightweight repo/trunk trunk-checkout
;;  cd trunk-checkout
;;  (add files here)
(defun bzr-start-project ()
  "Initializes a repository with a trunk branch and finally checks out a working copy.
The following functions are called:
`bzr-init-repository': create a shared repository
`bzr-init':            create the trunk branch in the repository above
`bzr-checkout':        check out the trunk branch to the entered working directory"
  (interactive)
  (let ((init-repo-dir)
        (branch-repo-dir)
        (checkout-dir))
    (setq init-repo-dir (call-interactively 'bzr-init-repository))
    (setq branch-repo-dir (dvc-uniquify-file-name (concat init-repo-dir "/trunk")))
    (bzr-init branch-repo-dir)
    (setq checkout-dir (dvc-uniquify-file-name
                        (dvc-read-directory-name "checkout the branch to: " nil
                                                 (concat default-directory
                                                         (file-name-nondirectory init-repo-dir)))))
    (bzr-checkout branch-repo-dir checkout-dir t)))

(defun bzr-parse-diff (changes-buffer)
  (dvc-trace "bzr-parse-diff")
  (dvc-trace-current-line)
  (save-excursion
    (while (re-search-forward
            "^=== \\([a-z]*\\) file '\\([^']*\\)'\\( => '\\([^']*\\)'\\)?$" nil t)
      (let* ((origname (match-string-no-properties 2))
             (newname  (or (match-string-no-properties 4) origname))
             (renamed  (string= (match-string-no-properties 1) "renamed"))
             (removed  (string= (match-string-no-properties 1) "removed"))
             (added    (string= (match-string-no-properties 1) "added")))
        (with-current-buffer changes-buffer
          (ewoc-enter-last
           dvc-fileinfo-ewoc (make-dvc-fileinfo-file
                              :mark nil
                              :dir ""
                              :file newname
                              :status (cond
                                       (added   'added)
                                       (renamed 'rename-source)
                                       (removed 'missing)
                                       (t       'modified))
                              :more-status (when (and renamed (not added))
                                             origname))))))))

(defun bzr-revisionspec-to-rev (string-revspec path)
  "Converts a bzr revision specifier (string) into a DVC revision.

TODO: just revision number and last:N are implemented.
"
  `(bzr ,(cond ((string-match "^\\(revno:\\)?\\([0-9]+\\)$"
                              string-revspec)
                `(revision (local ,path
                                  ,(string-to-number
                                    (match-string 2 string-revspec)))))
               ((string-match "^\\(last:\\|-\\)\\([0-9]+\\)$"
                              string-revspec)
                `(last-revision ,path
                                ,(string-to-number
                                  (match-string 2 string-revspec))))
               (t (error "Not yet implemented, sorry!")))))

;;;###autoload
(defun bzr-diff-against (against &optional path dont-switch)
  "Run \"bzr diff\" against a particular revision.

Same as `bzr-dvc-diff', but the interactive prompt is different."
  (interactive
   (let ((root (bzr-tree-root)))
     (list (bzr-revisionspec-to-rev
            (read-string "Diff against revisionspec: ")
            root)
           root
           current-prefix-arg)))
  (bzr-diff against path dont-switch))

;;;###autoload
(defun bzr-dvc-diff (&optional against path dont-switch)
  "Run \"bzr diff\".

AGAINST must be a DVC revision id ('bzr number, last:N,
revid:foobar, ...).

TODO: DONT-SWITCH is currently ignored."
  (interactive (list nil nil current-prefix-arg))
  (let* ((dvc-temp-current-active-dvc 'bzr)
         (window-conf (current-window-configuration))
         (dir (or path default-directory))
         (root (bzr-tree-root dir))
         (against (or against `(bzr (last-revision ,root 1))))
         (buffer (dvc-prepare-changes-buffer
                  against
                  `(bzr (local-tree ,root))
                  'diff root 'bzr)))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-buffer-push-previous-window-config window-conf)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-async
     'bzr `("diff" ,@(when against
                       (list "--revision"
                             (bzr-revision-id-to-string
                              against))))
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (dvc-diff-no-changes (capture buffer)
                            "No changes in %s"
                            (capture root)))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (if (/= 1 status)
           (dvc-diff-error-in-process (capture buffer)
                                      "Error in diff process"
                                      output error)
         (dvc-show-changes-buffer output 'bzr-parse-diff
                                  (capture buffer)))))))

;;;###autoload
(defun bzr-delta (base modified &optional dont-switch extra-arg)
  "Run bzr diff -r BASE..MODIFIED.

TODO: dont-switch is currently ignored."
  (dvc-trace "bzr-delta: base=%S, modified=%S; dir=%S" base modified default-directory)
  (let* ((base-str (bzr-revision-id-to-string base))
         (modified-str (bzr-revision-id-to-string modified))
         (extra-string (if extra-arg (format ", %s" extra-arg) ""))
         (buffer (dvc-prepare-changes-buffer
                  base modified
                  'revision-diff
                  (concat (bzr-revision-id-to-string base)
                          ".."
                          (bzr-revision-id-to-string modified)
                          extra-string)
                  'bzr)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (let ((default-directory
            (cond ((bzr-revision-id-is-local modified)
                   (bzr-revision-id-location modified))
                  ((bzr-revision-id-is-local base)
                   (bzr-revision-id-location base))
                  (t default-directory))))
      (dvc-run-dvc-async
       'bzr `("diff"
              "--revision" ,(concat base-str ".." modified-str)
              ,extra-arg)
       :finished
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-no-changes (capture buffer)
                              "No changes between %s"
                              (concat (capture base-str) " and " (capture modified-str))))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (if (/= 1 status)
             (dvc-diff-error-in-process (capture buffer)
                                        "Error in diff process"
                                        output error)
           (dvc-show-changes-buffer output 'bzr-parse-diff
                                    (capture buffer)))))
      ;; We must return the buffer (even in asynchronous mode)
      (with-current-buffer buffer (goto-char (point-min)))
      buffer)))

(defun bzr-revision-at-point-localp ()
  "Decide whether the revision at point is in the local tree.
This is done by looking at the 'You are missing ... revision(s):' string in the current buffer."
  (save-excursion
    (not (re-search-backward "^You are missing [0-9]+ revision(s):" nil t))))

(defun bzr-get-revision-at-point ()
  (int-to-string
   (nth 2 (dvc-revlist-get-revision-at-point))))

;; FIXME: Does not attempt to find the right entry in
;; bzr-mail-notification-destination according to branch nick, and it
;; really ought to.
(defun bzr-send-commit-notification ()
  "Send a commit notification email for the changelog entry at point.

`bzr-mail-notification-destination' can be used to specify a prefix for
the subject line, the rest of the subject line contains the summary line
of the commit. Additionally the destination email address can be specified."
  (interactive)
  (let* ((dest-specs (cadar bzr-mail-notification-destination)) ;;(tla--name-match-from-list
         ;;(tla--name-split (tla-changelog-revision-at-point))
         ;;tla-mail-notification-destination))
         (rev (bzr-get-revision-at-point))
         (branch-location (nth 2 dest-specs))
         (log-message (bzr-revision-st-message (dvc-revlist-current-patch-struct)))
         (summary (car (split-string log-message "\n"))))
    (if (not (bzr-revision-at-point-localp))
        (message "Not a local revision: %s - no commit notification prepared." rev)
      (message "Preparing commit email for revision %s" rev)
      (let ((gnus-newsgroup-name nil))
        (compose-mail (if dest-specs (cadr dest-specs) "")
                      (concat (if dest-specs (car dest-specs) "")
                              "rev " rev ": " summary)))
      (message-goto-body)
      (while (looking-at "<#part[^>]*>")
        (forward-line 1))
      (insert (concat "Committed revision " rev
                      (if branch-location (concat " to " branch-location) "")
                      "\n\n"))
      (insert log-message)
      (unless (and (bolp) (looking-at "^$"))
        (insert "\n"))
      (message-goto-body))))


(defun bzr-unknowns ()
  "Run bzr unknowns."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("unknowns")))

(defun bzr-parse-status (changes-buffer)
  (dvc-trace "bzr-parse-status (while)")
  (let (current-status)
    (while (> (point-max) (point))
      (dvc-trace-current-line)

      ;; Typical output:
      ;;
      ;; modified:
      ;;  lisp/bzr.el
      ;;  lisp/dvc-diff.el
      ;;  lisp/dvc-fileinfo.el
      ;; removed:
      ;;  lisp/deleted-file.el
      ;; unknown:
      ;;  lisp/new-file.el
      ;; conflicts:
      ;;  lisp/dvc-status.el
      ;; pending merges:
      ;;   Stefan Reichoer 2007-11-05 Set dvc-bookmarks-show-partners to t per default
      ;;    Stefan Reichoer 2007-11-05 Implemented dvc-bookmarks-find-file-in-tree (...
      ;;
      ;;
      ;; So we need to save the status from the message line, and
      ;; apply it to following file lines.

      (cond ((looking-at "^\\([^ ][^\n]*:\\)")
             ;; a file group message ('missing:' etc)
             (let ((msg (match-string-no-properties 1)))
               (with-current-buffer changes-buffer
                 (ewoc-enter-last dvc-fileinfo-ewoc
                                  (make-dvc-fileinfo-message :text msg)))
               (cond
                ((string-equal msg "added:")
                 (setq current-status 'added))
                ((string-equal msg "conflicts:")
                 (setq current-status 'conflict))
                ((string-equal msg "modified:")
                 (setq current-status 'modified))
                ((string-equal msg "removed:")
                 (setq current-status 'missing))
                ((string-equal msg "unknown:")
                 (setq current-status 'unknown))
                ((string-equal msg "pending merges:")
                 (setq current-status nil))
                ((string-equal msg "renamed:")
                 ;; Rename case is handled explictly below
                 (setq current-status nil))
                (t
                 (error "unrecognized label %s in bzr-parse-status" msg)))))

            ((looking-at "^ +\\([^ ][^\n]*?\\)\\([/@]\\)? => \\([^\n]*?\\)\\([/@]\\)?$")
             ;; a renamed file
             (let ((oldname (match-string-no-properties 1))
                   (dir (match-string-no-properties 2))
                   (newname (match-string-no-properties 3)))
               (with-current-buffer changes-buffer
                 (ewoc-enter-last dvc-fileinfo-ewoc
                                  (make-dvc-fileinfo-file
                                   :mark nil
                                   :dir dir
                                   :file newname
                                   :status 'rename-target
                                   :more-status oldname))
                 (ewoc-enter-last dvc-fileinfo-ewoc
                                  (make-dvc-fileinfo-file
                                   :mark nil
                                   :dir dir
                                   :file oldname
                                   :status 'rename-source
                                   :more-status newname)))))

            ((looking-at " +\\(?:Text conflict in \\)?\\([^\n]*?\\)\\([/@*]\\)?$")
             ;; A typical file in a file group, or a pending merge message
             (if (not current-status)
                 (let ((msg (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                   (with-current-buffer changes-buffer
                     (ewoc-enter-last dvc-fileinfo-ewoc
                                      (make-dvc-fileinfo-message
                                       :text msg))))
               (let ((file (match-string-no-properties 2))
                     (dir (match-string-no-properties 1)))
                 (with-current-buffer changes-buffer
                   (ewoc-enter-last dvc-fileinfo-ewoc
                                    (make-dvc-fileinfo-file
                                     :mark nil
                                     :dir dir
                                     :file file
                                     :status current-status
                                     :more-status ""))))))

            (t (error "unrecognized context in bzr-parse-status")))
      (forward-line 1))))

(defun bzr-dvc-status ()
  "Run \"bzr status\" in `default-directory', which must be a tree root."
  (let* ((window-conf (current-window-configuration))
         (root default-directory)
         (buffer (dvc-prepare-changes-buffer
                  `(bzr (last-revision ,root 1))
                  `(bzr (local-tree ,root))
                  'status root 'bzr)))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-buffer-push-previous-window-config window-conf)
    (setq dvc-buffer-refresh-function 'bzr-dvc-status)
    (dvc-run-dvc-async
     'bzr '("status")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'bzr-parse-status
                                      (capture buffer))
           (dvc-diff-no-changes (capture buffer)
                                "No changes in %s"
                                (capture root))))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-error-in-process (capture buffer)
                                    "Error in diff process"
                                    output error))))))

(defun bzr-parse-inventory (changes-buffer)
  ;;(dvc-trace "bzr-parse-inventory (while)")
  (while (> (point-max) (point))
    ;;(dvc-trace-current-line)
    (cond
     ((looking-at "\\([^\n]*?\\)\\([/@]\\)?$")
      (let ((file (match-string-no-properties 1))
            (dir (match-string-no-properties 2)))
        (with-current-buffer changes-buffer
          (ewoc-enter-last
           dvc-fileinfo-ewoc (make-dvc-fileinfo-file
                              :mark nil
                              :dir dir
                              :file file
                              :status 'known
                              :more-status "")))))
     (t (error "unrecognized context in bzr-parse-inventory")))
    (forward-line 1)))

;;;###autoload
(defun bzr-inventory ()
  "Run \"bzr inventory\"."
  (interactive)
  (let* ((dir default-directory)
         (root (bzr-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(bzr (last-revision ,root 1))
                  `(bzr (local-tree ,root))
                  'inventory root 'bzr)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'bzr-inventory)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-async
     'bzr '("inventory")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (dvc-show-changes-buffer output 'bzr-parse-inventory
                                  (capture buffer)))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-error-in-process (capture buffer)
                                    "Error in inventory process"
                                    output error))))))

;;;###autoload
(defun bzr-add (file)
  "Adds FILE to the repository."
  (interactive "fAdd file or directory: ")
  (message "%s"
           (let ((default-directory (bzr-tree-root)))
             (dvc-run-dvc-sync
              'bzr (list "add" (file-relative-name file))
              :finished 'dvc-output-and-error-buffer-handler))))

;;;###autoload
(defun bzr-dvc-add-files (&rest files)
  "Run bzr add."
  (dvc-trace "bzr-add-files: %s" files)
  (let ((default-directory (bzr-tree-root)))
    (dvc-run-dvc-sync 'bzr (append '("add" "--no-recurse") (mapcar #'file-relative-name
                                                                   files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "bzr add finished")))))

;;;###autoload
(defun bzr-dvc-revert-files (&rest files)
  "Run bzr revert."
  (dvc-trace "bzr-revert-files: %s" files)
  (let ((default-directory (bzr-tree-root)))
    (dvc-run-dvc-sync 'bzr (append '("revert") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (dvc-revert-some-buffers default-directory)
                                  (message "bzr revert finished")))))

;;;###autoload
(defun bzr-dvc-remove-files (&rest files)
  "Run bzr remove."
  (dvc-trace "bzr-remove-files: %s" files)
  (dvc-run-dvc-sync 'bzr (append '("remove") files)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "bzr remove finished"))))

;;;###autoload
(defun bzr-dvc-rename (from to &optional after)
  "Run bzr rename."
  (interactive
   (let* ((from-name (dvc-confirm-read-file-name "bzr rename: "))
          (to-name (dvc-confirm-read-file-name (concat "bzr rename '" from-name "' to: ") nil "" from-name)))
     (list from-name to-name nil)))
  (dvc-run-dvc-sync 'bzr (list "rename" (dvc-uniquify-file-name from) (dvc-uniquify-file-name to)
                               (when after "--after"))
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "bzr rename finished"))))

(defun bzr-is-bound (&optional path)
  "True if branch containing PATH is bound"
  (file-exists-p (concat (file-name-as-directory
                          (bzr-tree-root
                           (or path default-directory)))
                         ".bzr/branch/bound")))

(defun bzr-log-edit-commit-local ()
  "Local commit"
  (interactive)
  (bzr-log-edit-commit t))

(defun bzr-log-edit-commit (&optional local)
  "Commit without --local by default.

If LOCAL (prefix argument) is non-nil, commit with --local.
\(don't update bound branch).

LOCAL is ignored on non-bound branches."
  (interactive "P")
  (let ((buffer (find-file-noselect (dvc-log-edit-file-name))))
    (dvc-log-flush-commit-file-list)
    (save-buffer buffer)
    (let ((default-directory (dvc-uniquify-file-name default-directory)))
      (dvc-run-dvc-async
       'bzr
       (append
        (list "commit" "--verbose" "--file" (dvc-log-edit-file-name)
              (when (and local (bzr-is-bound))
                "--local"))
        ;; Get marked  files to  do  a selected  file commit.  Nil
        ;; otherwise (which means commit all files).
        (when (buffer-live-p dvc-partner-buffer)
          (with-current-buffer dvc-partner-buffer
            (mapcar #'dvc-uniquify-file-name
                    (dvc-current-file-list 'nil-if-none-marked)))))
       :finished (dvc-capturing-lambda (output error status arguments)
                   (dvc-show-error-buffer output 'commit)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (with-current-buffer error
                               (buffer-string))))
                   (dvc-log-close (capture buffer))
                   (dvc-diff-clear-buffers
                    'bzr
                    (capture default-directory)
                    "* Just committed! Please refresh buffer\n")
                   (message "Bzr commit finished !"))))
    (dvc-tips-popup-maybe)))

(defcustom bzr-work-offline 'prompt
  "*Whether bzr commit should use --local for bound branches by default.

Possible values are:
t: work offline  (use --local systematialy)
nil: work online (don't use --local)
'prompt: prompt when needed."
  :type '(choice (const t)
                 (const nil)
                 (const prompt))
  :group 'dvc)

(defun bzr-inform-offline-status ()
  "Informs the user about the offline status of bzr."
  (interactive)
  (message "DVC-bzr will now %s.
Use M-x bzr-change-offline-status RET to change."
           (cond ((eq bzr-work-offline t)
                  "work offline (use commit --local)")
                 ((eq bzr-work-offline nil)
                  "work online (don't provide --local to commit)")
                 ((eq bzr-work-offline 'prompt)
                  "prompt to use --local or not"))))

(defun bzr-change-offline-status ()
  "Change the offline status of DVC-bzr.

Prompt the user and change `bzr-work-offline' accordingly."
  (interactive)
  (discard-input)
  (save-window-excursion
    (let (answer commit-locally)
      (while (null answer)
        (message "Change offline status to ([C]onnected, [D]isconnected, [P]rompt)): ")
        (let ((tem (downcase (let ((cursor-in-echo-area t))
                               (read-char-exclusive)))))
          (setq answer
                (if (= tem help-char)
                    'help
                  (cdr (assoc tem '((?c . connect)
                                    (?d . t)
                                    (?p . prompt))))))
          (cond ((null answer)
                 (beep)
                 (message "Please type c, p or d")
                 (sit-for 3))
                ((eq answer 'connect)
                 (setq bzr-work-offline nil))
                (t
                 (setq bzr-work-offline answer)))
          (bzr-inform-offline-status))))))


(defun bzr-ask-user-about-offline ()
  "Return non-nil if bzr should work offline."
  (cond ((eq bzr-work-offline t)
         t)
        ((eq bzr-work-offline nil)
         nil)
        (t
         (discard-input)
         (save-window-excursion
           (let (answer commit-locally)
             (while (null answer)
               (message "Commit locally only? (y, n, c, d) ")
               (let ((tem (downcase (let ((cursor-in-echo-area t))
                                      (read-char-exclusive)))))
                 (setq answer
                       (if (= tem help-char)
                           'help
                         (cdr (assoc tem '((?y . yes)
                                           (?n . no)
                                           (?c . connect)
                                           (?d . disconnect)
                                           (?? . help))))))
                 (cond ((null answer)
                        (beep)
                        (message "Please type y, n or r; or ? for help")
                        (sit-for 3))
                       ((eq answer 'help)
                        (message "Yes (commit locally), No (commit remotely too),
Connect (commit remotely from now), Disconnect (commit locally from now)")
                        (sit-for 5)
                        (setq answer nil))
                       ((eq answer 'yes)
                        (setq commit-locally t))
                       ((eq answer 'no)
                        (setq commit-locally nil))
                       ((eq answer 'connect)
                        (setq bzr-work-offline nil
                              commit-locally nil)
                        (bzr-inform-offline-status))
                       ((eq answer 'disconnect)
                        (setq bzr-work-offline t
                              commit-locally t)
                        (bzr-inform-offline-status)))))
             commit-locally)))))

(defun bzr-log-edit-done ()
  "Commit. Interactive prompt to know whether this should be local.

See `bzr-log-edit-commit' and `bzr-log-edit-commit-local' for
non-interactive versions."
  (interactive)
  (bzr-log-edit-commit (and (bzr-is-bound)
                            (bzr-ask-user-about-offline))))


(eval-when-compile
  (defvar smerge-mode))

;;;###autoload
(defun bzr-resolved (file)
  "Command to delete .rej file after conflicts resolution.
Asks confirmation if the file still has diff3 markers.
Then, run \"bzr resolve\".

TODO: should share some code with `tla-resolved'."
  (interactive
   (list (let ((file (buffer-file-name)))
           (if (string-match "^\\(.*\\)\\.\\(BASE\\|OTHER\\|THIS\\)$" file)
               (let ((norej (match-string 1 file)))
                 (if (and (file-exists-p norej)
                          (y-or-n-p (format "Use file %s instead of %s? "
                                            (file-name-nondirectory norej)
                                            (file-name-nondirectory file))))
                     norej
                   file))
             file))))
  (with-current-buffer (find-file-noselect file)
    (if (and (boundp 'smerge-mode) smerge-mode)
        (progn
          (when (and
                 (save-excursion
                   (goto-char (point-min))
                   (dvc-funcall-if-exists smerge-find-conflict))
                 (not (y-or-n-p (concat "Buffer still has diff3 markers. "
                                        "Mark as resolved anyway? "))))
            (error "Not marking file as resolved"))
          (dvc-funcall-if-exists smerge-mode -1)))
    (dolist (ext '("BASE" "OTHER" "THIS"))
      (let ((buf (find-buffer-visiting (concat file ext))))
        (when buf (kill-buffer buf))))
    (dvc-run-dvc-sync 'bzr
                      `("resolved"
                        ,file)
                      :finished 'dvc-null-handler)))

(defun bzr-file-has-conflict-p (file-name)
  "Return non-nil if FILE-NAME has conflicts.

In practice, check for the existance of \"FILE.BASE\"."
  (let ((rej-file-name (concat default-directory
                               (file-name-nondirectory file-name)
                               ".BASE")))
    (file-exists-p rej-file-name)))


;; Revisions

(defun bzr-revision-id-location (rev-id)
  "Extract the location component from REV-ID."
  (case (dvc-revision-get-type rev-id)
    ((revision previous-revision)
     (let* ((data (car (dvc-revision-get-data rev-id)))
            (location (nth 1 data)))
       location))
    (otherwise nil)))

(defun bzr-revision-id-is-local (rev-id)
  "Non-nil if rev-id has the same path as the local tree."
  (case (dvc-revision-get-type rev-id)
    ((revision previous-revision)
     (let ((data (car (dvc-revision-get-data rev-id))))
       (eq (nth 0 data) 'local)))
    (otherwise nil)))

(defun bzr-revision-nth-ancestor (rev-id n)
  "Get the N-th ancestor of REV-ID."
  (case (dvc-revision-get-type rev-id)
    ((revision previous-revision)
     (let ((data (car (dvc-revision-get-data rev-id))))
       `(bzr (revision (,(nth 0 data)
                        ,(nth 1 data)
                        ,(- (nth 2 data) n))))))
    (otherwise (error "TODO: not implemented. REV-ID=%S" rev-id))))

(defun bzr-revision-id-to-string (rev-id)
  "Turn a DVC revision ID to a bzr revision spec.

\(bzr (revision (local \"/path/to/archive\" 3)))
=> \"revno:3\".
"
  (case (dvc-revision-get-type rev-id)
    (revision (let* ((data (car (dvc-revision-get-data rev-id)))
                     (location (nth 0 data)))
                (cond ((eq location 'local)
                       (concat "revno:" (int-to-string (nth 2 data))))
                      ((eq location 'remote)
                       (concat "revno:" (int-to-string (nth 2 data))
                               ":" (nth 1 data))))))
    (previous-revision
     (bzr-revision-id-to-string
      (let* ((previous-list (nth 1 rev-id))
             (rev `(bzr ,(nth 1 previous-list)))
             (n-prev (nth 2 previous-list)))
        (bzr-revision-nth-ancestor rev n-prev))))
    (last-revision
     (let* ((data (dvc-revision-get-data rev-id))
            (num (nth 1 data)))
       (concat "last:" (int-to-string num))))
    (tag
     (car (dvc-revision-get-data rev-id)))
    (otherwise (error "TODO: not implemented: %S" rev-id))))


(defun bzr-revision-get-file-revision (file revision)
  "Insert the content of FILE in REVISION, in current buffer.

REVISION is a back-end-revision, not a dvc revision-id. It looks like
\(local \"path\" NUM)."
  (let ((bzr-rev
         (if (eq (car (car revision)) 'local)
             (int-to-string (nth 2 (car revision)))
           (error "TODO: revision=%S" revision)))
        (path (if (eq (car (car revision)) 'local)
                  (nth 1 (car revision))
                default-directory)))
    (let ((default-directory path))
      (insert
       (dvc-run-dvc-sync
        ;; TODO what if I'm not at the tree root ?
        'bzr (list "cat" "--revision" bzr-rev file)
        :finished 'dvc-output-buffer-handler-withnewline)))))

;;;###autoload
(defun bzr-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"root\" NUM)
"
  (let ((bzr-rev (concat "last:" (int-to-string
                                  (nth 1 last-revision))))
        (default-directory (car last-revision)))
    (insert
     (dvc-run-dvc-sync
      'bzr (list "cat" "--revision" bzr-rev file)
      :finished 'dvc-output-buffer-handler-withnewline))))

;;;###autoload
(defun bzr-command-version ()
  "Run bzr version."
  (interactive)
  (setq bzr-command-version
        (dvc-run-dvc-sync
         'bzr (list "version")
         :finished (lambda (output error status arguments)
                     (set-buffer output)
                     (goto-char (point-min))
                     (buffer-substring (point) (point-at-eol)))))
  (when (interactive-p)
    (message "Bazaar-NG Version: %s" bzr-command-version))
  bzr-command-version)

(defun bzr-whoami ()
  "Run bzr whoami."
  (interactive)
  (let ((whoami (dvc-run-dvc-sync 'bzr (list "whoami")
                                  :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "bzr whoami: %s" whoami))
    whoami))

(defun bzr-save-diff (filename)
  "Save the current bzr diff to a file named FILENAME."
  (interactive (list (read-file-name "Save the bzr diff to: ")))
  (with-current-buffer
      (find-file-noselect filename)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (dvc-run-dvc-sync 'bzr (list "diff")
                                ;; bzr diff has a non-zero status
                                :error 'dvc-output-and-error-buffer-handler))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun bzr-nick (&optional new-nick)
  "Run bzr nick.
When called with a prefix argument, ask for the new nick-name, otherwise
display the current one."
  (interactive "P")
  (let ((nick (dvc-run-dvc-sync 'bzr (list "nick")
                                :finished 'dvc-output-buffer-handler)))
    (if (not new-nick)
        (progn
          (when (interactive-p)
            (message "bzr nick: %s" nick))
          nick)
      (when (interactive-p)
        (setq new-nick (read-string (format "Change nick from '%s' to: " nick) nil nil nick)))
      (dvc-run-dvc-sync 'bzr (list "nick" new-nick)))))

(defun bzr-info ()
  "Run bzr info."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("info")))

(defun bzr-testament ()
  "Run bzr testament."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("testament")))

(defun bzr-plugins ()
  "Run bzr plugins."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("plugins")))

(defun bzr-check ()
  "Run bzr check."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("check") t))

(defun bzr-ignored ()
  "Run bzr ignored."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("ignored")))

(defun bzr-conflicts ()
  "Run bzr conflicts."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("conflicts")))

(defun bzr-deleted ()
  "Run bzr deleted."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("deleted")))

(defun bzr-renames ()
  "Run bzr renames."
  (interactive)
  (dvc-run-dvc-display-as-info 'bzr '("renames")))

(defun bzr-version-info ()
  "Run bzr verision-info."
  (interactive)
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'bzr '("version-info"))
    (dvc-run-dvc-sync 'bzr (list "version-info")
                      :finished 'dvc-output-buffer-handler)))

(defun bzr-upgrade ()
  "Run bzr upgrade."
  (interactive)
  (let ((default-directory (dvc-tree-root)))
    (dvc-run-dvc-display-as-info 'bzr '("upgrade") t)))

(defun bzr-ignore (pattern)
  "Run bzr ignore PATTERN."
  (interactive "sbzr ignore: ")
  (dvc-run-dvc-sync 'bzr (list "ignore" pattern)))

(defun bzr-uncommit ()
  "Run bzr uncommit.
Ask the user before uncommitting."
  (interactive)
  (let ((window-conf (current-window-configuration)))
    (dvc-run-dvc-display-as-info 'bzr (list "uncommit" "--dry-run" "--force"))
    (if (yes-or-no-p "Remove the bzr revision? ")
        (progn
          (message "Removing bzr revision")
          (set-window-configuration window-conf)
          (dvc-run-dvc-sync 'bzr (list "uncommit" "--force")))
      (message "Aborted bzr uncommit")
      (set-window-configuration window-conf))))

(defun bzr-config-directory ()
  "Path of the configuration directory for bzr."
  (file-name-as-directory
   (if (eq system-type 'windows-nt)
       (expand-file-name  "bazaar/2.0" (getenv "APPDATA"))
     (expand-file-name "~/.bazaar"))))

(defun bzr-config-file (file)
  "Path of configuration file FILE for bzr.

File can be, i.e. bazaar.conf, ignore, locations.conf, ..."
  (concat (bzr-config-directory) file))

(defvar bzr-ignore-list ".tmp-bzr*\n"
  "List of newline-terminated ignore patterns that DVC should add to
  ~/.bazaar/ignore.")

(defun bzr-ignore-setup ()
  "Sets up a default ignore list for DVC in ~/.bazaar/ignore"
  (interactive)
  (let* ((file (bzr-config-file "ignore"))
         (buffer (or (when (file-exists-p file)
                       (find-file-noselect file))
                     ;; let bzr create the file.
                     (let* ((dir (dvc-make-temp-dir "dvc-bzr-ignore"))
                            (default-directory dir))
                       (dvc-run-dvc-sync 'bzr (list "init")
                                         :finished 'dvc-null-handler)
                       (with-current-buffer (find-file-noselect
                                             (expand-file-name "foo"))
                         (insert "foo")
                         (save-buffer))
                       (dvc-run-dvc-sync 'bzr (list "ignored")
                                         :finished 'dvc-null-handler)
                       (dvc-delete-recursively dir)
                       (if (file-exists-p file)
                           (find-file-noselect file)
                         (message "WARNING: Could not find or create bzr user-wide ignore file.")
                         nil))))
         (ins t))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (if (re-search-forward "^# DVC ignore (don't edit !!)\n\\(\\(.\\|\n\\)*\n\\)# end DVC ignore$" nil 'end)
            (progn
              (if (string= bzr-ignore-list (match-string 1))
                  (setq ins nil)
                (message "Overriding old DVC ignore list for bzr")
                (delete-region (match-beginning 0) (match-end 0))))
          (message "Setting up DVC ignore list for bzr"))
        (when ins
          (insert "# DVC ignore (don't edit !!)\n")
          (insert bzr-ignore-list)
          (insert "# end DVC ignore\n")
          (save-buffer)))
      (kill-buffer buffer))))

(defun bzr-do-annotate (file)
  "Annote the FILE"
  (let* ((file (expand-file-name file))
         (abuffer (dvc-get-buffer-create 'bzr 'annotate))
         (args (list "annotate" "--all" "--long" file)))
    (dvc-switch-to-buffer-maybe abuffer)
    (dvc-run-dvc-sync 'bzr args
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture abuffer)
                            (let ((inhibit-read-only t))
                              (erase-buffer)
                              (setq truncate-lines t)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (bzr-annotate-mode))))))))

(defun bzr-annotate ()
  "Run bzr annotate"
  (interactive)
  (let* ((line (dvc-line-number-at-pos))
         (filename (dvc-confirm-read-file-name "Filename to annotate: ")))
    (bzr-do-annotate filename)
    (goto-line line)))

(defconst bzr-annon-parse-re
  "^\\(\\S-*\\)\\s-+\\(\\S-*\\)\\s-+\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\s-+|")

(defun bzr-annotate-time ()
  (interactive)
  (when (< (point) (point-max))
    (beginning-of-line)
    (if (re-search-forward bzr-annon-parse-re nil t)
        (let* ((year (string-to-number (match-string 3)))
               (month (string-to-number (match-string 4)))
               (day (string-to-number (match-string 5)))
               (ct (dvc-annotate-convert-time
                    (encode-time 1 1 1 day month year))))
          ct))))

(define-derived-mode  bzr-annotate-mode fundamental-mode "bzr-annotate"
  "Major mode to display bzr annotate output."
  (dvc-annotate-display-autoscale t)
  (dvc-annotate-lines (point-max))
  ;;(xgit-annotate-hide-revinfo)
  (toggle-read-only 1))

(defun bzr-switch-checkout (target)
  "Switch the checkout to the branch TARGET"
  (interactive "sURL of the branch to switch to: ")
  (dvc-run-dvc-sync 'bzr (list "switch" target)
                    :finished 'dvc-output-buffer-handler)
  (dvc-revert-some-buffers)
  (dvc-trace "Switched checkout to  %s" target)
  )

(defun bzr-switch-checkout-l (target)
  "Switch the checkout to a local branch"
  (interactive "DBranch to switch to: ")
  (let ((target (expand-file-name target)))
    (bzr-switch-checkout target))
  )

(defun bzr-create-bundle (rev file-name &optional extra-parameter-list)
  "Call bzr send --output to create a file containing a bundle"
  (interactive (list (bzr-read-revision "Create bundle for revision: ")
                     (read-file-name "Name of the bzr bundle file: ")
                     (split-string (read-string "Extra parameters: "))))
  (let ((arg-list (list "send" "-o" (expand-file-name file-name) "-r" rev)))
    (when extra-parameter-list
      (setq arg-list (append arg-list extra-parameter-list)))
    (dvc-run-dvc-sync 'bzr arg-list
                      :finished
                      (lambda (output error status arguments)
                        (message "Created bundle for revision %s in %s." rev file-name)))))

;;; FIXME: this should probably be a defcustom
;;;###autoload
(defvar bzr-export-via-email-parameters nil
  "list of (PATH (EMAIL BRANCH-NICK (EXTRA-ARG ...)))")
;;(add-to-list 'bzr-export-via-email-parameters '("~/work/myprg/dvc" ("joe@host.com" "dvc-el")))
;; or:
;;(add-to-list 'bzr-export-via-email-parameters
;; '("~/work/myprg/dvc" ("joe@host.com" "dvc-el" ("--no-bundle" "." "../dvc-bundle-base"))))

(defun bzr-export-via-email ()
  "Export the revision at point via email.
`bzr-export-via-email-parameters' can be used to customize the behaviour of
this function."
  (interactive)

  (require 'message)
  (require 'mml)

  (let* ((rev (bzr-get-revision-at-point))
         (log-message (bzr-revision-st-message (dvc-revlist-current-patch-struct)))
         (base-file-name nil)
         (summary (car (split-string log-message "\n")))
         (file-name nil)
         (description nil)
         (destination-email "")
         (extra-parameter-list nil))
    (dolist (m bzr-export-via-email-parameters)
      (when (string= (dvc-uniquify-file-name (car m)) (dvc-uniquify-file-name (bzr-tree-root)))
        ;;(message "%S" (cadr m))
        (setq destination-email (car (cadr m)))
        (setq base-file-name (nth 1 (cadr m)))
        (setq extra-parameter-list (nth 2 (cadr m)))))
    (message "bzr-export-via-email %s: %s to %s" rev summary destination-email)
    (setq file-name (concat (dvc-uniquify-file-name dvc-temp-directory)
			    (or base-file-name "") rev ".patch"))
    (bzr-create-bundle rev file-name extra-parameter-list)

    (setq description
          (dvc-run-dvc-sync 'bzr (list "log" "-r" rev)
                            :finished 'dvc-output-buffer-handler))

    (require 'reporter)
    (delete-other-windows)
    (reporter-submit-bug-report
     destination-email
     nil
     nil
     nil
     nil
     description)

    ;; we need MML converted to MIME or the attachment isn't attached!
    (when (eq mail-user-agent 'sendmail-user-agent)
      (add-hook 'mail-send-hook 'mml-to-mime nil t))

    ;; delete emacs version - its not needed here
    (delete-region (point) (point-max))

    (mml-attach-file file-name "text/x-patch")
    (goto-char (point-min))
    (mail-position-on-field "Subject")
    (insert (concat "[PATCH] " summary))))

;; provide 'bzr before running bzr-ignore-setup, because bzr-ignore-setup
;; loads a file and this triggers the loading of bzr.
(provide 'bzr)

;; Must remain toplevel, and should not be autoloaded.
(when (executable-find bzr-executable)
  (bzr-ignore-setup))

;;; bzr.el ends here

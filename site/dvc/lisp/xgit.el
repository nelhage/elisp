;;; xgit.el --- git interface for dvc

;; Copyright (C) 2006-2008 by all contributors

;; Author: Stefan Reichoer <stefan@xsteve.at>
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

;; This is the git backend for DVC.  It requires git version 1.5.0 or
;; later.

;;; History:

;;

;;; Code:

(require 'dvc-core)
(require 'dvc-diff)
(require 'xgit-core)
(require 'xgit-log)
(eval-when-compile (require 'cl))
(require 'xgit-annotate)
(require 'cus-edit)

;;;###autoload
(defun xgit-init (&optional dir)
  "Run git init."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for git init: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (let ((default-directory (or dir default-directory)))
    (dvc-run-dvc-sync 'xgit (list "init-db")
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "git init finished")))))

;;;###autoload
(defun xgit-clone (src &optional dest)
  "Run git clone."
  (interactive (list (read-string "git clone from: ")))
  (dvc-run-dvc-async 'xgit (list "clone" src dest)))

;;;###autoload
(defun xgit-add (file)
  "Add FILE to the current git project."
  (interactive (list (dvc-confirm-read-file-name "Add file or directory: ")))
  (xgit-dvc-add-files file))

;;;###autoload
(defun xgit-dvc-add-files (&rest files)
  "Run git add."
  (dvc-trace "xgit-add-files: %s" files)
  (let ((default-directory (xgit-tree-root)))
    (dvc-run-dvc-sync 'xgit (append '("add")
                                    (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "git add finished")))))

;;;###autoload
(defun xgit-remove (file &optional force)
  "Remove FILE from the current git project.
If FORCE is non-nil, then remove the file even if it has
uncommitted changes."
  (interactive (list (dvc-confirm-read-file-name "Remove file: ")
                     current-prefix-arg))
  (let ((default-directory (xgit-tree-root)))
    (dvc-run-dvc-sync
     'xgit (list "rm" (when force "-f") "--" (file-relative-name file))
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (message "git remove finished")))))

;;;###autoload
(defun xgit-dvc-remove-files (&rest files)
  "Run git rm."
  (dvc-trace "xgit-remove-files: %s" files)
  (dvc-run-dvc-sync 'xgit (nconc (list "rm" "--")
                                 (mapcar #'file-relative-name files))
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "git rm finished"))))

(defun xgit-command-version ()
  "Run git version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xgit (list "version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Git Version: %s" version))
    version))

;;;###autoload
(defun xgit-add-all-files (arg)
  "Run 'git add .' to add all files in the current directory tree to git.

Normally run 'git add -n .' to simulate the operation to see
which files will be added.

Only when called with a prefix argument, add the files."
  (interactive "P")
  (dvc-run-dvc-sync 'xgit (list "add" (unless arg "-n") ".")))

;;;###autoload
(defun xgit-addremove ()
  "Add all new files to the index, remove all deleted files from
the index, and add all changed files to the index.

This is done only for files in the current directory tree."
  (interactive)
  (dvc-run-dvc-sync
   'xgit (list "add" ".")
   :finished (lambda (output error status arguments)
               (dvc-run-dvc-sync
                'xgit (list "add" "-u" ".")
                :finished
                (lambda (output error status args)
                  (message "Finished adding and removing files to index"))))))

;;;###autoload
(defun xgit-reset-hard (&rest extra-param)
  "Run 'git reset --hard'"
  (interactive)
  (when (interactive-p)
    (setq extra-param (list (ido-completing-read "git reset --hard " '("HEAD" "ORIG_HEAD")
                                                 nil nil nil nil '("HEAD" "ORIG_HEAD")))))
  (dvc-run-dvc-sync 'xgit (append '("reset" "--hard") extra-param)))

(defvar xgit-status-line-regexp
  "^#[ \t]+\\([[:alpha:]][[:alpha:][:blank:]]+\\):\\(?:[ \t]+\\(.+\\)\\)?$"
  "Regexp that matches a line of status output.
The first match string is the status type, and the optional
second match is the file.")

(defvar xgit-status-untracked-regexp "^#\t\\(.+\\)$"
  "Regexp that matches a line of status output indicating an
untracked file.

The first match is the file.")

(defvar xgit-status-renamed-regexp "^\\(.+\\) -> \\(.+\\)$"
  "Regexp that divides a filename string.
The first match is the original file, and the second match is the
new file.")

(defun xgit-parse-status-sort (status-list)
  "Sort STATUS-LIST according to :status in the order
conflict, added, modified, renamed, copied, deleted, unknown."
  (let ((order '((conflict . 0)
                 (added . 1) (modified . 2)
                 (rename-source . 3) (rename-target . 3)
                 (copy-source . 4) (copy-target . 4)
                 (deleted . 5) (unknown . 6)))
        (get (lambda (item)
               (catch 'status
                 (while item
                   (if (eq (car item) :status)
                       (throw 'status (cadr item))
                     (setq item (cddr item))))))))
    (sort status-list
          (dvc-capturing-lambda (a b)
            (let ((ao (cdr (assq (funcall (capture get) a) order)))
                  (bo (cdr (assq (funcall (capture get) b) order))))
              (and (integerp ao) (integerp bo)
                   (< ao bo)))))))

(defun xgit-parse-status  (changes-buffer)
  (dvc-trace "xgit-parse-status (dolist)")
  (let ((output (current-buffer)))
    (with-current-buffer changes-buffer
      (setq dvc-header (format "git status for %s\n" default-directory))
      (with-current-buffer output
        (save-excursion
          (goto-char (point-min))
          (let ((buffer-read-only)
                (grouping "")
                status-string
                file status dir
                status-list
                indexed)
            (while (re-search-forward xgit-status-line-regexp nil t)
              (setq status-string (match-string 1)
                    file (match-string 2)
                    indexed t)
              (cond ((or (null file) (string= "" file))
                     (when (string= status-string "Untracked files")
                       (let ((end
                              (save-excursion
                                (re-search-forward xgit-status-line-regexp
                                                   nil 'end)
                                (point))))
                         (forward-line 2)
                         (while (re-search-forward xgit-status-untracked-regexp
                                                   end t)
                           (when (match-beginning 1)
                             (setq status-list
                                   (cons (list :file (match-string 1)
                                               :status 'unknown
                                               :indexed t)
                                         status-list))))
                         (forward-line -1)))
                     (setq grouping status-string
                           status nil))
                    ((string= status-string "modified")
                     (setq status 'modified)
                     (when (string= grouping "Changed but not updated")
                       (setq indexed nil)))
                    ((string= status-string "new file")
                     (setq status 'added))
                    ((string= status-string "deleted")
                     (setq status 'deleted)
                     (when (string= grouping "Changed but not updated")
                       (setq indexed nil)))
                    ((string= status-string "renamed")
                     (setq status nil)
                     (when (string-match xgit-status-renamed-regexp file)
                       (let ((orig (match-string 1 file))
                             (new (match-string 2 file)))
                         (setq status-list
                               (cons
                                (list :file new :dir nil
                                      :status 'rename-target :indexed t)
                                (cons (list :file orig :dir nil
                                            :status 'rename-source :indexed t)
                                      status-list))))))
                    ((string= status-string "copied")
                     (setq status nil)
                     (when (string-match xgit-status-renamed-regexp file)
                       (let ((orig (match-string 1 file))
                             (new (match-string 2 file)))
                         (setq status-list
                               (cons
                                (list :file new :dir nil
                                      :status 'copy-target :indexed t)
                                (cons (list :file orig :dir nil
                                            :status 'copy-source :indexed t)
                                      status-list))))))
                    ((string= status-string "unmerged")
                     (setq status 'conflict))
                    (t
                     (setq status nil)))
              (when status
                (setq status-list
                      (cons (list :file file :dir nil
                                  :status status :indexed indexed)
                            status-list))))
            (with-current-buffer changes-buffer
              (dolist (elem (xgit-parse-status-sort (nreverse status-list)))
                (ewoc-enter-last dvc-fileinfo-ewoc
                                 (apply #'make-dvc-fileinfo-file elem))))))))))

(defun xgit-dvc-status (&optional verbose)
  "Run git status."
  (let* ((root default-directory)
         (buffer (dvc-prepare-changes-buffer
                  `(xgit (last-revision ,root 1))
                  `(git (local-tree ,root))
                  'status root 'xgit)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'xgit-dvc-status)
    (dvc-save-some-buffers root)
    (let ((show-changes-buffer
           (dvc-capturing-lambda (output error status arguments)
             (with-current-buffer (capture buffer)
               (if (> (point-max) (point-min))
                   (dvc-show-changes-buffer output 'xgit-parse-status
                                            (capture buffer))
                 (dvc-diff-no-changes (capture buffer)
                                      "No changes in %s"
                                      (capture root)))))))
      (dvc-run-dvc-sync
       'xgit `("status" ,(when verbose "-v"))
       :finished show-changes-buffer
       :error show-changes-buffer))))

(defun xgit-status-verbose ()
  (interactive)
  (xgit-status t))

(defun xgit-status-add-u ()
  "Run \"git add -u\" and refresh current buffer."
  (interactive)
  (lexical-let ((buf (current-buffer)))
    (dvc-run-dvc-async
     'xgit '("add" "-u")
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (with-current-buffer buf
                   (dvc-generic-refresh))))))

(defun xgit-status-reset-mixed ()
  "Run \"git reset --mixed\" and refresh current buffer.

This reset the index to HEAD, but doesn't touch files."
  (interactive)
  (lexical-let ((buf (current-buffer)))
    (dvc-run-dvc-async
     'xgit '("reset" "--mixed")
     :finished (dvc-capturing-lambda
                   (output error status arguments)
                 (with-current-buffer buf
                   (dvc-generic-refresh))))))

(defvar xgit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?A] 'xgit-status-add-u)
    (define-key map [?R] 'xgit-status-reset-mixed)
    map))

(easy-menu-define xgit-diff-mode-menu xgit-diff-mode-map
  "`Git specific changes' menu."
  `("GIT-Diff"
    ["Re-add modified files (add -u)" xgit-status-add-u t]
    ["Reset index (reset --mixed)" xgit-status-reset-mixed t]
    "---"
    ["View staged changes" xgit-diff-cached t]
    ["View unstaged changes" xgit-diff-index t]
    ["View all local changes" xgit-diff-head t]
    ))

(define-derived-mode xgit-diff-mode dvc-diff-mode "xgit-diff"
  "Mode redefining a few commands for diff."
  )

(dvc-add-uniquify-directory-mode 'xgit-diff-mode)

(defun xgit-parse-diff (changes-buffer)
  (save-excursion
    (while (re-search-forward
            "^diff --git [^ ]+ b/\\(.*\\)$" nil t)
      (let* ((name (match-string-no-properties 1))
             ;; added, removed are not yet working
             (added (progn (forward-line 1)
                           (looking-at "^new file")))
             (removed (looking-at "^deleted file")))
        (with-current-buffer changes-buffer
          (ewoc-enter-last
           dvc-fileinfo-ewoc
           (make-dvc-fileinfo-legacy
            :data (list 'file
                        name
                        (cond (added   "A")
                              (removed "D")
                              (t " "))
                        (cond ((or added removed) " ")
                              (t "M"))
                        " "             ; dir. directories are not
                                        ; tracked in git
                        nil))))))))

(defun xgit-diff-1 (against-rev path dont-switch base-rev)
  (let* ((cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (xgit-tree-root cur-dir))
         (against (if against-rev
                      (dvc-revision-to-string against-rev
                                              xgit-prev-format-string "HEAD")
                    "HEAD"))
         (against-rev (or against-rev (if (xgit-use-index-p)
                                          '(xgit (index))
                                        `(xgit (last-revision ,root 1)))))
         (base (if base-rev
                   (dvc-revision-to-string base-rev xgit-prev-format-string
                                           "HEAD")
                 nil))
         (local-tree `(xgit (local-tree ,root)))
         (base-rev (or base-rev local-tree))
         (buffer (dvc-prepare-changes-buffer
                  against-rev base-rev
                  'diff root 'xgit))
         (command-list (if (equal against-rev '(xgit (index)))
                           (if (equal base-rev local-tree)
                               '("diff" "-M")
                             (message "%S != %S" base-rev local-tree)
                             `("diff" "-M" "--cached" ,against))
                         `("diff" "-M" ,base ,against))))
    (dvc-switch-to-buffer-maybe buffer)
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync 'xgit command-list
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (dvc-show-changes-buffer output
                                                 'xgit-parse-diff
                                                 (capture buffer)
                                                 nil nil
                                                 (mapconcat
                                                  (lambda (x) x)
                                                  (cons "git" command-list)
                                                  " "))))))

(defun xgit-last-revision (path)
  (if (xgit-use-index-p)
      '(xgit (index))
    `(xgit (last-revision ,path 1))))

;; TODO offer completion here, e.g. xgit-tag-list
(defun xgit-read-revision-name (prompt)
  (read-string prompt))

;;;###autoload
(defun xgit-dvc-diff (&optional against-rev path dont-switch)
  (interactive (list nil nil current-prefix-arg))
  (xgit-diff-1 against-rev path dont-switch nil))

;;;###autoload
(defun xgit-diff-cached (&optional against-rev path dont-switch)
  "Call \"git diff --cached\"."
  (interactive (list nil nil current-prefix-arg))
  (let ((xgit-use-index 'always))
    (xgit-diff-1 against-rev path dont-switch '(xgit (index)))))

;;;###autoload
(defun xgit-diff-index (&optional against-rev path dont-switch)
  "Call \"git diff\" (diff between tree and index)."
  (interactive (list nil nil current-prefix-arg))
  (let ((path (or path (xgit-tree-root)))
        (against-rev (or against-rev '(xgit (index)))))
    (xgit-diff-1 against-rev path dont-switch
                 `(xgit (local-tree ,path)))))

;;;###autoload
(defun xgit-diff-head (&optional path dont-switch)
  "Call \"git diff HEAD\"."
  (interactive (list nil current-prefix-arg))
  (xgit-diff-1 `(xgit (local-tree ,path))
               path dont-switch
               `(xgit (last-revision ,path 1))))

;;;###autoload
(defun xgit-diff2 (base-rev against-rev &optional path dont-switch)
  "Call \"git diff BASE-REV AGAINST-REV\"."
  (interactive (list
                (xgit-read-revision-name "Base Revision: ")
                (xgit-read-revision-name "Against Revision: ")
                nil
                current-prefix-arg))
  (xgit-diff-1 `(xgit (revision ,against-rev))
               path dont-switch
               `(xgit (revision ,base-rev))))

(defvar xgit-prev-format-string "%s~%s"
  "This is a format string which is used by `dvc-revision-to-string'
when encountering a (previous ...) component of a revision indicator.
.
The first argument is a commit ID, and the second specifies how
many generations back we want to go from the given commit ID.")

(defun xgit-delta (base-rev against &optional dont-switch)
  (interactive (list nil nil current-prefix-arg))
  (let* ((root (xgit-tree-root))
         (buffer (dvc-prepare-changes-buffer
                  `(xgit (last-revision ,root 1))
                  `(xgit (local-tree ,root))
                  'diff root 'xgit)))
    (xgit-diff-1 against root dont-switch base-rev)
    (with-current-buffer buffer (goto-char (point-min)))
    buffer))

;;;###autoload
(defun xgit-fetch (&optional repository)
  "Call git fetch.
When called with a prefix argument, ask for the fetch source."
  (interactive "P")
  (when (interactive-p)
    (when current-prefix-arg
      (setq repository (read-string "Git fetch from: "))))
  (dvc-run-dvc-async 'xgit (list "fetch" repository)))

;;;###autoload
(defun xgit-pull (&optional repository)
  "Call git pull.
When called with a prefix argument, ask for the pull source."
  (interactive "P")
  (when (interactive-p)
    (when current-prefix-arg
      (setq repository (read-string "Git pull from: "))))
  (dvc-run-dvc-async 'xgit (list "pull" repository)
                     :finished
                     (dvc-capturing-lambda (output error status arguments)
                       (with-current-buffer output
                         (xgit-parse-pull-result t))
                       (when xgit-pull-result
                         (dvc-switch-to-buffer output)
                         (when (y-or-n-p "Run xgit-whats-new? ")
                           (xgit-whats-new))))))

(defvar xgit-pull-result nil)
(defun xgit-parse-pull-result (reset-parameters)
  "Parse the output of git pull."
  (when reset-parameters
    (setq xgit-pull-result nil))
  (goto-char (point-min))
  (cond ((looking-at "Updating \\([0-9a-z]+\\)\.\.\\([0-9a-z]+\\)")
         (setq xgit-pull-result (list (match-string 1) (match-string 2)))
         (message "Execute M-x xgit-whats-new to see the arrived changes."))
        ((looking-at "Already up-to-date.")
         (message "Already up-to-date."))))

(defun xgit-whats-new ()
  "Show the changes since the last git pull."
  (interactive)
  (when xgit-pull-result
    (xgit-changelog (car xgit-pull-result) (cadr xgit-pull-result) t)))

(defun xgit-split-out-added-files (files)
  "Remove any files that have been newly added to git from FILES.
This returns a two-element list.

The first element of the returned list is a list of the
newly-added files from FILES.

The second element is the remainder of FILES."
  (let* ((tree-added nil)
         (added nil)
         (not-added nil))
    ;; get list of files that have been added
    (with-temp-buffer
      (dvc-run-dvc-sync 'xgit (list "status")
                        :output-buffer (current-buffer)
                        :finished #'ignore :error #'ignore)
      (goto-char (point-min))
      (while (re-search-forward xgit-status-line-regexp nil t)
        (when (string= (match-string 1) "new file")
          (setq tree-added (cons (match-string 2) tree-added)))))
    ;; filter FILES
    (dolist (file files)
      (if (member file tree-added)
          (setq added (cons file added))
        (setq not-added (cons file not-added))))
    (list added not-added)))

;;;###autoload
(defun xgit-revert-file (file)
  "Revert uncommitted changes made to FILE in the current branch."
  (interactive "fRevert file: ")
  (xgit-revert-files file))

;;;###autoload
(defun xgit-dvc-revert-files (&rest files)
  "Revert uncommitted changes made to FILES in the current branch."
  (let ((default-directory (xgit-tree-root)))
    (setq files (mapcar #'file-relative-name files))
    (destructuring-bind (added not-added)
        (xgit-split-out-added-files files)
      ;; remove added files from the index
      (when added
        (let ((args (nconc (list "update-index" "--force-remove" "--")
                           added)))
          (dvc-run-dvc-sync 'xgit args
                            :finished #'ignore)))
      ;; revert other files using "git checkout HEAD ..."
      (when not-added
        (let ((args (nconc (list "checkout" "HEAD")
                           not-added)))
          (dvc-run-dvc-sync 'xgit args
                            :finished #'ignore)))
      (if (or added not-added)
          (message "git revert finished")
        (message "Nothing to do")))))

(defcustom xgit-show-filter-filename-func nil
  "Function to filter filenames in xgit-show.
Function is passed a list of files as a parameter.

Function should return list of filenames that is passed to
git-show or nil for all files."
  :type '(choice (const xgit-show-filter-filename-not-quilt)
                 (function)
                 (const :tag "None" nil))
  :group 'dvc-xgit)

(defun xgit-show-filter-filename-not-quilt (files)
  "Function to filter-out quilt managed files under .pc/ and patches/."
  (loop for f in files
        when (not (string-match "\.pc/\\|patches/" f))
        collect f))

(defun xgit-changed-files (dir rev)
  "Returns list of files changed in given revision"
  (let* ((repo (xgit-git-dir-option dir))
         (cmd "diff-tree")
         (args (list repo cmd "--numstat" rev))
         (result (dvc-run-dvc-sync
                  'xgit args
                  :finished 'dvc-output-buffer-split-handler)))
    (mapcar (lambda (x) (nth 2 (split-string x)))
            (cdr result ))))

(defun xgit-show (dir rev &optional files)
  "Shows diff for a given revision.
Optional argument FILES is a string of filename or list of
filenames of to pass to git-show.

If FILES is nil and `xgit-show-filter-filename-func' is non-nil,
files changed in the revision is passed to
`xgit-show-filter-filename-func' and result is used."
  (interactive (list default-directory (read-string "Revision: ")))
  (if (and (null files) xgit-show-filter-filename-func)
      (setq files (funcall xgit-show-filter-filename-func
                           (xgit-changed-files dir rev))))
  (let* ((buffer (dvc-get-buffer-create 'xgit 'diff dir))
         (cmd "show")
         (args (list cmd rev "--")))
    (if files
        (setq args (nconc args (if (stringp files) (list files) files))))
    (dvc-switch-to-buffer-maybe buffer)
    (with-current-buffer buffer
      (dvc-run-dvc-sync 'xgit args
                        :finished
                        (dvc-capturing-lambda (output error status arguments)
                          (progn
                            (with-current-buffer (capture buffer)
                              (let ((inhibit-read-only t))
                                (erase-buffer)
                                (insert-buffer-substring output)
                                (goto-char (point-min))
                                (insert (format "git %s\n\n"
                                                (mapconcat #'identity
                                                           args " ")))
                                (diff-mode)
                                (toggle-read-only 1)))))))))

(defvar xgit-describe-regexp "^\\(.*?\\)-\\([0-9]+\\)-g[[:xdigit:]]\\{7\\}")

(defun xgit-describe-tag? (abbrev)
  (not (string-match xgit-describe-regexp abbrev)))

(defun xgit-describe (dir rev)
  "Show the most recent tag that is reachable from a commit.
If there is no tag return nil,
if revision is a tag, return tag in a string,
else returns list of '(tag offset all-described-string)."
  (interactive (list default-directory (read-string "Revision: ")))
  (let* ((repo (xgit-git-dir-option dir))
         (cmd "describe")
         (args (list repo cmd rev))
         (info (dvc-run-dvc-sync 'xgit args
                                 :finished 'dvc-output-buffer-handler
                                 :error 'dvc-output-buffer-handler)))
    (if (string= "" info)
        nil                             ;no tag yet
      (if (xgit-describe-tag? info)
          info
        (progn
          (list (match-string 1 info)
                (match-string 2 info)
                info))))))

(defun xgit-do-annotate (dir file)
  "Run git annotate for FILE in DIR.
DIR is a directory controlled by Git.
FILE is filename in the repository at DIR."
  (let* ((buffer (dvc-get-buffer-create 'xgit 'annotate))
         (repo (xgit-git-dir-option dir))
         (cmd "blame")
         (fname (file-relative-name file (xgit-tree-root dir)))
         (args (list repo cmd "--" fname)))
    (dvc-switch-to-buffer-maybe buffer)
    (dvc-run-dvc-sync 'xgit args
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (progn
                          (with-current-buffer (capture buffer)
                            (let ((inhibit-read-only t))
                              (buffer-disable-undo)
                              (erase-buffer)
                              (insert-buffer-substring output)
                              (goto-char (point-min))
                              (xgit-annotate-mode))))))))

(defun xgit-annotate ()
  "Run git annotate"
  (interactive)
  (let* ((line (dvc-line-number-at-pos))
         (filename (dvc-confirm-read-file-name "Filename to annotate: "))
         (default-directory (xgit-tree-root filename)))
    (xgit-do-annotate default-directory filename)
    (goto-line line)))

(defun xgit-tag-list ()
  "Run \"git tag\" and list all defined tags"
  (interactive)
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'xgit (list "tag"))
    (dvc-run-dvc-sync 'xgit (list "tag")
                      :finished 'dvc-output-buffer-split-handler)))

(defun xgit-branch-list (&optional all)
  "Run \"git branch\" and list all known branches.
When ALL is given, show all branches, using \"git branch -a\".
When called via lisp, return the list of branches. The currently selected branch is
returned as first entry."
  (interactive "P")
  (if (interactive-p)
      (dvc-run-dvc-display-as-info 'xgit (list "branch" (when all "-a")))
    (let ((branch-list-raw
           (dvc-run-dvc-sync 'xgit (list "branch" (when all "-a"))
                             :finished 'dvc-output-buffer-split-handler))
          (branch-list))
      (dolist (branch-entry branch-list-raw)
        (cond ((string= (substring branch-entry 0 2) "* ")
               (add-to-list 'branch-list (substring branch-entry 2)))
              ((string= (substring branch-entry 0 2) "  ")
               (add-to-list 'branch-list (substring branch-entry 2) t))))
      branch-list)))

(defun xgit-branch (branch-name)
  "Run \"git branch BRANCH-NAME\" to create a new branch."
  (interactive "sCreate new git branch: ")
  (dvc-run-dvc-sync 'xgit (list "branch" branch-name)))

(defun xgit-checkout (branch-name)
  "Run \"git checout BRANCH-NAME\" to checkout an existing branch."
  (interactive (list (dvc-completing-read "Checkout git branch: " (xgit-branch-list t))))
  (dvc-run-dvc-sync 'xgit (list "checkout" branch-name))
  (message "git checkout %s done." branch-name))

;;;###autoload
(defun xgit-apply-patch (file)
  "Run \"git apply\" to apply the contents of FILE as a patch."
  (interactive (list (dvc-confirm-read-file-name
                      "Apply file containing patch: " t)))
  (dvc-run-dvc-sync 'xgit
                    (list "apply" (expand-file-name file))
                    :finished
                    (lambda (output error status arguments)
                      (message "Imported git patch from %s" file))
                    :error
                    (lambda (output error status arguments)
                      (dvc-show-error-buffer error)
                      (error "Error occurred while applying patch(es)"))))

;;;###autoload
(defun xgit-apply-mbox (mbox &optional force)
  "Run \"git am\" to apply the contents of MBOX as one or more patches.
If this command succeeds, it will result in a new commit being added to
the current git repository."
  (interactive (list (dvc-confirm-read-file-name
                      "Apply mbox containing patch(es): " t)))
  (dvc-run-dvc-sync 'xgit
                    (delq nil (list "am" (when force "-3")
                                    (expand-file-name mbox)))
                    :finished
                    (lambda (output error status arguments)
                      (message "Imported git mbox from %s" mbox))
                    :error
                    (lambda (output error status arguments)
                      (dvc-show-error-buffer error)
                      (error "Error occurred while applying patch(es)"))))

;;; DVC revision support

;;;###autoload
(defun xgit-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "xgit-revision-get-last-revision file:%S last-revision:%S"
             file last-revision)
  (let* ((xgit-rev (int-to-string (1- (nth 1 last-revision))))
         (default-directory (car last-revision))
         (fname (file-relative-name file (xgit-tree-root))))
    (insert (dvc-run-dvc-sync
             'xgit (list "cat-file" "blob"
                         (format "HEAD~%s:%s" xgit-rev fname))
             :finished 'dvc-output-buffer-handler-withnewline))))

(defcustom xgit-use-index 'ask
  "Whether xgit should use the index (aka staging area).

\"Use the index\" means commit the content of the index, not the
content of the working tree. In practice, this means commit with
\"git commit\" (without -a), and diff with \"git diff\".

\"Not use the index\" means commit the content of the working tree,
like most version control systems do. In practice, this means commit
with \"git commit -a\", and diff with \"git diff HEAD\".

This option can be set to

 'ask : ask whenever xgit needs the value,
 'always : always use the index,
 'never : never use the index.
"
  :type '(choice (const ask)
                 (const always)
                 (const never))
  :group 'dvc-xgit)

(defun xgit-use-index-p ()
  "Whether xgit should use the index this time.

The value is determined based on `xgit-use-index'."
  (case xgit-use-index
    (always t)
    (never nil)
    (ask (message "Use git index (y/n/a/e/c/?)? ")
         (let ((answer 'undecided))
           (while (eq answer 'undecided)
             (case (progn
                     (let* ((tem (downcase (let ((cursor-in-echo-area t))
                                             (read-char-exclusive)))))
                       (if (= tem help-char)
                           'help
                         (cdr (assoc tem '((?y . yes)
                                           (?n . no)
                                           (?a . always)
                                           (?e . never)
                                           (?c . customize)
                                           (?? . help)))))))
               (yes (setq answer t))
               (no (setq answer nil))
               (always
                (setq xgit-use-index 'always)
                (setq answer t))
               (never
                (setq xgit-use-index 'never)
                (setq answer nil))
               (customize
                (customize-variable 'xgit-use-index)
                (message "Use git index (y/n/a/e/c/?)? "))
               (help (message
                      "\"Use the index\" (aka staging area) means add file content
explicitly before commiting. Concretely, this means run commit
without -a, and run diff without options.

Use git index?
 y (Yes): yes, use the index this time
 n (No) : no, not this time
 a (Always) : always use the index from now
 e (nEver) : never use the index from now
 c (Customize) : customize the option so that you can save it for next
    Emacs sessions. You'll still have to answer the question after.

\(y/n/a/e/c/?)? "))))
           answer))))

(defun xgit-get-root-exclude-file (&optional root)
  "returns exclude file for ROOT"
  (concat (file-name-as-directory (xgit-git-dir root))
	  "info/"
	  "exclude"))

(provide 'xgit)
;;; xgit.el ends here

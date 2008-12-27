;;; baz.el --- baz related code for dvc

;; Copyright (C) 2005-2007  Free Software Foundation, Inc.

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords:

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

;;;###autoload
(progn
  (defvar baz-tla-only-commands '(tla-tag)
    "List of commands available only with tla.")

  (defun baz-make-alias-for-tla-commands ()
    "Creates baz- aliases for tla- commands.

For each commands beginning with \"tla-\", except the ones in
`baz-tla-only-list', create the corresponding \"baz-\" alias.

Most functions in tla*.el are prefixed with tla-, but this allows you to
type M-x baz-whatever RET instead. Some functions are available only
with baz. They're prefixed with baz- and have no alias."
    (interactive)
    (dolist (tla-cmd (apropos-internal "^tla-" 'commandp))
      (unless (member tla-cmd baz-tla-only-commands)
        (let* ((tla-cmd-post (substring (symbol-name tla-cmd) 4))
               (baz-cmd (intern (concat "baz-" tla-cmd-post))))
          (unless (or (fboundp baz-cmd)
                      (string-match "^dvc" tla-cmd-post))
            (defalias baz-cmd tla-cmd))))))

  (baz-make-alias-for-tla-commands)
  ;; baz--name-construct is used in baz-dvc.el
  (eval-after-load "tla"
    '(progn (defalias 'baz--name-construct 'tla--name-construct) (baz-make-alias-for-tla-commands))))

(require 'tla)

;;;###autoload
(defun baz-branch (source-revision tag-version &optional cacherev synchronously)
  "Create a tag from SOURCE-REVISION to TAG-VERSION.
Run baz branch.
If SYNCHRONOUSLY is non-nil, the process for tagging runs synchronously.
Else it runs asynchronously."
  (interactive
   (list (unless (y-or-n-p "Branch from local tree? ")
           (tla--name-construct
            (tla-name-read "Source revision (or version): "
                           'prompt 'prompt 'prompt 'prompt 'maybe)))
         (tla--name-construct
          (tla-name-read "New branch: "
                         'prompt 'prompt 'prompt 'prompt))
         (tla--tag-does-cacherev)
         nil))
  (funcall (if synchronously 'tla--run-tla-sync 'tla--run-tla-async)
           (list "branch"
                 (when (not cacherev) "--no-cacherev")
                 source-revision tag-version)))

;;;###autoload
(defun baz-status-goto (&optional root against)
  "Switch to status buffer or run `baz-dvc-status'."
  (interactive (list (dvc-read-project-tree-maybe
                      (format "Run %s in: "
                              (tla--changes-command)))
                     current-prefix-arg))
  (unless (tla-has-status-command)
    (error "status not available with this arch branch"))
  (let* ((default-directory root)
         (buffer (dvc-get-buffer 'status default-directory)))
    (if buffer
        (dvc-switch-to-buffer buffer)
      (baz-dvc-status))))

(defun baz-dvc-status ()
  "Run \"baz status\" in `default-directory', which must be a tree root.

Doesn't work with tla. Use `tla-changes' or `tla-tree-lint'
instead."
  (unless (tla-has-status-command)
    (error "status not available with this arch branch"))
  (let* ((root default-directory)
         (buffer (dvc-prepare-changes-buffer
                  (list 'last-revision root)
                  (list 'local-tree root)
                  'status
                  default-directory 'baz)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (ewoc-enter-first
         dvc-fileinfo-ewoc
         (make-dvc-fileinfo-message
          :text (concat "* Running baz status in tree " root
                        "...\n\n")))
        (ewoc-enter-last dvc-fileinfo-ewoc
                         (make-dvc-fileinfo-legacy :data (list 'searching-subtrees)))
        (ewoc-refresh dvc-fileinfo-ewoc)))
    (dvc-save-some-buffers)
    (baz--status-internal root buffer nil)
    (tla--run-tla-async
     '("inventory" "--nested" "--trees")
     :related-buffer buffer
     :finished
     (lexical-let ((buffer-lex buffer))
       (lambda (output error status arguments)
         (let ((subtrees (delete ""
                                 (split-string
                                  (with-current-buffer
                                      output (buffer-string)) "\n"))))
           (with-current-buffer buffer-lex
             (let ((subtree-message (car (tla--changes-find-subtree-message))))
               (dolist (subtree subtrees)
                 (let ((buffer-sub (dvc-get-buffer-create
                                    'status subtree)))
                   (with-current-buffer buffer-sub
                     (let ((inhibit-read-only t))
                       (erase-buffer))
                     (dvc-diff-mode)
                     (set (make-local-variable
                           'tla--changes-buffer-master-buffer)
                          buffer-lex))
                   (ewoc-enter-after dvc-fileinfo-ewoc
                                     subtree-message
                                     (make-dvc-fileinfo-legacy
                                      :data (list 'subtree buffer-sub subtree
                                                  nil)))
                   (baz--status-internal
                    subtree
                    buffer-sub
                    buffer-lex)))
               (dvc-ewoc-delete dvc-fileinfo-ewoc subtree-message))
             (recenter))))))))


(defun baz--status-error-handle (output error status arguments root
                                        buffer master-buffer)
  "Handler for error in \"baz status\"."
  (if (with-current-buffer error
        (goto-char (point-min))
        (looking-at "^Tree is not lint clean"))
      (let ((buffer (tla--tree-lint-prepare-buffer
                     root
                     (lexical-let ((root-lex root) (buffer-lex buffer) (master-buffer-lex
                                                                        master-buffer))
                       (lambda ()
                         (baz--status-internal root-lex buffer-lex
                                               master-buffer-lex)
                         (switch-to-buffer buffer-lex))))))
        (message "Tree is not lint clean")
        (save-excursion
          (tla--tree-lint-parse-buffer output buffer))
        (with-current-buffer buffer
          (tla--tree-lint-cursor-goto
           (ewoc-nth tla--tree-lint-cookie 0)))
        (switch-to-buffer buffer))
    (dvc-show-changes-buffer output 'tla--parse-baz-status buffer
                             master-buffer "^[^*\\.]")
    (with-current-buffer buffer
      (setq dvc-buffer-refresh-function 'baz-dvc-status))
    (when master-buffer
      (with-current-buffer master-buffer
        (ewoc-map (lambda (fi)
                    (let ((x (dvc-fileinfo-legacy-data fi)))
                      (when (and (eq (car x) 'subtree)
                                 (eq (cadr x) buffer))
                        (setcar (cdddr x) 'changes)))
                    )
                  dvc-fileinfo-ewoc)))))

(defun baz--status-internal (root buffer master-buffer)
  "Internal function to run \"baz status\".

Run the command in directory ROOT.
The output will be displayed in buffer BUFFER.

BUFFER must already be in changes mode, but mustn't contain any change
information. Only roots of subprojects are already in the ewoc.

If MASTER-BUFFER is non-nil, this run of tla changes is done in a
nested project of a bigger one. MASTER-BUFFER is the buffer in which
the root of the projects is displayed."
  (with-current-buffer buffer
    (tla--run-tla-async
     `("status")
     :finished
     (lexical-let ((root-lex root) (buffer-lex buffer) (master-buffer-lex
                                                        master-buffer)
                   (-current-buffer--lex (current-buffer)))
       (lambda (output error status arguments)
         (if (with-current-buffer output
               (goto-char (point-min))
               (re-search-forward
                tla--files-conflicted-regexp nil t))
             (baz--status-error-handle
              output error status arguments root-lex buffer-lex
              master-buffer-lex)
           (if master-buffer-lex
               (message "No changes in subtree %s" root-lex)
             (message "No changes in %s" root-lex))
           (with-current-buffer -current-buffer--lex
             (let ((inhibit-read-only t))
               (dvc-fileinfo-delete-messages)
               (ewoc-enter-last
                dvc-fileinfo-ewoc
                (make-dvc-fileinfo-message
                 :text (concat "* No changes in "
                               root-lex ".\n\n")))
               (when master-buffer-lex
                 (with-current-buffer master-buffer-lex
                   (ewoc-map (lambda (fi)
                               (let ((x (dvc-fileinfo-legacy-data fi)))
                                 (when (and (eq (car x) 'subtree)
                                            (eq (cadr x) buffer-lex))
                                   (setcar (cdddr x) 'no-changes)))
                               )
                             dvc-fileinfo-ewoc)))
               (ewoc-refresh dvc-fileinfo-ewoc))))))
     :error
     (lexical-let ((root-lex root) (buffer-lex buffer) (master-buffer-lex
                                                        master-buffer))
       (lambda (output error status arguments)
         (baz--status-error-handle
          output error status arguments root-lex buffer-lex master-buffer-lex)))
     )))

;;;###autoload
(defalias 'baz-merge 'tla-star-merge)

;;;###autoload
(defun baz-annotate (file)
  "Run \"baz annotate\" on FILE.

Shows the result in a buffer, and create an annotation table for the
annotated file's buffer. This allows you to run `baz-trace-line' and
`baz-trace-line-show-log'."
  (interactive (list (read-file-name "Annotate file: "
                                     nil nil t
                                     (file-name-nondirectory
                                      (or (buffer-file-name) "")))))
  (let ((file (expand-file-name file))
        (buffer (get-file-buffer file)))
    (with-current-buffer buffer
      (when (or (not (buffer-modified-p))
                (y-or-n-p (concat "Save buffer "
                                  (buffer-name buffer)
                                  "? ")))
        (save-buffer buffer))
      (find-file-noselect file)
      (let* ((default-directory (tla-tree-root file))
             (buffer (dvc-get-buffer-create tla-arch-branch 'annotate)))
        (when dvc-switch-to-buffer-first
          (dvc-switch-to-buffer buffer))
        (tla--run-tla-async
         `("annotate"
           ,(tla-file-name-relative-to-root file))
         :finished (lexical-let ((buffer-lex buffer) (file-lex file))
                     (lambda (output error status arguments)
                       (with-current-buffer buffer-lex
                         (erase-buffer)
                         (insert-buffer-substring output))
                       (tla-annotate-mode)
                       (baz-parse-annotate
                        output
                        (find-buffer-visiting file-lex))))
         :error
         (lambda (output error status arguments)
           (dvc-show-error-buffer error)
           (dvc-show-last-process-buffer)))))))

(defvar tla-annotation-table nil
  "table line-number -> revision built by `baz-parse-annotate'.")

(defun baz-parse-annotate (annotate-buffer buffer)
  "Builds a table line-number -> revision from ANNOTATE-BUFFER.

ANNOTATE-BUFFER must be the output of \"baz annotate\", and BUFFER is
the corresponding source buffer."
  (set-buffer annotate-buffer)
  (goto-char (point-min))
  (re-search-forward "^[^ ]*:")
  (beginning-of-line)
  (let* ((nb-lines (1+ (count-lines (point)
                                    (point-max))))
         (table (make-vector nb-lines ""))
         (n 0))
    (while (looking-at "^\\([^ ]*\\):")
      (aset table n (match-string 1))
      (setq n (1+ n))
      (forward-line 1))
    (with-current-buffer buffer
      (set (make-local-variable 'tla-annotation-table)
           table))
    ))

(defun baz-trace-line (line buffer)
  "Returns the changeset that lead to LINE in FILE."
  (interactive (list (count-lines (point-min) (point))
                     (current-buffer)))
  (unless tla-annotation-table
    (error "No annotate table in buffer. Run baz-annotate first."))
  (with-current-buffer buffer
    (let ((changeset (aref tla-annotation-table line)))
      (when (interactive-p)
        (message changeset))
      changeset)))

(defun baz-trace-line-show-log (line buffer)
  "Show the log of the changeset that lead to LINE in FILE."
  (interactive (list (count-lines (point-min) (point))
                     (current-buffer)))
  (tla-cat-log (baz-trace-line line buffer)))

(provide 'baz)

;;; baz.el ends here

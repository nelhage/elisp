;;; xgit-revision.el --- Management of revision lists for git

;; Copyright (C) 2006-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Keywords:

;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
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


(eval-when-compile (require 'cl))

(defstruct (xgit-revision-st)
  commit
  tree
  parent
  author
  committer
  date
  message)

;; cg dvc revision list

(defun xgit-revision-list-entry-patch-printer (elem)
  (insert (if (dvc-revlist-entry-patch-marked elem)
              (concat " " dvc-mark " ") "   "))
  (let ((struct (dvc-revlist-entry-patch-struct elem)))
    (insert (dvc-face-add "commit:    " 'dvc-header)
            (dvc-face-add (xgit-revision-st-commit struct) 'dvc-revision-name)
            "\n")
    (when (xgit-revision-st-tree struct)
      (insert "   " (dvc-face-add "tree:      " 'dvc-header)
              (dvc-face-add (xgit-revision-st-tree struct) 'dvc-revision-name)
              "\n"))
    (when (xgit-revision-st-parent struct)
      (insert "   " (dvc-face-add "parent:    " 'dvc-header)
              (dvc-face-add (xgit-revision-st-parent struct) 'dvc-revision-name)
              "\n"))
    (when dvc-revisions-shows-creator
      (insert "   " (dvc-face-add "author:    " 'dvc-header)
              (or (xgit-revision-st-author struct) "?") "\n")
      (insert "   " (dvc-face-add "committer: " 'dvc-header)
              (or (xgit-revision-st-committer struct) "?") "\n"))
    (when dvc-revisions-shows-date
      (insert "   " (dvc-face-add "timestamp: " 'dvc-header)
              (or (xgit-revision-st-date struct) "?") "\n"))
    (when dvc-revisions-shows-summary
      (insert "   " (dvc-face-add "summary:   " 'dvc-header)
              (or (xgit-revision-st-message struct) "?") "\n"))))

;;; cg dvc log

(defun xgit-dvc-log-parse (log-buffer)
  (goto-char (point-min))
  (let ((root (xgit-tree-root))
        (elem (make-xgit-revision-st))
        (field)
        (field-value))
    (while (> (point-max) (point))
      (beginning-of-line)
      (when (looking-at "^\\([a-z]+\\) +\\(.+\\)$")
        (setq field (match-string-no-properties 1))
        (setq field-value (match-string-no-properties 2))
        ;; (dvc-trace "field: %s, field-value: %s" field field-value)
        (cond ((string= field "commit")
               (setf (xgit-revision-st-commit elem) field-value))
              ((string= field "tree")
               (setf (xgit-revision-st-tree elem) field-value))
              ((string= field "parent")
               (setf (xgit-revision-st-parent elem) field-value))
              ((string= field "author")
               (setf (xgit-revision-st-author elem) field-value))
              ((string= field "committer")
               (setf (xgit-revision-st-committer elem) field-value))
              (t (dvc-trace "xgit-dvc-log-parse: unmanaged field %S" field)))
        (forward-line 1))
      (when (looking-at "^$")
        ;; (dvc-trace "empty line")
        (unless (re-search-forward "^commit" nil t)
          (goto-char (point-max)))
        (with-current-buffer log-buffer
          (ewoc-enter-last
           dvc-revlist-cookie
           `(entry-patch
             ,(make-dvc-revlist-entry-patch
               :dvc 'xgit
               :struct elem
               :rev-id `(xgit (revision
                               (local ,root ,
                                      (xgit-revision-st-commit elem))))))))
        (setq elem (make-xgit-revision-st)))))
  (with-current-buffer log-buffer
    (goto-char (point-min))))


(provide 'xgit-revision)
;;; xgit-revision.el ends here

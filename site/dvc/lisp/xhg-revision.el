;;; xhg-revision.el --- Management of revision lists in xhg

;; Copyright (C) 2006, 2007 by all contributors

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

(require 'dvc-revlist)

(eval-when-compile (require 'cl))

(defstruct (xhg-revision-st)
  changeset
  message
  creator
  tag
  date)

;; xhg dvc revision list

(defun xhg-revision-list-entry-patch-printer (elem)
  (insert (if (dvc-revlist-entry-patch-marked elem)
              (concat " " dvc-mark " ") "   "))
  (let ((struct (dvc-revlist-entry-patch-struct elem)))
    (insert (dvc-face-add "changeset: " 'dvc-header)
            (dvc-face-add (xhg-revision-st-changeset struct) 'dvc-revision-name)
            "\n")
    (when dvc-revisions-shows-creator
      (insert "   " (dvc-face-add "user:      " 'dvc-header)
              (or (xhg-revision-st-creator struct) "?") "\n"))
    (when dvc-revisions-shows-date
      (insert "   " (dvc-face-add "timestamp: " 'dvc-header)
              (or (xhg-revision-st-date struct) "?") "\n"))
    (when (xhg-revision-st-tag struct)
      (insert "   " (dvc-face-add "tag:       " 'dvc-header)
              (xhg-revision-st-tag struct) "\n"))
    (when dvc-revisions-shows-summary
      (insert "   " (dvc-face-add "summary:   " 'dvc-header)
              (or (xhg-revision-st-message struct) "?") "\n"))))

;;; xhg dvc log

(defun xhg-dvc-log-parse (log-buffer location)
  (goto-char (point-min))
  (let ((root location)
        (elem (make-xhg-revision-st))
        (field)
        (field-value))
    (while (> (point-max) (point))
      (beginning-of-line)
      (when (looking-at "^\\([a-z][a-z ]*[a-z]\\): +\\(.+\\)$")
        (setq field (match-string-no-properties 1))
        (setq field-value (match-string-no-properties 2))
        ;; (dvc-trace "field: %s, field-value: %s" field field-value)
        (cond ((string= field "changeset")
               (setf (xhg-revision-st-changeset elem) field-value))
              ((string= field "user")
               (setf (xhg-revision-st-creator elem) field-value))
              ((string= field "tag")
               (setf (xhg-revision-st-tag elem) field-value))
              ((string= field "date")
               (setf (xhg-revision-st-date elem) field-value))
              ((string= field "summary")
               (setf (xhg-revision-st-message elem) field-value))
              (t (dvc-trace "xhg-dvc-log-parse: unmanaged field %S" field)))
        (forward-line 1))
      (when (looking-at "^$")
        ;; (dvc-trace "empty line")
        (with-current-buffer log-buffer
          (ewoc-enter-last
           dvc-revlist-cookie
           `(entry-patch
             ,(make-dvc-revlist-entry-patch
               :dvc 'xhg
               :struct elem
               :rev-id `(xhg (revision (local ,root ,(xhg-revision-st-changeset elem))))))))
        (setq elem (make-xhg-revision-st))
        (forward-line 1))))
  (with-current-buffer log-buffer
    (goto-char (point-min))))

;;;###autoload
(defun xhg-dvc-log (path last-n)
  "Show a dvc formatted log for xhg."
  (interactive (list default-directory nil))
  (dvc-build-revision-list 'xhg 'log (xhg-tree-root (or path default-directory)) '("log") 'xhg-dvc-log-parse
                           t last-n path
                           (dvc-capturing-lambda ()
                             (xhg-dvc-log (capture path) (capture last-n)))))

(defun xhg-revlog-get-revision (rev-id)
  (let ((rev (car (dvc-revision-get-data rev-id))))
    (case (car rev)
      (local
       (dvc-run-dvc-sync 'xhg `("log" "-r" ,(nth 2 rev))
                         :finished 'dvc-output-buffer-handler))
      (t (error "Not implemented (rev=%s)" rev)))))

(defun xhg-name-construct (rev-id)
  (case (car rev-id)
    (local (nth 1 rev-id))
    (t (error "Not implemented (rev-id=%s)" rev-id))))

(provide 'xhg-revision)
;;; xhg-revision.el ends here

;;; tla-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2006 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

(require 'tla-core)
(require 'dvc-gnus)

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

;; Integration into gnus

(autoload 'tla-categories-string "tla")
(autoload 'tla-branches-string "tla")
(autoload 'tla-versions-string "tla")
(autoload 'tla-revisions-string "tla")
(autoload 'tla--button-revision-fn "tla")

(defun tla-gnus-setup-buttons ()
  "Make archive@host.com/something clickable in Gnus Article buffer."
  (interactive)
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 0 t t) 1 t
                 tla-categories-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 1 t t) 1 t
                 tla-branches-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 2 t t) 1 t
                 tla-versions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 3 t t) 1 t
                 tla-revisions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 4 t t) 1 t
                 tla--button-revision-fn 1)))

;;;###autoload
(defun tla-insinuate-gnus ()
  "Integrate the tla backend of DVC into Gnus.
Add the `tla-submit-patch-done' function to the
`message-sent-hook'.

The archives/categories/branches/version/revision names are buttonized
in the *Article* buffers."
  (interactive)
  (add-hook 'message-sent-hook 'tla-submit-patch-done)
  (tla-gnus-setup-buttons))

(defun tla-gnus-article-view-patch (n)
  "View MIME part N in a gnus article, as a tla changeset.
The patch can be embedded or external.  If external, the
parameter N is ignored."
  (interactive)
  (gnus-summary-select-article-buffer)
  (if (> (gnus-article-mime-total-parts) 1)
      (tla-gnus-article-view-attached-patch 2)
    (tla-gnus-article-view-external-patch)))

(defun tla-gnus-article-view-attached-patch (n)
  "View MIME part N, as tla patchset."
  (interactive "p")
  (gnus-article-part-wrapper n 'tla-gnus-view-patch))

(defun tla-gnus-article-view-external-patch ()
  "View an external patch that is referenced in this mail.

The mail must either contain a line starting with 'Committed ' and ending
with the fully qualified revision name.

The second supported format contains an extra line for Revision and Archive."
  (interactive)
  (let ((revision)
        (archive)
        (version)
        (window-conf (current-window-configuration)))
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (goto-char (point-min))
    (cond ((re-search-forward (concat "Committed " (tla-make-name-regexp 4 nil t)) nil t)
           (setq version (buffer-substring-no-properties
                          (+ (match-beginning 0) 10) (- (match-end 0) 1))))
          (t
           (when (search-forward "Revision: " nil t)
             (setq revision (buffer-substring-no-properties (point) (line-end-position))))
           (when (search-forward "Archive: " nil t)
             (setq archive (buffer-substring-no-properties (point) (line-end-position))))
           (when (and archive revision)
             (setq version (concat archive "/" revision)))))
    (gnus-article-show-summary)
    (if version
        (progn
          (tla-get-changeset version t)
          (save-excursion
            (set-buffer (dvc-get-buffer tla-arch-branch 'changeset version))
            (dvc-buffer-push-previous-window-config window-conf)))
      (message "No external arch patch found in this article.")
      (set-window-configuration window-conf))))


(defun tla-gnus-view-patch (handle)
  "View a patch within gnus.  HANDLE should be the handle of the part."
  (let ((archive-name (dvc-make-temp-name "gnus-patch-tgz"))
        (window-conf (current-window-configuration)))
    (mm-save-part-to-file handle archive-name)
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (tla-show-changeset-from-tgz archive-name)
    (dvc-buffer-push-previous-window-config window-conf)
    (delete-file archive-name)))

(defun tla-gnus-article-apply-patch (n)
  "Apply MIME part N, as tla patchset.
When called with no prefix arg, set N := 2."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (gnus-article-part-wrapper n 'tla-gnus-apply-patch))

(defun tla-gnus-apply-patch (handle)
  "Apply the patch corresponding to HANDLE."
  (dvc-gnus-article-extract-log-message)
  (let ((archive-name (dvc-make-temp-name "gnus-patch-tgz"))
        (tree-dir (tla--name-match-from-list
                   (when dvc-memorized-version
                     (tla--name-split dvc-memorized-version))
                   tla-apply-patch-mapping))
        (tree)
        (window-conf (current-window-configuration)))
    (mm-save-part-to-file handle archive-name)
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (tla-show-changeset-from-tgz archive-name)
    (dvc-buffer-push-previous-window-config window-conf)
    (setq tree (dvc-read-directory-name "Apply to tree: "
                                        tree-dir tree-dir))
    (tla-apply-changeset-from-tgz archive-name tree nil)
    (delete-file archive-name)
    (when (eq major-mode 'tla-inventory-mode)
      (delete-other-windows))))

(provide 'tla-gnus)
;;; tla-gnus.el ends here

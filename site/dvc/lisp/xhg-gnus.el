;;; xhg-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2007 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

;;;###autoload
(defun xhg-insinuate-gnus ()
  "Integrate Xhg into Gnus.
The following keybindings are installed for gnus-summary:
K t s `xhg-gnus-article-view-status-for-import-patch'"
  (interactive)
  (dvc-gnus-initialize-keymap)
  (define-key gnus-summary-dvc-submap [?s] 'xhg-gnus-article-view-status-for-import-patch)
  )

(defvar xhg-apply-patch-mapping nil)
;;(add-to-list 'xhg-apply-patch-mapping '("my-wiki" "~/work/wiki/"))

(defvar xhg-gnus-patch-from-user nil)

(defvar xhg-gnus-import-patch-force nil)
(defun xhg-gnus-article-import-patch (n)
  "Import MIME part N, as hg patch.
When N is negative, force applying the patch, even if there are
outstanding uncommitted changes."
  (interactive "p")
  (if (and (numberp n) (< n 0))
      (progn
        (setq xhg-gnus-import-patch-force t)
        (setq n (- n)))
    (setq xhg-gnus-import-patch-force nil))
  (gnus-article-part-wrapper n 'xhg-gnus-import-patch))

(defun xhg-gnus-import-patch (handle)
  "Import a hg patch via gnus.  HANDLE should be the handle of the part."
  (let ((patch-file-name (concat (dvc-make-temp-name "gnus-xhg-import-") ".patch"))
        (window-conf (current-window-configuration))
        (import-dir))
    (gnus-summary-select-article-buffer)
    (save-excursion
      (goto-char (point-min))
      ;; handle does not seem to exist for text/x-patch ...
      (when (re-search-forward "^user: +\\(.+\\)$" nil t)
        (setq xhg-gnus-patch-from-user (match-string-no-properties 1))))
    (save-excursion
      (goto-char (point-min))
      ;; handle does not seem to exist for text/x-patch ...
      (search-forward "text/x-patch; ")
      (mm-save-part-to-file (get-text-property (point) 'gnus-data) patch-file-name)
      (dolist (m xhg-apply-patch-mapping)
        (when (looking-at (car m))
          (setq import-dir (dvc-uniquify-file-name (cadr m))))))
    (delete-other-windows)
    (dvc-buffer-push-previous-window-config)
    (find-file patch-file-name)
    (setq import-dir (dvc-read-directory-name "Import hg patch to: " nil nil t import-dir))
    (when import-dir
      (let ((default-directory import-dir)
            (current-dvc))
        (setq current-dvc (dvc-current-active-dvc))
        (case current-dvc
          ('xhg (xhg-import patch-file-name xhg-gnus-import-patch-force))
          ('xgit (xgit-apply-patch patch-file-name))
          ;; TODO Add here new backend
          (t (error "Unknow backend")))))
    (delete-file patch-file-name)
    (kill-buffer (current-buffer)) ;; the patch file
    (set-window-configuration window-conf)
    (let ((default-directory import-dir)
          (current-dvc))
      (setq current-dvc (dvc-current-active-dvc))
      (case current-dvc
        ;; TODO Add here new backend
        ('xhg (when (and import-dir (y-or-n-p "Run hg log in patched directory? "))
                (xhg-log "tip" "-10")
                (delete-other-windows)))
        ('xgit (when (and import-dir (y-or-n-p "Run xgit-status?"))
                 (xgit-status)))))))


(defvar xhg-gnus-status-window-configuration nil)
(defun xhg-gnus-article-view-status-for-import-patch (n)
  "View the status for the repository, where MIME part N would be applied as hg patch.

Use the same logic as in `xhg-gnus-article-import-patch' to guess the repository path
via `xhg-apply-patch-mapping'."
  (interactive "p")
  (gnus-article-part-wrapper n 'xhg-gnus-view-status-for-import-patch)
  (set-window-configuration xhg-gnus-status-window-configuration))

(defun xhg-gnus-view-status-for-import-patch (handle)
  "View the status for a repository before applying a hg patch via gnus.
HANDLE should be the handle of the part."
  (let ((window-conf (current-window-configuration))
        (import-dir))
    (gnus-summary-select-article-buffer)
    (save-excursion
      (goto-char (point-min))
      ;; handle does not seem to exist for text/x-patch ...
      (search-forward "text/x-patch; ")
      (dolist (m xhg-apply-patch-mapping)
        (when (looking-at (car m))
          (setq import-dir (dvc-uniquify-file-name (cadr m))))))
    (unless import-dir ;; when we find the directory in xhg-apply-patch-mapping don't ask for confirmation
      (setq import-dir (dvc-read-directory-name "View hg repository status for: " nil nil t import-dir)))
    (let ((default-directory import-dir))
      (xhg-dvc-status)
      (delete-other-windows)
      (setq xhg-gnus-status-window-configuration (current-window-configuration))
      (dvc-buffer-push-previous-window-config window-conf))))

(provide 'xhg-gnus)
;;; xhg-gnus.el ends here

;;; xgit-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2007 by all contributors

;; Author: Michael Olson <mwolson@gnu.org>,
;;         Stefan Reichoer <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

;;;###autoload
(defun xgit-insinuate-gnus ()
  "Integrate Xgit into Gnus."
  (interactive)
  ;; bindings are set up by dvc-insinuate-gnus
  )

;;; Applying patches from email messages

(defcustom xgit-apply-patch-mapping nil
  "*Working directories in which patches should be applied.

An alist of rules to map a regexp matching an email address to a
working directory.

This is used by the `xgit-gnus-apply-patch' function.
Example setting: '((\".*erc-discuss@gnu.org\" \"~/proj/emacs/erc/master\"))"
  :type '(repeat (list :tag "Rule"
                       (string :tag "Email address regexp")
                       (string :tag "Working directory")))
  :group 'dvc-xgit)

(defvar xgit-gnus-patch-from-user nil)

(defun xgit-gnus-article-apply-patch (n)
  "Apply the current article as a git patch.
N is the mime part given to us by DVC.

If N is negative, then force applying of the patch by doing a
3-way merge.

We ignore the use of N as a mime part, since git can extract
patches from the entire message."
  (interactive "p")
  (let ((force nil))
    (when (and (numberp n) (< n 0))
      (setq force t))
    (xgit-gnus-apply-patch force)))

(defun xgit-gnus-apply-patch (force)
  "Apply a git patch via gnus.  HANDLE should be the handle of the part."
  (let ((patch-file-name (concat (dvc-make-temp-name "gnus-xgit-apply-")
                                 ".patch"))
        (window-conf (current-window-configuration))
        (err-occurred nil)
        (trigger-commit nil)
        working-dir patch-buffer)
    (gnus-summary-show-article 'raw)
    (gnus-summary-select-article-buffer)
    (save-excursion
      (let ((require-final-newline nil)
            (coding-system-for-write mm-text-coding-system))
        (gnus-write-buffer patch-file-name))
      (goto-char (point-min))
      (re-search-forward "^To: " nil t)
      (catch 'found
        (dolist (m xgit-apply-patch-mapping)
          (when (looking-at (car m))
            (setq working-dir (dvc-uniquify-file-name (cadr m)))
            (throw 'found t)))))
    (gnus-summary-show-article)
    (delete-other-windows)
    (dvc-buffer-push-previous-window-config)
    (find-file patch-file-name)
    (setq patch-buffer (current-buffer))
    (setq working-dir (dvc-read-directory-name "Apply git patch to: "
                                               nil nil t working-dir))
    (when working-dir
      (setq working-dir (file-name-as-directory working-dir)))
    (unwind-protect
        (progn
          (when working-dir
            (let ((default-directory working-dir))
              (if (or (xgit-lookup-external-git-dir)
                      (file-exists-p ".git/"))
                  ;; apply the patch and commit if it applies cleanly
                  (xgit-apply-mbox patch-file-name force)
                ;; just apply the patch, since we might not be in a
                ;; git repo
                (xgit-apply-patch patch-file-name)
                (setq trigger-commit t))))
          (set-window-configuration window-conf)
          (when working-dir
            (if trigger-commit
                (xgit-gnus-stage-patch-for-commit working-dir patch-buffer)
              (when (y-or-n-p "Run git log in working directory? ")
                (xgit-log working-dir nil)
                (delete-other-windows)))))
      ;; clean up temporary file
      (delete-file patch-file-name)
      (kill-buffer patch-buffer))))

(defun xgit-gnus-stage-patch-for-commit (working-dir patch-buffer)
  "Switch to directory WORKING-DIR and set up a commit based on the patch
contained in PATCH-BUFFER."
  (let ((default-directory working-dir))
    (destructuring-bind (subject body)
        (with-current-buffer patch-buffer
          (let (subject body)
            (goto-char (point-min))
            (when (re-search-forward "^Subject: *\\(.+\\)$" nil t)
              (setq subject (match-string 1)))
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (forward-line 1)
              (let ((beg (point)))
                (when (re-search-forward "^---$" nil t)
                  (setq body (buffer-substring beg (match-beginning 0))))))
            (list subject body)))
      ;; strip "[COMMIT]" prefix
      (when (and subject
                 (string-match "\\`\\[[^]]+\\] *" subject))
        (setq subject (substring subject (match-end 0))))
      (message "Staging patch for commit ...")
      (dvc-diff)
      (dvc-log-edit)
      (erase-buffer)
      (insert subject "\n\n" body))))

(defvar xgit-gnus-status-window-configuration nil)
(defun xgit-gnus-article-view-status-for-apply-patch (n)
  "View the status for the repository, where MIME part N would be applied
as a git patch.

Use the same logic as in `xgit-gnus-article-apply-patch' to
guess the repository path via `xgit-apply-patch-mapping'."
  (interactive "p")
  (xgit-gnus-view-status-for-apply-patch)
  (set-window-configuration xgit-gnus-status-window-configuration))

(defun xgit-gnus-view-status-for-apply-patch ()
  "View the status for a repository before applying a git patch via gnus."
  (let ((window-conf (current-window-configuration))
        (working-dir))
    (gnus-summary-select-article-buffer)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^To: " nil t)
      (dolist (m xgit-apply-patch-mapping)
        (when (looking-at (car m))
          (setq working-dir (dvc-uniquify-file-name (cadr m))))))
    (unless working-dir
      ;; when we find the directory in xgit-apply-patch-mapping don't
      ;; ask for confirmation
      (setq working-dir (dvc-read-directory-name
                         "View git repository status for: "
                         nil nil t working-dir)))
    (when working-dir
      (setq working-dir (file-name-as-directory working-dir)))
    (let ((default-directory working-dir))
      (xgit-dvc-status)
      (delete-other-windows)
      (setq xgit-gnus-status-window-configuration
            (current-window-configuration))
      (dvc-buffer-push-previous-window-config window-conf))))

(defun xgit-gnus-article-view-patch (n)
  "View the currently looked-at patch.

All this does is switch to the article and move to where the
patch begins."
  (interactive "p")
  (gnus-summary-select-article-buffer)
  (goto-char (point-min))
  (re-search-forward "^---$" nil t)
  (forward-line 1))

;;; Sending commit notifications

(defcustom xgit-mail-notification-destination nil
  "An alist of rules which map working directories to both target
email addresses and the prefix string for the subject line.

This is used by the `xgit-send-commit-notification' function."
  :type '(repeat (list :tag "Rule"
                       (string :tag "Working directory")
                       (string :tag "Email subject prefix")
                       (string :tag "Email address")
                       (string :tag "Repo location (optional)")))
  :group 'dvc-xgit)

(defcustom xgit-mail-notification-sign-off-p nil
  "If non-nil, add a Signed-Off-By header to any mail commit notifications."
  :type 'boolean
  :group 'dvc-xgit)

(defun xgit-gnus-send-commit-notification (&optional to)
  "Send a commit notification email for the changelog entry at point.

The option `xgit-mail-notification-destination' can be used to
specify a prefix for the subject line, the destination email
address, and an optional repo location.  The rest of the subject
line contains the summary line of the commit.

If the optional argument TO is provided, send an email to that
address instead of consulting
`xgit-mail-notification-destination'.  If the prefix
argument (C-u) is given, then prompt for this value."
  (interactive (list current-prefix-arg))
  (let (dest-specs)
    (when (equal to '(4))
      (setq to (read-string "Destination email address: ")))
    (if to
        (setq dest-specs (list nil to nil))
      (catch 'found
        (dolist (m xgit-mail-notification-destination)
          (when (string= default-directory (file-name-as-directory (car m)))
            (setq dest-specs (cdr m))
            (throw 'found t)))))
    (let* ((rev (dvc-revlist-get-revision-at-point))
           (repo-location (nth 2 dest-specs)))
      (destructuring-bind (from subject body)
          (dvc-run-dvc-sync
           'xgit (delq nil (list "format-patch" "--stdout" "-k" "-1"
                                 (when xgit-mail-notification-sign-off-p "-s")
                                 rev))
           :finished
           (lambda (output error status args)
             (with-current-buffer output
               (let (from subject body)
                 (goto-char (point-min))
                 (when (re-search-forward "^From: *\\(.+\\)$" nil t)
                   (setq from (match-string 1)))
                 (goto-char (point-min))
                 (when (re-search-forward "^Subject: *\\(.+\\)$" nil t)
                   (setq subject (match-string 1)))
                 (goto-char (point-min))
                 (when (re-search-forward "^$" nil t)
                   (forward-line 1)
                   (setq body (buffer-substring (point) (point-max))))
                 (list from subject body)))))
        (message "Preparing commit email for revision %s" rev)
        (let ((gnus-newsgroup-name nil))
          (compose-mail (if dest-specs (cadr dest-specs) "")
                        (concat (if dest-specs (car dest-specs) "")
                                subject)))
        (when from
          (dvc-message-replace-header "From" from))
        (message-goto-body)
        ;; do not PGP sign the message as per git convention
        (when (looking-at "<#part[^>]*>")
          (let ((beg (point)))
            (forward-line 1)
            (delete-region beg (point))))
        (save-excursion
          (when body
            (insert body))
          (when repo-location
            (message-goto-body)
            (when (re-search-forward "^---$" nil t)
              (insert "\nCommitted revision " rev "\n"
                      "to <" repo-location ">.\n")))
          (goto-char (point-max))
          (unless (and (bolp) (looking-at "^$"))
            (insert "\n"))
          (message-goto-body))))))

(provide 'xgit-gnus)
;;; xgit-gnus.el ends here

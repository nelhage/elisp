;;; xdarcs.el --- darcs interface for dvc

;; Copyright (C) 2006, 2007, 2008 by all contributors

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

;; The darcs interface for dvc

;;; History:

;;

;;; Code:

(require 'dvc-core)
(require 'dvc-utils)
(require 'xdarcs-core)

(defun xdarcs-initialize (&optional dir)
  "Run darcs initialize."
  (interactive
   (list (expand-file-name (dvc-read-directory-name "Directory for darcs initialize: "
                                                    (or default-directory
                                                        (getenv "HOME"))))))
  (let ((default-directory dir))
    (dvc-run-dvc-sync 'xdarcs (list "initialize")
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "darcs initialize finished")))))

;;;###autoload
(defun xdarcs-dvc-add-files (&rest files)
  "Run darcs add."
  (dvc-trace "xdarcs-add-files: %s" files)
  (dvc-run-dvc-sync 'xdarcs (append '("add") files)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "darcs add finished"))))

(defun xdarcs-command-version ()
  "Run darcs --version."
  (interactive)
  (let ((version (dvc-run-dvc-sync 'xdarcs '("--version")
                                   :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "darcs version: %s" version))
    version))

;; --------------------------------------------------------------------------------
;; whatsnew
;; --------------------------------------------------------------------------------
;;
;; (defun xdarcs-whatsnew ()
;;   "Run darcs whatsnew.
;; When called with a prefix argument, specify the --look-for-adds parameter."
;;   (interactive)
;;   (let ((param-list '("whatsnew")))
;;     (when current-prefix-arg
;;       (add-to-list 'param-list "--look-for-adds" t))
;;     (dvc-run-dvc-display-as-info 'xdarcs param-list)))
(defun xdarcs-parse-whatsnew  (changes-buffer)
  (dvc-trace "xdarcs-parse-whatsnew (dolist)")
  (let ((status-list
         (split-string (dvc-buffer-content (current-buffer)) "\n")))
    (with-current-buffer changes-buffer
      (setq dvc-header (format "darcs whatsnew --look-for-adds for %s\n" default-directory))
      (let ((buffer-read-only)
            status modif modif-char)
        (dolist (elem status-list)
          (unless (string= "" elem)
            (setq modif-char (aref elem 0))
            (cond ((eq modif-char ?M)
                   (setq status "M"
                         modif "M")
                   (when (or (string-match "\\(.+\\) -[0-9]+ \\+[0-9]+$"
                                           elem)
                             (string-match "\\(.+\\) [+-][0-9]+$"
                                           elem))
                     (setq elem (match-string 1 elem))))
                  ;; ???a
                  ((eq modif-char ?a)
                   (setq status "?"))
                  ((eq modif-char ?A)
                   (setq status "A"
                         modif " "))
                  ((eq modif-char ?R)
                   (setq status "D"))
                  ((eq modif-char ??)
                   (setq status "?"))
                  (t
                   (setq modif nil
                         status nil)))
            (when (or modif status)
              (ewoc-enter-last
               dvc-fileinfo-ewoc
               (make-dvc-fileinfo-legacy
                :data (list 'file
                            ;; Skip the status and "./" in the filename
                            (substring elem 4)
                            status
                            modif))))))))))

;;;###autoload
(defun xdarcs-whatsnew (&optional path)
  "Run darcs whatsnew."
  (interactive (list default-directory))
  (let* ((dir (or path default-directory))
         (root (xdarcs-tree-root dir))
         (buffer (dvc-prepare-changes-buffer
                  `(xdarcs (last-revision ,root 1))
                  `(xdarcs (local-tree ,root))
                  'status root 'xdarcs)))
    (dvc-switch-to-buffer-maybe buffer)
    (setq dvc-buffer-refresh-function 'xdarcs-whatsnew)
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync
     'xdarcs '("whatsnew" "--look-for-adds")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer (capture buffer)
         (if (> (point-max) (point-min))
             (dvc-show-changes-buffer output 'xdarcs-parse-whatsnew
                                      (capture buffer))
           (dvc-diff-no-changes (capture buffer)
                                "No changes in %s"
                                (capture root))))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (dvc-diff-error-in-process (capture buffer)
                                    "Error in diff process"
                                    output error))))))

;;;###autoload
(defun xdarcs-dvc-missing (&optional other)
  "Run 'darcs pull --dry-run -s -v' to see what's missing"
  (interactive)
  (let ((buffer (dvc-get-buffer-create 'xdarcs 'missing)))
    (dvc-run-dvc-async
     'xdarcs '("pull" "--dry-run" "-s" "-v")
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (progn
         (with-current-buffer (capture buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert-buffer-substring output)
             (goto-char (point-min))
             (re-search-forward "^Would pull the following changes:" nil t)
             (xdarcs-missing-next 1)
             (xdarcs-missing-mode)))
         (goto-char (point-min))
         (dvc-switch-to-buffer (capture buffer)))))))

(defvar xdarcs-review-recenter-position-on-next-diff 5)

(defvar xdarcs-missing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map [?n] 'xdarcs-missing-next)
    (define-key map [?p] 'xdarcs-missing-previous)
    (define-key map [?\ ] 'xdarcs-missing-dwim-next)
    (define-key map (dvc-prefix-merge ?f) 'dvc-pull) ;; hint: fetch, p is reserved for push
    map)
  "Keymap used in a xdarcs missing buffer.")

(defvar xdarcs-missing-patch-start-regexp
  "^\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\).+$")

(defvar xdarcs-missing-font-lock-keywords
  `((,xdarcs-missing-patch-start-regexp . font-lock-function-name-face)
    ("^hunk.+" . font-lock-variable-name-face))
  "Keywords in `xdarcs-missing-mode'.")

(define-derived-mode xdarcs-missing-mode fundamental-mode
  "xdarcs missing mode"
  "Major mode to show the output of a call to `xdarcs-missing'."
  (dvc-install-buffer-menu)
  (set (make-local-variable 'font-lock-defaults)
       (list 'xdarcs-missing-font-lock-keywords t nil nil))
  (toggle-read-only 1))

(defun xdarcs-missing-next (n)
  (interactive "p")
  (end-of-line)
  (re-search-forward xdarcs-missing-patch-start-regexp nil t n)
  (beginning-of-line)
  (when xdarcs-review-recenter-position-on-next-diff
    (recenter xdarcs-review-recenter-position-on-next-diff)))

(defun xdarcs-missing-previous (n)
  (interactive "p")
  (end-of-line)
  (re-search-backward xdarcs-missing-patch-start-regexp)
  (re-search-backward xdarcs-missing-patch-start-regexp nil t n)
  (when xdarcs-review-recenter-position-on-next-diff
    (recenter xdarcs-review-recenter-position-on-next-diff)))

(defun xdarcs-missing-dwim-next ()
  "Either move to the next changeset via `xdarcs-missing-next' or call `scroll-up'.
When the beginning of the next changeset is already visible, call `xdarcs-missing-next',
otherwise call `scroll-up'."
  (interactive)
  (let* ((start-pos (point))
         (window-line (count-lines (window-start) start-pos))
         (window-height (dvc-window-body-height))
         (distance-to-next-changeset (save-window-excursion (xdarcs-missing-next 1) (count-lines start-pos (point)))))
    (goto-char start-pos)
    (when (eq distance-to-next-changeset 0) ; last changeset
      (setq distance-to-next-changeset (count-lines start-pos (point-max))))
    (if (< (- window-height window-line) distance-to-next-changeset)
        (scroll-up)
      (xdarcs-missing-next 1))))


(defun xdarcs-pull-finish-function (output error status arguments)
  (let ((buffer (dvc-get-buffer-create 'xdarcs 'pull)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring output)
        (toggle-read-only 1)))
    (let ((dvc-switch-to-buffer-mode 'show-in-other-window))
      (dvc-switch-to-buffer buffer))))

;;;###autoload
(defun xdarcs-pull (&optional other)
  "Run darcs pull --all.
If OTHER is nil, pull from the repository most recently pulled
from or pushed to.  If OTHER is a string, pull from that
repository."
  (interactive)
  (dvc-run-dvc-async 'xdarcs (list "pull" "--all" other)
                     :error 'xdarcs-pull-finish-function
                     :finished 'xdarcs-pull-finish-function))

;; --------------------------------------------------------------------------------
;; diff
;; --------------------------------------------------------------------------------
(defun xdarcs-parse-diff (changes-buffer)
  nil)

;;;###autoload
(defun xdarcs-dvc-diff (&optional against path dont-switch)
  (interactive (list nil nil current-prefix-arg))
  (let* ((cur-dir (or path default-directory))
         (orig-buffer (current-buffer))
         (root (dvc-tree-root cur-dir))
         (buffer (dvc-prepare-changes-buffer
                  `(xdarcs (last-revision ,root 1))
                  `(xdarcs (local-tree ,root))
                  'diff root 'xdarcs))
         (command-list '("diff" "--unified")))
    (dvc-switch-to-buffer-maybe buffer)
    (when dont-switch (pop-to-buffer orig-buffer))
    (dvc-save-some-buffers root)
    (dvc-run-dvc-sync 'xdarcs command-list
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (dvc-show-changes-buffer output 'xdarcs-parse-diff
                                                 (capture buffer))))))
;; --------------------------------------------------------------------------------
;; dvc revision support
;; --------------------------------------------------------------------------------
;;
;; It seems that there if no subcommand in darcs to get specified
;; revision of a file. So I use following trick:
;; 1. Make a diff between the file in local copy and the last revision
;;    of file. Then
;; 2. Apply the diff as patch reversely(-R) to the file in the local
;;    copy with patch command. With -o option, patch command doesn't
;;    modify the file in local copy; patch command create the applied
;;    file at /tmp. Finally
;; 3. Do insert-file-contents to the current buffer.
;;
;; Darcs experts, if you know better way, please, let us know.
;;
;; - Masatake
;;
;;;###autoload
(defun xdarcs-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)"
  (dvc-trace "xdarcs-revision-get-last-revision file:%S last-revision:%S" file last-revision)
  (let* (;;(xdarcs-rev (int-to-string (nth 1 last-revision)))
         (default-directory (car last-revision))
         ;; TODO: support the last-revision parameter??
         (patch (dvc-run-dvc-sync
                 'xdarcs (list "diff" "--unified" file)
                 :finished 'dvc-output-buffer-handler))
         (output-buffer (current-buffer))
         (output-file   (dvc-make-temp-name "xdarcs-file-find"))
         (patch-cmdline (format "cd \"%s\"; patch -R -o \"%s\""
                                default-directory
                                output-file))
         ;; TODO: Use dvc's process/buffer management facility.
         (status (with-temp-buffer
                   (insert patch)
                   (shell-command-on-region (point-min)
                                            (point-max)
                                            patch-cmdline
                                            output-buffer))))
    (when (zerop status)
      (with-current-buffer output-buffer
        (insert-file-contents output-file)
        ;; TODO: remove output-file
        ))))

;;;###autoload
(defun xdarcs-dvc-revert-files (&rest files)
  "Run darcs revert."
  (dvc-trace "xdarcs-revert-files: %s" files)
  (let ((default-directory (xdarcs-tree-root)))
    (dvc-run-dvc-sync 'xdarcs (append '("revert" "-a") (mapcar #'file-relative-name files))
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "xdarcs revert finished")))))

;;;###autoload
(defun xdarcs-dvc-remove-files (&rest files)
  "Run darcs remove."
  (dvc-trace "xdarcs-remove-files: %s" files)
  (dvc-run-dvc-sync 'xdarcs (append '("remove" "-a") files)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "xdarcs remove finished"))))


(provide 'xdarcs)
;;; xdarcs.el ends here

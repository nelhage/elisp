;;; dvc-gnus.el --- dvc integration to gnus

;; Copyright (C) 2003-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer <stefan@xsteve.at>

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

(require 'tla-core)

;; gnus is optional. Load it at compile-time to avoid warnings.
(eval-when-compile
  (condition-case nil
      (progn
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-sum))
    (error nil)))

(defvar gnus-summary-dvc-submap nil
  "DVC Key mapping added to gnus summary.")

(defun dvc-gnus-initialize-keymap ()
  "Initialize the keymap for DVC in `gnus-summary-mode-map'.

Prefix key is 'K t'."
  (unless gnus-summary-dvc-submap
    (require 'gnus)
    (require 'gnus-sum)
    (require 'gnus-art)
    (setq gnus-summary-dvc-submap (make-sparse-keymap))
    (define-key gnus-summary-mode-map [?K ?t] gnus-summary-dvc-submap)))

;;;###autoload
(defun dvc-insinuate-gnus ()
  "Insinuate Gnus for each registered DVC back-end.

Runs (<backend>-insinuate-gnus) for each registered back-end having
this function.

Additionally the following key binding is defined for the gnus summary mode map:
K t l `dvc-gnus-article-extract-log-message'
K t v `dvc-gnus-article-view-patch'
K t m `dvc-gnus-article-view-missing'
K t a `dvc-gnus-article-apply-patch'
K t p `dvc-gnus-article-apply-patch-with-selected-destination'"
  (interactive)
  (dvc-gnus-initialize-keymap)
  (define-key gnus-summary-dvc-submap [?a] 'dvc-gnus-article-apply-patch)
  (define-key gnus-summary-dvc-submap [?p] 'dvc-gnus-article-apply-patch-with-selected-destination)
  (define-key gnus-summary-dvc-submap [?l] 'dvc-gnus-article-extract-log-message)
  (define-key gnus-summary-dvc-submap [?v] 'dvc-gnus-article-view-patch)
  (define-key gnus-summary-dvc-submap [?m] 'dvc-gnus-article-view-missing)
  (mapcar (lambda (x)
            (let ((fn (dvc-function x "insinuate-gnus" t)))
              (when (fboundp fn)
                (dvc-trace "Insinuating Gnus for %S" x)
                (funcall fn))))
          dvc-registered-backends))

(defun dvc-gnus-article-extract-log-message ()
  "Parse the mail and extract the log information.
Save it to `dvc-memorized-log-header', `dvc-memorized-patch-sender',
`dvc-memorized-log-message' and `dvc-memorized-version'."
  (interactive)
  (gnus-summary-select-article-buffer)
  (save-excursion
    (goto-char (point-min))
    (let* ((start-pos (or (search-forward "[PATCH] " nil t) (search-forward "Subject: ")))
           (end-pos (line-end-position))
           (log-header (buffer-substring-no-properties start-pos end-pos)))
      (setq dvc-memorized-log-header log-header))
    (goto-char (point-min))
    (let* ((start-pos (re-search-forward "From: +" nil t))
           (end-pos (line-end-position))
           (sender (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq dvc-memorized-patch-sender (and start-pos sender)))
    (goto-char (point-min))
    (let* ((start-pos (search-forward "[VERSION] " nil t))
           (end-pos (line-end-position))
           (version (when start-pos (buffer-substring-no-properties start-pos end-pos))))
      (setq dvc-memorized-version (and start-pos version)))
    (dolist (delim-pair '(("^<<LOG-START>>" "^<<LOG-END>>") ("^\\[\\[\\[" "^\\]\\]\\]")))
      (goto-char (point-min))
      (when (and (re-search-forward (car delim-pair) nil t)
                 (re-search-forward (cadr delim-pair) nil t))
        (goto-char (point-min))
        (let* ((start-pos (+ (re-search-forward (car delim-pair)) 1))
               (end-pos (- (progn (re-search-forward (cadr delim-pair)) (line-beginning-position)) 1))
               (log-message (buffer-substring-no-properties start-pos end-pos)))
          (setq dvc-memorized-log-message log-message)
          (message "Extracted the patch log message from '%s'" dvc-memorized-log-header)))))
  (gnus-article-show-summary))

(defvar dvc-gnus-article-apply-patch-deciders nil
  "A list of functions that can be used to determine the patch type in a given mail.
The function is called when the article buffer is active. It should return nil if
the patch type could not be determined, otherwise one of the following:
'tla, 'xhg, 'bzr-merge-or-pull-url, 'bzr-merge-bundle, 'xgit")
(defvar dvc-gnus-override-window-config nil)
(defun dvc-gnus-article-apply-patch (n)
  "Apply MIME part N, as patchset.
When called with no prefix arg, set N := 2.

DVC will try to figure out which VCS to use when applying the patch.

First we check to see if it is a tla changeset created with DVC.
If that is the case, `tla-gnus-apply-patch' is called.

The next check is whether it is a patch suitable for xhg.  In that case
`xhg-gnus-article-import-patch' is called.

Then we check to see whether the patch was prepared with git
format-patch.  If so, then call `xgit-gnus-article-apply-patch'.

Otherwise `dvc-gnus-apply-patch' is called."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (let ((patch-type)
        (bzr-merge-or-pull-url)
        (patch-decider-list dvc-gnus-article-apply-patch-deciders))
    (save-window-excursion
      (gnus-summary-select-article-buffer)
      (goto-char (point-min))
      (while (and (not patch-type) patch-decider-list)
        (setq patch-type (funcall (car patch-decider-list)))
        (setq patch-decider-list (cdr patch-decider-list)))
      (unless patch-type
        (cond ((re-search-forward (concat "\\[VERSION\\] "
                                          (tla-make-name-regexp 4 t t))
                                  nil t)
               (setq patch-type 'tla))
              ((progn (goto-char (point-min))
                      (re-search-forward "^changeset: +[0-9]+:[0-9a-f]+$" nil t))
               (setq patch-type 'xhg))
              ((progn (goto-char (point-min))
                      (or (re-search-forward "^New revision in \\(.+\\)$" nil t)
                          (re-search-forward
                           "^Committed revision [0-9]+ to \\(.+\\)$" nil t)))
               (setq patch-type 'bzr-merge-or-pull
                     bzr-merge-or-pull-url (match-string-no-properties 1)))
              ((progn (goto-char (point-min))
                      (re-search-forward "^# Bazaar merge directive format" nil t))
               (setq patch-type 'bzr-merge-bundle))
              ((progn (goto-char (point-min))
                      (and (re-search-forward "^---$" nil t)
                           (re-search-forward "^diff --git" nil t)))
               (setq patch-type 'xgit))
              (t (setq patch-type 'dvc)))))
    (message "patch-type: %S" patch-type)
    (cond ((eq patch-type 'tla)
           (tla-gnus-article-apply-patch n))
          ((eq patch-type 'xhg)
           (xhg-gnus-article-import-patch n))
          ((eq patch-type 'xgit)
           (xgit-gnus-article-apply-patch n))
          ((eq patch-type 'bzr-merge-or-pull)
           (bzr-merge-or-pull-from-url bzr-merge-or-pull-url))
          ((eq patch-type 'bzr-merge-bundle)
           (bzr-gnus-article-merge-bundle n))
          ((eq patch-type 'bzr-pull-bundle-in-branch)
           (bzr-gnus-article-pull-bundle-in-branch n))
          ((eq patch-type nil)
           (let ((dvc-gnus-override-window-config))
             (gnus-article-part-wrapper n 'dvc-gnus-apply-patch)
             (when dvc-gnus-override-window-config
               (set-window-configuration dvc-gnus-override-window-config))))
          (t
           (error "Unknown patch type %S" patch-type)))))

(defvar dvc-gnus-select-patch-dir-function nil)
(defun dvc-gnus-article-apply-patch-with-selected-destination (n)
  "Apply a patch via the emacs diff-mode.
Allow to select the target directory from one of
`dvc-gnus-patch-desitination-candidates'."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (let ((dvc-gnus-override-window-config)
        (dvc-gnus-select-patch-dir-function 'dvc-gnus-select-patch-destination))
    (gnus-article-part-wrapper n 'dvc-gnus-apply-patch)
    (when dvc-gnus-override-window-config
      (set-window-configuration dvc-gnus-override-window-config))))

(defvar dvc-gnus-patch-desitination-candidates nil)
(defun dvc-gnus-select-patch-destination ()
  (expand-file-name (dvc-completing-read "Patch destination: " dvc-gnus-patch-desitination-candidates)))

(defun dvc-gnus-article-view-missing ()
  "Apply MIME part N, as patchset.
When called with no prefix arg, set N := 2.
First is checked, if it is a tla changeset created with DVC.
If that is the case, `tla-gnus-apply-patch' is called.
The next check is whether it is a patch suitable for xhg. In that case
`xhg-gnus-article-import-patch' is called.
Otherwise `dvc-gnus-apply-patch' is called."
  (interactive)
  (save-window-excursion
    (gnus-summary-select-article-buffer)
    (goto-char (point-min))
    (goto-char (point-min))
    (if (or (re-search-forward "^New revision in \\(.+\\)$" nil t)
            (re-search-forward "^Committed revision [0-9]+ to \\(.+\\)$" nil t))
        (let* ((bzr-missing-url (match-string-no-properties 1))
               (dest (cdr (assoc bzr-missing-url bzr-merge-or-pull-from-url-rules)))
               (path (cadr dest))
               (doit t))
          (when path
            (setq doit (y-or-n-p (format "Run missing from %s in %s? " bzr-missing-url path))))
          (when doit
            (unless path
              (setq path (dvc-read-directory-name (format "Run missing from %s in: " bzr-missing-url))))
            (let ((default-directory path))
              (message "Running bzr missing from %s in %s" bzr-missing-url path)
              (bzr-missing bzr-missing-url)))))))

(defun dvc-gnus-article-view-patch (n)
  "View MIME part N, as patchset.
When called with no prefix arg, set N := 2.
First is checked, if it is a tla changeset created with DVC.
If that is the case, `tla-gnus-article-view-patch' is called.
The next check looks at commit notification mails for bzr, when
such a message is detected, `bzr-gnus-article-view-patch' is called.
Otherwise `dvc-gnus-view-patch' is called."
  (interactive "p")
  (unless current-prefix-arg
    (setq n 2))
  (let ((patch-type))
    (save-window-excursion
      (gnus-summary-select-article-buffer)
      (goto-char (point-min))
      (if (or (re-search-forward (concat "\\[VERSION\\] " (tla-make-name-regexp 4 t t)) nil t)
              (progn (goto-char (point-min))
                     (and (search-forward "Revision: " nil t)
                          (search-forward "Archive: " nil t))))
          (setq patch-type 'tla)
        (goto-char (point-min))
        ;; Committed revision 129 to http://my-arch.org/branch1
        (if (re-search-forward "^Committed revision [0-9]+ to " nil t)
            (setq patch-type 'bzr)
          (setq patch-type 'dvc))))
    (cond ((eq patch-type 'tla)
           (tla-gnus-article-view-patch n))
          ((eq patch-type 'bzr)
           (bzr-gnus-article-view-patch n))
          (t
           (let ((dvc-gnus-override-window-config))
             (gnus-article-part-wrapper n 'dvc-gnus-view-patch)
             (when dvc-gnus-override-window-config
               (set-window-configuration dvc-gnus-override-window-config)))))))

(defvar dvc-apply-patch-mapping nil)
;;e.g.: (add-to-list 'dvc-apply-patch-mapping '("psvn" "~/work/myprg/psvn"))

(defun dvc-gnus-suggest-apply-patch-directory ()
  "Use `dvc-apply-patch-mapping' to suggest a directory where
the patch sould be applied."
  (if dvc-gnus-select-patch-dir-function
      (funcall dvc-gnus-select-patch-dir-function)
    (save-window-excursion
      (gnus-summary-select-article-buffer)
      (let ((patch-directory "~/")
            (m dvc-apply-patch-mapping))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward "text/x-patch; " nil t)
            (while m
              (if (looking-at (caar m))
                  (progn
                    (setq patch-directory (cadar m))
                    (setq m nil))
                (setq m (cdr m))))))
        (gnus-article-show-summary)
        (expand-file-name patch-directory)))))

(defun dvc-gnus-apply-patch (handle)
  "Apply the patch corresponding to HANDLE."
  (dvc-gnus-article-extract-log-message)
  (let ((dvc-patch-name (concat (dvc-make-temp-name "dvc-patch") ".diff"))
        (window-conf (current-window-configuration))
        (patch-buff))
    (dvc-buffer-push-previous-window-config window-conf)
    (mm-save-part-to-file handle dvc-patch-name)
    (find-file dvc-patch-name)
    (diff-mode)
    (toggle-read-only 1)
    (setq patch-buff (current-buffer))
    (delete-other-windows)
    (setq default-directory (dvc-gnus-suggest-apply-patch-directory))
    ;; 07.07.2008: applying with ediff only works well when only one file is given.
    ;; (flet ((ediff-get-default-file-name (&optional default) (if default default default-directory)))
    ;;   (ediff-patch-file 2 patch-buff))
    (diff-hunk-next)
    (message "You can apply the patch hunks now by using C-c C-a.")
    (setq dvc-gnus-override-window-config (current-window-configuration))))

(defun dvc-gnus-view-patch (handle)
  "View the patch corresponding to HANDLE."
  (let ((dvc-patch-name (concat (dvc-make-temp-name "dvc-patch") ".diff"))
        (cur-buf (current-buffer))
        (window-conf (current-window-configuration))
        (patch-buff))
    (mm-save-part-to-file handle dvc-patch-name)
    (gnus-summary-select-article-buffer)
    (split-window-vertically)
    (find-file-other-window dvc-patch-name)
    (diff-mode)
    (setq dvc-gnus-override-window-config (current-window-configuration))
    (dvc-buffer-push-previous-window-config window-conf)
    (toggle-read-only 1)
    (other-window -1)
    (gnus-article-show-summary)))

(provide 'dvc-gnus)
;;; dvc-gnus.el ends here

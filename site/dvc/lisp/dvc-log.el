;;; dvc-log.el --- Manipulation of the log before committing

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(require 'dvc-unified)
(require 'ediff)
(require 'vc)

(defcustom dvc-log-edit-other-frame nil
  "If non-nil, dvc-log-edit defaults to other-frame."
  :type 'boolean
  :group 'dvc)

;;
;; Log edit mode
;;
(defvar dvc-log-edit-font-lock-keywords
  `(("^\t?\\* \\([^ ,:([\n]+\\)"
     (1 'change-log-file-face)
     ("\\=, \\([^ ,:([\n]+\\)" nil nil
      (1 'change-log-file-face))
     ("\\= (\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face))
     ("\\=, *\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face)))
    ;;    (,(concat "^" (regexp-quote dvc-log-edit-file-list-marker) "$")
    ;;     . 'dvc-header)
    )
  "Keywords in dvc-log-edit mode.")

(defvar dvc-log-edit-flush-prefix "## ")

(defvar dvc-log-edit-file-list-marker
  "--This line, and those below, will be ignored--"
  "A marker separating the actual log message from the list of files to commit.")

(defvar dvc-log-edit-init-functions (make-hash-table :test 'equal)
  "A hash table that holds the mapping from work directory roots to
functions that provide the initial content for a commit.")

;; --------------------------------------------------------------------------------
;; Menus
;; --------------------------------------------------------------------------------

;;;###autoload
(define-derived-mode dvc-log-edit-mode text-mode "dvc-log-edit"
  "Major Mode to edit DVC log messages.
Commands:
\\{dvc-log-edit-mode-map}
"
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (use-local-map dvc-log-edit-mode-map)
  (easy-menu-add dvc-log-edit-mode-menu)
  (dvc-install-buffer-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(dvc-log-edit-font-lock-keywords t))
  (set (make-local-variable 'fill-paragraph-function)
       'dvc-log-fill-paragraph)
  (setq fill-column 73)
  (when (eq (point-min) (point-max))
    (dvc-log-edit-insert-initial-commit-message))
  (run-hooks 'dvc-log-edit-mode-hook))

(define-key dvc-log-edit-mode-map [(control ?c) (control ?c)] 'dvc-log-edit-done)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?d)] 'dvc-diff)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?l)] 'dvc-log)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?f)] 'dvc-log-insert-commit-file-list)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?p)] 'dvc-buffer-pop-to-partner-buffer)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?m)] 'dvc-log-edit-insert-memorized-log)
(define-key dvc-log-edit-mode-map [(control ?c) (control ?i)] 'dvc-log-edit-insert-initial-commit-message)

(easy-menu-define dvc-log-edit-mode-menu dvc-log-edit-mode-map
  "`dvc-log-edit-mode' menu"
  '("Log Edit"
    ["Show changes"             dvc-diff          t]
    ["Commit"                   dvc-log-edit-done    t]
    ["Show Changelog"           dvc-log              t]
    ["Pop to partner buffer"    dvc-buffer-pop-to-partner-buffer t]
    ["Insert/Flush commit file list"  dvc-log-insert-commit-file-list t]
    ["Insert memorized log"  dvc-log-edit-insert-memorized-log t]
    "--"
    ["Abort"                    dvc-log-edit-abort   t]))

;; Internal variables
(defvar dvc-pre-commit-window-configuration nil)

;;;###autoload
(defun dvc-dvc-log-edit (root other-frame no-init)
  "Edit the log file for tree ROOT before a commit.

OTHER_FRAME if non-nil puts log edit buffer in a separate frame.
NO-INIT if non-nil suppresses initialization of the buffer if one
is reused."
  (setq dvc-pre-commit-window-configuration
        (current-window-configuration))
  (let ((start-buffer (current-buffer)))
    (dvc-switch-to-buffer
     (dvc-get-buffer-create (dvc-current-active-dvc) 'log-edit)
     other-frame)
    ;; `no-init' is somewhat misleading here. It is set to t in
    ;; dvc-add-log-entry, and nil in dvc-log-edit. That prevents
    ;; changing dvc-partner-buffer when we shouldn't. But the user
    ;; might call dvc-log-edit multiple times from the same diff or
    ;; status buffer, and expect edits in the log-edit buffer to be
    ;; preserved.
    (unless no-init
      (let ((buffer-name (buffer-name))
            (file-name (dvc-log-edit-file-name)))
        (set-visited-file-name file-name t t)
        ;; `set-visited-file-name' modifies default-directory
        (setq default-directory root)
        ;; Read in the current log file, unless the user has already
        ;; edited the buffer.
        (when (and (= (point-min) (point-max)) (file-readable-p file-name))
          (insert-file-contents file-name)
          (set-buffer-modified-p nil))
        (rename-buffer buffer-name)
        (setq dvc-partner-buffer start-buffer)
        (dvc-log-edit-mode)))))

(defun dvc-log-edit-abort ()
  "Abort the current log edit."
  (interactive)
  (bury-buffer)
  (set-window-configuration dvc-pre-commit-window-configuration))

(defun dvc-log-close (buffer)
  "Close the log buffer, and delete the file."
  (if vc-delete-logbuf-window
      (kill-buffer buffer)
    (quit-window))
  (delete-file (dvc-log-edit-file-name)))

(defun dvc-log-flush-commit-file-list ()
  "Remove the list of the files to commit.
All lines starting with `dvc-log-edit-flush-prefix' are deleted."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines (concat "^" dvc-log-edit-flush-prefix))))

(defun dvc-log-fill-paragraph (&optional justify)
  "Fill the paragraph, but preserve open parentheses at beginning of lines.
Prefix arg means justify as well."
  (interactive "P")
  (let ((end (progn (forward-paragraph) (point)))
        (beg (progn (backward-paragraph) (point)))
        (paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
    (fill-region beg end justify)
    t))

(defun dvc-log-insert-commit-file-list (arg)
  "Insert the file list that will be committed.
With a negative prefix argument just remove the file list
by calling `dvc-log-flush-commit-file-list'."
  (interactive "p")
  (if (< arg 0)
      (dvc-log-flush-commit-file-list)
    (let ((file-list (funcall (dvc-function (dvc-current-active-dvc) "dvc-files-to-commit")))
          (mark))
      (dvc-trace "Files to commit: %S" file-list)
      (save-excursion
        (goto-char (point-min))
        (dvc-log-flush-commit-file-list)
        (insert dvc-log-edit-flush-prefix)
        (insert (format "Lines beginning with '%s' will be deleted from this buffer before committing\n" dvc-log-edit-flush-prefix))
        (insert dvc-log-edit-flush-prefix)
        (insert "Files to commit:\n")
        (dolist (f file-list)
          (setq mark (cdr (assoc (car f) '( (dvc-modified . "M ") (dvc-added . "A ") (dvc-deleted . "R ") ))))
          (insert dvc-log-edit-flush-prefix)
          (insert (dvc-face-add (concat mark (cdr f)) (car f)))
          (newline))))))

(defun dvc-log-edit-insert-memorized-log ()
  "Insert a memorized log message."
  (interactive)
  (when dvc-memorized-log-header
    (goto-char (point-min))
    (delete-region (point) (line-end-position))
    (insert dvc-memorized-log-header))
  (when dvc-memorized-log-message
    (goto-char (point-min))
    (end-of-line)
    (newline)
    (newline)
    (when dvc-memorized-patch-sender
      (if (looking-at "Patch from ")
          (forward-line 1)
        (progn
          (undo-boundary)
          (insert (format "Patch from %s\n" dvc-memorized-patch-sender)))))
    (when (looking-at "\* .+: ") ;; e.g.: "* lisp/dvc.el: "
      (end-of-line)
      (newline))
    (insert dvc-memorized-log-message)))

;;;###autoload
(defun dvc-add-log-entry (&optional other-frame)
  "Add new ChangeLog style entry to the current DVC log-edit buffer.
If OTHER-FRAME xor `dvc-log-edit-other-frame' is non-nil,
show log-edit buffer in other frame."
  (interactive "P")
  (save-restriction
    (dvc-add-log-entry-internal other-frame)))

(defun dvc-add-log-file-name (buffer-file)
  "Return a file name for a log entry for BUFFER-FILE; including path from tree root.
For use as add-log-file-name-function."
  ;; This is better than the default algorithm in add-log-file-name,
  ;; when the log file is not in the workspace root (as is true for
  ;; monotone)
  (if (string-match
       (concat "^" (regexp-quote (dvc-tree-root)))
       buffer-file)
      (substring buffer-file (match-end 0))
    (file-name-nondirectory buffer-file)))

(defun dvc-ediff-add-log-entry (&optional other-frame)
  "Add new DVC log ChangeLog style entry; intended to be invoked
from the ediff control buffer."
  (interactive "P")
  (let ((dvc-temp-current-active-dvc dvc-buffer-current-active-dvc))
    (set-buffer ediff-buffer-B)         ; DVC puts workspace version here
    (dvc-add-log-entry-internal other-frame)))

(defun dvc-ediff-setup ()
  (define-key 'ediff-mode-map "t" 'dvc-ediff-add-log-entry)) ; matches dvc-diff-mode-map

;; ediff hooks that run after ediff-mode-map is created:
;; ediff-prepare-buffer-hook, ediff-startup-hook
(add-hook 'ediff-startup-hook 'dvc-ediff-setup)

(defun dvc-add-log-entry-internal (other-frame)
  "Similar to `add-change-log-entry'.

Inserts the entry in the dvc log-edit buffer instead of the ChangeLog."
  ;; This is mostly copied from add-log.el.  Perhaps it would be better to
  ;; split add-change-log-entry into several functions and then use them, but
  ;; that wouldn't work with older versions of Emacs.
  ;;
  ;; We don't set add-log-file-name-function globally because
  ;; dvc-diff-mode needs a different one.
  (if (not (featurep 'add-log)) (require 'add-log))
  (let* ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
         (add-log-file-name-function 'dvc-add-log-file-name)
         (defun (add-log-current-defun))
         (buf-file-name (if (and (boundp 'add-log-buffer-file-name-function)
                                 add-log-buffer-file-name-function)
                            (funcall add-log-buffer-file-name-function)
                          buffer-file-name))
         (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
         (file-name (dvc-log-edit-file-name))
         ;; Set ENTRY to the file name to use in the new entry.
         (entry (add-log-file-name buffer-file file-name))
         beg
         bound
         narrowing)

    (dvc-log-edit other-frame t)

    (undo-boundary)
    (goto-char (point-min))
    (when (re-search-forward (regexp-opt
                              (list "^Patches applied:"
                                    (regexp-quote
                                     ;; TODO
                                     dvc-log-edit-file-list-marker)))
                             nil t)
      (narrow-to-region (point-min) (match-beginning 0))
      (setq narrowing t)
      (goto-char (point-min)))
    (re-search-forward "\n\n\\|\\'")
    (setq beg (point))
    (if (looking-at "\n*[^\n* \t]")
        (progn
          (skip-chars-forward "\n")
          (setq bound (point)))
      (goto-char (point-max))
      (setq bound (point))
      (unless (and (boundp 'add-log-keep-changes-together)
                   add-log-keep-changes-together)
        (backward-paragraph)            ; paragraph delimits entries for file
        (forward-line 1)
        (setq beg (point))))
    (goto-char beg)
    (forward-line -1)
    ;; Now insert the new line for this entry.
    (cond ((re-search-forward "^\\s *\\*\\s *$" bound t)
           ;; Put this file name into the existing empty entry.
           (if entry
               (insert entry)))
          ((let (case-fold-search)
             (re-search-forward
              (concat (regexp-quote (concat "* " entry))
                      ;; Don't accept `foo.bar' when
                      ;; looking for `foo':
                      "\\(\\s \\|[(),:]\\)")
              bound t))
           ;; Add to the existing entry for the same file.
           (if (re-search-forward "^\\s *$\\|^\\s \\*" nil t)
               (goto-char (match-beginning 0))
             (goto-char (point-max))
             (insert-char ?\n 1))
           ;; Delete excess empty lines; make just 2.
           (while (and (not (eobp)) (looking-at "^\\s *$"))
             (delete-region (point) (line-beginning-position 2)))
           (insert-char ?\n 2)
           (forward-line -2)
           (indent-relative))
          (t
           ;; Make a new entry.
           (if dvc-log-insert-last
               (progn
                 (goto-char (point-max))
                 (re-search-backward "^." nil t)
                 (end-of-line)
                 (insert "\n\n* ")
                 )
             (forward-line 1)
             (while (looking-at "\\sW")
               (forward-line 1))
             (while (and (not (eobp)) (looking-at "^\\s *$"))
               (delete-region (point) (line-beginning-position 2)))
             (insert-char ?\n 3)
             (forward-line -2)
             (indent-to left-margin)
             (insert "* "))
           (if entry (insert entry))))
    (if narrowing (widen))
    ;; Now insert the function name, if we have one.
    ;; Point is at the entry for this file,
    ;; either at the end of the line or at the first blank line.
    (if defun
        (progn
          ;; Make it easy to get rid of the function name.
          (undo-boundary)
          (unless (save-excursion
                    (beginning-of-line 1)
                    (looking-at "\\s *$"))
            (insert ?\ ))
          ;; See if the prev function name has a message yet or not
          ;; If not, merge the two entries.
          (let ((pos (point-marker)))
            (if (and (skip-syntax-backward " ")
                     (skip-chars-backward "):")
                     (looking-at "):")
                     (progn (delete-region (+ 1 (point)) (+ 2 (point))) t)
                     (> fill-column (+ (current-column) (length defun) 3)))
                (progn (delete-region (point) pos)
                       (insert ", "))
              (goto-char pos)
              (insert "("))
            (set-marker pos nil))
          ;; Check for previous function name using re-search-backward
          ;; instead of looking-back, because looking-back is not
          ;; implemented in all variants of (X)Emacs.  We could create
          ;; a compatibility function for it, but nobody else seems to
          ;; use it yet, so there is no point.
          (when (re-search-backward (concat (regexp-quote defun) ",\\s *\\=") nil t)
            (replace-match ""))
          (insert defun "): "))
      ;; No function name, so put in a colon unless we have just a star.
      (unless (save-excursion
                (beginning-of-line 1)
                (looking-at "\\s *\\(\\*\\s *\\)?$"))
        (insert ": ")))))

(defun dvc-log-edit-register-initial-content-function (working-copy-root the-function)
  "Register a mapping from a work directory root to a function that provide the initial content for a commit."
  (puthash (dvc-uniquify-file-name working-copy-root) the-function dvc-log-edit-init-functions))

(defun dvc-log-edit-insert-initial-commit-message ()
  "Insert the initial commit message at point.
See `dvc-log-edit-register-initial-content-function' to register functions that provide the message text."
  (interactive)
  (let ((initial-content-function (gethash (dvc-uniquify-file-name (dvc-tree-root)) dvc-log-edit-init-functions)))
    (when initial-content-function
      (insert (funcall initial-content-function)))))


(provide 'dvc-log)
;;; dvc-log.el ends here

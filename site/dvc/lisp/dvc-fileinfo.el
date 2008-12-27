;;; dvc-fileinfo.el --- An ewoc structure for displaying file information
;;; for DVC

;; Copyright (C) 2007, 2008 by all contributors

;; Author: Stephen Leake, <stephen_leake@stephe-leake.org>

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

(require 'dvc-defs)
(require 'dvc-core)
(require 'ewoc)
(eval-when-compile (require 'cl))

(defstruct (dvc-fileinfo-root
            (:constructor nil)
            (:copier nil))
  ;; no slots; root of class for ewoc entries.
  )

(defvar dvc-fileinfo-ewoc nil
  "Buffer-local ewoc for displaying workspace file status.

All dvc-fileinfo functions operate on this ewoc.
The elements must all be of class dvc-fileinfo-root.")
;; We could have each mode that uses fileinfo declare their own
;; buffer-local ewoc variable (ie dvc-diff-cookie). However, then the
;; interactive functions declared here (like dvc-fileinfo-next) would
;; take an ewoc argument, making them harder to bind directly to keys.
;;
;; We assume there will only be one ewoc structure of interest in a
;; given buffer.
(make-variable-buffer-local 'dvc-fileinfo-ewoc)

(defstruct (dvc-fileinfo-file
            (:include dvc-fileinfo-root)
            (:copier nil))
  mark          ;; t/nil.
  exclude       ;; t/nil. If t, don't commit unless also mark = t.
  dir           ;; Directory the file resides in, relative to dvc-root.
  file          ;; File name sans directory.
                ;; (concat dir file) gives a valid path.
  status        ;; Symbol; see dvc-fileinfo-status-image-full for list
  (indexed t)   ;; Whether changes made to the file have been recorded
                ;; in the index.  Use t if the back-end does not
                ;; support an index.
  more-status   ;; String; whatever else the backend has to say
  )

(defun dvc-fileinfo-status-image-full (status)
  "String image of STATUS.
This is used by `dvc-fileinfo-printer-full'."
  (ecase status
    (added          "added        ")
    (conflict       "conflict     ")
    (deleted        "deleted      ")
    (ignored        "ignored      ")
    (invalid        "invalid      ")
    (known          "known        ")
    (missing        "missing      ")
    (modified       "modified     ")
    (copy-source    "copy         ")
    (copy-target    "         ==> ")
    (rename-source  "rename-source")
    (rename-target  "rename-target")
    (unknown        "unknown      ")))

(defun dvc-fileinfo-status-image-terse (status)
  "String image of STATUS.
This is used by `dvc-fileinfo-printer-terse'."
  (ecase status
    (added          "A")
    (conflict       "X")
    (deleted        "D")
    (ignored        "G")
    (invalid        "I")
    (known          "-")
    (missing        "D")
    (modified       "M")
    (copy-source    "C")
    (copy-target    'target)
    (rename-source  "R")
    (rename-target  'target)
    (unknown        "?")))

(defun dvc-fileinfo-choose-face-full (status)
  "Return a face appropriate for STATUS.
This is used by `dvc-fileinfo-printer-full'."
  (ecase status
    (added         'dvc-added)
    (conflict      'dvc-conflict)
    (deleted       'dvc-deleted)
    (ignored       'dvc-ignored)
    (invalid       'dvc-unrecognized)
    (known         'dvc-source)
    (missing       'dvc-move)
    (modified      'dvc-modified)
    (copy-source   'dvc-copy)
    (copy-target   'dvc-copy)
    (rename-source 'dvc-move)
    (rename-target 'dvc-move)
    (unknown       'dvc-unknown)))

(defalias 'dvc-fileinfo-choose-face-terse 'dvc-fileinfo-choose-face-full)

(defstruct (dvc-fileinfo-dir
            (:include dvc-fileinfo-file)
            (:copier nil))
  ;; no extra slots
  )

(defstruct (dvc-fileinfo-message
            (:include dvc-fileinfo-root)
            (:copier nil))
  text ;; String
  )

(defstruct (dvc-fileinfo-legacy
            (:include dvc-fileinfo-root)
            (:copier nil))
  ;; This type is has the same form as the old dvc-diff-cookie ewoc
  ;; element. It is provided to ease the transition to the new
  ;; structure; current parsing code needs very few changes to use
  ;; this, and can be more gradually changed to use a dvc-fileinfo
  ;; struct.

  data
  ;; one of:
  ;; (file \"filename\" \"[CRADP?]\" \"M\" \"/\" \"origname\")
  ;; (subtree \"name\" related-buffer changes?)
  ;; (searching-subtree \"<message>\" )
  )

(defun dvc-fileinfo-printer (fileinfo)
  "Ewoc pretty-printer for dvc-fileinfo types. Actual pretty-printer
is specified by `dvc-fileinfo-printer-interface'."
  (let* ((interface (or dvc-fileinfo-printer-interface 'full))
         (fun (intern (concat "dvc-fileinfo-printer-"
                              (symbol-name interface)))))
    ;; Allow people to use a complete function name if they like
    (when (and (not (fboundp fun))
               (fboundp interface))
      (setq fun interface))
    (funcall fun fileinfo)))

(defun dvc-fileinfo-printer-full (fileinfo)
  "Ewoc pretty-printer for dvc-fileinfo types which uses full text to
indicate statuses."
  (etypecase fileinfo
    (dvc-fileinfo-file ;; also matches dvc-fileinfo-dir
     (let ((line (concat
                  (dvc-fileinfo-status-image-full
                   (dvc-fileinfo-file-status fileinfo))
                  " "
                  (dvc-fileinfo-file-dir fileinfo)
                  (dvc-fileinfo-file-file fileinfo)))
           (face (cond
                  ((dvc-fileinfo-file-mark fileinfo) 'dvc-marked)
                  ((dvc-fileinfo-file-exclude fileinfo) 'dvc-excluded)
                  (t (dvc-fileinfo-choose-face-full
                      (dvc-fileinfo-file-status fileinfo))))))
       (insert " ")
       (cond
        ((dvc-fileinfo-file-mark fileinfo) (insert dvc-mark))
        ((dvc-fileinfo-file-exclude fileinfo) (insert dvc-exclude))
        (t (insert " ")))

       (insert " ")
       (insert (dvc-face-add line face))
       (if (> (length (dvc-fileinfo-file-more-status fileinfo)) 0)
           (progn
             (newline)
             (insert "      ")
             (insert (dvc-fileinfo-file-more-status fileinfo))))))

    (dvc-fileinfo-legacy
     (dvc-diff-printer (dvc-fileinfo-legacy-data fileinfo)) )

    (dvc-fileinfo-message
     (insert (dvc-fileinfo-message-text fileinfo)))))

(defun dvc-fileinfo-printer-terse (fileinfo)
  "Ewoc pretty-printer for dvc-fileinfo types which uses a single letter
to indicate statuses."
  (let ((inhibit-read-only t))
    (etypecase fileinfo
      (dvc-fileinfo-file ;; also matches dvc-fileinfo-dir
       (let* ((image (dvc-fileinfo-status-image-terse
                      (dvc-fileinfo-file-status fileinfo)))
              (indexed (if (or (dvc-fileinfo-file-indexed fileinfo)
                               (eq (dvc-fileinfo-file-status fileinfo)
                                   'unknown))
                           " " "?"))
              (line (if (stringp image)
                        (concat image indexed " "
                                (dvc-fileinfo-file-dir fileinfo)
                                (dvc-fileinfo-file-file fileinfo))
                      (concat "   ==>  "
                              (dvc-fileinfo-file-dir fileinfo)
                              (dvc-fileinfo-file-file fileinfo))))
              (face (cond
                     ((dvc-fileinfo-file-mark fileinfo) 'dvc-marked)
                     ((dvc-fileinfo-file-exclude fileinfo) 'dvc-excluded)
                     (t (dvc-fileinfo-choose-face-terse
                         (dvc-fileinfo-file-status fileinfo))))))
         (cond
          ((dvc-fileinfo-file-mark fileinfo) (insert dvc-mark))
          ((dvc-fileinfo-file-exclude fileinfo) (insert dvc-exclude))
          (t (insert " ")))

         (insert " ")
         (insert (dvc-face-add line face))
         (if (> (length (dvc-fileinfo-file-more-status fileinfo)) 0)
             (progn
               (newline)
               (insert "      ")
               (insert (dvc-fileinfo-file-more-status fileinfo))))))

      (dvc-fileinfo-legacy
       (dvc-diff-printer (dvc-fileinfo-legacy-data fileinfo)) )

      (dvc-fileinfo-message
       (insert (dvc-fileinfo-message-text fileinfo))))))

(defun dvc-fileinfo-current-fileinfo ()
  "Return the fileinfo (a dvc-fileinfo-file, or
dvc-fileinfo-legacy) for the ewoc element at point. Throws an
error if point is not on a file or directory."
  (let ((ewoc-entry (ewoc-locate dvc-fileinfo-ewoc)))
    (if (not ewoc-entry)
        ;; ewoc is empty
        (error "not on a file or directory"))
    (let ((fileinfo (ewoc-data ewoc-entry)))
      (etypecase fileinfo
        (dvc-fileinfo-file              ; also matches dvc-fileinfo-dir
         fileinfo)

        (dvc-fileinfo-legacy
         (let ((data (dvc-fileinfo-legacy-data fileinfo)))
           (cond
            ((eq (car data) 'file)
             fileinfo)

            (t
             (error "not on a file or directory")))))

        (dvc-fileinfo-message
         (error "not on a file or directory"))))))

(defun dvc-fileinfo-file-or-legacy-file-p (fileinfo)
  "Return t if FILEINFO is a dvc-fileinfo-file, or a dvc-fileinfo-legacy
containing a 'file."
  (or (dvc-fileinfo-file-p fileinfo)
      (and (dvc-fileinfo-legacy-p fileinfo)
           (eq 'file (car (dvc-fileinfo-legacy-data fileinfo))))))

(defun dvc-fileinfo-path (fileinfo)
  "Return directory and file from fileinfo, as a string."
  (etypecase fileinfo
    (dvc-fileinfo-file
     (concat (dvc-fileinfo-file-dir fileinfo)
             (dvc-fileinfo-file-file fileinfo)))

    (dvc-fileinfo-legacy
     (let ((data (dvc-fileinfo-legacy-data fileinfo)))
       (if (eq 'file (car data))
           (cadr data)
         (error "Not on a file entry"))))))

(defun dvc-fileinfo-current-file ()
  "Return a string giving the filename (including path from root)
of the file element on the line at point. Throws an error if
point is not on a file element line."
  (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
    (etypecase fileinfo
      (dvc-fileinfo-file                ; also matches dvc-fileinfo-dir
       (dvc-fileinfo-path fileinfo))

      (dvc-fileinfo-legacy
       (cadr (dvc-fileinfo-legacy-data fileinfo))))))

(defun dvc-fileinfo-all-files ()
  "Return list of all files (as strings) in file list"
  (let (result)
    (ewoc-map
     (lambda (fileinfo)
       (when (dvc-fileinfo-file-or-legacy-file-p fileinfo)
         ;; we use 'add-to-list', because some back-ends put files in
         ;; the ewoc more than once
         (add-to-list 'result (dvc-fileinfo-path fileinfo)))
       nil)
     dvc-fileinfo-ewoc)
    result))

(defun dvc-fileinfo-delete-messages ()
  "Remove all message elements from the ewoc."
  (ewoc-filter dvc-fileinfo-ewoc
               (lambda (fileinfo)
                 (not (dvc-fileinfo-message-p fileinfo)))))

(defun dvc-fileinfo-kill ()
  "Remove the current element(s) from the ewoc."
  (interactive)

  (if (= 0 (length dvc-buffer-marked-file-list))
      ;; no marked files
      (progn
        ;; binding inhibit-read-only doesn't seem to work here
        (toggle-read-only 0)
        (dvc-ewoc-delete dvc-fileinfo-ewoc (ewoc-locate dvc-fileinfo-ewoc))
        (toggle-read-only 1))
    ;; marked files
    (setq dvc-buffer-marked-file-list nil)
    (ewoc-filter dvc-fileinfo-ewoc
                 (lambda (fileinfo)
                   (not (dvc-fileinfo-file-mark fileinfo)))
                 )))

(defun dvc-fileinfo-mark-dir-1 (fileinfo mark)
  ;; `dir-compare' must be let-bound to the directory being marked.
  ;; It can't be a normal parameter because this is called via ewoc-map.
  (if (string-equal dir-compare (dvc-fileinfo-file-dir fileinfo))
      (let ((file (dvc-fileinfo-path fileinfo)))
        (if (dvc-fileinfo-file-exclude fileinfo)
            (if mark
                (message "not marking %s; excluded" file))
          (setf (dvc-fileinfo-file-mark fileinfo) mark)
          (if mark
              (add-to-list 'dvc-buffer-marked-file-list file)
            (setq dvc-buffer-marked-file-list
                  (delete file dvc-buffer-marked-file-list))))
        (etypecase fileinfo
          (dvc-fileinfo-dir
           (dvc-fileinfo-mark-dir file mark)
           ;; return non-nil so this element is refreshed
           t)

          (dvc-fileinfo-file
           ;; return non-nil so this element is refreshed
           t)))))

(defun dvc-fileinfo-mark-dir (dir mark)
  "Set the mark for all files in DIR to MARK, recursively."
  (let ((dir-compare (file-name-as-directory dir)))
    (ewoc-map (lambda (fileinfo)
                (etypecase fileinfo
                  (dvc-fileinfo-file    ; also matches dvc-fileinfo-dir
                   (dvc-fileinfo-mark-dir-1 fileinfo mark))

                  (dvc-fileinfo-message nil)))
              dvc-fileinfo-ewoc)))

(defun dvc-fileinfo-mark-file-1 (mark)
  "Set the mark for file under point to MARK. If a directory, mark all files
in that directory."
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (fileinfo (ewoc-data current)))
    (etypecase fileinfo
      (dvc-fileinfo-dir
       (let ((file (dvc-fileinfo-path fileinfo)))
         (if (dvc-fileinfo-file-exclude fileinfo)
             (if mark
                 (progn
                   ;; we don't throw an error here, because we might
                   ;; be marking a higher-level directory, and we
                   ;; don't want to skip the rest of it.
                   (ding)
                   (message "not marking %s; excluded" file)))
           ;; not excluded
           (setf (dvc-fileinfo-file-mark fileinfo) mark)
           (if mark
               (add-to-list 'dvc-buffer-marked-file-list file)
             (setq dvc-buffer-marked-file-list
                   (delete file dvc-buffer-marked-file-list)))
           (ewoc-invalidate dvc-fileinfo-ewoc current)
           (dvc-fileinfo-mark-dir file mark))))

      (dvc-fileinfo-file
       (let ((file (dvc-fileinfo-path fileinfo)))
         (if (dvc-fileinfo-file-exclude fileinfo)
             (if mark
                 (progn
                   ;; we don't throw an error here, because we might
                   ;; be marking a higher-level directory, and we
                   ;; don't want to skip the rest of it.
                   (ding)
                   (message "not marking %s; excluded" file)))
           ;; not excluded
           (setf (dvc-fileinfo-file-mark fileinfo) mark)
           (if mark
               (add-to-list 'dvc-buffer-marked-file-list file)
             (setq dvc-buffer-marked-file-list
                   (delete file dvc-buffer-marked-file-list)))
           (ewoc-invalidate dvc-fileinfo-ewoc current))))

      (dvc-fileinfo-message
       (error "not on a file or directory")))))

(defun dvc-fileinfo-mark-file ()
  "Mark the file under point. If a directory, mark all files in
that directory. Then move to next ewoc entry."
  (interactive)
  (dvc-fileinfo-mark-file-1 t)
  (dvc-fileinfo-next))

(defun dvc-fileinfo-unmark-file (&optional prev)
  "Unmark the file under point. If a directory, unmark all files
in that directory. If PREV non-nil, move to previous ewoc entry;
otherwise move to next."
  (interactive)
  (dvc-fileinfo-mark-file-1 nil)
  (if prev
      (dvc-fileinfo-prev)
    (dvc-fileinfo-next)))

(defun dvc-fileinfo-unmark-file-up ()
  "Unmark the file under point. If a directory, unmark all files
in that directory. Then move to previous ewoc entry."
  (interactive)
  (dvc-fileinfo-unmark-file t))

(defun dvc-fileinfo-mark-all ()
  "Mark all files and directories."
  (interactive)
  (ewoc-map (lambda (fileinfo)
              (etypecase fileinfo
                (dvc-fileinfo-file      ; also matches dvc-fileinfo-dir
                 (if (dvc-fileinfo-file-exclude fileinfo)
                     (progn
                       (message "not marking %s; excluded" (dvc-fileinfo-path fileinfo))
                       ;; don't need to refresh
                       nil)
                   (setf (dvc-fileinfo-file-mark fileinfo) t)
                   (add-to-list 'dvc-buffer-marked-file-list
                                (dvc-fileinfo-path fileinfo))
                   ;; return non-nil so this element is refreshed
                   t))

                (dvc-fileinfo-message
                 nil)))
            dvc-fileinfo-ewoc))

(defun dvc-fileinfo-unmark-all ()
  "Unmark all files and directories."
  (interactive)
  (setq dvc-buffer-marked-file-list nil)
  (ewoc-map (lambda (fileinfo)
              (etypecase fileinfo
                (dvc-fileinfo-file      ; also matches dvc-fileinfo-dir
                 (if (dvc-fileinfo-file-mark fileinfo)
                     (progn
                       (setf (dvc-fileinfo-file-mark fileinfo) nil)
                       ;; return non-nil so this element is refreshed
                       t)))

                (dvc-fileinfo-message
                 nil)))
            dvc-fileinfo-ewoc))

(defun dvc-fileinfo-toggle-exclude ()
  "Toggle exclude for file under point. Does not edit default exclude file."
  (interactive)
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (fileinfo (ewoc-data current)))
    (typecase fileinfo
      (dvc-fileinfo-file
       (if (dvc-fileinfo-file-mark fileinfo)
           (error "Cannot exclude marked file"))
       (setf (dvc-fileinfo-file-exclude fileinfo)
             (not (dvc-fileinfo-file-exclude fileinfo)))
       (ewoc-invalidate dvc-fileinfo-ewoc current))

      (otherwise
       (error "not on a file or directory")))))

(defun dvc-fileinfo-next (&optional no-ding)
  "Move to the next ewoc entry. If optional NO-DING, don't ding
if there is no next."
  (interactive)
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (cur-location (ewoc-location current))
         (next (ewoc-next dvc-fileinfo-ewoc current)))
    (cond
     ((> cur-location (point))
      ;; not exactly at an element; move there
      (goto-char cur-location))

     (next
      (goto-char (ewoc-location next)))

     (t
      ;; at last element
      (unless no-ding (ding))))))

(defun dvc-fileinfo-prev (&optional no-ding)
  "Move to the previous ewoc entry. If optional NO-DING, don't ding
if there is no prev."
  (interactive)
  (let* ((current (ewoc-locate dvc-fileinfo-ewoc))
         (cur-location (ewoc-location current))
         (prev (ewoc-prev dvc-fileinfo-ewoc current)))
    (cond
     ((> (point) cur-location)
      (goto-char cur-location))

     (prev
      (goto-char (ewoc-location prev)))

     (t
      ;; at first element
      (unless no-ding (ding))))))

(defun dvc-fileinfo-find-file (file)
  "Return ewoc element for FILE (full path)."
  (let ((elem (ewoc-nth dvc-fileinfo-ewoc 0)))
    (while
        (and elem
             (let ((fileinfo (ewoc-data elem)))
               (not (and
                     (dvc-fileinfo-file-or-legacy-file-p fileinfo)
                     (string= (expand-file-name (dvc-fileinfo-path fileinfo))
                              file)))))
      ;; not found yet
      (setq elem (ewoc-next dvc-fileinfo-ewoc elem)))
    (if elem
        elem
      (error "Can't find file %s in list" file))))

(defun dvc-fileinfo-marked-elems ()
  "Return list of ewoc elements that are marked files."
  ;; This does _not_ include legacy fileinfo structs; they do not
  ;; contain a mark field. We are planning to eventually eliminate
  ;; dvc-buffer-marked-file-list and legacy fileinfos.
  (let ((elem (ewoc-nth dvc-fileinfo-ewoc 0))
        result)
    (while elem
      (let ((fi (ewoc-data elem)))
        (if (and (dvc-fileinfo-file-p fi)
                 (dvc-fileinfo-file-mark fi))
            (setq result (append result (list elem))))
        (setq elem (ewoc-next dvc-fileinfo-ewoc elem))))
    result))

(defun dvc-fileinfo-excluded-files ()
  "Return list of filenames that are excluded files."
  ;; This does _not_ include legacy fileinfo structs; they do not
  ;; contain an excluded field.
  (let ((elem (ewoc-nth dvc-fileinfo-ewoc 0))
        result)
    (while elem
      (let ((fi (ewoc-data elem)))
        (if (and (dvc-fileinfo-file-p fi)
                 (dvc-fileinfo-file-exclude fi))
            (setq result (append result (list (dvc-fileinfo-path fi)))))
        (setq elem (ewoc-next dvc-fileinfo-ewoc elem))))
    result))

(defun dvc-fileinfo-same-status (elems)
  "If all ELEMS (list of ewoc elements with data of class
  dvc-fileinfo-file) have same status, return t. Otherwise
  throw an error."
  (if (null elems)
      t
    (let (status)
      (dolist (elem elems)
        (let ((fileinfo (ewoc-data elem)))
          (if status
              (if (not (equal status (dvc-fileinfo-file-status fileinfo)))
                  (error (concat "cannot Do The Right Thing on files with"
                                 " different status")))
            (setq status (dvc-fileinfo-file-status fileinfo)))))
      status)))

;;; actions
(defun dvc-fileinfo-add-log-entry-1 (fileinfo other-frame)
  "Add an entry in the current log-edit buffer for FILEINFO.
If OTHER-FRAME (default prefix) xor `dvc-log-edit-other-frame' is
non-nil, show log-edit buffer in other frame."
  (dvc-log-edit other-frame t)
  (undo-boundary)
  (goto-char (point-max))
  (newline 2)
  (insert "* ")
  (insert (dvc-fileinfo-path fileinfo))
  (insert ": ")

  (if (typep fileinfo 'dvc-fileinfo-file)
      (ecase (dvc-fileinfo-file-status fileinfo)
        (added
         (insert "New file.")
         (newline))

        ((copy-source copy-target)
         (insert "copied")
         (newline))

        ((rename-source rename-target)
         (insert "renamed")
         (newline))

        ((conflict
          deleted
          ignored
          invalid
          known
          missing
          modified
          unknown)
         nil))))

(defun dvc-fileinfo-add-log-entry (&optional other-frame)
  "Add an entry in the current log-edit buffer for the current file.
If OTHER-FRAME (default prefix) xor `dvc-log-edit-other-frame' is
non-nil, show log-edit buffer in other frame."
  (interactive "P")
  (dvc-fileinfo-add-log-entry-1 (dvc-fileinfo-current-fileinfo) other-frame))

(defun dvc-fileinfo-remove-files ()
  "Remove current files. If status `unknown', delete from
workspace. Otherwise, call `dvc-remove-files'."
  (interactive)
  (let ((elems (or (dvc-fileinfo-marked-elems)
                   (list (ewoc-locate dvc-fileinfo-ewoc))))
        (inhibit-read-only t)
        known-files)

    (while elems
      (let ((fileinfo (ewoc-data (car elems))))
        (typecase fileinfo
          (dvc-fileinfo-file
           (if (equal 'unknown (dvc-fileinfo-file-status fileinfo))
               (progn
                 (delete-file (dvc-fileinfo-path fileinfo))
                 (dvc-ewoc-delete dvc-fileinfo-ewoc (car elems)))
             ;; `add-to-list' gets a stack overflow here
             (setq known-files (cons (car elems) known-files))))

          (dvc-fileinfo-legacy
           ;; Assume files are known
           (add-to-list known-files fileinfo))

          (otherwise
           ;; just ignore
           nil))
        (setq elems (cdr elems))))

    (if known-files
        (progn
          (apply 'dvc-remove-files
                 (mapcar (lambda (elem)
                           (dvc-fileinfo-path (ewoc-data elem)))
                         known-files))
          (mapc
           (lambda (elem)
             (let ((fileinfo (ewoc-data elem)))
               (etypecase fileinfo
                 (dvc-fileinfo-file
                  (setf (dvc-fileinfo-file-status fileinfo) 'deleted)
                  (ewoc-invalidate dvc-fileinfo-ewoc elem))

                 (dvc-fileinfo-legacy
                  ;; Don't have enough info to update this
                  nil))))
           known-files)))))

(defun dvc-fileinfo--do-rename (fi-source fi-target elems)
  (dvc-rename (dvc-fileinfo-path fi-source)
              (dvc-fileinfo-path fi-target))
  (setf (dvc-fileinfo-file-status fi-source) 'rename-source)
  (setf (dvc-fileinfo-file-status fi-target) 'rename-target)
  (setf (dvc-fileinfo-file-mark fi-source) nil)
  (setf (dvc-fileinfo-file-mark fi-target) nil)
  (apply 'ewoc-invalidate dvc-fileinfo-ewoc elems))

(defun dvc-fileinfo-rename ()
  "Record a rename for two currently marked files.
One file must have status `missing', the other `unknown'."
  (interactive)
  (let* ((elems (dvc-fileinfo-marked-elems))
         (fis (mapcar 'ewoc-data elems))
         (stati (mapcar 'dvc-fileinfo-file-status fis)))

    (if (not (= 2 (length stati)))
        (error "rename requires exactly 2 marked files"))

    (cond
     ((and (eq 'missing (nth 0 stati))
           (eq 'unknown (nth 1 stati)))
      (dvc-fileinfo--do-rename (nth 0 fis) (nth 1 fis) elems))

     ((and (eq 'missing (nth 1 stati))
           (eq 'unknown (nth 0 stati)))
      (dvc-fileinfo--do-rename (nth 1 fis) (nth 0 fis) elems))

     (t
      (error (concat "must rename from a file with status `missing' to a"
                     " file with status `unknown'"))))))

(defun dvc-fileinfo-rename-possible (marked-elems)
  "Return nil if `dvc-fileinfo-rename' will throw an error for
MARKED-ELEMS, non-nil otherwise."
  (and
   marked-elems
   (= 2 (length marked-elems))
   (let* ((fis (mapcar 'ewoc-data marked-elems))
          (stati (mapcar 'dvc-fileinfo-file-status fis)))
     (or
      (and (eq 'missing (nth 0 stati))
           (eq 'unknown (nth 1 stati)))

      (and (eq 'missing (nth 1 stati))
           (eq 'unknown (nth 0 stati)))))))

(provide 'dvc-fileinfo)
;;; end of file

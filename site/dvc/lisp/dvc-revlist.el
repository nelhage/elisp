;;; dvc-revlist.el --- Revision list in DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; Generic stuff to display revision lists.
;; Revision lists are the core of the "decentralized" aspect of DVC.

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  (require 'dvc-lisp)
  (require 'dvc-utils)
  (require 'dvc-core)
  )

(require 'dvc-ui)

;; Display parameters
(defvar dvc-revlist-brief nil)
(make-variable-buffer-local 'dvc-revlist-brief)

(defvar dvc-revlist-last-n nil
  "Buffer-local value of dvc-log-last-n.")
(make-variable-buffer-local 'dvc-revlist-last-n)

(defvar dvc-revlist-path nil)
(make-variable-buffer-local 'dvc-revlist-path)

(defstruct (dvc-revlist-entry-patch)
  dvc ;; the back-end
  marked
  struct
  rev-id ;; DVC revision ID.
  merged-by
  log-buffer
  diff-buffer)

(defvar dvc-revlist-cookie nil
  "Ewoc cookie for dvc-revlist.")

;; elem of dvc-revlist-cookie should be one of:
;; ('separator "string" kind)
;;    `kind' is: one of
;;    partner: ???
;;    bookmark: ???
;;
;; ('entry-patch struct)
;;    `struct' is a dvc-revlist-entry-patch struct type.
;;
;; ('entry-change "changes")
;;
;; ('message "message")
;;
;; The second element tells if the element is marked or not.

(defun dvc-revlist-printer (elem)
  "Print an element ELEM of the revision list."
  (let ()
    (case (car elem)
      (entry-patch
       (funcall
        (dvc-function (dvc-revlist-entry-patch-dvc (nth 1 elem))
                      "revision-list-entry-patch-printer" t) (nth 1 elem)))
      (entry-change (insert  (cadr elem)))
      (message (insert (dvc-face-add (cadr elem)
                                     'dvc-messages)))
      (separator
       (case (car (cddr elem))
         (partner (insert "\n" (dvc-face-add (cadr elem)
                                             'dvc-separator)))
         (bookmark (insert "\n" (dvc-face-add
                                 (concat "*** "
                                         (cadr elem)
                                         " ***")
                                 'dvc-separator) "\n")))))))

(dvc-make-move-fn ewoc-next dvc-revision-next
  dvc-revlist-cookie)

(dvc-make-move-fn ewoc-prev dvc-revision-prev
  dvc-revlist-cookie)

(dvc-make-move-fn ewoc-next dvc-revision-next-unmerged
  dvc-revlist-cookie t)

(dvc-make-move-fn ewoc-prev dvc-revision-prev-unmerged
  dvc-revlist-cookie t)

(defun dvc-revlist-current-patch ()
  "Get the dvc-revlist-entry-patch at point."
  (nth 1 (ewoc-data (ewoc-locate dvc-revlist-cookie))))

(defun dvc-revlist-current-patch-struct ()
  "Get the dvc-revlist-entry-patch-struct at point."
  (dvc-revlist-entry-patch-struct (dvc-revlist-current-patch)))

(defun dvc-revision-mark-revision ()
  "Mark revision at point."
  (interactive)
  (let* ((pos (point))
         (current (ewoc-locate
                   dvc-revlist-cookie))
         (data (ewoc-data current)))
    (setf (dvc-revlist-entry-patch-marked (nth 1 data)) t)
    (ewoc-invalidate dvc-revlist-cookie current)
    (goto-char pos)
    (dvc-revision-next)))

(defun dvc-revision-marked-revisions ()
  "Return the revisions that are currently marked."
  (let ((acc '()))
    (ewoc-map (lambda (x) (when (and (eq (car x) 'entry-patch)
                                     (dvc-revlist-entry-patch-marked
                                      (cadr x)))
                            (push (dvc-revlist-entry-patch-struct
                                   (nth 1 x)) acc)))
              dvc-revlist-cookie)
    (nreverse acc)))

(defun dvc-revision-unmark-revision ()
  "Unmark the revision at point."
  (interactive)
  (let* ((pos (point))
         (current (ewoc-locate
                   dvc-revlist-cookie))
         (data (ewoc-data current)))
    (setf (dvc-revlist-entry-patch-marked (nth 1 data)) nil)
    (ewoc-invalidate dvc-revlist-cookie current)
    (goto-char pos)
    (dvc-revision-next)))

;; TODO bind this to something
(defun dvc-revision-unmark-all ()
  "Unmark all revisions."
  (interactive)
  (let ((pos (point)))
    (ewoc-map (lambda (x) (when (and (eq (car x) 'entry-patch)
                                     (nth 2 x))
                            (setcar (cddr x) nil)))
              dvc-revlist-cookie)
    (ewoc-refresh dvc-revlist-cookie)
    (goto-char pos)))


(defcustom dvc-revisions-shows-summary t
  "*Whether summary should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom dvc-revisions-shows-creator t
  "*Whether creator should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defcustom dvc-revisions-shows-date t
  "*Whether date should be displayed for `dvc-revisions'."
  :type 'boolean
  :group 'tla-revisions)

(defun dvc-revision-refresh-maybe ()
  (let ((refresh-fn
         (dvc-function (dvc-current-active-dvc)
                       "revision-refresh-maybe" t)))
    (when (fboundp refresh-fn)
      (funcall refresh-fn))))

(defun dvc-revlist-toggle-date ()
  "Toggle display of the date in the revision list."
  (interactive)
  (setq dvc-revisions-shows-date (not dvc-revisions-shows-date))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun dvc-revlist-toggle-summary ()
  "Toggle display of the summary information in the revision list."
  (interactive)
  (setq dvc-revisions-shows-summary (not dvc-revisions-shows-summary))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun dvc-revlist-toggle-creator ()
  "Toggle display of the creator in the revision list."
  (interactive)
  (setq dvc-revisions-shows-creator (not dvc-revisions-shows-creator))
  (dvc-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun dvc-revlist-more (&optional delta)
  "If revision list was limited by `dvc-log-last-n', show more revisions.
Increment DELTA may be specified interactively; default 10."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (if dvc-revlist-last-n
      (progn
        (setq dvc-revlist-last-n (+ dvc-revlist-last-n delta))
        (dvc-generic-refresh))))

(defun dvc-revlist-toggle-brief ()
  "Toggle between brief and full revisions."
  (interactive)
  (setq dvc-revlist-brief (not dvc-revlist-brief))
  (dvc-generic-refresh))

(defvar dvc-get-revision-info-at-point-function nil
  "Variable should be local to each buffer.
Function used to get the revision info at point")

(defun dvc-get-info-at-point ()
  "Get the version information that point is on."
  (when (fboundp dvc-get-revision-info-at-point-function)
    (funcall dvc-get-revision-info-at-point-function)))

(defun dvc-revlist-get-revision-at-point ()
  "Retrieve the revision structure at point in a DVC revlist mode buffer."
  (let* ((entry (dvc-revlist-entry-patch-rev-id
                 (nth 1 (ewoc-data (ewoc-locate dvc-revlist-cookie)))))
         (type (dvc-revision-get-type entry))
         (data (dvc-revision-get-data entry)))
    (case type
      (revision (nth 0 data))
      (t (error "No revision at point")))))

(autoload 'dvc-revlog-revision "dvc-revlog")

(defun dvc-revlist-show-item (&optional scroll-down)
  "Show a changeset for the current revision."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate
                          dvc-revlist-cookie)))
        (dvc-temp-current-active-dvc (dvc-current-active-dvc)))
    (case (car elem)
      (entry-patch
       ;; reuse existing buffer if possible
       (let ((buffer (dvc-revlist-entry-patch-log-buffer
                      (nth 1 elem)))
             (log-buf (current-buffer)))
         (if (and buffer (buffer-live-p buffer))
             (dvc-buffer-show-or-scroll buffer scroll-down)
           (setq buffer (setf (dvc-revlist-entry-patch-log-buffer
                               (nth 1 elem))
                              (dvc-revlog-revision
                               (dvc-revlist-entry-patch-rev-id (nth 1 elem)))))
           (with-current-buffer buffer
             ;; goto the beginning of the shown buffer
             (goto-char (point-min))))
         (pop-to-buffer log-buf)))
      ;; TODO: untested.
      (entry-change (let ((default-directory (car (cddr elem))))
                      (dvc-diff))))))

(defun dvc-revlist-show-item-scroll-down ()
  (interactive)
  (dvc-revlist-show-item t))

(dvc-make-bymouse-function dvc-revlist-show-item)

(defun dvc-revlist-diff (&optional scroll-down)
  "Show the diff for the current revision."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie))))
    (unless (eq (car elem) 'entry-patch)
      (error "Cursor is not on a revision."))
    ;; get the buffer from the ewoc structure.
    (let ((buffer (dvc-revlist-entry-patch-diff-buffer
                   (nth 1 elem)))
          (log-buf (current-buffer)))
      (dvc-trace "buffer1=%S" buffer)
      (if (and buffer (buffer-live-p buffer))
          (dvc-buffer-show-or-scroll buffer scroll-down)
        (setf (dvc-revlist-entry-patch-diff-buffer
               (nth 1 elem))
              (let* ((rev-id (dvc-revlist-entry-patch-rev-id (nth 1 elem)))
                     (rev-type (dvc-revision-get-type rev-id))
                     (rev-data (dvc-revision-get-data rev-id)))
                (unless (eq rev-type 'revision)
                  (error "Only 'revision type is supported here. Got %S" rev-type))
                (let* ((prev-rev-id `(,(car rev-id) (previous-revision
                                                     ,(cadr rev-id) 1))))
                  ;;(dvc-trace "prev-rev-id=%S" prev-rev-id)
                  ;;(dvc-trace "rev-id=%S" rev-id)
                  (dvc-delta prev-rev-id rev-id))))
        (setq buffer (dvc-revlist-entry-patch-diff-buffer
                      (nth 1 elem)))
        (dvc-trace "buffer2=%S" buffer))
      (with-current-buffer buffer
        (setq dvc-partner-buffer log-buf))
      (pop-to-buffer log-buf)
      (setq dvc-partner-buffer buffer))))

(defun dvc-revlist-diff-to-current-tree (&optional scroll-down)
  "Show the diff between the revision at point and the local tree."
  (interactive)
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie))))
    (unless (eq (car elem) 'entry-patch)
      (error "Cursor is not on a revision."))
    (dvc-diff (dvc-revlist-entry-patch-rev-id (nth 1 elem)) (dvc-tree-root) nil)))

(defun dvc-revlist-diff-scroll-down ()
  (interactive)
  (dvc-revlist-diff t))

(defvar dvc-revlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] 'dvc-generic-refresh)
    (define-key map [tab] 'dvc-revision-next)
    (define-key map [(control ?i)] 'dvc-revision-next)
    (define-key map [(shift tab)] 'dvc-revision-prev)
    (unless (featurep 'xemacs)
      (define-key map [(shift iso-lefttab)] 'dvc-revision-prev)
      (define-key map [(shift control ?i)] 'dvc-revision-prev))
    (define-key map [?+] 'dvc-revlist-more)
    (define-key map [?b] 'dvc-revlist-toggle-brief)
    (define-key map [?n] 'dvc-revision-next)
    (define-key map [?p] 'dvc-revision-prev)
    (define-key map [?N] 'dvc-revision-next-unmerged)
    (define-key map [?P] 'dvc-revision-prev-unmerged)
    (define-key map [?A] 'dvc-send-commit-notification) ;; Mnemonic: announce
    (define-key map [?E] 'dvc-export-via-email)
    (define-key map "\C-m" 'dvc-revlist-show-item)
    (define-key map [return] 'dvc-revlist-show-item)
    (define-key map [(meta return)] 'dvc-revlist-show-item-scroll-down)
    (define-key map [?=]              'dvc-revlist-diff)
    (define-key map [(control ?=)]    'dvc-revlist-diff-to-current-tree)
    (define-key map [(meta ?=)]       'dvc-revlist-diff-scroll-down)
    (define-key map (dvc-prefix-toggle ?d) 'dvc-revlist-toggle-date)
    (define-key map (dvc-prefix-toggle ?c) 'dvc-revlist-toggle-creator)
    (define-key map (dvc-prefix-toggle ?s) 'dvc-revlist-toggle-summary)
    (define-key map dvc-keyvec-mark   'dvc-revision-mark-revision)
    (define-key map dvc-keyvec-unmark 'dvc-revision-unmark-revision)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'dvc-bookmarks)
    (define-key map (dvc-prefix-merge ?u)                     'dvc-revlist-update)
    (define-key map (dvc-prefix-merge ?U)                     'dvc-update)
    (define-key map (dvc-prefix-merge ?m)  '(lambda () (interactive) (dvc-missing nil default-directory)))
    (define-key map (dvc-prefix-merge ?M)                     'dvc-merge)
    (define-key map dvc-keyvec-inventory 'dvc-pop-to-inventory)
    (define-key map [?h] 'dvc-buffer-pop-to-partner-buffer)
    (define-key map dvc-keyvec-help 'describe-mode)

    (define-key map dvc-keyvec-kill-ring nil)
    (define-key map (dvc-prefix-kill-ring ?l) 'dvc-revision-save-log-message-as-kill)
    map))

(easy-menu-define dvc-revlist-mode-menu dvc-revlist-mode-map
  "`dvc-revlist' menu"
  '("DVC-Revlist"
    ["Diff single rev" dvc-revlist-diff t]
    ["Diff with workspace" dvc-revlist-diff-to-current-tree t]
    ["Update to rev at point" dvc-revlist-update t]
    ["Update to head" dvc-update t]
    ["Merge"  dvc-merge          t]
    ["Show missing" (lambda () (interactive) (dvc-missing nil default-directory)) t]
    ))

;; dvc-revlist-create-buffer will use "<back-end>-revlist-mode", if
;; defined, instead of this one. If so, it should be derived from
;; dvc-revlist-mode (via `define-derived-mode'), and rely on it for as
;; many features as possible (one can, for example, extend the menu
;; and keymap). See `xmtn-revlist-mode' in xgit.el for a good example.
;;
;; Remember to add the new mode to
;; `uniquify-list-buffers-directory-modes' using
;; `dvc-add-uniquify-directory-mode'.
(define-derived-mode dvc-revlist-mode fundamental-mode
  "dvc-revlist"
  "Major mode to show revision list.

Commands are:
\\{dvc-revlist-mode-map}"
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (dvc-install-buffer-menu)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (set (make-local-variable 'dvc-revlist-cookie)
       (ewoc-create (dvc-ewoc-create-api-select
                     #'dvc-revlist-printer)))
  (toggle-read-only 1)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'dvc-revlist-get-rev-at-point))

(dvc-add-uniquify-directory-mode 'dvc-revlist-mode)

(defun dvc-revlist-create-buffer (back-end type location refresh-fn brief last-n)
  "Create (or reuse) and return a buffer to display a revision list.
BACK-END is the the back-end.
TYPE must be in dvc-buffer-type-alist.
LOCATION is root or a buffer name, depending on TYPE."
  (let ((dvc-temp-current-active-dvc back-end)
        (buffer (dvc-get-buffer-create back-end type location)))
    (with-current-buffer buffer
      (funcall (dvc-function back-end "revlist-mode"))
      (setq dvc-buffer-refresh-function refresh-fn)
      (setq dvc-revlist-brief brief)
      (setq dvc-revlist-last-n last-n))
    buffer))

(defun dvc-build-revision-list (back-end type location arglist parser
                                         brief last-n path refresh-fn)
  "Runs the back-end BACK-END to build a revision list.

A buffer of type TYPE with location LOCATION is created or reused.

The back-end is launched with the arguments ARGLIST, and the
caller has to provide the function PARSER which will actually
build the revision list.

BRIEF, if non-nil, means show a brief entry for each revision;
nil means show full entry.

LAST-N limits the number of revisions to display; all if nil.

PATH, if non-nil, restricts the log to that file.

REFRESH-FN specifies the function to call when the user wants to
refresh the revision list buffer.  It must take no arguments."
  (let ((buffer (dvc-revlist-create-buffer back-end type location refresh-fn brief last-n)))
    (with-current-buffer buffer
      (setq dvc-revlist-path path)
      (setq dvc-revlist-brief brief)
      (setq dvc-revlist-last-n last-n))
    (dvc-switch-to-buffer-maybe buffer t)
    (dvc-run-dvc-async
     back-end arglist
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer output
         (funcall (capture parser) (capture buffer) (capture location))))
     :error
     ;; TODO handle error messages, only treat the bzr missing command
     ;; like this (errorcode=1)
     (dvc-capturing-lambda (output error status arguments)
       (with-current-buffer output
         (funcall (capture parser) (capture buffer) (capture location)))))))

(defun dvc-revision-log-message-at-point ()
  (dvc-call "revision-st-message" (dvc-revlist-current-patch-struct)))

(defun dvc-revision-save-log-message-as-kill ()
  "Save the log message for the actual patch."
  (interactive)
  (kill-new (dvc-revision-log-message-at-point)))
;; TODO: (message "Copied log message for %s" (tla-changelog-revision-at-point)))

(defun dvc-revlist-update ()
  "Update current workspace to revision at point"
  (interactive)
  (dvc-update (dvc-revlist-entry-patch-rev-id (dvc-revlist-current-patch))))

(provide 'dvc-revlist)
;;; dvc-revlist.el ends here

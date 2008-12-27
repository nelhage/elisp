;;; dvc-diff.el --- A generic diff mode for DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>

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

(require 'diff-mode)
(require 'dvc-ui)
(require 'dvc-unified)
(require 'dvc-defs)
(require 'dvc-core)
(require 'dvc-fileinfo)
(eval-when-compile
  (require 'cl))

(defvar dvc-diff-base nil
  "BASE revision-id for the changes currently displayed.")
(make-variable-buffer-local 'dvc-diff-base)

(defvar dvc-diff-modified nil
  "MODIFIED revision-id for the changes currently displayed.")
(make-variable-buffer-local 'dvc-diff-modified)

(defun dvc-dvc-search-file-in-diff (file)
  "Default for \"dvc-search-file-in-diff\". Place point on diff hunk for FILE."
  (re-search-forward (concat "^\\+\\+\\+ \\(b\\|mod\\)/" file "\\(.+[0-9][0-9][0-9][0-9]\\)?$")))

(defun dvc-prepare-changes-buffer (base modified type path dvc)
  "Create and return a buffer to run command showing diffs.

Sets `dvc-diff-base' and `dvc-diff-modified' to BASE and
MODIFIED.

TYPE must be found in `dvc-buffer-type-alist'.

PATH must match mode in `dvc-buffer-type-alist' for TYPE.

TYPE and PATH are passed to `dvc-get-buffer-create'."
  (with-current-buffer
      (dvc-get-buffer-create dvc type path)
    (let ((inhibit-read-only t)) (erase-buffer))
    (let ((dvc-temp-current-active-dvc dvc))
      (funcall (dvc-function dvc "diff-mode")))
    (setq dvc-diff-base base)
    (setq dvc-diff-modified modified)
    (current-buffer)))

(defun dvc-diff-chose-face (status modif)
  "Return a face appropriate for STATUS or MODIF."
  (cond
   ((string= "A" status) 'dvc-added)
   ((string= "?" status) 'dvc-unknown)
   ((string= "M" modif) 'dvc-modified)
   ((string= "M" status) 'dvc-modified)
   ((string= "-" modif)  'dvc-modified)
   ((string= "P" status) 'dvc-modified)
   ((string= "C" status) 'dvc-conflict)
   ((string= "D" status) 'dvc-conflict)
   ((string= "R" status) 'dvc-move)
   ((string= " " status) 'default)
   (t
    (dvc-trace "unknown status=%S or modif=%S" status modif)
    'default)))

;; ----------------------------------------------------------------------------
;; dvc-diff-mode
;; ----------------------------------------------------------------------------

(defun dvc-diff-printer (elem)
  "Ewoc pretty-printer for `dvc-fileinfo-legacy'.

Pretty-print ELEM."
  (cond
   ((eq (car elem) 'file)
    (let* ((empty-mark " ")
           (mark (when (member (cadr elem) dvc-buffer-marked-file-list)
                   dvc-mark))
           (file     (nth 1 elem))
           (status   (nth 2 elem))
           (modif    (nth 3 elem))
           (dir      (nth 4 elem))
           (origname (nth 5 elem))
           (line (concat status modif " "
                         (when origname (concat origname dir "\t => "))
                         file dir))
           (face (if mark
                     'dvc-marked
                   (dvc-diff-chose-face status modif))))
      (if mark
          (insert mark)
        (insert empty-mark))
      (insert (dvc-face-add line
                            face
                            'dvc-diff-file-map
                            dvc-diff-file-menu))))
   ((eq (car elem) 'subtree)
    (insert (dvc-face-add
             (concat " T" (cond ((not (cadddr elem)) "?")
                                ((eq  (cadddr elem) 'changes) "M")
                                ((eq  (cadddr elem) 'updated) "U")
                                ((eq  (cadddr elem) 'no-changes) "-"))
                     " " (car (cddr elem)))
             'dvc-nested-tree)))

   ((eq (car elem) 'message)
    (insert (cadr elem)))

   ((eq (car elem) 'searching-subtrees)
    (insert (dvc-face-add " T  Searching for subtrees ..."
                          'dvc-nested-tree))))
  )

(defvar dvc-diff-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))
    (define-key map dvc-keyvec-help                           'describe-mode)
    (define-key map "\C-m"                                    'dvc-diff-jump-to-change)
    (define-key map [return]                                  'dvc-diff-jump-to-change)
    (define-key map [(control x) (control j)]                 'dvc-dired-jump)
    (define-key map "\M-="                                    'dvc-diff-scroll-up-or-diff)
    (define-key map [(meta return)]                           'dvc-diff-scroll-down-or-diff)
    (define-key map "\M-\C-m"                                 'dvc-diff-scroll-down-or-diff)
    (define-key map [?=]                                      'dvc-diff-diff)
    (define-key map dvc-keyvec-add                            'dvc-add-files)
    (define-key map "\M-d"                                    'dvc-diff-dtrt)
    (define-key map "E"                                       'dvc-fileinfo-toggle-exclude)
    (define-key map "\M-e"                                    'dvc-edit-exclude)
    (define-key map [?h]                                      'dvc-buffer-pop-to-partner-buffer)
    (define-key map dvc-keyvec-logs                           'dvc-diff-log-tree)
    (define-key map "l"                                       'dvc-diff-log-single)
    (define-key map dvc-keyvec-ediff                          'dvc-diff-ediff)
    (define-key map dvc-keyvec-refresh                        'dvc-generic-refresh)
    (define-key map "R"                                       'dvc-fileinfo-rename)
    (define-key map dvc-keyvec-commit                         'dvc-log-edit)
    (define-key map "t"                                       'dvc-diff-add-log-entry)

    (define-key map dvc-keyvec-next                           'dvc-diff-next)
    (define-key map dvc-keyvec-previous                       'dvc-diff-prev)
    (define-key map dvc-keyvec-revert                         'dvc-revert-files)
    (define-key map dvc-keyvec-quit                           'dvc-buffer-quit)
    (define-key map dvc-keyvec-remove                         'dvc-fileinfo-remove-files)
    (define-key map [?d]                                      'dvc-remove-files) ; as in dired
    (define-key map dvc-keyvec-mark                           'dvc-diff-mark-file)
    (define-key map dvc-keyvec-mark-all                       'dvc-fileinfo-mark-all)
    (define-key map dvc-keyvec-unmark                         'dvc-diff-unmark-file)
    (define-key map [backspace]                               'dvc-diff-unmark-file-up)
    (define-key map dvc-keyvec-unmark-all                     'dvc-fileinfo-unmark-all)
    (define-key map [?v]                                      'dvc-diff-view-source)
    (define-key map dvc-keyvec-parent                         'dvc-diff-master-buffer)
    (define-key map [?j]                                      'dvc-diff-diff-or-list)
    (define-key map (dvc-prefix-kill-ring ?d)                 'dvc-diff-save-current-defun-as-kill)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p)                    'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?e)                    'dvc-show-last-error-buffer)
    (define-key map (dvc-prefix-buffer ?L)                    'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'dvc-bookmarks)

    ;; Ignore file handling
    (define-key map (dvc-prefix-tagging-method ?i)            'dvc-ignore-files)
    (define-key map (dvc-prefix-tagging-method ?I)            'dvc-ignore-file-extensions)
    (define-key map (dvc-prefix-tagging-method ?e)            'dvc-edit-ignore-files)
    (define-key map [?i]                                      'dvc-ignore-files)
    (define-key map [?I]                                      'dvc-ignore-file-extensions-in-dir)
    (define-key map "\M-I"                                    'dvc-ignore-file-extensions)

    ;; working copy bindings
    (define-key map (vector dvc-key-working-copy) nil) ;; unbind ?W, before it can be used
    (define-key map (dvc-prefix-working-copy ?s)              'dvc-save-diff)

    ;; the merge group
    (define-key map (dvc-prefix-merge ?u)                     'dvc-update)
    (define-key map (dvc-prefix-merge ?f)                     'dvc-pull) ;; hint: fetch, p is reserved for push
    (define-key map (dvc-prefix-merge ?m)  '(lambda () (interactive) (dvc-missing nil default-directory)))
    (define-key map (dvc-prefix-merge ?M)                     'dvc-merge)
    map)
  "Keymap used in `dvc-diff-mode'.")

;;
;; Menu
;;
(defconst dvc-diff-file-menu-list
  '("File Changes"
    ["Jump to File"                   dvc-diff-jump-to-change t]
    ["Jump to Diffs"                  dvc-diff-diff-or-list   t]
    ["View Diff in Separate Buffer"   dvc-diff-diff           t]
    ["View Diff with Ediff"           dvc-diff-ediff          t]
    ["Log (full tree)"                dvc-diff-log-tree       t]
    ["Log (single file)"              dvc-diff-log-single     t]
    "--"
    ["Delete File"                    dvc-remove-files        t]
    ["Delete File"                    dvc-remove-files        t]
    ["Add File"                       dvc-add-files           t]
    )
  "Used both in the global and the context menu of `dvc-diff-mode'.")

(easy-menu-define dvc-diff-file-menu nil
  "Menu used on a `dvc-diff' file"
  dvc-diff-file-menu-list)

(defconst dvc-diff-mode-menu-list
  `(["Refresh Buffer" dvc-generic-refresh t]
    ["Edit log before commit" dvc-log-edit t]
    ["Add log entry" dvc-add-log-entry t]
    ("Merge"
     ["Update" dvc-update t]
     ["Pull" dvc-pull t]
     ["Show missing" (lambda () (interactive) (dvc-missing nil default-directory)) t]
     ["Merge" dvc-merge t]
     )
    ("Mark"
     ["Mark File"   dvc-diff-mark-file      t]
     ["Mark all"    dvc-fileinfo-mark-all   t]
     ["Unmark File" dvc-diff-unmark-file    t]
     ["Unmark all"  dvc-fileinfo-unmark-all t]
     )
    ("Ignore"
     ["Ignore Files" dvc-ignore-files t]
     ["Ignore File Extensions" dvc-ignore-file-extensions t]
     ["Edit Ignore File" dvc-edit-ignore-files t]
     )
    ("Exclude"
     ["Exclude File" dvc-fileinfo-toggle-exclude t]
     ["Edit Exclude File" dvc-edit-exclude t]
     )
    ,dvc-diff-file-menu-list
    ))

(easy-menu-define dvc-diff-mode-menu dvc-diff-mode-map
  "`dvc-changes' menu"
  `("DVC-Diff"
    ,@dvc-diff-mode-menu-list))

(defvar dvc-diff-file-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'dvc-diff-jump-to-change-by-mouse)
    map)
  "Keymap used on files in `dvc-diff-mode' buffers.")

;; dvc-prepare-changes-buffer will call "<back-end>-diff-mode", if
;; defined, instead of this one. If so, it should be derived from
;; dvc-diff-mode (via `define-derived-mode'), and rely on it for as
;; many features as possible (one can, for example, extend the menu
;; and keymap). See `xgit-diff-mode' in xgit.el for a good example.
;;
;; Remember to add the new mode to
;; `uniquify-list-buffers-directory-modes' using
;; `dvc-add-uniquify-directory-mode'.
(define-derived-mode dvc-diff-mode fundamental-mode "dvc-diff"
  "Major mode to display changesets. Derives from `diff-mode'.

Use '\\<dvc-diff-mode-map>\\[dvc-diff-mark-file]' to mark files, and '\\[dvc-diff-unmark-file]' to unmark.
If you commit from this buffer (with '\\<dvc-diff-mode-map>\\[dvc-log-edit]'), then,
the list of selected files (in this buffer) will be commited (with the text you
entered as a comment) at the time you actually commit with \\<dvc-log-edit-mode-map>\\[dvc-log-edit-done].

Commands:
\\{dvc-diff-mode-map}
"
  (let ((diff-mode-shared-map (copy-keymap dvc-diff-mode-map))
        major-mode mode-name)
    (diff-mode))

  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (setq font-lock-defaults (list 'diff-font-lock-keywords t nil nil))
  (set (make-local-variable 'dvc-get-file-info-at-point-function)
       'dvc-diff-get-file-at-point)
  (setq dvc-buffer-refresh-function 'dvc-diff-generic-refresh)
  (setq dvc-fileinfo-ewoc (ewoc-create 'dvc-fileinfo-printer))
  (setq dvc-buffer-marked-file-list nil)
  (dvc-install-buffer-menu)
  (toggle-read-only 1)
  (set-buffer-modified-p nil))

(dvc-add-uniquify-directory-mode 'dvc-diff-mode)

(defun dvc-diff-generic-refresh ()
  "Refresh the diff buffer."
  (interactive)
  (if (eq (dvc-revision-get-type dvc-diff-modified) 'local-tree)
      ;; Don't specify dvc-diff-base here; it may have changed due to an update
      (dvc-diff)
    (error "Don't know how to refresh buffer")))

(defun dvc-diff-in-ewoc-p ()
  "Return non-nil if in ewoc section of diff buffer."
  (let ((elem (ewoc-locate dvc-fileinfo-ewoc)))
    (when elem
      (>= (ewoc-location elem) (line-beginning-position)))))

(defun dvc-diff-jump-to-change (&optional other-file)
  "Jump to the corresponding file and location of the change at point.
OTHER-FILE (default prefix) if non-nil means visit the original
file; otherwise visit the modified file."
  (interactive "P")
  (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc)))
    (if (dvc-diff-in-ewoc-p)
        (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
          (etypecase fileinfo
            (dvc-fileinfo-file          ; also matches dvc-fileinfo-dir
             ;; FIXME: support OTHER-FILE here
             (find-file (dvc-fileinfo-current-file)))

            (dvc-fileinfo-legacy
             (let ((data (dvc-fileinfo-legacy-data fileinfo)))
               (cond
                ((eq (car data) 'file)
                 (find-file (cadr data)))

                ((eq (car data) 'subtree)
                 (dvc-switch-to-buffer (cadr data)))

                (t (error "Not on a recognized location")))))))

      ;; not in the ewoc part
      (diff-goto-source other-file))))

(defun dvc-diff-scroll-or-diff (up-or-down)
  "If file-diff buffer is visible, scroll. Otherwise, show it."
  (let ((file (dvc-get-file-info-at-point)))
    (unless file
      (error "No file at point."))
    (let ((buffer (dvc-get-buffer dvc-buffer-current-active-dvc 'file-diff file)))
      (unless (dvc-scroll-maybe buffer up-or-down)
        (dvc-file-diff file dvc-diff-base dvc-diff-modified t)))))

(defun dvc-diff-scroll-up-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-up))

(defun dvc-diff-scroll-down-or-diff ()
  (interactive)
  (dvc-diff-scroll-or-diff 'scroll-down))

(defun dvc-diff-diff-or-list ()
  "Jump between list entry and corresponding diff hunk.
When in the list, jump to the corresponding
diff. When on a diff, jump to the corresponding entry in the list."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
        (etypecase fileinfo
          (dvc-fileinfo-file
           (dvc-call "dvc-search-file-in-diff" (dvc-fileinfo-current-file))
           (diff-hunk-next))

          (dvc-fileinfo-legacy
           (let ((data (dvc-fileinfo-legacy-data fileinfo)))
             (cond
              ((eq (car data) 'file)
               (dvc-call "dvc-search-file-in-diff" (cadr data))
               (diff-hunk-next))

              ((eq (car data) 'subtree)
               (dvc-switch-to-buffer (cadr data)))

              (t (error "Not on a recognized location")))))))

    ;; not in list
    (goto-char (ewoc-location (dvc-fileinfo-find-file (dvc-diff-get-file-at-point))))))

(defun dvc-diff-mark-file ()
  "Mark the file under point, and move to next file.
If on a message, mark the group to the next message."
  (interactive)
  (if (not (dvc-diff-in-ewoc-p))
      (error "not in file list"))

  (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
    (etypecase fileinfo
      (dvc-fileinfo-file
       (dvc-fileinfo-mark-file))

      (dvc-fileinfo-message
       (dvc-diff-mark-group))

      (dvc-fileinfo-legacy
       (let ((current (ewoc-locate dvc-fileinfo-ewoc))
             (file (dvc-get-file-info-at-point)))
         (add-to-list 'dvc-buffer-marked-file-list file)
         (ewoc-invalidate dvc-fileinfo-ewoc current)
         (dvc-fileinfo-next))))))

(defun dvc-diff-mark-group (&optional unmark)
  "Mark a group of files.

Must be called with the cursor on a 'message ewoc entry. Marks all
files until the end of the ewoc, or the next ewoc entry which is not
a 'file."
  (if (not (dvc-diff-in-ewoc-p))
      (error "not in file list"))

  (if (not (dvc-fileinfo-message-p (dvc-fileinfo-current-fileinfo)))
      (error "not on a message"))

  (dvc-fileinfo-next)

  (if (not (dvc-fileinfo-file-or-legacy-file-p (dvc-fileinfo-current-fileinfo)))
      (error "next in list is not on a file"))

  (let ((ewoc-elem (ewoc-locate dvc-fileinfo-ewoc)))
    (while (and ewoc-elem
                (ewoc-data ewoc-elem)
                (dvc-fileinfo-file-or-legacy-file-p (ewoc-data ewoc-elem)))
      (let* ((fileinfo (ewoc-data ewoc-elem))
             (file (dvc-fileinfo-path fileinfo)))
        (dvc-trace "mark/unmark %S" file)
        (if (dvc-fileinfo-file-p fileinfo)
            (if unmark
                (dvc-fileinfo-unmark-file)
              (dvc-fileinfo-mark-file))
          ;; legacy
          (if unmark
              (setq dvc-buffer-marked-file-list
                    (delete file dvc-buffer-marked-file-list))
            (add-to-list 'dvc-buffer-marked-file-list file))))
      (setq ewoc-elem (ewoc-next dvc-fileinfo-ewoc ewoc-elem)))

    (ewoc-refresh dvc-fileinfo-ewoc)
    (if ewoc-elem
        (goto-char (ewoc-location ewoc-elem))
      (goto-char (point-max)))))

(defun dvc-diff-unmark-file (&optional up)
  "Unmark the file under point.
If on a message, unmark the group to the next message. If
optional UP, move to previous file first; otherwise move to next
file after."
  (interactive)
  (if (not (dvc-diff-in-ewoc-p))
      (error "not in file list"))

  (if up (dvc-fileinfo-prev t))

  (let ((fileinfo (dvc-fileinfo-current-fileinfo)))
    (etypecase fileinfo
      (dvc-fileinfo-file
       (dvc-fileinfo-mark-file-1 nil))

      (dvc-fileinfo-message
       (dvc-diff-mark-group t))

      (dvc-fileinfo-legacy
       (let ((current (ewoc-locate dvc-fileinfo-ewoc))
             (file (dvc-get-file-info-at-point)))
         (setq dvc-buffer-marked-file-list (delete file dvc-buffer-marked-file-list))
         (ewoc-invalidate dvc-fileinfo-ewoc current)))))

  (unless up (dvc-fileinfo-next)))

(defun dvc-diff-unmark-file-up ()
  "Unmark the file under point and move up."
  (interactive)
  (dvc-diff-unmark-file t))

(defun dvc-diff-diff ()
  "Show diff for file at point."
  (interactive)
  (let ((on-modified-file (dvc-get-file-info-at-point)))
    (if on-modified-file
        (let ((buf (current-buffer)))
          (dvc-file-diff on-modified-file dvc-diff-base
                         dvc-diff-modified t)
          (pop-to-buffer buf))
      (error "Not on a modified file"))))

(defun dvc-diff-next ()
  "Move to the next list line or diff hunk."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-next)
    (diff-hunk-next)))

(defun dvc-diff-prev ()
  "Move to the previous list line or diff hunk."
  (interactive)
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-prev)
    (diff-hunk-prev)))

(defun dvc-diff-ediff ()
  "Run ediff on the current changes."
  (interactive)
  (unless (and (car dvc-diff-base)
               (car dvc-diff-modified))
    (error "No revision information to base ediff on"))
  (let ((on-modified-file (dvc-get-file-info-at-point))
        (loc (point)))

    (if (and on-modified-file
             (dvc-diff-in-ewoc-p))
        ;; on ewoc item; just ediff
        (dvc-file-ediff-revisions on-modified-file
                                  dvc-diff-base
                                  dvc-diff-modified)
      ;; in diff section; find hunk index, so we can jump to it in the ediff.
      (end-of-line)
      (dvc-trace "loc=%S" loc)
      (let ((hunk 1))
        (re-search-backward "^--- ")
        (re-search-forward "^--- ")
        (diff-hunk-next)
        (while (<= (re-search-forward "\\(^[\\+-].*\n\\)+" nil t) loc)
          (dvc-trace "hunk=%S" hunk)
          (setq hunk (1+ hunk)))
        (goto-char loc)
        (with-current-buffer
            (dvc-file-ediff-revisions on-modified-file
                                      dvc-diff-base
                                      dvc-diff-modified)
          (ediff-jump-to-difference hunk))))))

(defun dvc-diff-log-single (&optional last-n)
  "Show log for current file, LAST-N entries (default
`dvc-log-last-n'; all if nil). LAST-N may be specified
interactively."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) dvc-log-last-n)))
  (dvc-log (dvc-get-file-info-at-point) last-n))

(defun dvc-diff-log-tree (&optional last-n)
  "Show log for current tree, LAST-N entries (default
`dvc-log-last-n'; all if nil). LAST-N may be specified
interactively."
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) dvc-log-last-n)))
  (dvc-log nil last-n))

(defun dvc-diff-find-file-name ()
  "Same as `diff-find-file-name', but works in more cases."
  (cond ((re-search-backward "^\\+\\+\\+ \\(mod/\\|b/\\)?\\([^\n]*?\\)\\([ \t].*\\)?$" nil t)
         (match-string-no-properties 2))
        ((not (ewoc-locate dvc-fileinfo-ewoc (point))) ;; the buffer contains no diff
         "")
        (t
         (diff-find-file-name))))

(defun dvc-diff-get-file-at-point ()
  "Return filename for file at point.
Throw an error when not on a file."
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-current-file)
    (save-excursion
      (expand-file-name (concat (file-name-as-directory
                                 default-directory)
                                (dvc-diff-find-file-name))))))

(defun dvc-diff-add-log-entry (&optional other-frame)
  "Add a log entry for file or diff hunk at point."
  (interactive "P")
  (if (dvc-diff-in-ewoc-p)
      (dvc-fileinfo-add-log-entry other-frame)
    (dvc-add-log-entry other-frame)))

(defvar dvc-header nil
  "Free variable used to pass info from the parser to
`dvc-show-changes-buffer' (defined with a (let ...) in
dvc-show-changes-buffer, and altered by called functions).

This is just a lint trap.")

(defun dvc-show-changes-buffer (buffer parser &optional
                                       output-buffer no-switch
                                       header-end-regexp cmd)
  "Show the *{dvc}-diff* buffer built from the *{dvc}-process* BUFFER.
default-directory of process buffer must be a tree root.

PARSER is a function to parse the diff and fill in the
dvc-fileinfo-ewoc list; it will be called with one arg,
OUTPUT-BUFFER. Data to be parsed will be in current buffer.
dvc-header will have been set as described below. After PARSER is
called, dvc-header is set as the dvc-fileinfo-ewoc header, and
OUTPUT-BUFFER contents are set as the dvc-fileinfo-ewoc footer.

Display changes in OUTPUT-BUFFER (must be non-nil; create with
dvc-prepare-changes-buffer).

If NO-SWITCH is nil, don't switch to the created buffer.

If non-nil, HEADER-END-REGEXP is a regexp matching the first line
which is not part of the diff header. Lines preceding
HEADER-END-REGEXP are copied into dvc-header.

CMD, if non-nil, is prepended to dvc-header."
  ;; We assume default-directory is correct, rather than calling
  ;; dvc-tree-root, because dvc-tree-root might prompt if there is
  ;; more than one back-end present. Similarly, we assume
  ;; output-buffer is created, to avoid calling dvc-current-active-dvc
  ;; for dvc-get-buffer-create.
  (let* ((root (with-current-buffer buffer default-directory))
         (dvc (dvc-current-active-dvc))
         (changes-buffer output-buffer)
         (dvc-header ""))
    (if (or no-switch dvc-switch-to-buffer-first)
        (set-buffer changes-buffer)
      (dvc-switch-to-buffer changes-buffer))
    (let (buffer-read-only)
      (dvc-fileinfo-delete-messages)
      (with-current-buffer buffer
        (goto-char (point-min))
        (when cmd
          (setq dvc-header
                (concat (dvc-face-add cmd 'dvc-header) "\n"
                        (dvc-face-add (make-string  72 ?\ ) 'dvc-separator))))
        (when header-end-regexp
          (setq dvc-header
                (concat dvc-header
                        (buffer-substring-no-properties
                         (goto-char (point-min))
                         (progn (re-search-forward header-end-regexp nil t) ;; "^[^*\\.]"
                                (beginning-of-line)
                                (point))))))
        (beginning-of-line)
        (funcall parser changes-buffer)
        ;; Footer is back-end output from point to end-of-buffer; should be the diff output.
        (let ((footer (concat
                       (dvc-face-add (make-string  72 ?\ ) 'dvc-separator)
                       "\n\n"
                       (buffer-substring-no-properties
                        (point) (point-max)))))
          (with-current-buffer changes-buffer
            (ewoc-set-hf dvc-fileinfo-ewoc dvc-header footer)
            (if root (cd root)))))))
  (toggle-read-only 1)
  (if (progn (goto-char (point-min))
             (re-search-forward "^---" nil t))
      (when (or global-font-lock-mode font-lock-mode)
        (let ((font-lock-verbose nil))
          (font-lock-fontify-buffer)))
    ;; Disabling font-lock mode (it's useless and it removes other
    ;; faces with Emacs 21)
    (setq font-lock-keywords nil)
    (font-lock-mode -1)
    (ewoc-refresh dvc-fileinfo-ewoc))
  (if (ewoc-nth dvc-fileinfo-ewoc 0)
      (goto-char (ewoc-location (ewoc-nth dvc-fileinfo-ewoc 0)))))

(defun dvc-diff-no-changes (diff-buffer msg dir)
  "Function to call from diff parser when there are no changes in a tree.

Inserts a message in the changes buffer, and in the minibuffer.

DIFF-BUFFER is the buffer prepared by dvc-prepare-changes-buffer.
MSG is a format string for a message to the user.
DIR is a string, passed to `format' with MSG."
  (with-current-buffer diff-buffer
    (let ((inhibit-read-only t))
      (dvc-fileinfo-delete-messages)
      (ewoc-enter-last dvc-fileinfo-ewoc
                       (make-dvc-fileinfo-message
                        :text (concat "* " (format msg dir) ".\n\n")))
      (ewoc-refresh dvc-fileinfo-ewoc)
      (recenter '(4))))
  (message msg dir))

(defun dvc-diff-error-in-process (diff-buffer msg output error)
  "Enter a message in DIFF-BUFFER (created by
dvc-prepare-changes-buffer), consisting of MSG and the contents of
OUTPUT and ERROR. Should be called by the error handler in the
diff parser."
  (with-current-buffer diff-buffer
    (let ((inhibit-read-only t))
      (dvc-fileinfo-delete-messages)
      (ewoc-enter-last dvc-fileinfo-ewoc
                       (make-dvc-fileinfo-message
                        :text (concat "* " msg ":\n"
                                      (dvc-buffer-content output)
                                      (dvc-buffer-content error))))
      (ewoc-refresh dvc-fileinfo-ewoc)
      (recenter)))
  (message msg))

(defun dvc-diff-clear-buffers (dvc root msg &optional header)
  "Clears all DVC diff and status buffers with root ROOT, insert MSG and optional HEADER.
Useful to clear diff buffers after a commit."
  (dvc-trace "dvc-diff-clear-buffers (%S %S)" root msg)
  ;; Don't need to clear 'revision-diff; that is not changed by a commit
  (dolist (buffer (list (dvc-get-buffer dvc 'diff root)
                        (dvc-get-buffer dvc 'status root)))
    (when buffer
      (dvc-trace "buffer=%S" buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (ewoc-filter
           dvc-fileinfo-ewoc
           (lambda (fileinfo)
             (and (dvc-fileinfo-legacy-p fileinfo)
                  (eq (car (dvc-fileinfo-legacy-data fileinfo)) 'subtree))))
          (if header
              (ewoc-set-hf dvc-fileinfo-ewoc header "")
            (ewoc-set-hf dvc-fileinfo-ewoc "" ""))
          (ewoc-enter-first dvc-fileinfo-ewoc (make-dvc-fileinfo-message :text msg))
          (ewoc-refresh dvc-fileinfo-ewoc))))))

(defun dvc-diff-dtrt (prefix)
  "Do The Right Thing in a dvc-diff buffer."
  (interactive "P")

  (let* ((marked-elems (dvc-fileinfo-marked-elems))
         (length-marked-elems (length marked-elems))
         (fileinfo
          (if (< 1 length-marked-elems)
              (ewoc-data (car marked-elems))
            (save-excursion
              (unless (dvc-diff-in-ewoc-p) (dvc-diff-diff-or-list))
              (dvc-fileinfo-current-fileinfo))))
         (status (dvc-fileinfo-file-status fileinfo)))

    (ecase status
      (added
       (if (< 1 length-marked-elems)
           (error "cannot Do The Right Thing on more than one 'added' file"))
       (dvc-fileinfo-add-log-entry-1 fileinfo prefix))

      (deleted
       ;; typically nothing to do; just need commit
       (ding)
       (dvc-fileinfo-next))

      (missing
       ;; File is in database, but not in workspace
       (cond
        ((dvc-fileinfo-rename-possible marked-elems)
         (dvc-fileinfo-rename))

        (t
         (dvc-fileinfo-same-status marked-elems)
         (ding)
         (dvc-offer-choices (concat (dvc-fileinfo-current-file) " does not exist in working directory")
                            '((dvc-revert-files "revert")
                              (dvc-remove-files "remove")
                              (dvc-fileinfo-rename "rename"))))))

      (modified
       ;; Don't offer undo here; not a common action
       (if (dvc-diff-in-ewoc-p)
           (if (< 1 length-marked-elems)
               (error "cannot ediff more than one file")
             (dvc-diff-ediff))
         (if (< 1 length-marked-elems)
             (error "cannot add a log entry for more than one file")
           (dvc-diff-add-log-entry))))

      ((copy-source copy-target rename-source rename-target)
       ;; typically nothing to do; just need commit
       (ding)
       (dvc-fileinfo-next))

      (unknown
       (cond
        ((dvc-fileinfo-rename-possible marked-elems)
         (dvc-fileinfo-rename))

        (t
         (dvc-fileinfo-same-status marked-elems)
         (dvc-offer-choices nil
                            '((dvc-add-files "add")
                              (dvc-ignore-files "ignore")
                              (dvc-remove-files "remove")
                              (dvc-fileinfo-rename "rename"))))))
      )))

;;;###autoload
(defun dvc-file-ediff (file)
  "Run ediff of FILE (default current buffer file) against last revision."
  (interactive (list (buffer-file-name)))
  (let ((file-buffer (find-file-noselect file))
        (pristine-buffer
         (dvc-revision-get-file-in-buffer
          file `(,(dvc-current-active-dvc)
                 (last-revision
                  ,(dvc-tree-root file t)
                  1)))))
    (with-current-buffer pristine-buffer
      (set-buffer-modified-p nil)
      (toggle-read-only 1)
      (let ((buffer-file-name file))
        (set-auto-mode t)))
    (dvc-ediff-buffers pristine-buffer file-buffer)))

(defun dvc-file-ediff-revisions (file base modified)
  "View changes in FILE between BASE and MODIFIED using ediff."
  (dvc-ediff-buffers
   (dvc-revision-get-file-in-buffer file base)
   (dvc-revision-get-file-in-buffer file modified)))

;;;###autoload
(defun dvc-dvc-file-diff (file &optional base modified dont-switch)
  "Default for back-end-specific file diff. View changes in FILE
between BASE (default last-revision) and MODIFIED (default
workspace version)."
  (let* ((dvc (or (car base) (dvc-current-active-dvc)))
         (base (or base `(,dvc (last-revision ,file 1))))
         (modified (or modified `(,dvc (local-tree ,file))))
         (diff-buffer (dvc-get-buffer-create
                       dvc
                       'file-diff
                       (dvc-uniquify-file-name file)))
         (base-buffer
          (dvc-revision-get-file-in-buffer file base))
         (modified-buffer
          (dvc-revision-get-file-in-buffer file modified))
         (base-file (make-temp-file "DVC-file-diff-base"))
         (modified-file (make-temp-file "DVC-file-diff-mod")))
    (with-temp-file base-file
      (insert (with-current-buffer base-buffer (buffer-string)))
      (setq buffer-file-coding-system (with-current-buffer base-buffer
                                        buffer-file-coding-system)))
    (with-temp-file modified-file
      (insert (with-current-buffer modified-buffer (buffer-string)))
      (setq buffer-file-coding-system (with-current-buffer modified-buffer
                                        buffer-file-coding-system)))
    (dvc-switch-to-buffer diff-buffer)
    (let ((inhibit-read-only t)
          (slash (unless (file-name-absolute-p file) "/")))
      (erase-buffer)
      (call-process dvc-diff-executable nil diff-buffer nil
                    "-u"
                    ;; FIXME: If the file has been renamed between
                    ;; BASE and MODIFIED, the file names as
                    ;; displayed here may be incorrect.  The
                    ;; protocol needs to be extended to allow the
                    ;; backend to supply the correct file names.
                    (concat "-La" slash file)
                    (concat "-Lb" slash file)
                    base-file modified-file))
    (delete-file base-file)
    (delete-file modified-file)
    (message "")
    (toggle-read-only 1)
    (goto-char (point-min))
    (diff-mode)))

(defun dvc-ediff-startup-hook ()
  "Passed as a startup hook for ediff.

Programs ediff to return to the current window configuration after
quitting."
  ;; ediff-after-quit-hook-internal is local to an ediff session.
  (add-hook 'ediff-after-quit-hook-internal
            (dvc-capturing-lambda ()
              (set-window-configuration (capture dvc-window-config)))
            nil 'local)

  ;; Set dvc-buffer-current-active-dvc for dvc-ediff-add-log-entry.
  ;; When this hook is called, current buffer is the ediff control
  ;; buffer, default-directory is the tree root.
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc)))

(defvar dvc-window-config nil
  "Keep byte-compiler happy; declare let-bound variable used by dvc-ediff-startup-hook.")

(defun dvc-ediff-buffers (bufferA bufferB)
  "Wrapper around `ediff-buffers'.

Calls `ediff-buffers' on BUFFERA and BUFFERB."
  (let ((dvc-window-config (current-window-configuration))
        (dvc-temp-current-active-dvc (dvc-current-active-dvc)))
    (ediff-buffers bufferA bufferB
                   '(dvc-ediff-startup-hook) 'dvc-ediff)))

(provide 'dvc-diff)
;;; dvc-diff.el ends here

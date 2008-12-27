;;; tla.el --- Arch interface for emacs

;; Copyright (C) 2003-2008 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

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

;; Some documentation can be found on the wiki here
;;     http://wiki.gnuarch.org/xtla
;; The manual is available online
;;     http://download.gna.org/xtla-el/docs/xtla-snapshot.html
;; and in the texinfo directory of the Xtla distribution.

;; There is a project page at
;;     https://gna.org/projects/xtla-el
;; You can subscribe to the mailing list via
;;     https://mail.gna.org/listinfo/xtla-el-dev

;; Usage: put the following in your .emacs: (require 'xtla-autoloads)

;; The main commands are available with the prefix key C-x V.
;; Type C-x V C-h for a list.

;; M-x tla-inventory shows a tla inventory
;; In this inventory buffer the following commands are available:
;; e ... tla-edit-log
;; = ... tla-changes
;; l ... tla-changelog
;; L ... tla-logs

;; To Edit a logfile issue: M-x tla-edit-log
;; In this mode you can hit C-c C-d to show the changes
;; Edit the log file
;; After that you issue M-x tla-commit (bound to C-c C-c) to commit the files

;; M-x tla-archives starts the interactive archive browser

;; M-x tla-make-archive creates a new archive directory
;; Many commands are available from here.  Look at the menus, they're
;; very helpful to begin.

;; M-x tla-bookmarks RET
;; Is another good starting point.  This is the place where you put the
;; project you work on most often, and you can get a new version, see
;; the missing patches, and a few other useful features from here.
;; Use `a' to add a bookmark.  Add your own projects, and your
;; contributor's projects too.  Select several related projects with
;; `m' (unselect with M-u or M-del).  Make them partners with 'M-p'.
;; Now, with your cursor on a bookmark, view the uncommitted changes,
;; the missing patches from your archive and your contributors with
;; 'M'.

;; M-x tla-file-ediff RET
;; Is an wrapper to tla file-diff, ediff to view the changes
;; interactively.

;; Misc commands:
;; tla-tag-insert inserts a arch-tag entry generated with uuidgen

;; If you find xtla.el useful, and you have some ideas to improve it
;; please share them with us (Patches are preferred :-))

;;; Todo:
;; See docs/Todo


;;; History:
;;
;; Beginning of 2004: Initial version by Stefan Reichoer
;;
;;

;;; Code:

(eval-and-compile
  (if (featurep 'xemacs)
      (require 'dvc-xemacs)
    (require 'dvc-emacs))
  (require 'dvc-lisp)
  (require 'dvc-revlist)

  (when (locate-library "dvc-version")
    (require 'dvc-version)))

;; runtime use of 'cl package is discouraged. Please keep this
;; "eval-when-compile"
;;       ^^^^
(eval-when-compile (require 'cl))
(eval-and-compile (require 'dvc-about))
(eval-and-compile (require 'dvc-utils))
(eval-and-compile (require 'dvc-cmenu))
(eval-and-compile (require 'dvc-core))

(eval-and-compile (require 'tla-gnus))

(autoload 'dired-get-filename "dired")

(eval-and-compile
  (require 'ediff)
  (require 'font-lock)
  ;; on some systems, sendmail is not available.
  (when (locate-library "sendmail")
    (require 'sendmail)))

(require 'pp)
(require 'ewoc)
(require 'diff)
(require 'time-date)
(require 'dvc-diff)
(require 'dvc-state)

(eval-and-compile
  (require 'tla-defs)
  (require 'tla-core)
  (require 'tla-autoconf)
  (when (locate-library "smerge-mode")
    (require 'smerge-mode)))

(eval-when-compile
  (if (locate-library "hl-line")
      (require 'hl-line)
    (if (locate-library "highline")
        (require 'highline))))

;; ----------------------------------------------------------------------------
;; Internal variables
;; ----------------------------------------------------------------------------
(defvar tla-edit-arch-command nil)
(defvar tla-pre-commit-window-configuration nil)
(defvar tla-pre-tree-lint-window-configuration nil)
(defvar tla-log-edit-file-name nil)
(defvar tla-log-edit-file-buffer nil)
(defvar tla-my-id-history nil)
(defvar tla-last-commit-message nil)

(defvar tla-buffer-archive-name nil)
(defvar tla-buffer-category-name nil)
(defvar tla-buffer-branch-name nil)
(defvar tla-buffer-version-name nil)

(defvar tla-mode-line-process "")
(defvar tla-mode-line-process-status "")

;; Overlay category
(put 'tla-default-button 'mouse-face 'highlight)
(put 'tla-default-button 'evaporate t)
;;(put 'tla-default-button 'rear-nonsticky t)
;;(put 'tla-default-button 'front-nonsticky t)

;; ----------------------------------------------------------------------------
;; Macros
;; ----------------------------------------------------------------------------
(defmacro tla-toggle-list-entry (list entry)
  "Either add or remove from the value of LIST the value ENTRY."
  `(if (member ,entry ,list)
       (setq ,list (delete ,entry ,list))
     (add-to-list ',list ,entry)))

;; ----------------------------------------------------------------------------
;; Common used functions for many xtla modes
;; ----------------------------------------------------------------------------
(defun tla-edit-=tagging-method-file ()
  "Edit the {arch}/=tagging-method file."
  (interactive)
  (find-file (expand-file-name "{arch}/=tagging-method" (tla-tree-root))))

(defun tla-edit-.arch-inventory-file (&optional dir)
  "Edit DIR/.arch-inventory file.
`default-directory' is used as DIR if DIR is nil.
If it is called interactively and the prefix argument is given via DIR,
use the directory of a file associated with the point to find .arch-inventory.
In the case no file is associated with the point, it reads the directory name
with `dvc-read-directory-name'."
  (interactive
   (list (if (not (interactive-p))
             default-directory
           (let ((file (dvc-get-file-info-at-point)))
             (if file
                 (if (not (file-name-absolute-p file))
                     (concat default-directory
                             (file-name-directory file))
                   (file-name-directory file))
               (expand-file-name (dvc-read-directory-name
                                  "Directory containing \".arch-inventory\":  ")))))))
  (let* ((dir (or dir default-directory))
         (file (expand-file-name ".arch-inventory" dir))
         (newp (not (file-exists-p file))))
    (find-file file)
    (save-excursion
      (when (and newp (y-or-n-p
                       (format "Insert arch tag to \"%s\"? " file)))
        (tla-tag-insert)))))

(defun tla--insert-right-justified (string count &optional face)
  "Insert a string with a right-justification.

Inserts STRING preceded by spaces so that the line ends exactly at
COUNT characters (or after if STRING is too long).
If FACE is non-nil, insert the string fontified with FACE."
  (insert-char ?\  (max 0 (- count (length string))))
  (insert (if face (dvc-face-add string face) string))
  )

;; ----------------------------------------------------------------------------
;; Name read engine helpers
;; ----------------------------------------------------------------------------
;;
;; Extended version of tla--read-name
;;
(defun tla-name-read-reinit-minibuf-map ()
  "Redefine `tla--name-read-minibuf-map'.

Compute the new value based on the current
`minibuffer-local-completion-map'. This is usefull if you want to add
bindings to your `minibuffer-local-completion-map' globally after
loading Xtla."
  (setq tla--name-read-minibuf-map (tla-name-read-minibuf-map-fn)))

(defun tla-name-read-help ()
  "Displays a help message with keybindings for the minibuffer prompt."
  (interactive)
  (set-buffer (get-buffer-create "*Help*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (kill-all-local-variables)
    (help-mode)
    (view-mode -1)
    (insert "This buffer describes the name reading engine for xtla

You are prompted for a fully qualified archive, category, branch,
version, or revision, which means a string like
\"John.Smith@rt.fm--arch/xtla--revolutionary--1.0\". Completion is
available with TAB. Only the item being entered is proposed for
completion, which means that if you're typing the archive name,
pressing TAB will give you the list of archives. If you started to
type the category name, you'll get the list of category for this
archive.

Here's a list of other interesting bindings available in the
minibuffer:

")
    (let ((interesting (mapcar (lambda (pair) (cdr pair))
                               tla--name-read-extension-keydefs)))
      (dolist (func interesting)
        (let* ((keys (where-is-internal func tla--name-read-minibuf-map))
               (keys1 ""))
          (while keys
            (when (not (eq 'menu-bar (aref (car keys) 0)))
              (setq keys1 (if (string= keys1 "") (key-description (car keys))
                            (concat keys1 ", "
                                    (key-description (car keys))))))
            (setq keys (cdr keys)))
          (insert (format "%s%s\t`%s'\n" keys1
                          (make-string (max 0 (- 5 (length keys1))) ?\ )
                          (symbol-name func))))))
    (goto-char (point-min))
    (dvc-funcall-if-exists
     help-setup-xref (list 'tla-name-read-help)
     (interactive-p)))
  (display-buffer (current-buffer))
  (toggle-read-only 1))

(defun tla-name-read-inline-help ()
  "Displays a help message in echo area."
  (interactive)
  (let ((interesting (mapcar (lambda (pair) (cdr pair))
                             tla--name-read-extension-keydefs))
        (line ""))
    (dolist (func interesting)
      (let* ((keys (where-is-internal func tla--name-read-minibuf-map))
             (keys1 "")
             (func (symbol-name func)))
        (while keys
          (when (not (eq 'menu-bar (aref (car keys) 0)))
            (setq keys1 (if (string= keys1 "") (key-description (car keys))
                          (concat keys1 ", "
                                  (key-description (car keys))))))
          (setq keys (cdr keys)))
        (setq func (progn (string-match "tla-name-read-\\(.+\\)"
                                        func)
                          (match-string 1 func)))
        (setq line (concat line (format "%s => `%s'" keys1 func) "    "))))
    (dvc-about-message-with-rolling line)
    ))




(defun tla--read-revision-with-default-tree (&optional prompt tree)
  "Read revision name with `tla-name-read'.
PROMPT is passed to `tla-name-read' without changing.
Default version associated with TREE, a directory is used as default arguments
for`tla-name-read'."
  (setq tree (tla-tree-root (or tree default-directory) t))
  (let ((tree-rev (tla-tree-version-list tree)))
    (tla-name-read prompt
                   (if tree-rev (tla--name-archive tree-rev) 'prompt)
                   (if tree-rev (tla--name-category tree-rev) 'prompt)
                   (if tree-rev (tla--name-branch tree-rev) 'prompt)
                   (if tree-rev (tla--name-version tree-rev) 'prompt)
                   'prompt)))

;;
;; Version for the tree of default directory
;;
(defvar tla--name-read-insert-version-associated-with-default-directory nil)
(defun tla-name-read-insert-version-associated-with-default-directory (&optional force)
  "Insert the version for the tree of the directory specified by .

If FORCE is non-nil, insert the version even if the minibuffer isn't empty."
  (interactive "P")
  (let ((version-for-tree
         (tla--name-mask
          (tla-tree-version-list
           (if tla--name-read-insert-version-associated-with-default-directory
               tla--name-read-insert-version-associated-with-default-directory
             default-directory))
          t
          (tla--name-read-arguments 'archive)
          (tla--name-read-arguments 'category)
          (tla--name-read-arguments 'branch)
          (tla--name-read-arguments 'version))))
    (if (and (window-minibuffer-p (selected-window))
             (or force (equal "" (minibuffer-contents))))
        (insert version-for-tree))))

;;
;; Default archive
;;
(defun tla-name-read-insert-default-archive (&optional force)
  "Insert default archive name into the minibuffer if it is empty.

If FORCE is non-nil, insert the archive name even if the minibuffer
isn't empty."
  (interactive "P")
  (if (and (window-minibuffer-p (selected-window))
           (or (equal "" (minibuffer-contents)) force)
           (member
            (tla--name-read-arguments 'archive)
            '(prompt maybe)))
      (insert (tla-my-default-archive))))

;;
;; Info at point
;;
(defvar tla-name-read-insert-info-at-point nil)
(defvar tla--name-read-insert-info-at-point-overlay nil)
(defun tla-name-read-insert-info-at-point (&optional force)
  "Insert the info(maybe revision) under the point to the minibuffer.

If FORCE is non-nil, insert the version even if the minibuffer isn't
empty."
  (interactive "P")
  (let ((info-at-point
         (or tla-name-read-insert-info-at-point
             (tla-name-read-insert-version-associated-with-default-directory))))
    (when (and (window-minibuffer-p (selected-window))
               (or (equal "" (minibuffer-contents)) force)
               info-at-point)
      (insert info-at-point))))

(defun tla--name-read-insert-info-at-point-init ()
  "This function retrieves the info at point.

Further call to `tla--name-read-insert-info-at-point-final' will
actuall insert the value computed here."
  (setq tla-name-read-insert-info-at-point
        (let ((raw-info (tla--revision-get-revision-at-point))
              (b (dvc-cmenu-beginning (point)))
              (e (dvc-cmenu-end (point))))
          (when raw-info
            (when (and b e)
              (setq tla--name-read-insert-info-at-point-overlay
                    (make-overlay (1- b) e))
              (overlay-put tla--name-read-insert-info-at-point-overlay
                           'face 'dvc-highlight))
            (tla--name-mask
             (tla--name-split raw-info) t
             (tla--name-read-arguments 'archive)
             (tla--name-read-arguments 'category)
             (tla--name-read-arguments 'branch)
             (tla--name-read-arguments 'version)
             (tla--name-read-arguments 'revision))))))

(defun tla--name-read-insert-info-at-point-final (&optional no-use)
  "Called when exitting the minibuffer prompt.

Cancels the effect of `tla--name-read-insert-info-at-point-init'.

Argument NO-USE is ignored."
  (when tla--name-read-insert-info-at-point-overlay
    (delete-overlay tla--name-read-insert-info-at-point-overlay)
    (setq tla--name-read-insert-info-at-point-overlay nil)))

;;
;; Partner file
;;
(defvar tla--name-read-insert-partner-ring-position nil)
(defun tla--name-read-insert-partner-init ()
  "Initialize \"Insert Partner Version\" menu used in `tla-name-read'."
  (setq tla--name-read-insert-partner-ring-position nil)
  ;; Create menu items
  (setq xtla--name-read-partner-menu (cons "Insert Partner Version" nil))
  (let ((partners (reverse (tla-partner-list))))
    (mapc (lambda (p)
            (setq p (tla--name-mask
                     (tla--name-split p) t
                     (tla--name-read-arguments 'archive)
                     (tla--name-read-arguments 'category)
                     (tla--name-read-arguments 'branch)
                     (tla--name-read-arguments 'version)
                     (tla--name-read-arguments 'revision)))
            (setcdr xtla--name-read-partner-menu
                    (cons (cons p
                                (cons p
                                      (lexical-let ((this-p p))
                                        (lambda () (interactive)
                                          (delete-region
                                           (minibuffer-prompt-end) (point-max))
                                          (insert this-p)))))
                          (cdr xtla--name-read-partner-menu))))
          partners))
  (fset 'xtla--name-read-partner-menu (cons 'keymap xtla--name-read-partner-menu)))

(defun tla-name-read-insert-partner-previous ()
  "Insert the previous partner version into miniffer."
  (interactive)
  (let* ((partners (tla-partner-list))
         (plen (length partners))
         (pos (if tla--name-read-insert-partner-ring-position
                  (if (eq tla--name-read-insert-partner-ring-position 0)
                      (1- plen)
                    (1- tla--name-read-insert-partner-ring-position))
                0))
         (pversion (when partners (tla--name-mask
                                   (tla--name-split (nth pos partners)) t
                                   (tla--name-read-arguments 'archive)
                                   (tla--name-read-arguments 'category)
                                   (tla--name-read-arguments 'branch)
                                   (tla--name-read-arguments 'version)
                                   (tla--name-read-arguments 'revision)))))
    (when (and (window-minibuffer-p (selected-window))
               partners
               pversion)
      (delete-region (minibuffer-prompt-end) (point-max))
      (insert pversion)
      (setq tla--name-read-insert-partner-ring-position pos))))

(defun tla-name-read-insert-partner-next ()
  "Insert the next partner version into the miniffer."
  (interactive)
  (let* ((partners (tla-partner-list))
         (plen (length partners))
         (pos (if tla--name-read-insert-partner-ring-position
                  (if (eq tla--name-read-insert-partner-ring-position (1- plen))
                      0
                    (1+ tla--name-read-insert-partner-ring-position))
                0))
         (pversion (when partners (tla--name-mask
                                   (tla--name-split (nth pos partners)) t
                                   (tla--name-read-arguments 'archive)
                                   (tla--name-read-arguments 'category)
                                   (tla--name-read-arguments 'branch)
                                   (tla--name-read-arguments 'version)
                                   (tla--name-read-arguments 'revision)))))
    (when (and (window-minibuffer-p (selected-window))
               partners
               pversion)
      (delete-region (minibuffer-prompt-end) (point-max))
      (insert pversion)
      (setq tla--name-read-insert-partner-ring-position pos))))

;;
;; Ancestor
;;
(defun tla-name-read-insert-ancestor (&optional force)
  "Insert the ancestor name into the minibuffer if it is empty.

If FORCE is non-nil, insert the ancestor even if the minibuffer isn't
empty."
  (interactive "P")
  (let* ((version (tla-tree-version-list default-directory))
         (ancestor (when (and version
                              (not (eq this-command 'tla-revision-direct-ancestor)))
                     (tla-revision-direct-ancestor
                      (tla--name-mask version nil
                                      t t t t "base-0")))))
    (when (and ancestor
               (window-minibuffer-p (selected-window))
               (or (equal "" (minibuffer-contents)) force)
               (member
                (tla--name-read-arguments 'archive)
                '(prompt maybe)))
      (insert (tla--name-mask
               ancestor t
               t
               (member
                (tla--name-read-arguments 'category)
                '(prompt maybe))
               (member
                (tla--name-read-arguments 'branch)
                '(prompt maybe))
               (member
                (tla--name-read-arguments 'version)
                '(prompt maybe))
               (member
                (tla--name-read-arguments 'revision)
                '(prompt maybe)))))))

;;
;; Partners in Bookmark
;;
(defvar tla--name-read-insert-bookmark-ring-position nil)
(defun tla--name-read-insert-bookmark-init ()
  "Initialize \"Insert Version in Bookmark\" menu used in `tla-name-read'."
  (setq tla--name-read-insert-bookmark-ring-position nil)
  ;; Create menu items
  (setq xtla--name-read-bookmark-menu (cons "Insert Version in Bookmark" nil))
  (let* ((default-version (tla-tree-version-list default-directory 'no-error))
         (bookmarks (when default-version
                      (nreverse (tla-bookmarks-get-partner-versions default-version)))))
    (mapc (lambda (p)
            (setq p (tla--name-mask
                     p t
                     (tla--name-read-arguments 'archive)
                     (tla--name-read-arguments 'category)
                     (tla--name-read-arguments 'branch)
                     (tla--name-read-arguments 'version)
                     (tla--name-read-arguments 'revision)))
            (setcdr xtla--name-read-bookmark-menu
                    (cons (cons p
                                (cons p
                                      (lexical-let ((lex-p p))
                                        (lambda () (interactive)
                                          (delete-region
                                           (minibuffer-prompt-end) (point-max))
                                          (insert p)))))
                          (cdr xtla--name-read-bookmark-menu))))
          bookmarks))
  (fset 'xtla--name-read-bookmark-menu (cons 'keymap xtla--name-read-bookmark-menu)))

(defun tla-name-read-insert-bookmark-previous ()
  "Insert the previous partner version in the bookmark into miniffer."
  (interactive)
  (let* ((default-version (tla-tree-version-list default-directory))
         (bookmarks (when default-version
                      (nreverse (tla-bookmarks-get-partner-versions default-version))))
         (plen (length bookmarks))
         (pos (if tla--name-read-insert-bookmark-ring-position
                  (if (eq tla--name-read-insert-bookmark-ring-position 0)
                      (1- plen)
                    (1- tla--name-read-insert-bookmark-ring-position))
                0))
         (pversion (when bookmarks (tla--name-mask
                                    (nth pos bookmarks) t
                                    (tla--name-read-arguments 'archive)
                                    (tla--name-read-arguments 'category)
                                    (tla--name-read-arguments 'branch)
                                    (tla--name-read-arguments 'version)
                                    (tla--name-read-arguments 'revision)))))
    (when (and (window-minibuffer-p (selected-window))
               bookmarks
               pversion)
      (delete-region (minibuffer-prompt-end) (point-max))
      (insert pversion)
      (setq tla--name-read-insert-bookmark-ring-position pos))))

(defun tla-name-read-insert-bookmark-next ()
  "Insert the next partner version in the bookmark into the miniffer."
  (interactive)
  (let* ((default-version (tla-tree-version-list default-directory))
         (bookmarks (when default-version
                      (nreverse (tla-bookmarks-get-partner-versions default-version))))
         (plen (length bookmarks))
         (pos (if tla--name-read-insert-bookmark-ring-position
                  (if (eq tla--name-read-insert-bookmark-ring-position (1- plen))
                      0
                    (1+ tla--name-read-insert-bookmark-ring-position))
                0))
         (pversion (when bookmarks (tla--name-mask
                                    (nth pos bookmarks) t
                                    (tla--name-read-arguments 'archive)
                                    (tla--name-read-arguments 'category)
                                    (tla--name-read-arguments 'branch)
                                    (tla--name-read-arguments 'version)
                                    (tla--name-read-arguments 'revision)))))
    (when (and (window-minibuffer-p (selected-window))
               bookmarks
               pversion)
      (delete-region (minibuffer-prompt-end) (point-max))
      (insert pversion)
      (setq tla--name-read-insert-bookmark-ring-position pos))))

(add-hook 'tla-name-read-init-hook
          'tla--name-read-insert-info-at-point-init)
(add-hook 'tla-name-read-final-hook
          'tla--name-read-insert-info-at-point-final)
(add-hook 'tla-name-read-error-hook
          'tla--name-read-insert-info-at-point-final)
(add-hook 'tla-name-read-init-hook
          'tla--name-read-insert-partner-init)
(add-hook 'tla-name-read-init-hook
          'tla--name-read-insert-bookmark-init)

(defun tla-file-name-relative-to-root (file)
  (let* ((file (dvc-uniquify-file-name file))
         (tree-root (tla-tree-root file)))
    (replace-regexp-in-string
     ;; note: tree-root always ends with a slash, so the effect of "*"
     ;; is to match one or more trailing slashes
     (concat "^" (regexp-quote tree-root) "*")
     ""
     file)))

(defun tla--read-directory-maybe (&optional prompt directory)
  "Read a directory name inside an arch managed tree.

Return a directory name which is a subdirectory or the root of some
project tree.  Works in a way similar to
`dvc-read-project-tree-maybe', but is customized with the variable
`dvc-read-directory-mode'.

PROMPT is the user prompt, and DIRECTORY is the default directory."
  (let ((root (tla-tree-root (or directory default-directory) t))
        (default-directory (or directory default-directory))
        (prompt (or prompt "Use directory: ")))
    (case dvc-read-directory-mode
      (always (dvc-read-directory-name prompt))
      (sometimes (if root (or directory default-directory)
                   (dvc-read-directory-name prompt)))
      (never (if root (or directory default-directory)
               (error "Not in a project tree")))
      (t (error "Wrong value for dvc-read-directory-mode")))))

(defun tla-close-project (&optional tree)
  "Close all buffers whose directory is in the same project as TREE."
  (interactive)
  (let ((tree (dvc-uniquify-file-name (tla-tree-root tree))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let ((current-proj (dvc-uniquify-file-name
                             (tla-tree-root default-directory t))))
          (when (string= tree current-proj)
            ;; Keep the buffer if the file doesn't exist
            (kill-buffer buffer)))))))

;; ----------------------------------------------------------------------------
;; tla help system for commands that get input from the user via the minibuffer
;; ----------------------------------------------------------------------------

;; GENERIC: This functionality should be in emacs itself. >> Masatake
;;  to check: we should use some other binding for this, perhaps f1 C-m
(defun tla--display-command-help (command)
  "Help system for commands that get input via the minibuffer.

This is an internal function called by `tla-show-command-help'.

COMMAND is the last command executed."
  (with-electric-help
   (lambda ()
     (let ((cmd-help (when (fboundp command)
                       (documentation command))))
       (delete-region (point-min) (point-max))
       (insert (if cmd-help
                   (format "Help for %S:\n%s" command cmd-help)
                 (format "No help available for %S" command)))))
   (concat " *" (tla-arch-branch-name) "-command-help*")))

(defvar tla-command-stack nil)

(defun tla-minibuffer-setup ()
  "Function called in `minibuffer-setup-hook'.

Memorize last command run."
  (push  this-command tla-command-stack))

(defun tla-minibuffer-exit ()
  "Function called in `minibuffer-exit-hook'.

Cancels the effect of `tla-minibuffer-setup'."
  (pop tla-command-stack))

(defun tla-show-command-help ()
  "Help system for commands that get input via the minibuffer.

When the user is asked for input in the minibuffer, a help for the
command will be shown, if the user hits \\<minibuffer-local-map>\\[tla-show-command-help].
This functionality is not only for xtla commands available it is
available for all Emacs commands."
  (interactive)
  (tla--display-command-help (car tla-command-stack)))

(when tla-install-command-help-system
  (define-key minibuffer-local-map            [f1]
    'tla-show-command-help)
  (define-key minibuffer-local-completion-map [f1]
    'tla-show-command-help)
  (define-key minibuffer-local-must-match-map [f1]
    'tla-show-command-help)
  (define-key minibuffer-local-map            [(control meta ?h)]
    'tla-show-command-help)
  (define-key minibuffer-local-completion-map [(control meta ?h)]
    'tla-show-command-help)
  (define-key minibuffer-local-must-match-map [(control meta ?h)]
    'tla-show-command-help)
  (add-hook 'minibuffer-setup-hook 'tla-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook 'tla-minibuffer-exit))

;; ----------------------------------------------------------------------------
;; Top level tla commands
;; ----------------------------------------------------------------------------
(defcustom tla-make-log-function 'tla-default-make-log-function
  "*Function used to create the log buffer.

Must return a string which is the absolute name of the log file.  This
function is called only when the log file doesn't exist already.  The
default is `tla-default-make-log-function', which just calls \"tla
make-log\". If you want to override this function, you may just write
a wrapper around `tla-default-make-log-function'."
  :type 'function
  :group 'xtla)

(defun tla-make-log (&optional nocreate)
  "Create the log file and return its filename.

If the file exists, its name is returned.  Otherwise, the log file is
created by the function specified by `tla-make-log-function', which,
by default, calls \"tla make-log\"."
  (interactive)
  (let* ((version (tla-tree-version-list))
         (file (concat (tla-tree-root) "++log."
                       (tla--name-category version) "--"
                       (tla--name-branch   version) "--"
                       (tla--name-version  version) "--"
                       (tla--name-archive  version))))
    (cond ((file-exists-p file)
           file)
          (nocreate nil)
          (t (funcall tla-make-log-function)))))

(defun tla-default-make-log-function ()
  "Candidate (and default value) for `tla-make-log-function'.
Calls \"tla make-log\" to generate the log file."
  (tla--run-tla-sync '("make-log")
                     :finished
                     (lambda (output error status arguments)
                       (dvc-buffer-content output))))

(defun dvc-pop-to-inventory ()
  "Call `tla-inventory' with a prefix arg."
  (interactive)
  (tla-inventory nil t))

(defvar tla-inventory-cookie nil)
(defvar tla-inventory-list nil
  "Full list for the inventory.")

(defun tla-inventory-goto-file (file)
  "Put cursor on FILE.  nil return means the file hasn't been found."
  (goto-char (point-min))
  (let ((current (ewoc-locate tla-inventory-cookie)))
    (while (and current (not (string= (car (cddr (ewoc-data current)))
                                      file)))
      (setq current (ewoc-next tla-inventory-cookie current)))
    (when current (tla-inventory-cursor-goto current))
    current))


(defun tla-inventory-make-toggle-fn-and-var (variable function)
  "Define the VARIABLE and the toggle FUNCTION for type TYPE."
  (make-variable-buffer-local variable)
  (eval `(defun ,function ()
           (interactive)
           (setq ,variable (not ,variable))
           (tla-inventory-redisplay))))

(dolist (type-arg tla-inventory-file-types-manipulators)
  (tla-inventory-make-toggle-fn-and-var (cadr type-arg) (car (cddr type-arg))))

(defun tla-inventory-redisplay ()
  "Refresh inventory buffer."
  (let* ((elem (ewoc-locate tla-inventory-cookie))
         (file (when elem (car (cddr (ewoc-data elem)))))
         (pos (point)))
    (tla-inventory-display)
    (or (and file
             (tla-inventory-goto-file file))
        (goto-char pos))
    (tla-inventory-cursor-goto (ewoc-locate tla-inventory-cookie))))


(defun tla-inventory-set-toggle-variables (new-value)
  "Set all tla-inventory-display-* variables.
If NEW-VALUE is 'toggle set the values to (not tla-inventory-display-*
Otherwise set it to NEW-VALUE."
  (dolist (type-arg tla-inventory-file-types-manipulators)
    (eval `(setq ,(cadr type-arg)
                 (if (eq new-value 'toggle)
                     (not ,(cadr type-arg))
                   new-value)))))

(defun tla-inventory-set-all-toggle-variables ()
  "Set all inventory toggle variables to t."
  (interactive)
  (tla-inventory-set-toggle-variables t)
  (tla-inventory-redisplay))

(defun tla-inventory-reset-all-toggle-variables ()
  "Set all inventory toggle variables to nil."
  (interactive)
  (tla-inventory-set-toggle-variables nil)
  (tla-inventory-redisplay))

(defun tla-inventory-toggle-all-toggle-variables ()
  "Toggle the value of all inventory toggle variables."
  (interactive)
  (tla-inventory-set-toggle-variables 'toggle)
  (tla-inventory-redisplay))

(defun tla-inventory-goto (&optional directory arg)
  "Goto inventory buffer, or run `tla-inventory'."
  (interactive (list (tla--read-directory-maybe
                      "Run inventory in (directory): ")
                     current-prefix-arg))
  (let* ((default-directory (or directory default-directory))
         (buffer (dvc-get-buffer tla-arch-branch 'inventory default-directory)))
    (if buffer
        (if arg
            (pop-to-buffer buffer)
          (switch-to-buffer buffer))
      (tla-inventory directory arg))))

;;;###autoload
(defun tla-inventory (&optional directory arg)
  "Show a tla inventory at DIRECTORY.
When called with a prefix arg, pop to the inventory buffer.
DIRECTORY defaults to the current one when within an arch managed tree,
unless prefix argument ARG is non-nil."
  (interactive (list (tla--read-directory-maybe
                      "Run inventory in (directory): ")
                     current-prefix-arg))
  (let ((default-directory (or directory default-directory)))
    (if arg
        (pop-to-buffer (dvc-get-buffer-create tla-arch-branch 'inventory
                                              default-directory))
      (switch-to-buffer (dvc-get-buffer-create tla-arch-branch 'inventory
                                               default-directory))))
  (tla-inventory-mode)
  (tla--run-tla-sync
   ;; We have to provide all file types or tla inventory won't display
   ;; junk files
   `("inventory" "--both" "--kind" "--source" "--backups" "--junk"
     "--unrecognized" "--precious"
     ,(when (and (tla-inventory-has-no-recursion-option)
                 tla-non-recursive-inventory)
        "--no-recursion"))
   :finished
   (lambda (output error status arguments)
     (let ((list (split-string (dvc-buffer-content output) "\n"))
           (inventory-list '()))
       (mapc
        (lambda (item)
          (when (string-match "\\([A-Z]\\)\\([\\? ]\\) +\\([^ ]\\) \\(.*\\)"
                              item)
            (let ((tla-type (string-to-char (match-string 1 item)))
                  (question (string= (match-string 2 item) "?"))
                  (escaped-filename (match-string 4 item))
                  (type (string-to-char (match-string 3 item))))
              (push (list tla-type
                          question
                          (tla-unescape escaped-filename)
                          type)
                    inventory-list))))
        list)
       (setq inventory-list (reverse inventory-list))
       (set (make-local-variable 'tla-inventory-list)
            inventory-list)
       (tla-inventory-display)))))

(defun tla-inventory-display ()
  "Display the inventory.
This function creates the ewoc from the variable `tla-inventory-list',
selecting only files to print."
  (interactive)
  (setq dvc-buffer-marked-file-list nil)
  (let (buffer-read-only)
    (erase-buffer)
    (set (make-local-variable 'tla-inventory-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'tla-inventory-printer)))
    (tla-inventory-insert-headers)
    (dolist (elem tla-inventory-list)
      (let ((type (car elem))
            (file (nth 2 elem)))
        (when (eval (cadr (assoc type
                                 tla-inventory-file-types-manipulators)))
          (when (member file dvc-buffer-all-marked-file-list)
            (push file dvc-buffer-marked-file-list))
          (ewoc-enter-last tla-inventory-cookie elem)))))
  (goto-char (point-min)))

;; When there are too many files, tla-inventory is
;; too slow. Putting faces and inserting type-character
;; are the reason of slowness.
;; About putting faces, setting `dvc-highlight' to nil
;; helps. For making inserting type-character faster
;; I(Masatake) introduces a table-lookup code instead
;; of case statement.
;; OLD case based code is here:
;;(defun tla--inventory-chose-face (type)
;;  "Return a face adapted to TYPE, which can be J, S, P, T or U."
;;  (case type
;;    (?J 'tla-junk)                      ; 74
;;    (?P 'dvc-ignored)                  ; 80
;;    (?S 'dvc-source)                    ; 83
;;    (?T 'dvc-nested-tree)               ; 84
;;    (?U 'dvc-unrecognized)              ; 85
;;    ))

;; The new table-lookup code is here:
(defconst tla--inventory-chose-face-table
  [
   nil                                  ; ?B: 66->0
   nil                                  ; 67
   nil                                  ; 68
   nil                                  ; 69
   nil                                  ; 79
   nil                                  ; 71
   nil                                  ; 72
   nil                                  ; 73
   tla-junk                             ; ?J: 74->0
   nil                                  ; 75
   nil                                  ; 76
   nil                                  ; 77
   nil                                  ; 78
   nil                                  ; 79
   dvc-ignored                          ; ?P: 80
   nil                                  ; 81
   nil                                  ; 82
   dvc-source                           ; ?S: 83
   dvc-nested-tree                      ; :T: 84
   dvc-unrecognized                     ; :U: 85
   ]
  "from-type-to-face table used in 'tla--inventory-chose-face'
This is for optimization. ")

(defun tla--inventory-chose-face (type)
  "Return a face adapted to TYPE, which can be J, S, P, T or U."
  (aref
   tla--inventory-chose-face-table
   (- type ?B)))

(defun tla-inventory-printer (elem)
  "Ewoc printer for `tla-inventory-cookie'.
Pretty print ELEM."
  (let* ((type (nth 0 elem))
         (question (nth 1 elem))
         (file (nth 2 elem))
         (file-type (nth 3 elem))
         (face (tla--inventory-chose-face type)))
    (insert (if (member file dvc-buffer-marked-file-list)
                (concat " " dvc-mark " ") "   "))
    (insert (dvc-face-add (format "%c%s  "
                                  type
                                  (if question "?" " "))
                          face)
            (dvc-face-add
             (format "%s%s" file
                     (case file-type (?d "/") (?> "@") (t "")))
             face
             'tla-inventory-item-map
             tla-inventory-item-menu))))

(defun tla-inventory-mark-file ()
  "Mark file at point in inventory mode.

Adds it to the variable `dvc-buffer-marked-file-list', and move cursor
to the next entry."
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie))
        (file (dvc-get-file-info-at-point)))
    (add-to-list 'dvc-buffer-marked-file-list file)
    (add-to-list 'dvc-buffer-all-marked-file-list file)
    (ewoc-invalidate tla-inventory-cookie current)
    (tla-inventory-cursor-goto (or (ewoc-next tla-inventory-cookie
                                              current)
                                   current))))

(defun tla-inventory-unmark-file ()
  "Unmark file at point in inventory mode."
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie))
        (file (dvc-get-file-info-at-point)))
    (setq dvc-buffer-marked-file-list
          (delete file dvc-buffer-marked-file-list))
    (setq dvc-buffer-all-marked-file-list
          (delete file dvc-buffer-all-marked-file-list))
    (ewoc-invalidate tla-inventory-cookie current)
    (tla-inventory-cursor-goto (or (ewoc-next tla-inventory-cookie
                                              current)
                                   current))))

(defun tla-inventory-unmark-all ()
  "Unmark all files in inventory mode."
  (interactive)
  (let ((current (ewoc-locate tla-inventory-cookie)))
    (setq dvc-buffer-marked-file-list nil)
    (setq dvc-buffer-all-marked-file-list nil)
    (ewoc-refresh tla-inventory-cookie)
    (tla-inventory-cursor-goto current)))

(defvar tla-generic-select-files-function nil
  "Function called by `tla--generic-select-files'.
Must be local to each buffer.")

(defun tla--generic-select-files (msg-singular
                                  msg-plural msg-err
                                  msg-prompt
                                  &optional
                                  no-group ignore-marked
                                  no-prompt
                                  y-or-n)
  "Get the list of files at point, and ask confirmation of the user.

This is a generic function calling
`tla-generic-select-files-function', defined locally for each tla
buffer. The behavior should be the following:

Prompt with either MSG-SINGULAR, MSG-PLURAL, MSG-ERR OR MSG-PROMPT. If
NO-GROUP is nil and if the cursor is on the beginning of a group, all
the files belonging to this message are selected. If some files are
marked \(i.e. `dvc-buffer-marked-file-list' is non-nil) and
IGNORE-MARKED is non-nil, the list of marked files is returned. If
NO-PROMPT is non-nil, don't ask for confirmation. If Y-OR-N is
non-nil, then this function is used instead of `y-or-n-p'."
  (when tla-generic-select-files-function
    (funcall tla-generic-select-files-function
             msg-singular msg-plural msg-err msg-prompt no-group
             ignore-marked no-prompt y-or-n)))

(defun tla-inventory-get-file-info-at-point ()
  "Gets the file at point in inventory mode."
  (let ((cookie (ewoc-locate tla-inventory-cookie)))
    (when cookie (car (cddr (ewoc-data cookie))))))

(defun tla-inventory-insert-headers ()
  "Insert the header (top of buffer) for *{tla|baz}-inventory*."
  (let* ((tree-version (tla--name-construct
                        (tla-tree-version-list nil 'no-error)))
         (tagging-method (tla-id-tagging-method nil))
         (separator
          (dvc-face-add (make-string
                         (max (+ (length "Directory: ")   (length default-directory))
                              (+ (length "Default Tree Version: ") (length tree-version))
                              (+ (length "ID Tagging Method: ") (length tagging-method)))
                         ?\ )
                        'dvc-separator)))
    (ewoc-set-hf
     tla-inventory-cookie
     (concat
      "Directory: "    (dvc-face-add default-directory 'dvc-local-directory
                                     (lexical-let
                                         ((map  (make-sparse-keymap))
                                          (func (lambda ()
                                                  (interactive)
                                                  (dired default-directory))))
                                       (define-key map [return]  func)
                                       (define-key map "\C-m"    func)
                                       (define-key map [mouse-2] func)
                                       map)
                                     nil
                                     "Run Dired Here") "\n"
      "Default Tree Version: " (dvc-face-add tree-version 'tla-archive-name
                                             'tla-inventory-default-version-map
                                             (tla--partner-create-menu
                                              'tla-generic-set-tree-version
                                              "Change the Default Tree Version")) "\n"
      "ID Tagging Method: " (dvc-face-add tagging-method 'tla-tagging-method
                                          'tla-inventory-tagging-method-map
                                          tla-inventory-tagging-method-menu) "\n"
      separator "\n")
     (concat "\n" separator))))

(defvar tla-buffer-source-buffer nil
  "Buffer from where a command was called.")

(defun tla-edit-log-delete-file-list (&optional noerror)
  "Delete the temporary file list in the current buffer.

Return t if something was actually deleted, nil otherwise.
Raise an error if the file list was not found, unless NOERROR is
specified."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward
         (concat "\n" dvc-log-edit-file-list-marker "\n")
         (point-max) t)
        (progn
          (delete-region (1+ (match-beginning 0)) (point-max))
          (goto-char (point-max))
          (delete-blank-lines)
          (beginning-of-line)
          (forward-line -1)
          (when (looking-at "Keywords:")
            (end-of-line)
            (newline))
          t) ;; return
      ;; (if (not (or noerror
      ;;              (yes-or-no-p (format "The marker %S was not found! Commit anyway? "
      ;;                                   dvc-log-edit-file-list-marker))))
      ;;     (error (format "The marker %s was not found!"
      ;;                    dvc-log-edit-file-list-marker))
      ;;   nil)
      )))

(defun tla-changes-file-list ()
  "Return the list of modified files in a changes buffer.

Return 'dont-know if the list can't be computed easily.

The result is based on `dvc-fileinfo-ewoc'."
  (if dvc-fileinfo-ewoc
      (let ((res nil))
        (ewoc-map (lambda (fi)
                    (let ((x (dvc-fileinfo-legacy-data fi)))
                      (when (eq (car x) 'file)
                        (push (cadr x) res))))
                  dvc-fileinfo-ewoc)
        res)
    'dont-know))

;;;###autoload
(defun tla-edit-log (&optional insert-changelog source-buffer other-frame)
  "Edit the tla log file.

With an optional prefix argument INSERT-CHANGELOG, insert the last
group of entries from the ChangeLog file.  SOURCE-BUFFER, if non-nil,
is the buffer from which the function was called.  It is used to get
the list of marked files, and potentially run a selected file commit."
  (interactive "P")
  (setq source-buffer (or source-buffer
                          (dvc-get-buffer tla-arch-branch 'diff)
                          (dvc-get-buffer tla-arch-branch 'status)))
  (setq tla-pre-commit-window-configuration
        (current-window-configuration))
  (setq tla-log-edit-file-name (tla-make-log))
  (dvc-switch-to-buffer
   (find-file-noselect tla-log-edit-file-name))
  (when insert-changelog
    (goto-char (point-max))
    (let ((buf (find-file-noselect (find-change-log))))
      (insert-buffer-substring buf))
    (when (re-search-forward "^2" nil t)
      (delete-region (line-beginning-position)
                     (line-beginning-position 3)))
    (when (re-search-forward "^2" nil t)
      (delete-region (line-beginning-position) (point-max)))
    (goto-char (point-min)))
  ;; append list of marked files that will be committed with this log message
  (save-excursion
    (let ((file-list (if (buffer-live-p source-buffer)
                         (with-current-buffer source-buffer
                           (or dvc-buffer-marked-file-list
                               (tla-changes-file-list)))
                       'dont-know))
          (deleted (tla-edit-log-delete-file-list t)))
      (unless (and (eq file-list 'dont-know) (not deleted))
        (goto-char (point-max))
        ;; Previous line is not empty or headers are too close.
        (while (save-excursion (or (not (progn (forward-line -1)
                                               (looking-at "[ \t]*\n")))
                                   (progn (forward-line -1)
                                          (looking-at "[a-zA-Z]*:"))
                                   (progn (forward-line -1)
                                          (looking-at "[a-zA-Z]*:"))))
          (insert "\n"))
        (insert dvc-log-edit-file-list-marker "\n")
        (if (not file-list)
            (insert "No modified files.\n")
          (insert "Files to commit:\n")
          (if (eq file-list 'dont-know)
              (insert "   <Can't compute list right now. Run diff or status if you want it>\n")
            (while file-list
              (insert "   " (car file-list) "\n")
              (setq file-list (cdr file-list)))))
        (insert "\nThis list might be incomplete or outdated if editing the log")
        (insert "\nmessage was not invoked from an up-to-date changes buffer!"))))
  (tla-log-edit-mode)
  (set (make-local-variable 'tla-buffer-source-buffer)
       source-buffer)
  (goto-char (point-min))
  (tla-log-edit-next-field t)
  (let ((previous-point nil))
    (while (and (not (or (looking-at "$")
                         (equal (line-beginning-position)
                                (point))))
                ;; avoid loop.
                (not (equal previous-point (point))))
      (tla-log-edit-next-field t)
      (setq previous-point (point)))))

(defvar tla--changes-file-list nil
  "List of modified files.")

(defun tla--ewoc-collect-elem (ewoc predicate &rest args)
  "Same as `ewoc-collect', but returns the list of ewoc element."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((header (ewoc--header ewoc))
       (node (ewoc--node-nth dll -2))
       result)
    (while (not (eq node header))
      (if (apply predicate (ewoc--node-data node) args)
          (push node result))
      (setq node (ewoc--node-prev dll node)))
    (nreverse result)))

(defun tla--changes-find-subtree-message ()
  "Finds the messages \"searching subtree\" from the ewoc."
  (when dvc-fileinfo-ewoc
    (tla--ewoc-collect-elem
     dvc-fileinfo-ewoc
     (lambda (fi)
       (let ((elem (when (dvc-fileinfo-legacy-p fi) (dvc-fileinfo-legacy-data fi))))
         (eq (car elem) 'searching-subtrees))))))

(defvar tla--changes-summary nil
  "Wether the current buffer display only a summary or a full diff.")

(defvar tla--changes-buffer-master-buffer nil
  "Master buffer for a nested *{tla|baz}-changes* buffer.")

(defvar tla--changes-summary nil
  "Wether the current buffer display only a summary or a full diff.")

(defun tla--changes-command ()
  "\"tla changes\" or \"baz diff\" depending on `tla-arch-branch'."
  (if (eq tla-arch-branch 'tla)
      "tla changes" "baz diff"))

(defun tla-changes-goto (&optional summary)
  "Go to the changes buffer, or run `tla-changes'."
  (interactive "P")
  (let* ((root (dvc-read-project-tree-maybe
                (format "Run %s in: "
                        (tla--changes-command))))
         (default-directory root)
         (buffer (dvc-get-buffer tla-arch-branch 'diff root)))
    (if buffer (dvc-switch-to-buffer buffer)
      (tla-changes summary))))

(defmacro tla-recursive-command (function-to-define
                                 args command
                                 prepare-buffer
                                 recursive
                                 expression
                                 &optional expression-rec)
  (declare (indent 2) (debug (&define name sexp form form symbolp body)))
  `(defun ,function-to-define ,args
     ,(format "Run \"tla %s\".

When called without a prefix argument: show the detailed diffs also.
When called with a prefix argument SUMMARY: do not show detailed
diffs. When AGAINST is non-nil, use it as comparison tree." command)
     (interactive "P")
     (let* ((root (dvc-read-project-tree-maybe
                   (format "Run %s in: "
                           ,command)))
            (default-directory root)
            (buffer ,prepare-buffer))
       (with-current-buffer buffer
         (make-local-variable 'tla--changes-summary)
         (let ((inhibit-read-only t))
           (ewoc-enter-first
            dvc-fileinfo-ewoc
            (make-dvc-fileinfo-message
             :text (concat "* running " ,command " in tree " root
                           "...\n\n"))))
         (when ,recursive
           (ewoc-enter-last dvc-fileinfo-ewoc
                            (make-dvc-fileinfo-legacy
                             :data (list 'searching-subtrees))))
         (ewoc-refresh dvc-fileinfo-ewoc))
       (when dvc-switch-to-buffer-first
         (dvc-switch-to-buffer buffer))
       (dvc-save-some-buffers)
       ,expression
       (when ,recursive
         (tla--run-tla-async
          '("inventory" "--nested" "--trees")
          :related-buffer buffer
          :finished
          (dvc-capturing-lambda (output error status arguments)
            (let ((subtrees (delete ""
                                    (split-string
                                     (with-current-buffer
                                         output (buffer-string)) "\n"))))
              (with-current-buffer (capture buffer)
                (let ((subtree-message (car (tla--changes-find-subtree-message)))
                      (buffer-read-only nil))
                  (dolist (subtree subtrees)
                    (let ((buffer-sub (dvc-get-buffer-create tla-arch-branch
                                                             'diff subtree)))
                      (with-current-buffer buffer-sub
                        (dvc-save-some-buffers)
                        (let ((inhibit-read-only t))
                          (erase-buffer))
                        (dvc-diff-mode)
                        (set (make-local-variable
                              'tla--changes-buffer-master-buffer)
                             (capture buffer)))
                      (ewoc-enter-after dvc-fileinfo-ewoc
                                        subtree-message
                                        (make-dvc-fileinfo-legacy
                                         :data (list 'subtree buffer-sub subtree
                                                     nil)))
                      ,(or expression-rec expression)))
                  (dvc-ewoc-delete dvc-fileinfo-ewoc
                                   subtree-message))))))))))

(tla-recursive-command tla-changes-rec (&optional summary against)
  (tla--changes-command)
  (dvc-prepare-changes-buffer
   (or against
       `(,tla-arch-branch (last-revision ,root 1)))
   `(tla (local-tree ,root))
   'diff
   default-directory
   tla-arch-branch)
  tla-changes-recursive
  (progn
    (setq tla--changes-summary summary)
    (tla--changes-internal (not summary)
                           against
                           root buffer nil))
  (progn
    (setq tla--changes-summary (capture summary))
    (tla--changes-internal
     (not (capture summary))
     nil ;; TODO "against" what for a nested tree?
     subtree
     buffer-sub
     (capture buffer))))

;;;###autoload
(defun tla-changes (&optional summary against dont-switch)
  "Run \"tla changes\".

When called without a prefix argument: show the detailed diffs also.
When called with a prefix argument SUMMARY: do not show detailed
diffs. When AGAINST is non-nil, use it as comparison tree.

DONT-SWITCH is necessary for DVC, but currently ignored."
  (interactive "P")
  (tla-changes-rec summary against))

(defun tla--update-command ()
  (cond ((eq tla-update-strategy 'update) "update")
        ((eq tla-update-strategy 'merge)  (if (tla-has-merge-command) "merge" "star-merge"))
        ((eq tla-update-strategy 'replay) "replay")))

(defun tla--three-way-merge-option ()
  "Returns \"--three-way\", \"--two-way\", or nil.

Value is chosen depending on user configuration and arch branch."
  (if tla-three-way-merge
      (if (tla-merge-has-two-way-option)
          nil          ;; Requested a 3-way, but it's the default.
        "--three-way") ;; Requested a 3-way, not the default.
    (if (tla-merge-has-two-way-option)
        "--two-way"
      nil)))

(defun tla--show-ancestor-option ()
  "Returns \"--show-ancestor\" or nil.

Value is chosen depending on user configuration and arch branch."
  (if (and tla-show-ancestor
           (tla-merge-has-show-ancestor-option))
      "--show-ancestor"
    nil))

(defun tla--update-internal (root buffer master-buffer handle)
  (with-current-buffer (or buffer (current-buffer))
    (tla--run-tla-async
     (list (tla--update-command)
           (when (eq tla-update-strategy 'merge)
             (tla--three-way-merge-option))
           (when (eq tla-update-strategy 'merge)
             (tla--show-ancestor-option)))
     :finished (dvc-capturing-lambda (output error status arguments)
                 (let ((modifs (with-current-buffer output
                                 (goto-char (point-min))
                                 (not (re-search-forward
                                       "^\\* \\(tree is already up to date\\|skipping (empty delta)\\)"
                                       nil t)))))
                   (with-current-buffer (or (capture master-buffer)
                                            (capture buffer))
                     ;; (dvc-trace "buf=%S modifs=%S" (capture buffer) modifs)
                     (ewoc-map (lambda (fi)
                                 (let ((x (dvc-fileinfo-legacy-data fi)))
                                   (when (and (eq (car x) 'subtree)
                                              (eq (cadr x) (capture buffer)))
                                     (setcar (cdr (cddr x))
                                             (if modifs 'updated 'no-changes)))
                                   ))
                               ;; (ewoc-refresh dvc-fileinfo-ewoc)))
                               dvc-fileinfo-ewoc)
                     ))
                 (dvc-show-changes-buffer
                  output 'tla--parse-other (capture buffer))
                 (message "`%s update' finished" (tla--executable))
                 (dvc-revert-some-buffers (capture root))
                 (when (capture handle) (funcall (capture handle))))
     :error
     (lambda (output error status arguments)
       (dvc-show-error-buffer error)
       (dvc-show-last-process-buffer)
       ))))

(tla-recursive-command tla-update-rec (tree &optional handle recursive)
  (tla--update-command)
  (dvc-prepare-changes-buffer
   `(,tla-arch-branch (last-revision ,default-directory))
   `(,tla-arch-branch (local-tree ,default-directory))
   'diff
   default-directory
   tla-arch-branch)
  tla-update-recursive
  (tla--update-internal root buffer nil handle)
  (tla--update-internal subtree buffer-sub (capture buffer)
                        (capture handle)))

;;;###autoload
(defun tla-update (tree &optional handle recursive)
  "Run tla update in TREE.

Also runs update recursively for subdirectories.
After running update, execute HANDLE (function taking no argument)."
  (interactive (list (expand-file-name
                      (tla--read-directory-maybe "Update tree: "))))
  (tla-update-rec tree handle recursive))

;;;###autoload
(defun tla-changes-against (&optional summary against)
  "Wrapper for `tla-changes'.

When called interactively, SUMMARY is the prefix arg, and AGAINST is
read from the user."
  (interactive (list current-prefix-arg
                     `(,tla-arch-branch (revision ,(tla-name-read "Compute changes against: "
                                                                  'prompt 'prompt 'prompt 'prompt
                                                                  'maybe)))))
  (tla-changes summary against))

;;;###autoload
(defun tla-changes-last-revision (&optional summary)
  "Run `tla-changes' against the last but one revision.

The idea is that running this command just after a commit should be
equivalent to running `tla-changes' just before the commit.

SUMMARY is passed to `tla-changes'."
  (interactive "P")
  (let ((default-directory (dvc-read-project-tree-maybe
                            "Review last patch in directory: ")))
    (tla-changes summary `(,tla-arch-branch
                           (revision
                            ,(tla-revision-direct-ancestor))))))

(defun tla--changes-internal (diffs against root buffer master-buffer)
  "Internal function to run \"tla changes\".

If DIFFS is non nil, show the detailed diffs also.
Run the command against tree AGAINST in directory ROOT.
The output will be displayed in buffer BUFFER.

BUFFER must already be in changes mode, but mustn't contain any change
information. Only roots of subprojects are already in the ewoc.

If MASTER-BUFFER is non-nil, this run of tla changes is done in a
nested project of a bigger one. MASTER-BUFFER is the buffer in which
the root of the projects is displayed."
  (with-current-buffer buffer
    (dvc-trace "against=%S, (dvc-revision-get-data against)=%S"
               against (dvc-revision-get-data against))
    (tla--run-tla-async
     `(,(if (eq tla-arch-branch 'tla) "changes" "diff")
       ,(when (and (eq tla-arch-branch 'tla) diffs) "--diffs")
       ,@(when (and (eq tla-arch-branch 'baz) against root) (list "--dir" root))
       ,(case (dvc-revision-get-type against)
          (local-tree
           (error "Can not run tla changes or baz diff against a local tree"))
          (previous-revision (tla-revision-direct-ancestor
                              (dvc-revision-get-data against)))
          (last-revision (if (string= (dvc-uniquify-file-name
                                       (nth 0 (dvc-revision-get-data against)))
                                      (dvc-uniquify-file-name
                                       (tla-tree-root)))
                             nil
                           (error "Tla changes against last %s %s"
                                  "revision of local tree not"
                                  "implemented.")))
          (revision (tla--name-construct (car (dvc-revision-get-data against))))
          (t (when against (message "WRONG REVISION: %S" against) (sit-for 1) (debug)))))
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (if (capture master-buffer)
           (message "No changes in subtree %s" (capture root))
         (message "No changes in %s" (capture root)))
       (with-current-buffer (capture buffer)
         (let ((inhibit-read-only t))
           (dvc-fileinfo-delete-messages)
           (ewoc-enter-last dvc-fileinfo-ewoc
                            (make-dvc-fileinfo-message
                             :text (concat "* No changes in "
                                           (capture root) ".\n\n")))
           (when (capture master-buffer)
             (with-current-buffer (capture master-buffer)
               (ewoc-map (lambda (fi)
                           (let ((x (dvc-fileinfo-legacy-data fi)))
                             (when (and (eq (car x) 'subtree)
                                        (eq (cadr x) (capture buffer)))
                               (setcar (cdr (cddr x)) 'no-changes)))
                           )
                         ;; (ewoc-refresh dvc-fileinfo-ewoc)))
                         dvc-fileinfo-ewoc)))
           (ewoc-refresh dvc-fileinfo-ewoc))))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (if (/= 1 status)
           (let ((lint-pb
                  (with-current-buffer error
                    (goto-char (point-min))
                    (re-search-forward "(try \\(tree-lint\\|status --lint\\))"
                                       nil t))))
             (if lint-pb
                 (progn
                   (message "Tree is not lint clean. Running lint")
                   (save-window-excursion
                     (tla-tree-lint (capture root)))
                   (let ((buffer (dvc-get-buffer
                                  tla-arch-branch
                                  'tree-lint (capture root))))
                     (when buffer
                       (switch-to-buffer buffer)
                       ;; (dvc-trace "buf=%S" (buffer-name))
                       (set (make-local-variable
                             'tla--tree-lint-nowarning-fn)
                            ;; I prefer not trying to nest
                            ;; dvc-capturing-lambda ...
                            `(lambda ()
                               (tla--changes-internal
                                ,(capture diffs) ,(capture against)
                                ,(capture root) ,(capture buffer)
                                ,(capture master-buffer))
                               (switch-to-buffer
                                (dvc-get-buffer
                                 tla-arch-branch 'diff
                                 ,(capture root))))))))
               (with-current-buffer (capture buffer)
                 (dvc-fileinfo-delete-messages)
                 (ewoc-enter-last
                  dvc-fileinfo-ewoc
                  (make-dvc-fileinfo-message
                   :text (concat "* error in process:\n"
                                 (dvc-buffer-content output)
                                 (dvc-buffer-content error))))
                 (ewoc-refresh dvc-fileinfo-ewoc)
                 (message "Error in diff process"))))
         (dvc-show-changes-buffer output
                                  (if (eq tla-arch-branch 'tla)
                                      'tla--parse-other 'tla--parse-baz-diff)
                                  (capture buffer)
                                  (capture master-buffer)
                                  "^[^*\\.]")
         ;; FIXME: DVC does not currently support nested trees
         (when (capture master-buffer)
           (with-current-buffer (capture master-buffer)
             (ewoc-map (lambda (fi)
                         (let ((x (dvc-fileinfo-legacy-data fi)))
                           (when (and (eq (car x) 'subtree)
                                      (eq (cadr x) (capture buffer)))
                             (setcar (cdddr x) 'changes))))
                       dvc-fileinfo-ewoc)))))
     )))

(defconst tla-verbose-format-spec
  '(("added files"    "A" " ")
    ("modified files" "M" " ")
    ("removed files"  "D" " "))
  "Internal variable used to parse the output of tla show-changeset."
  )

(defun tla--parse-show-changeset (changes-buffer)
  (progn
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^\\* \\(" (regexp-opt
                                (mapcar 'car tla-verbose-format-spec))
                    "\\)\n")
            nil t)
      (let* ((elem (assoc (match-string 1)
                          tla-verbose-format-spec))
             (modif (cadr elem))
             (dir (caddr elem)))
        ;; (dvc-trace "modif=%S" modif)
        (if (string= modif "M")
            (while (re-search-forward "^--- orig/\\(.*\\)$"
                                      nil t)
              (let ((file (match-string 1)))
                (with-current-buffer changes-buffer
                  (ewoc-enter-last
                   dvc-fileinfo-ewoc
                   (make-dvc-fileinfo-legacy
                    :data (list 'file (tla-unescape file)
                                modif dir))))))
          (while (looking-at "^$") (forward-line 1))
          (while (looking-at "^ +\\([^ ].*\\)$")
            (let ((file (match-string 1)))
              (with-current-buffer changes-buffer
                (ewoc-enter-last
                 dvc-fileinfo-ewoc
                 (make-dvc-fileinfo-legacy
                  :data (list 'file (tla-unescape file)
                              modif dir))))
              (forward-line 1)))
          (while (looking-at "^--- /dev/null\n\\+\\+\\+ mod/\\(.*\\)$")
            (let ((file (match-string 1)))
              (with-current-buffer changes-buffer
                (ewoc-enter-last
                 dvc-fileinfo-ewoc
                 (make-dvc-fileinfo-legacy
                  :data (list 'file (tla-unescape file)
                              modif dir))))
              (forward-line 1)
              (re-search-forward "^\\(---\\|$\\|\\*\\)" nil t)
              (beginning-of-line))))))
    (goto-char (point-min))
    (re-search-forward "^---" nil t)
    (beginning-of-line)))

(defconst tla--files-conflicted-regexp
  "^\\* The following.*files are conflicted:")

(defun tla--parse-baz-status (changes-buffer)
  "Called from the output buffer of \"baz status\".

CHANGES-BUFFER is the target buffer."
  (goto-char (point-min))
  (re-search-forward "^[^*\\.]" nil t)
  (beginning-of-line)
  ;; point is at the beginning of first relevant line.
  (unless (re-search-backward tla--files-conflicted-regexp nil t)
    (while (looking-at
            "\\([CRADP\\? ]\\)\\(.\\) *\\([^ \n\t]*\\)\\( => \\([^ \n\t]*\\)\\)?$")
      (let ((file-es    (match-string-no-properties 3))
            (status     (match-string-no-properties 1))
            (modif      (match-string-no-properties 2))
            (origname-es (match-string-no-properties 5)))
        (let ((file     (tla-unescape file-es))
              (origname (tla-unescape origname-es)))
          (if origname
              (let ((tmp origname))
                (setq origname file)
                (setq file tmp)))
          (with-current-buffer changes-buffer
            (ewoc-enter-last
             dvc-fileinfo-ewoc
             (make-dvc-fileinfo-legacy
              :data (list 'file
                          file status modif
                          (if (file-directory-p file)
                              "/" " ")
                          origname))))
          (forward-line 1)))))
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           tla--files-conflicted-regexp nil t)
      (re-search-forward "[^ \n\t]" nil t)
      (beginning-of-line)
      (while (looking-at "[ \t]*\\([^ \t\n]+\\)$")
        (let ((file (match-string-no-properties 1)))
          (with-current-buffer changes-buffer
            (ewoc-enter-last
             dvc-fileinfo-ewoc
             (make-dvc-fileinfo-legacy
              :data (list 'file
                          file "C" " "
                          (if (file-directory-p file)
                              "/" " ")))))
          (forward-line 1))))))

(defun tla--parse-other (changes-buffer)
  "Parses, for example, the output of tla-changes."
  (beginning-of-line)
  (while (or (eq (char-after) ?*)
             (eq (char-after) ?.)
             (looking-at "Searching for best merge")
             ;; WARNING: If `-' doesn't stand for a range,
             ;; it must be at the last in `[]'.
             (looking-at "^\\([^=]\\|=[A-Z>]\\)\\([ /Abfl>M-]?\\)\\(/?\\) +\\([^\t\n]*\\)\\(\t\\(.*\\)\\)?$"))
    (if (or (looking-at "Searching for best merge")
            (eq (char-after) ?*)
            (eq (char-after) ?.))
        (let ((msg (buffer-substring-no-properties
                    (point) (line-end-position))))
          (with-current-buffer changes-buffer
            (ewoc-enter-last dvc-fileinfo-ewoc
                             (make-dvc-fileinfo-message :text msg))))
      (let ((file (match-string 4))
            (modif (match-string 1))
            (dir (match-string 2))
            (maybedir (match-string 3))
            (newname (match-string 6)))
        ;;         (dvc-trace "file=%S modif=%S dir=%S maybedir=%S newname=%S"
        ;;                     file modif dir maybedir newname)
        (if (and (string= modif "-")
                 (string= dir "-"))
            (setq dir maybedir))
        (when (and (not (string= dir "/"))
                   (not (string= dir " ")))
          (setq dir " "))
        (when (string= dir "b")
          (setq dir " "))
        (let ((baz-modif modif)
              (baz-status " "))
          (cond ((string= modif "M")
                 (setq baz-modif "M"))
                ((string= modif "A")
                 (setq baz-status "A")
                 (setq baz-modif " "))
                ((string= modif "D")
                 (setq baz-status "D")
                 (setq baz-modif " "))
                ((and (string= modif "=")
                      (string= dir ">"))
                 (setq baz-modif " ")
                 (setq baz-status "R")
                 (setq dir " "))
                ((and (string= modif "/")
                      (string= dir ">"))
                 (setq baz-modif " ")
                 (setq baz-status "R")
                 (setq dir "/"))
                ((string= modif "-")
                 (setq baz-status "P")
                 (setq baz-modif " "))
                ((and (string= modif "?")
                      (string= dir "M"))
                 (setq baz-status "?")
                 (setq baz-modif " ")))
          (with-current-buffer changes-buffer
            (if newname
                (ewoc-enter-last
                 dvc-fileinfo-ewoc
                 (make-dvc-fileinfo-legacy
                  :data (list 'file
                              (tla-unescape newname)
                              baz-status baz-modif dir
                              (tla-unescape file))))
              (ewoc-enter-last
               dvc-fileinfo-ewoc
               (make-dvc-fileinfo-legacy
                :data (list 'file
                            (tla-unescape file)
                            baz-status
                            baz-modif dir))))))))
    (forward-line 1)))

(defun tla--parse-baz-diff (changes-buffer)
  (if (looking-at "^[^\\*]")
      (tla--parse-other changes-buffer)
    (save-excursion
      (while (re-search-forward
              "^--- \\(orig/\\)?\\([^\n]*\\)\n\\+\\+\\+ mod/\\([^\n]*\\)$" nil t)
        (let* ((origname (match-string-no-properties 2))
               (newname  (match-string-no-properties 3))
               (renamed  (not (string= origname newname)))
               (added    (not (string= (match-string-no-properties 1)
                                       "orig/"))))
          (dvc-trace "entering file %S in ewoc (orig=%S, renamed=%S, added=%S)"
                     newname origname renamed added)
          (with-current-buffer changes-buffer
            (ewoc-enter-last
             dvc-fileinfo-ewoc
             (make-dvc-fileinfo-legacy
              :data (list 'file
                          newname
                          (cond (added   "A")
                                (renamed "R")
                                (t " "))
                          (cond (added " ")
                                (t "M"))
                          " "           ; dir
                          (when (and renamed
                                     (not added))
                            origname))))))))))

(defun tla-changes-save (directory)
  "Run \"tla changes -o\" to create a changeset.
When tla has a diff command, use \"baz diff -o\".

The changeset is stored in DIRECTORY."
  (interactive "FDirectory to store the changeset: ")
  (tla--run-tla-sync (list (if (tla-has-diff-command)
                               "diff" "changes") "-o" directory)
                     :finished (lambda (output error status arguments)
                                 (dvc-trace "tla-changes-save: 0"))
                     :error (dvc-capturing-lambda (output error status arguments)
                              (case status
                                (1 (message "tla-changes-save to %s finished" (capture directory)))
                                (otherwise (dvc-default-error-function
                                            output error status arguments))))))

(defun tla-changes-save-as-tgz (file-name)
  "Run \"tla changes -o\" to create .tar.gz file.
The changeset is stored in the tarball 'FILE-NAME.tar.gz'."
  (interactive "FFile to store the changeset (without .tar.gz extension): ")
  (let* ((changeset-dir (expand-file-name file-name))
         (tgz-file-name (concat changeset-dir ".tar.gz")))
    (when (file-directory-p changeset-dir)
      (error "The changeset directory %s does already exist" changeset-dir))
    (when (file-exists-p tgz-file-name)
      (error "The changeset tarball %s does already exist" tgz-file-name))
    (tla-changes-save changeset-dir)
    (dvc-create-tarball-from-intermediate-directory changeset-dir tgz-file-name)))

(defun tla-changeset-save-as-tgz (revision file-name)
  "Create a changeset tarball for a given REVISION.

FILE-NAME specifies the base name. A '.tar.gz' extension is appended."
  (interactive (list
                (tla--name-construct
                 (tla-name-read "Revision: "
                                'prompt 'prompt 'prompt 'prompt 'prompt))
                (read-file-name "File to store the changeset (without .tar.gz extension): ")))
  (let ((changeset-dir (dvc-make-temp-name "tla-changeset"))
        (tgz-file-name (concat (expand-file-name file-name)  ".tar.gz")))
    (tla-get-changeset revision nil changeset-dir)
    (dvc-create-tarball-from-intermediate-directory changeset-dir tgz-file-name)))

;;;###autoload
(defun tla-delta (base modified &optional directory dont-switch)
  "Run tla delta BASE MODIFIED.
If DIRECTORY is a non-empty string, the delta is stored to it.
If DIRECTORY is ask, a symbol, ask the name of directory.
If DIRECTORY is nil or an empty string, just show the delta using --diffs."
  (interactive (list
                (tla--name-construct
                 (tla-name-read "Base: "
                                'prompt 'prompt 'prompt 'prompt 'prompt))
                (tla--name-construct
                 (tla-name-read "Modified: "
                                'prompt 'prompt 'prompt 'prompt 'prompt))
                (when current-prefix-arg
                  'ask)))

  (when (eq directory 'ask)
    (setq directory
          (dvc-read-directory-name "Stored to: "
                                   (tla-tree-root default-directory t)
                                   (tla-tree-root default-directory t)
                                   nil
                                   "")))

  (when (and directory (stringp directory) (string= directory ""))
    (setq directory nil))

  (when (and directory (file-directory-p directory))
    (error "%s already exists" directory))

  (let ((args
         (if directory
             (list "delta" base modified directory)
           (list "delta" "--diffs" base modified)))
        (run-dired-p (when directory 'ask))
        (buffer (dvc-prepare-changes-buffer
                 `(,tla-arch-branch
                   (revision ,(tla--name-split base)))
                 `(,tla-arch-branch
                   (revision ,(tla--name-split modified)))
                 'changeset
                 modified
                 tla-arch-branch)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer))
    (tla--run-tla-async args
                        :finished
                        (dvc-capturing-lambda (output error status arguments)
                          (if (capture directory)
                              (tla--delta-show-directory (capture directory) (capture run-dired-p))
                            (tla--delta-show-diff-on-buffer
                             (capture buffer)
                             output (capture base) (capture modified)
                             (capture dont-switch)))))
    buffer))

(defun tla--delta-show-diff-on-buffer (buffer output base modified &optional dont-switch)
  "Show the result of \"delta -diffs\".

OUTPUT is the output buffer of the tla process.
BASE is the name of the base revision, and MODIFIED is the name of the
modified revision, (then command being run is tla delta BASE
MODIFIED)."
  (with-current-buffer output
    (let ((no-changes
           ;; There were no changes if the last line of
           ;; the buffer is "* changeset report"
           (save-excursion
             (goto-char (point-max))
             (previous-line 1)
             (beginning-of-line)
             (looking-at "^* changeset report"))))
      (if no-changes
          (message
           (concat "tla delta finished: "
                   "No changes in this arch working copy"))
        (dvc-show-changes-buffer output 'tla--parse-other buffer)
        (unless dont-switch
          (dvc-switch-to-buffer buffer))
        (message "tla delta finished")))))

(defun tla--delta-show-directory (directory run-dired-p)
  "Called by `tla-delta' to show a changeset in DIRECTORY.

If RUN-DIRED-P is non-nil, run dired in the parent directory of the
changeset."
  (tla-show-changeset directory nil)
  (when (tla--do-dired (concat (file-name-as-directory directory) "..")  run-dired-p)
    (revert-buffer)
    (goto-char (point-min))
    (re-search-forward (concat
                        (regexp-quote (file-name-nondirectory directory))
                        "$"))
    (goto-char (match-beginning 0))
    (dvc-flash-line)))

;; (defvar tla--get-changeset-start-time nil)
;; (defvar tla--changeset-cache (make-hash-table :test 'equal)
;;   "The cache for `tla-get-changeset'.
;; A hashtable, where the revisions are used as keys.
;; The value is a list containing the time the cache data was recorded and
;; the text representation of the changeset.")

;;;###autoload
(defun tla-get-changeset (revision justshow &optional destination
                                   without-diff)
  "Gets the changeset corresponding to REVISION.

When JUSTSHOW is non-nil (no prefix arg), just show the diff.
Otherwise, store changeset in DESTINATION.
If WITHOUT-DIFF is non-nil, don't use the --diff option to show the
changeset."
  (interactive
   (list (let ((current-version (tla-tree-version nil t)))
           (tla--name-construct
            (apply 'tla-name-read "Revision to view: "
                   (if current-version
                       (mapcar (lambda (x)
                                 (or x 'prompt))
                               (tla--name-split current-version))
                     (list 'prompt 'prompt 'prompt 'prompt 'prompt)))))
         (not current-prefix-arg)))
  (let ((buffer (dvc-get-buffer tla-arch-branch 'changeset revision)))
    (if buffer (save-selected-window (dvc-switch-to-buffer buffer))
      (let* ((dest (or destination
                       (dvc-make-temp-name "tla-changeset")))
             (rev-list (if (stringp revision)
                           (tla--name-split revision) revision))
             (revision (if (stringp revision) revision
                         (tla--name-construct revision)))
             (buffer (and justshow
                          (dvc-prepare-changes-buffer
                           `(,tla-arch-branch
                             (previous-revision (,tla-arch-branch
                                                 (revision ,rev-list))
                                                1))
                           `(,tla-arch-branch
                             (revision ,rev-list))
                           'changeset revision
                           tla-arch-branch)))
             (dvc-switch-to-buffer-mode
              (if tla-switch-to-changes-buffer
                  dvc-switch-to-buffer-mode 'show-in-other-window)))
        (when (and justshow dvc-switch-to-buffer-first)
          (dvc-switch-to-buffer buffer))
        (tla--run-tla-async
         (list "get-changeset" revision dest)
         :finished
         (dvc-capturing-lambda (output error status arguments)
           (when (capture justshow)
             (tla-show-changeset
              (capture dest) (capture without-diff) (capture buffer))
             (call-process "rm" nil nil nil "-rf" (capture dest)))))
        buffer))))

(defun tla-show-changeset (directory &optional without-diff buffer
                                     base modified)
  "Run tla show-changeset on DIRECTORY.

If prefix argument, WITHOUT-DIFF is non-nil, just show the summary.
BUFFER is the target buffer to output.  If BUFFER is nil, create a new
one.

BASE and MODIFIED are the name of the base and modified.  Their values
will be used for the variables `dvc-diff-base' and
`dvc-diff-modified'."
  (interactive (list (let ((changeset-dir (or (dvc-get-file-info-at-point) "")))
                       (unless (file-directory-p (expand-file-name changeset-dir))
                         (setq changeset-dir ""))
                       (dvc-uniquify-file-name
                        (dvc-read-directory-name
                         "Changeset directory to view: "  changeset-dir changeset-dir)))))
  (unless buffer
    (setq buffer (dvc-prepare-changes-buffer
                  base modified ;; TODO don't seem to be set.
                  'changeset directory
                  tla-arch-branch))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)))
  (tla--run-tla-sync (list "show-changeset"
                           (unless without-diff
                             "--diffs")
                           directory)
                     :finished
                     (dvc-capturing-lambda (output error status arguments)
                       (dvc-show-changes-buffer output
                                                (if (capture without-diff)
                                                    'tla--parse-other
                                                  'tla--parse-show-changeset)
                                                (capture buffer)
                                                (capture dvc-switch-to-buffer-first))
                       (dvc-post-switch-to-buffer))))

(defun tla-show-changeset-from-tgz (file)
  "Show the archived changeset from a tar.gz FILE.
Such a changeset can be created via `tla-changes-save-as-tgz'."
  (interactive (list (let ((changeset-tarball (or (dvc-get-file-info-at-point)
                                                  (and
                                                   (eq major-mode 'dired-mode)
                                                   (dired-get-filename))
                                                  "")))
                       (expand-file-name
                        (read-file-name "Changeset tarball to view: "
                                        nil changeset-tarball t changeset-tarball)))))
  (let ((temp-dir (dvc-make-temp-name "tla-changeset-tgz"))
        (changeset-dir))
    ;;(message "temp-dir: %s" temp-dir)
    (call-process "mkdir" nil nil nil temp-dir)
    (call-process "tar" nil nil nil "xfz" file "-C" temp-dir)
    (setq changeset-dir (car (delete "." (delete ".." (directory-files temp-dir)))))
    (tla-show-changeset (concat (dvc-uniquify-file-name temp-dir) changeset-dir))
    (call-process "rm" nil nil nil "-rf" temp-dir)))

;;;###autoload
(defun tla-apply-changeset (changeset target &optional reverse)
  "Call \"tla apply-changeset\".

CHANGESET is the changeset to apply, TARGET is the directory in which
to apply the changeset. If REVERSE is non-nil, apply the changeset in
reverse."
  (interactive "DChangeset Directory: \nDTarget Directory: \nP")
  (if (file-directory-p changeset)
      (setq changeset (expand-file-name changeset))
    (error "%s is not directory" changeset))
  (if (file-directory-p target)
      (setq target (expand-file-name target))
    (error "%s is not directory" target))

  (or (dvc-save-some-buffers target)
      (y-or-n-p
       "Apply-change may delete unsaved changes.  Continue anyway? ")
      (error "Not applying"))
  (tla--apply-changeset-internal changeset target reverse)
  (when (y-or-n-p (format "Run inventory at `%s'? " target))
    (tla-inventory target)))

(defun tla--apply-changeset-internal (changeset target reverse)
  "Actually call \"tla apply-changeset CHANGESET TARGET\".

If REVERSE is non-nil, use --reverse too."
  (let ((buffer (dvc-prepare-changes-buffer nil nil 'diff default-directory tla-arch-branch)))
    (tla--run-tla-sync (list "apply-changeset"
                             (when reverse "--reverse")
                             ;; (when tla-use-forward-option "--forward")
                             changeset target)
                       :finished (dvc-capturing-lambda (output error status arguments)
                                   ;; (tla--show-last--process-buffer)
                                   (dvc-show-changes-buffer output 'tla--parse-other (capture buffer))
                                   (message "tla apply-changeset finished")
                                   (dvc-revert-some-buffers (capture target))))))

(defun tla-apply-changeset-from-tgz (file tree show-changeset)
  "Apply changeset in FILE to TREE.
If SHOW-CHANGESET is t: Show the changeset and ask the user, if the patch should
be applied. Otherwise apply the changeset without confirmation."
  (interactive "fApply changeset from tarball: \nDApply to tree: ")
  (let ((target (tla-tree-root tree))
        (temp-dir (dvc-make-temp-name "tla-changeset-tgz"))
        (changeset-dir))
    (call-process "mkdir" nil nil nil temp-dir)
    (call-process "tar" nil nil nil "xfz" (expand-file-name file) "-C" temp-dir)
    (setq changeset-dir (concat (dvc-uniquify-file-name temp-dir)
                                (car (delete "." (delete ".." (directory-files temp-dir))))))
    (when show-changeset
      (tla-show-changeset changeset-dir))
    (when (or (not show-changeset) (yes-or-no-p "Apply the changeset? "))
      (setq default-directory tree)
      (tla-apply-changeset changeset-dir target))
    (call-process "rm" nil nil nil "-rf" temp-dir)))


;;;###autoload
(defun tla-file-ediff-against (file &optional base)
  "View changes in FILE between BASE and MODIFIED using ediff."
  (interactive (let ((version-list (tla-tree-version-list)))
                 (list (buffer-file-name)
                       (list 'revision
                             (tla-name-read "Base revision: "
                                            (tla--name-archive version-list)
                                            (tla--name-category version-list)
                                            (tla--name-branch version-list)
                                            (tla--name-version version-list)
                                            'prompt)))))
  (dvc-ediff-buffers
   (or (get-file-buffer file) (find-file-noselect file))
   (tla-file-get-revision-in-buffer file base)))

;;;###autoload
(defun tla-file-diff (file &optional base modified dont-switch)
  "Run \"tla file-diff\" on file FILE.

In interactive mode, the file is the current buffer's file.
If REVISION is specified, it must be a string representing a revision
name, and the file will be diffed according to this revision."
  (interactive (list (buffer-file-name)))
  (let* ((file (dvc-uniquify-file-name file))
         (buffer (dvc-get-buffer-create tla-arch-branch 'file-diff file))
         (orig-buffer (current-buffer)))
    (if dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer)
      (set-buffer buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (diff-mode)
    (when dont-switch (pop-to-buffer orig-buffer))
    (let ((default-directory (tla-tree-root file))
          (file (tla-file-name-relative-to-root file)))
      (tla--run-tla-async
       (list (if (tla-has-file-diff-command)
                 "file-diff" "file-diffs")
             file base)
       :finished
       (lambda (output error status arguments)
         (message "No changes in this arch working copy"))
       :error
       (dvc-capturing-lambda (output error status arguments)
         (if (= 1 status)
             (progn
               (with-current-buffer (capture buffer)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert-buffer-substring output)
                   (toggle-read-only 1)))
               (unless (or dvc-switch-to-buffer-first (capture dont-switch))
                 (dvc-switch-to-buffer (capture buffer))))
           (dvc-default-error-function
            output error status arguments)))))))

(defvar tla-mine-string "TREE")
(defvar tla-his-string "MERGE-SOURCE")

(eval-when-compile
  (defvar smerge-mode))

;;;###autoload
(defun tla-resolved (file)
  "Command to delete .rej file after conflicts resolution.
Asks confirmation if the file still has diff3 markers.

If \"resolved\" command is available, also run it."
  (interactive
   (list (let ((file (buffer-file-name)))
           (if (string-match "^\\(.*\\)\\.rej$" file)
               (let ((norej (match-string 1 file)))
                 (if (y-or-n-p (format "Use file %s instead of %s? "
                                       (file-name-nondirectory norej)
                                       (file-name-nondirectory file)))
                     norej
                   file))
             file))))
  (with-current-buffer (find-file-noselect file)
    (if (and (boundp 'smerge-mode) smerge-mode)
        (progn
          (when (and
                 (save-excursion
                   (goto-char (point-min))
                   (dvc-funcall-if-exists smerge-find-conflict))
                 (not (y-or-n-p (concat "Buffer still has diff3 markers. "
                                        "Delete .rej file anyway? "))))
            (error "Not deleting .rej file"))
          (dvc-funcall-if-exists smerge-mode -1))
      (when (not (y-or-n-p (concat "Buffer "
                                   (buffer-name)
                                   " is not in in smerge-mode. "
                                   "Delete .rej file anyway? ")))
        (error "Not deleting .rej file")))
    ;; maybe run baz resolved.
    (if (tla-has-resolved-command)
        (let ((default-directory (tla-tree-root file)))
          (tla--run-tla-async `("resolved"
                                ,(tla-file-name-relative-to-root
                                  file))
                              :finished 'dvc-null-handler)))
    ;; delete .rej file
    (let ((rejfile (concat file ".rej")))
      (if (file-exists-p rejfile)
          (progn
            (when (get-file-buffer rejfile)
              (kill-buffer (get-file-buffer rejfile)))
            (delete-file rejfile)
            (message "deleted file %s" rejfile))
        (error (format "%s: no such file" rejfile))))))

(defalias 'tla-conflicts-finish 'tla-resolved)

;;;###autoload
(defun tla-view-conflicts (buffer)
  "*** WARNING: semi-deprecated function.
Use this function if you like, but M-x smerge-mode RET is actually
better for the same task ****

Graphical view of conflicts after tla star-merge --three-way. The
buffer given as an argument must be the content of a file with
conflicts markers like.

    <<<<<<< TREE
    my text
    =======
    his text
    >>>>>>> MERGE-SOURCE

Priority is given to your file by default. (This means all conflicts
will be rejected if you do nothing)."
  (interactive (list (find-file (read-file-name "View conflicts in: "))))
  (let ((mine-buffer buffer)
        (his-buffer (get-buffer-create "*tla-his*")))
    (with-current-buffer his-buffer
      (erase-buffer)
      (insert-buffer-substring mine-buffer)
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
                                        (regexp-quote tla-mine-string) "$")
                                nil t)
        (beginning-of-line)
        (delete-region (point) (progn
                                 (re-search-forward "^=======\n")))
        (re-search-forward
         (concat "^>>>>>>> "
                 (regexp-quote tla-his-string) "$"))
        (beginning-of-line)
        (delete-region (point) (1+ (line-end-position)))
        )
      )
    (with-current-buffer mine-buffer
      (goto-char (point-min))
      (while (re-search-forward (concat "^<<<<<<< "
                                        (regexp-quote tla-mine-string) "$")
                                nil t)
        (beginning-of-line)
        (delete-region (point) (1+ (line-end-position)))
        (re-search-forward "^=======$")
        (beginning-of-line)
        (delete-region (point) (progn
                                 (re-search-forward
                                  (concat "^>>>>>>> "
                                          (regexp-quote tla-his-string) "\n"))))
        ))
    (dvc-ediff-buffers mine-buffer his-buffer)
    ))

(defun tla-file-get-revision-in-file (file &optional revision)
  "Get the last-committed version of FILE.

If REVISION is non-nil, it must be a cons representing the revision,
and this revision will be used as a reference.

Return (file temporary).  temporary is non-nil when the file is
temporary and should be deleted."
  (case (car revision)
    (local-tree (list file nil))
    (previous-revision (tla-file-get-revision-in-file
                        file
                        (list 'revision
                              (tla-revision-direct-ancestor
                               (cadr revision)))))
    ((last-revision revision)
     (error "tla-file-get-revision-in-file has moved to DVC, use dvc-revision-get-file-in-buffer instead"))))

(defun tla-file-revert (file &optional revision)
  "Revert the file FILE to the last committed version.

Warning: You use version control to keep backups of your files.  This
function will by definition not keep any backup in the archive.

Most of the time, you should not use this function.  Call
`tla-file-ediff' instead, and undo the changes one by one with the key
`b', then save your buffer.

As a last chance, `tla-file-revert' keeps a backup of the last-saved in
~ backup file.

If REVISION is non-nil, it must be a cons representing the revision,
and this revision will be used as a reference."
  (interactive (list (progn (when (and (buffer-modified-p)
                                       (or dvc-do-not-prompt-for-save
                                           (y-or-n-p (format "Save buffer %s? "
                                                             (buffer-name
                                                              (current-buffer))))))
                              (save-buffer))
                            (buffer-file-name))))
  ;; set aside a backup copy
  (when (file-exists-p file)
    (copy-file file (car (find-backup-file-name file)) t))

  (let* ((file-unmo-temp (dvc-revision-get-file-in-buffer
                          file (if revision
                                   (list 'revision revision)
                                 `(baz (last-revision ,(tla-tree-root) 1)))))
         (original file-unmo-temp))

    ;; display diff
    (tla--run-tla-sync (list "file-diffs" file revision)
                       :finished
                       (lambda (output error status arguments)
                         (if (equal (nth 8 (file-attributes file))
                                    (nth 8 (file-attributes original)))
                             (error "File %s is not modified!"
                                    (cadr arguments))))
                       :error
                       (lambda (output error status arguments)
                         (if (/= 1 status)
                             (dvc-default-error-function
                              output error status arguments)
                           (dvc-show-last-process-buffer
                            'file-diff
                            (lambda ()
                              (goto-char (point-min))
                              (let ((inhibit-read-only t))
                                (insert
                                 (format "M %s\n" (cadr arguments))
                                 "Do you really want to revert ALL the changes listed below?\n")
                                (if dvc-highlight (font-lock-fontify-buffer)))
                              (diff-mode))))))


    (unless (yes-or-no-p (format "Really revert %s? " file))
      (bury-buffer)
      (error "Not reverting file %s!" file))
    (bury-buffer)
    (let ((buf (get-file-buffer file)))
      (erase-buffer)
      (insert-buffer-substring original)
      (save-buffer))))

(defun tla-undo (tree &optional
                      archive category branch version revision)
  ;;checkdoc-params: (archive category branch version revision)
  "Undo whole local TREE against ARCHIVE/CATEGORY-BRANCH-VERSION-REVISION.
If ARCHIVE is nil, default ARCHIVE/CATEGORY-BRANCH-VERSION-REVISION
associated with TREE.

When called interactively, call tla undo in the current tree.
When called interactively with a prefix argument, additionally ask
for the revision to revert to.

The tla-undo shows the changeset first, then it asks for confirmation before
running tla undo."
  (interactive
   (if (not current-prefix-arg)
       (list default-directory nil nil nil nil nil)
     (cons default-directory
           (tla--read-revision-with-default-tree "Undo against revision: "
                                                 default-directory))))
  (tla--undo-internal tree nil nil archive category branch version revision))


(defun tla--undo-internal (tree &optional dont-ask-for-confirmation no-output
                                archive category branch version revision)
  ;;checkdoc-params: (tree archive category branch version revision)
  "Internal function used by `tla-undo'.
If DONT-ASK-FOR-CONFIRMATION is given, don't show the changes buffer and don't
ask for confirmation.
If NO-OUTPUT is given, run tla undo with the --no-output flag."
  (unless dont-ask-for-confirmation
    (save-excursion (if archive
                        (tla-changes nil (tla--name-construct
                                          archive category branch version revision))
                      (tla-changes)))
    (sit-for 1)) ;;tla-changes should start before the yes-or-no-p query
  (when (or dont-ask-for-confirmation (yes-or-no-p
                                       (if archive
                                           (format "Revert whole local tree (%s) from `%s'? "
                                                   tree (tla--name-construct
                                                         archive category branch version revision))
                                         (format "Revert whole local tree (%s) from default revision? " tree))))
    (let ((default-directory tree)
          (rev (when archive (tla--name-construct archive category branch version revision)))
          (extra-flags (when no-output "--no-output")))
      (tla--run-tla-sync (delete nil (list "undo" extra-flags rev)))
      ;; TODO in case of files violating the naming
      ;; conventions we could offer to delete them  or
      ;; switch to inventory-mode and do it there,
      ;; basically saying YES should delete them and
      ;; perform the undo operation again
      ))
  (dvc-revert-some-buffers tree))

(defun tla--get-undo-changeset-names ()
  "Get the list of directories starting with \",,undo-\".

This is used by tla-redo to get the list of candidates for an undo
changeset."
  (interactive)
  (directory-files (tla-tree-root default-directory t) t ",,undo-"))

(defun tla--select-changeset (dir-list)
  "Select a changeset.

DIR-LIST is intended to be the result of
`tla--get-undo-changeset-names'."
  (dvc-completing-read "Select changeset: " (mapcar 'list dir-list) nil nil (car dir-list)))


(defun tla-redo (&optional target)
  "Run tla redo.
If TARGET directroy is given, TARGET should hold undo data generated by `tla undo'."
  (interactive)
  (let* ((undo-changesets (tla--get-undo-changeset-names))
         (undo-changeset (or target
                             (when (= (length undo-changesets) 1) (car undo-changesets))
                             (tla--select-changeset undo-changesets))))
    (tla-show-changeset undo-changeset)
    (when (yes-or-no-p (format "Redo the %s changeset? " undo-changeset))
      (tla--run-tla-sync (list "redo" undo-changeset)))))


;; TODO: being ported to DVC.
;;;###autoload
(defun tla-file-ediff (file &optional revision)
  "Interactive view of differences in FILE with ediff.

Changes are computed since last commit (or REVISION if specified)."
  (interactive (list (progn (when (and (buffer-modified-p)
                                       (y-or-n-p (format "Save buffer %s? "
                                                         (buffer-name
                                                          (current-buffer)))))
                              (save-buffer))
                            (buffer-file-name))))
  (let ((original (tla-file-get-revision-in-buffer
                   file (or revision (list 'last-revision
                                           (tla-tree-root))))))
    (when (string= (with-current-buffer original (buffer-string))
                   (buffer-string))
      (error "No modification in this file"))
    (dvc-ediff-buffers (or (get-file-buffer file)
                           (find-file-noselect file))
                       original)))

;;;###autoload
(defun tla-file-view-original (file &optional revision)
  "Get the last-committed version of FILE in a buffer.

If REVISION is specified, it must be a cons representing the revision
for which to get the original."
  (interactive (list (buffer-file-name)))
  (let ((original (tla-file-get-revision-in-buffer
                   file (or revision (list 'last-revision
                                           (tla-tree-root))))))
    (when (string= (with-current-buffer original (buffer-string))
                   (buffer-string))
      (message "No modification in this file"))
    (dvc-switch-to-buffer original)))

(defun tla--buffer-for-rev (file revision)
  "Return an empty buffer suitable for viewing FILE in REVISION.

The name of the buffer is chosen according to FILE and REVISION.

REVISION may have one of the values described in the docstring of
`dvc-diff-modified' or `dvc-diff-base'."
  (dvc-trace "OBSOLETE")
  (let ((name (concat
               (file-name-nondirectory file)
               "(" (cond
                    ((eq (car revision) 'revision)
                     (tla--name-construct (cadr revision)))
                    ((eq (car revision) 'local-tree)
                     (cadr revision))
                    ((eq (car revision) 'last-revision) "original")
                    ((eq (car revision) 'previous-revision)
                     (tla--name-construct-semi-qualified
                      (tla-revision-direct-ancestor (cadr revision))))
                    (t ""))
               ")")))
    ;; replace / by -- to work around uniquify
    (setq name (replace-regexp-in-string "\\/" "--" name))
    (generate-new-buffer name)))

;; TODO being ported to DVC. See below
(defun tla-file-get-revision-in-buffer (file &optional revision)
  "Get the last committed version of FILE in a buffer.

Returned value is the buffer.

REVISION can have any of the values described in the docstring of
`dvc-diff-base' and `dvc-diff-modified'"
  (dvc-trace "OBSOLETE")
  (let* ((default-directory (or (tla-tree-root nil t)
                                default-directory))
         (file-unmo-temp (tla-file-get-revision-in-file file revision))
         (original (car file-unmo-temp))
         (original-to-be-removed (cadr file-unmo-temp)))
    (if (eq (car revision) 'local-tree)
        (find-file-noselect original)
      (let ((buffer-orig (tla--buffer-for-rev file revision)))
        (with-current-buffer buffer-orig
          (erase-buffer)
          (insert-file-contents original)
          (set-buffer-modified-p nil)
          (toggle-read-only 1)
          (let ((buffer-file-name file))
            (set-auto-mode t))
          (when original-to-be-removed
            (delete-file original)))
        buffer-orig))))

(defun tla-revision-get-last-or-file-revision (file revision last)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

REVISION is either a string or nil. nil means the last commited
revision, non-nil means a revision to pass as command line argument."
  (let* ((original (progn
                     (tla--run-tla-sync
                      (list "file-find" file revision)
                      :finished
                      (dvc-capturing-lambda (output error
                                                    status
                                                    arguments)
                        (with-current-buffer output
                          (goto-char (point-min))
                          (re-search-forward "^[^*]")
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))))))
         file-unmodified-p)
    (if (file-exists-p original)
        (insert-file-contents original)
      ;; Probably tla is ran remotely or whatever. Well, get the
      ;; file using the old good tla file-diff | patch -R -o ...
      (setq original (dvc-make-temp-name "tla-ediff"))
      (tla--run-tla-sync (list (if (tla-has-file-diff-command)
                                   "file-diff" "file-diffs")
                               file revision)
                         :finished 'dvc-null-handler
                         :error
                         (lambda (output error status arguments)
                           (if (not (eq status 1))
                               (dvc-default-error-function
                                output error status arguments))))
      (with-current-buffer dvc-last-process-buffer
        (call-process-region (point-min) (point-max)
                             dvc-patch-executable
                             nil nil nil
                             "-R" "-o" original file)))))

;; TODO port of `tla-file-get-revision-in-buffer' to DVC
;;;###autoload
(defun tla-revision-get-last-revision (file last-revision)
  "Insert the content of FILE in LAST-REVISION, in current buffer.

LAST-REVISION looks like
\(\"path\" NUM)."
  (when (not (eq (nth 1 last-revision) 1))
    (error "TODO. revision=%S" last-revision))
  (let* ((default-directory (car last-revision)))
    (tla-revision-get-last-or-file-revision file nil t)))

(defun tla-revision-get-file-revision (file revision)
  "Insert the content of FILE in REVISION, in current buffer.

REVISION looks like
\(\"archive\" \"cat\" ...)."
  (let* ((default-directory (tla-tree-root file)))
    (tla-revision-get-last-or-file-revision
     file (tla--name-construct (car revision)) nil)))

(defalias 'baz-revision-get-last-revision 'tla-revision-get-last-revision)
(defalias 'baz-revision-get-file-revision 'tla-revision-get-file-revision)

(defun tla-commit-check-empty-line ()
  "Check that the headers are followed by an empty line.

Current buffer must be a log buffer.  This function checks it starts
with RFC822-like headers, followed by an empty line"
  (interactive)
  (goto-char (point-min))
  (while (not (looking-at "^$"))
    (unless (looking-at "^[A-Za-z0-9_-]*:")
      (error "A blank line must follow the last header field"))
    (forward-line 1)
    ;; space and tabs are continuation line.
    (while (looking-at "[ \t]+")
      (forward-line 1))))

(defun tla-commit-check-empty-headers ()
  "Check that the current buffer starts with non-empty headers.

Also checks that the the line following headers is empty (or the
notion of \"header\" would loose its meaning)."
  (interactive)
  (goto-char (point-min))
  (while (not (looking-at "^$"))
    (unless (looking-at "^[A-Za-z0-9_-]*:")
      (error "A blank line must follow the last header field"))
    (when (looking-at "^\\([A-Za-z0-9_-]*\\):[ \t]*$")
      (let ((header (match-string 1)))
        (unless (string-match tla-commit-headers-allowed-to-be-empty
                              header)
          (end-of-line)
          (when (eq (char-before) ?:) (insert " "))
          (error (format "Empty \"%s: \" header" header)))))
    (forward-line 1)
    ;; space and tabs are continuation line.
    (while (looking-at "[ \t]+")
      (forward-line 1))))

(defun tla-commit-check-missing-space ()
  "Check the space after the colon in each header:

Check that no header in the summary buffer miss the SPC character
following the semicolon.  Also checks that the the line following
headers is empty (or the notion of \"header\" would loose its
meaning)"
  (interactive)
  (goto-char (point-min))
  (let ((stg-changed))
    (while (not (looking-at "^$"))
      (unless (looking-at "^[A-Za-z0-9_-]*:")
        (error "A blank line must follow the last header field"))
      (when (looking-at "^\\([A-Za-z0-9_-]*\\):[^ ]")
        (let ((header (match-string 1)))
          (if tla-commit-fix-missing-space
              (progn
                (setq stg-changed t)
                (search-forward ":")
                (insert " "))
            (error (format "Missing space after colon for \"%s:\""
                           header)))))
      (forward-line 1)
      ;; space and tabs are continuation line.
      (while (looking-at "[ \t]+")
        (forward-line 1)))
    (when stg-changed
      (save-buffer))))

(defun tla-commit-check-log-buffer ()
  "Function to call from the ++log... buffer, before comitting.

\(`tla-commit' calls it automatically). This runs the tests listed in
`tla-commit-check-log-buffer-functions'.  Each function is called with
no argument and can raise an error in case the log buffer isn't
correctly filled in."
  (dolist (function tla-commit-check-log-buffer-functions)
    (funcall function)))

;;;###autoload
(defun tla-commit (&optional handler version-flag summary-line)
  "Run tla commit.

Optional argument HANDLER is the process handler for the commit
command. `nil' or a symbol(`seal' or `fix') is acceptable as
VERSION-FLAG.
When the commit finishes successful, `tla-commit-done-hook' is called."
  (interactive
   (list nil nil (when (and current-prefix-arg
                            (not (tla-make-log t)))
                   (read-string "Summary line for commit: "))))
  (let (file-list
        arglist
        dont-commit)
    (if current-prefix-arg
        (when (tla-make-log t)
          (tla-edit-log)
          (setq dont-commit t))
      (with-current-buffer
          (find-file-noselect (tla-make-log))
        (condition-case x
            (tla-commit-check-log-buffer)
          (error (progn (switch-to-buffer (current-buffer))
                        (eval x))))
        (or (dvc-save-some-buffers)
            (y-or-n-p
             "Commit with unsaved changes is a bad idea.  Continue anyway? ")
            (error "Not committing"))
        (setq tla-last-commit-message (buffer-substring-no-properties (point-min) (point-max)))
        (setq file-list (and (buffer-live-p tla-buffer-source-buffer)
                             (with-current-buffer tla-buffer-source-buffer
                               dvc-buffer-marked-file-list)))
        (when file-list (setq arglist (append arglist (cons "--"
                                                            file-list))))))
    (unless dont-commit
      (with-current-buffer
          (find-file-noselect (tla-make-log))
        ;; raises an error if commit isn't possible
        (tla--run-tla-async
         `("commit"
           ,(when tla-strict-commits "--strict")
           ,@(when summary-line (list "--summary" summary-line))
           ,(cond
             ((eq version-flag 'fix)  "--fix")
             ((eq version-flag 'seal) "--seal")
             ((eq version-flag nil)   nil)
             (t (error "Wrong version flag: %s" version-flag)))
           ,@arglist)
         :finished
         (dvc-capturing-lambda (output error status arguments)
           (dvc-show-error-buffer output 'commit)
           (run-hooks 'tla-commit-done-hook)
           (dvc-buffer-push-previous-window-config tla-pre-commit-window-configuration)
           (dvc-diff-clear-buffers
            tla-arch-branch
            (capture default-directory)
            "* Just committed! Please refresh buffer\n")
           (when (capture handler) (funcall (capture handler) output error status
                                            arguments))))))))

(defun tla-import (&optional dir synchronously)
  "Run tla import."
  (interactive)
  (let* ((default-directory (or dir default-directory))
         (logfile (tla-make-log t)))
    (when logfile
      (with-current-buffer
          (find-file-noselect logfile)
        (tla-edit-log-delete-file-list t)
        (save-buffer)))
    (funcall
     (if synchronously 'tla--run-tla-sync 'tla--run-tla-async)
     (list "import" (if (tla-import-has-setup-option) "--setup"))
     :finished (dvc-capturing-lambda (output error status arguments)
                 (tla-inventory (capture default-directory))
                 (message "Import finished."))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (let* ((default-directory (capture default-directory))
              (archive (tla--name-archive (tla-tree-version-list))))
         (with-current-buffer error
           (goto-char (point-min))
           (if (and (re-search-forward
                     "^No commitable locations for.*are registered" nil t)
                    (y-or-n-p (format "Archive %s not registered. Create it?"
                                      archive)))
               (progn
                 (tla--make-archive archive
                                    (tla--make-archive-read-location)
                                    (y-or-n-p "Sign the archive? ")
                                    (y-or-n-p "Create .listing files? "))
                 (tla-import (capture default-directory)))
             (dvc-default-error-function
              output error status arguments))))))))

(defun tla-archive-ensure-registration (archive)
  "Ensures ARCHIVE is registered.

If not, offer to create it or to register it."
  (interactive (list (tla--name-archive (tla-name-read "Archive: " 'prompt))))
  (tla--run-tla-sync
   (list "whereis-archive" archive)
   :error (dvc-capturing-lambda
              (output error status arguments)
            (cond ((y-or-n-p (format "Archive %s not registered. Create it? "
                                     archive))
                   (tla--make-archive
                    archive
                    (tla--make-archive-read-location)
                    (y-or-n-p "Sign the archive? ")
                    (y-or-n-p "Create .listing files? ")))
                  ((y-or-n-p (format "Register it? "))
                   (call-interactively
                    'tla--register-archive))
                  (t (message "Archive still not registered."))))
   :finished 'dvc-null-handler))

(defun tla-init-tree (&optional dir version)
  "Run tla init-tree."
  (interactive
   (let* ((dir (list (dvc-read-directory-name "Directory to init: "
                                              (or default-directory
                                                  (getenv "HOME")))))
          (version (tla-name-read (format "Set version for `%s' to: "
                                          default-directory)
                                  'prompt 'prompt 'prompt 'prompt)))
     (list dir version)))
  (let* ((default-directory (or dir default-directory))
         (project (tla--name-construct version)))
    (tla-archive-ensure-registration (tla--name-archive version))
    (tla--run-tla-sync (list "init-tree" "--nested" project)
                       :finished (dvc-capturing-lambda
                                     (output error status arguments)
                                   (message "init-tree finished")))))
;;
;; Import;;
;;;###autoload
(defun tla-start-project (&optional archive synchronously)
  "Start a new project.
Prompts for the root directory of the project and the fully
qualified version name to use.  Sets up and imports the tree and
displays an inventory buffer to allow the project's files to be
added and committed.
If ARCHIVE is given, use it when reading version.
Return a cons pair: its car is the new version name string, and
its cdr is imported location.
If SYNCHRONOUSLY is non-nil, run \"tla import\" synchronously.
Else run it asynchronously."
  (interactive)
  (let* ((base (dvc-read-directory-name "Directory containing files to import: "
                                        (or default-directory
                                            (getenv "HOME"))))
         (l (tla-name-read (format "Import `%s' to: " base)
                           (if archive archive (tla-my-default-archive))
                           'prompt 'prompt 'prompt))
         (project (tla--name-construct l)))
    (let ((default-directory (file-name-as-directory base)))
      (tla-init-tree default-directory l)
      (save-excursion
        (tla-inventory default-directory)
        (message "Type %s when ready to import"
                 (substitute-command-keys "\\[exit-recursive-edit]"))
        (recursive-edit))
      (tla-import default-directory synchronously)
      (cons project default-directory))))

;;;###autoload
(defun tla-rm (file)
  "Call tla rm on file FILE.  Prompts for confirmation before."
  (when (yes-or-no-p (format "Delete file %s? " file))
    (tla--run-tla-sync (list "rm" file)
                       :finished 'dvc-null-handler)))

(defun tla-pristines ()
  "Run \"tla pristine\"."
  (interactive)
  (tla--run-tla-sync '("pristines")))

;;;###autoload
(defun tla-changelog (&optional name)
  "Run \"tla changelog\".

display the result in an improved ChangeLog mode.
If NAME is given, name is passed to \"tla changelog\"
as the place where changelog is got."
  (interactive (when current-prefix-arg
                 (list (tla--name-construct
                        (tla-name-read "ChangeLog of: "
                                       'prompt 'prompt 'prompt 'prompt)))))
  (let ((default-directory (dvc-read-project-tree-maybe))
        arguments)
    (when name
      (setq arguments (cons name arguments)))
    (setq arguments (cons "changelog" arguments))
    (tla--run-tla-sync arguments
                       :finished 'dvc-null-handler)
    (dvc-show-last-process-buffer 'changelog 'tla-changelog-mode)
    (goto-char (point-min))))

;;;###autoload
(defun tla-logs ()
  "Run tla logs."
  (interactive)
  (let ((default-directory (dvc-read-project-tree-maybe))
        ;; (details (or dvc-revisions-shows-date
        ;;              dvc-revisions-shows-creator
        ;;              dvc-revisions-shows-summary))
        )
    (tla--run-tla-async
     (list "logs" "--full" "--reverse"
           (when (tla-revisions-has-complete-log-option) "--complete-log")

           ;;           (when details "--date")
           ;;           (when details "--creator")
           ;;           (when details "--summary"))
           )
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
             (buffer (dvc-get-buffer-create tla-arch-branch 'log (tla-tree-root))))
         (dvc-switch-to-buffer buffer)
         (tla-revision-list-mode)
         (tla--revisions-parse-list 'log nil ;;(capture details)
                                    nil      ;; TODO (merges)
                                    output nil
                                    dvc-revlist-cookie)
         (setq dvc-buffer-refresh-function 'tla-logs))
       (goto-char (point-min))
       (dvc-revision-prev)
       (recenter -4)))))

;;;###autoload
(defun tla-help (command)
  "Run tla COMMAND -H."
  (interactive
   (list (dvc-completing-read
          "Get help for: "
          (tla--run-tla-sync
           '("help")
           :finished
           (dvc-capturing-lambda (output error status arguments)
             (with-current-buffer output
               (goto-char (point-min))
               (let (listcmd)
                 (while (re-search-forward
                         " *\\([^ ]*\\) : " nil t)
                   (setq listcmd
                         (cons (list (match-string 1))
                               listcmd)))
                 listcmd)))))))
  (tla--run-tla-sync (list command "-H")))

(defun tla-tree-version-list-tla ()
  "Return the tree version, or nil if not in a project tree."
  (tla--run-tla-sync '("tree-version")
                     :finished
                     (lambda (output error status arguments)
                       (with-current-buffer output
                         (and
                          (goto-char (point-min))
                          (re-search-forward "\\(.*\\)/\\(.*\\)--\\(.*\\)--\\(.*\\)" nil t)
                          (list (match-string 1)
                                (match-string 2)
                                (match-string 3)
                                (match-string 4)))))))

(defun tla-tree-version-list (&optional location no-error)
  "Elisp implementation of `tla-tree-version-list-tla'.

A string, LOCATION is used as a directory where
\"/{arch}/++default-version\" is. If NO-ERROR is non-nil, errors are
not reported; just return nil."
  (let ((version-string (tla-tree-version location no-error)))
    (and version-string
         (string-match "\\(.*\\)/\\(.*\\)--\\(.*\\)--\\(.*\\)" version-string)
         (list (match-string 1 version-string)
               (match-string 2 version-string)
               (match-string 3 version-string)
               (match-string 4 version-string)))))

(defun tla-tree-root-tla ()
  "Run tla tree-root."
  (interactive)
  (let ((i-p (interactive-p)))
    (tla--run-tla-sync '("tree-root")
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (let ((result (dvc-buffer-content output)))
                           (when (capture i-p)
                             (message "tla tree-root is: %s"
                                      result))
                           result)))))

;;;###autoload
(defun tla-tree-version (&optional location no-error)
  "Equivalent of tla tree-version (but implemented in pure elisp).

Optional argument LOCATION is the directory in which the command must
be ran.  If NO-ERROR is non-nil, don't raise errors if ran outside an
arch managed tree."
  (interactive (list nil nil))
  (let* ((tree-root (tla-tree-root location no-error))
         (default-version-file (when tree-root
                                 (expand-file-name
                                  "{arch}/++default-version"
                                  tree-root)))
         (version (and (boundp 'tla-buffer-version-name)
                       (tla--name-construct
                        tla-buffer-archive-name
                        tla-buffer-category-name
                        tla-buffer-branch-name
                        tla-buffer-version-name))))
    (if (and (or (null version) (string= version ""))
             default-version-file
             (file-readable-p default-version-file))
        (with-temp-buffer
          (insert-file-contents default-version-file)
          (setq version (buffer-substring-no-properties (point-min)
                                                        (- (point-max) 1)))))
    (when (interactive-p)
      (message "%s" version))
    version))

;;;###autoload
(defun tla-my-id (&optional arg my-id)
  "Run tla my-id.

When called without a prefix argument ARG, just print the my-id from
tla and return it.  If MY-ID is not set yet, return an empty string.
When called with a prefix argument, ask for a new my-id.

The my-id should have the following format:

Your id is recorded in various archives and log messages as you use
arch.  It must consist entirely of printable characters and fit on one
line.  By convention, it should have the form of an email address, as
in this example:

Jane Hacker <jane.hacker@gnu.org>"
  (interactive "P")
  (let ((id (tla--run-tla-sync '("my-id")
                               :finished
                               (lambda (output error status arguments)
                                 (dvc-buffer-content output))
                               :error
                               (lambda (output error status arguments)
                                 nil))))
    (if arg
        ;; Set the user's ID
        (let ((new-id (or my-id
                          (read-string "New arch my-id: "
                                       id tla-my-id-history id))))
          (if (string= id new-id)
              (message "Id unchanged! Id = %s" new-id)
            (message "Setting id to: %s" new-id)
            (tla--run-tla-sync (list "my-id" new-id)
                               :finished (lambda (output error status arguments)
                                           (message "Id changed to '%s'" new-id))
                               :error
                               (lambda (output error status arguments)
                                 (message "Could not change Id")
                                 (dvc-show-error-buffer error)
                                 )))
          new-id)
      (cond (id (when (interactive-p)
                  (message "Arch my-id: %s" id))
                id)
            (t (when (interactive-p)
                 (message (concat "Arch my-id has not been given yet. "
                                  "Call `%s' to set.")
                          "tla-set-my-id"))
               "")))))

(defun tla-set-my-id ()
  "Set tla's my-id."
  (interactive)
  (tla-my-id 1))

;;;###autoload
(defun tla-tree-id ()
  "Call either 'baz tree-id' or 'tla logs -f -r' to get the tree-id."
  (interactive)
  (let ((tree-id)
        (cmd-list (if (tla-has-tree-id-command)
                      '("tree-id") '("logs" "-f" "-r"))))
    (tla--run-tla-sync
     cmd-list
     :finished
     (lambda (output error status arguments)
       (set-buffer output)
       (goto-char (point-min))
       (setq tree-id
             (buffer-substring-no-properties
              (point)
              (line-end-position))))
     :error
     (lambda (output error status arguments)
       (setq tree-id "<unknown>")))
    (when (interactive-p)
      (message "tree-id for %s: %s" default-directory tree-id))
    tree-id))

;;
;; Library
;;

;;;###autoload
(defun tla-my-revision-library (&optional arg)
  "Run tla my-revision-library.

When called without a prefix argument ARG, just print the
my-revision-library from tla.  When called with a prefix argument, ask
for a new my-revision-library.

my-revision-library specifies a path, where the revision library is
stored to speed up tla.  For example ~/tmp/arch-lib.

You can configure the parameters for the library via
`tla-library-config'."
  (interactive "P")
  (let ((result (tla--run-tla-sync '("my-revision-library")
                                   :finished 'dvc-status-handler
                                   :error 'dvc-null-handler))
        (rev-lib (dvc-get-process-output)))
    (when (eq 0 result)
      (if arg
          (tla--library-add-interactive rev-lib)
        (if (and rev-lib (string= "" rev-lib))
            (message "Arch my-revision-library has not been given yet. Call `%s' with prefix arguments to set."
                     this-command)
          (when (interactive-p) (message "Arch my-revision-library: %s" rev-lib)))
        rev-lib))))

(defun tla--library-add-interactive (&optional old-rev-lib)
  "Prompts for argument and run `tla--library-add'.

Argument OLD-REV-LIB is the previously set revision library (a
string)."
  (unless old-rev-lib (setq old-rev-lib ""))
  (let ((new-rev-lib (expand-file-name (dvc-read-directory-name
                                        "New arch revision library: " old-rev-lib))))
    (if (not (string= old-rev-lib new-rev-lib))
        (progn
          (message "Setting my-revision-library to: %s" new-rev-lib)
          (tla--library-add new-rev-lib))
      old-rev-lib)))

(defun tla-library-delete (rev-lib)
  "Unregister revision library REV-LIB."
  (interactive (list (tla--read-revision-library)))
  (tla--run-tla-sync (list "my-revision-library" "--delete" rev-lib)
                     :finished (lambda (output error status arguments)
                                 (message "Library %s removed."
                                          rev-lib))))

(defun tla--library-add (new-rev-lib)
  "Change the revision library path to NEW-REV-LIB."
  (let ((dir-attr (file-attributes new-rev-lib)))
    (unless dir-attr
      (make-directory new-rev-lib t))
    (tla--run-tla-sync (list "my-revision-library" new-rev-lib)
                       :finished
                       (lambda (output error status arguments)
                         (message (dvc-buffer-content output))))
    new-rev-lib))

(defun tla--revision-library-list ()
  "Parse `tla my-revision-library' into a list of revision libraries."
  (tla--run-tla-sync '("my-revision-library")
                     :finished
                     'dvc-output-buffer-split-handler))

(defvar tla--library-history nil)

(defun tla--read-revision-library (&optional prompt)
  "Read a revision library from keyboard.
Prompt the user with PROMPT if given."
  (let ((list-lib (tla--revision-library-list)))
    (if (null (cdr list-lib))
        (car list-lib)
      (dvc-completing-read (or prompt
                               (format "Revision library (default %s): "
                                       (car list-lib)))
                           (mapcar 'list (tla--revision-library-list))
                           nil t nil 'tla--library-history
                           (car list-lib)))))

(defun tla-library-config (&optional arg)
  "Run tla library-config.
When called without prefix argument ARG, just print the config.
When called with prefix argument ARG, let the user change the config."
  (interactive "P")
  (let ((rev-lib (tla--read-revision-library))
        (config-param (when arg
                        (dvc-completing-read "tla library config "
                                             (mapcar 'list '("--greedy"
                                                             "--sparse"
                                                             "--non-greedy"
                                                             "--non-sparse"))
                                             nil t "--"))))
    (tla--run-tla-sync (list "library-config" config-param rev-lib)
                       :finished 'dvc-null-handler)
    (message (dvc-get-process-output))))

(defun tla-library-add (archive category branch version &optional revision)
  "Add ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION to the revision
library. REVISION is optional argument; if it is omitted or `nil' is
given, the last revision in ARCHIVE/CATEGORY--BRANCH--VERSION is added
to the library."
  (dvc-show-last-process-buffer)
  (tla--run-tla-async `("library-add"
                        ,(tla--name-construct archive category
                                              branch version
                                              revision))))

(defun tla-library-find (archive category branch version revision
                                 &optional silent)
  "Find ARCHIVE--CATEGORY--BRANCH--VERSION--REVISION in the revision library.
If the revision is found, return the path for it. Else return nil."
  (if (zerop (tla--run-tla-sync (list "library-find" (when silent "--silent")
                                      (tla--name-construct
                                       archive category branch
                                       version revision))
                                :finished 'dvc-status-handler
                                :error 'dvc-status-handler))
      (dvc-get-process-output)))

;; completing-read: tagline, explicit, names, implicit
(defvar tla-id-tagging-method-history nil)

;;;###autoload
(defun tla-id-tagging-method (arg)
  "View (and return) or change the id-tagging method.
When called without prefix argument ARG: show the actual tagging method.
When called with prefix argument ARG: Ask the user for the new tagging method."
  (interactive "P")
  (let ((tm (progn (tla--run-tla-sync '("id-tagging-method")
                                      :finished
                                      (lambda (output error status arguments)
                                        (dvc-buffer-content output)))))
        (new-tagging-method))
    (if arg
        (progn
          (setq new-tagging-method
                (tla--id-tagging-method-read tm))
          (when (not (string= tm new-tagging-method))
            (tla--id-tagging-method-set new-tagging-method)))
      (when (interactive-p)
        (message "Arch id tagging method: %s" tm))
      tm
      )))

(defun tla--id-tagging-method-read (old-method)
  "Read id tagging method.
If OLD-METHOD is given, use it as the default method."
  (dvc-completing-read
   (if old-method
       (format "New id tagging method (default %s): " old-method)
     "New id tagging method: ")
   (mapcar 'list '("tagline" "explicit" "names" "implicit"))
   nil t nil
   'tla-id-tagging-method-history
   old-method))

(defun tla--id-tagging-method-set (method)
  "Set the tagging method to METHOD."
  (message "Setting tagging method to: %s" method)
  (tla--run-tla-sync (list "id-tagging-method"
                           method)
                     :finished 'dvc-null-handler))

(defun tla-archive-mirror (archive &optional category branch version to)
  "Synchronize the mirror for ARCHIVE.
Limit to CATEGORY--BRANCH--VERSION.  When called interactively you can specify
the limit as part of the source archive.  With a prefix arg also query for TO,
i.e. the destination mirror."
  (interactive (let ((from (tla-name-read "Mirror from: " 'prompt 'maybe 'maybe 'maybe))
                     (to   (if current-prefix-arg (tla-name-read "Mirror to: " 'maybe))))
                 (list (tla--name-archive from)
                       (tla--name-category from)
                       (tla--name-branch from)
                       (tla--name-version from)
                       (tla--name-archive to))))
  (let ((from archive)
        (limit (tla--name-construct-semi-qualified category branch
                                                   version))
        options)
    (if (string= limit "")
        (setq limit nil))
    (if (tla-archive-mirror-has-all-mirrors-option)
        (push "--all-mirrors" options)
      (if (not to)
          (if (string-match "-MIRROR$" from)
              (setq to from
                    from (replace-regexp-in-string "-MIRROR$" "" from))
            (setq to (concat from "-MIRROR")))))
    (tla--run-tla-async `("archive-mirror"
                          ,@options
                          ,from
                          ,to
                          ,limit)
                        :finished (dvc-capturing-lambda (output error status arguments)
                                    (message "tla archive-mirror finished"))
                        )))

(defun tla-archive-fixup (archive)
  "Run tla archive-fixup for ARCHIVE."
  (interactive (list (car (tla-name-read "Archive to fixup: " 'prompt))))
  (tla--run-tla-async (list "archive-fixup" archive)
                      :finished (dvc-capturing-lambda (output error status arguments)
                                  (message "tla archive-fixup %s finished" (capture archive)))
                      ))

;;;###autoload
(defun tla-star-merge (from &optional to-tree)
  "Star merge from version/revision FROM to local tree TO-TREE."
  (interactive (list (tla--name-construct
                      (tla-name-read "Merge from: " 'prompt 'prompt
                                     'prompt 'maybe 'maybe))
                     (dvc-read-directory-name "Merge to: ")))
  (let ((to-tree (when to-tree (expand-file-name to-tree))))
    (or (dvc-save-some-buffers (or to-tree default-directory))
        (y-or-n-p
         "Star-merge may delete unsaved changes.  Continue anyway? ")
        (error "Not running star-merge"))
    (let* ((default-directory (or to-tree default-directory))
           (buffer (dvc-prepare-changes-buffer
                    `(,tla-arch-branch
                      (last-revision ,default-directory))
                    `(,tla-arch-branch
                      (local-tree ,default-directory))
                    ;; TODO using tla-changes here makes it simpler.
                    ;; The user can just type `g' and get the real
                    ;; changes. Maybe a 'star-merge would be better
                    ;; here ...
                    'diff default-directory
                    tla-arch-branch)))
      (when dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer))
      (tla--run-tla-async (list (if (tla-has-merge-command) "merge" "star-merge")
                                (tla--three-way-merge-option)
                                (tla--show-ancestor-option)
                                from)
                          :finished (dvc-capturing-lambda (output error status arguments)
                                      ;; (tla--show-last--process-buffer)
                                      (dvc-show-changes-buffer
                                       output 'tla--parse-other (capture buffer))
                                      (message "merge command finished")
                                      (dvc-revert-some-buffers (capture to-tree)))
                          :error (dvc-capturing-lambda (output error status arguments)
                                   (case status
                                     ;; 2 stands for an error.
                                     (2 (dvc-default-error-function
                                         output error status arguments))
                                     ;; How about other status?
                                     (otherwise (dvc-show-changes-buffer output 'tla--parse-other)
                                                output nil (capture buffer))))))))

(defun tla--replay-arguments ()
  "Build an argument list for the replay command.
Used to factorize the code of (interactive ...) between `tla-replay-reverse'
and `tla-replay'."
  (list (tla--name-construct
         (tla-name-read (if current-prefix-arg
                            "Reversely relay version or revision: "
                          "Relay version or revision: ")
                        'prompt 'prompt 'prompt 'prompt 'maybe))
        (dvc-read-directory-name (if current-prefix-arg
                                     "Reversely replay in tree: "
                                   "Replay in tree: "))
        current-prefix-arg))

(defun tla-replay-reverse (from &optional to-tree arg)
  "Call `tla-replay' with the REVERSE option."
  (interactive (tla--replay-arguments))
  (tla-replay from to-tree t))


(defun tla-replay (from &optional to-tree reverse)
  "Replay the revision FROM into tree TO-TREE.
If FROM is a string, it should be a fully qualified revision.
If FROM is a list, it should be a list of fully qualified revisions to
be replayed.

If REVERSE is non-nil, reverse the requested revision."
  (interactive (tla--replay-arguments))
  (let ((default-directory (or to-tree default-directory)))
    (or (dvc-save-some-buffers)
        (y-or-n-p
         "Replay may delete unsaved changes.  Continue anyway? ")
        (error "Not replaying"))
    (dvc-show-last-process-buffer)
    (let ((buffer (dvc-prepare-changes-buffer
                   `(,tla-arch-branch
                     (last-revision ,default-directory))
                   `(,tla-arch-branch
                     (local-tree ,default-directory))
                   'diff default-directory
                   tla-arch-branch)))
      (when dvc-switch-to-buffer-first
        (dvc-switch-to-buffer buffer))
      (tla--run-tla-async `("replay"
                            ;; ,(when tla-use-forward-option "--forward")
                            ,(when reverse "--reverse")
                            ,(when tla-use-skip-present-option "--skip-present")
                            ,@(if (listp from)
                                  from
                                (list from)))
                          :finished (dvc-capturing-lambda (output error status arguments)
                                      (dvc-show-changes-buffer output
                                                               'tla--parse-other
                                                               (capture buffer))
                                      (message "tla replay finished")
                                      (dvc-revert-some-buffers (capture to-tree)))
                          :error (lambda (output error status arguments)
                                   (dvc-show-error-buffer error)
                                   (dvc-show-last-process-buffer))))))

(defun tla-sync-tree (from &optional to-tree)
  "Synchronize the patch logs of revision FROM and tree TO-TREE."
  (interactive (list
                (tla--name-construct
                 (tla-name-read "Sync tree with revision: "
                                'prompt 'prompt 'prompt 'prompt 'prompt))
                (dvc-read-directory-name "Sync tree: ")))
  (let ((default-directory (or to-tree default-directory)))
    (or (dvc-save-some-buffers)
        (y-or-n-p
         "Sync-tree may delete unsaved changes.  Continue anyway? ")
        (error "Not running Sync-tree."))
    (dvc-show-last-process-buffer)
    (tla--run-tla-async `("sync-tree" ,from)
                        :finished (dvc-capturing-lambda (output error status arguments)
                                    (dvc-show-last-process-buffer)
                                    (message "tla sync-tree finished")
                                    (dvc-revert-some-buffers (capture to-tree)))
                        :error (lambda (output error status arguments)
                                 (dvc-show-changes-buffer
                                  output 'tla--parse-other
                                  (dvc-prepare-changes-buffer nil nil 'diff default-directory tla-arch-branch))))))

;;;###autoload
(defun tla-switch (tree version &optional handle)
  "Run tla switch to VERSION in TREE.

After running update, execute HANDLE (function taking no argument)."
  (interactive (list (expand-file-name
                      (dvc-read-directory-name "Switch in tree: " nil
                                               nil nil ""))
                     (tla-name-read "Switch to version: "
                                    'prompt 'prompt 'prompt 'maybe 'maybe)))
  (unless (tla-has-switch-command)
    (error "switch not available with this arch branch"))
  (or (dvc-save-some-buffers tree)
      (y-or-n-p
       "Update may delete unsaved changes.  Continue anyway? ")
      (error "Not updating"))
  (let* ((default-directory (or tree default-directory))
         (buffer (dvc-prepare-changes-buffer
                  (list 'last-revision default-directory)
                  (list 'local-tree default-directory)
                  'status default-directory 'tla)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (tla--run-tla-async `("switch"
                          ,(when (and
                                  (tla-switch-has-show-ancestor-option)
                                  tla-show-ancestor)
                             "--show-ancestor")
                          ,(tla--name-construct version))
                        :finished (lexical-let ((buffer-lex buffer) (tree-lex tree) (handle-lex handle))
                                    (lambda (output error status arguments)
                                      ;; (tla--show-last--process-buffer)
                                      (dvc-show-changes-buffer
                                       output 'tla--parse-other buffer-lex)
                                      (message "`%s switch' finished" (tla--executable))
                                      (dvc-revert-some-buffers tree-lex)
                                      (when handle-lex (funcall handle-lex))))
                        :error
                        (lambda (output error status arguments)
                          (dvc-show-error-buffer error)
                          (dvc-show-last-process-buffer)
                          ))
    (dvc-revert-some-buffers tree)))

(defvar tla-default-export-directory nil "Default directory that is suggested for `tla-export'")
;;;###autoload
(defun tla-export (revision dir)
  "Run tla export to export REVISION to DIR."
  (interactive (list (tla-name-read "Export version: "
                                    'prompt 'prompt 'prompt 'maybe 'maybe)
                     (dvc-read-directory-name "Export to directory: " nil tla-default-export-directory nil)))
  (setq dir (dvc-uniquify-file-name dir))
  (tla--run-tla-async `("export" ,(tla--name-construct revision) ,dir)
                      :finished
                      (dvc-capturing-lambda (output error status arguments)
                        (message "Finished tla export %s to %s" (capture revision) (capture dir)))))

(defun tla-export-as-tgz (version export-directory)
  "Run tla export to export REVISION and create a tarball afterwards."
  (interactive (list (tla-name-read "Export version: "
                                    'prompt 'prompt 'prompt 'maybe 'maybe)
                     (dvc-read-directory-name "Export to directory: " nil tla-default-export-directory nil)))
  (let* ((export-dir (dvc-make-temp-name "tla-export"))
         (export-base-name (tla--name-construct-semi-qualified (cdr version)))
         (export-full-path (concat export-dir "/" export-base-name))
         (tgz-file-name (dvc-uniquify-file-name (concat export-directory "/" export-base-name ".tar.gz"))))
    (message "export as tgz to %s using %s" tgz-file-name export-full-path)
    (make-directory export-dir)
    (tla-export version export-full-path)
    (dvc-create-tarball-from-intermediate-directory export-full-path tgz-file-name)))

(defun tla--tag-does-cacherev ()
  (cond ((eq tla-tag-does-cacherev 'yes) t)
        ((eq tla-tag-does-cacherev 'no) nil)
        (t (y-or-n-p "Create cachedrev on tag? "))))

;;;###autoload
(defun tla-tag (source-revision tag-version &optional cacherev synchronously)
  "Create a tag from SOURCE-REVISION to TAG-VERSION.
Run tla tag --setup.
If SYNCHRONOUSLY is non-nil, the process for tagging runs synchronously.
Else it runs asynchronously."
  (interactive
   (list (tla--name-construct
          (tla-name-read "Source revision (or version): " 'prompt 'prompt 'prompt
                         'prompt 'maybe))
         (tla--name-construct
          (tla-name-read "Tag version: " 'prompt 'prompt 'prompt
                         'prompt))
         (tla--tag-does-cacherev)
         nil))
  (when (tla-has-merge-command)
    (error "tla-tag not available. Use baz-branch instead."))
  (funcall (if synchronously 'tla--run-tla-sync 'tla--run-tla-async)
           (list (when (tla-has-branch-command) "branch" "tag")
                 (when (tla-tag-has-setup-option) "--setup")
                 (when (not cacherev) "--no-cacherev")
                 source-revision tag-version)))

(defun tla-set-tree-version (version)
  "Run tla set-tree-version VERSION."
  (interactive (list (tla-name-read "Set tree version to: "
                                    'prompt 'prompt 'prompt 'prompt)))

  (let ((new-version (tla--name-construct version))
        (old-version (tla-tree-version)))
    (when (y-or-n-p (format "Switch tree version from `%s' to `%s'? "
                            old-version
                            new-version))
      (tla--run-tla-sync (list "set-tree-version" new-version)))))

;; ----------------------------------------------------------------------------
;; Xtla bookmarks
;; ----------------------------------------------------------------------------

(defvar tla-bookmarks-loaded nil
  "Whether `tla-bookmarks' have been loaded from file.")

(defvar tla-bookmarks-alist nil
  "Alist containing Xtla bookmarks.")

(defvar tla-bookmarks-show-details nil
  "Whether `tla-bookmarks' should show bookmark details.")

(defvar tla-bookmarks-cookie nil
  "Ewoc dll.")

(defvar tla-missing-buffer-todolist nil
  "List of (kind info).

Can be
\(separator \"label\" bookmark \"local-tree\")
\(changes \"local-tree\")
\(missing \"local-tree\" \"location\" \"bookmark-name\")")

(defvar tla-bookmarks-marked-list nil
  "A list of marked bookmarks.")

(defun tla-bookmarks-load-from-file-OBSOLETE (&optional force)
  "Load bookmarks from the bookmarks file.
If FORCE is non-nil, reload the file even if it was loaded before."
  (when (or force (not tla-bookmarks-loaded))
    (let ((file (dvc-config-file-full-path tla-bookmarks-file-name t)))
      (save-excursion
        (unless (file-exists-p file)
          (with-temp-file file
            (insert "()")))
        (unless (file-readable-p file)
          (error "Xtla bookmark file not readable"))
        (with-temp-buffer
          (insert-file-contents file)
          (setq tla-bookmarks-alist (read (current-buffer))
                tla-bookmarks-loaded t))))))

(defun tla-bookmarks-load-from-file (&optional force)
  "Load bookmarks from the file `tla-bookmarks-file-name'.

If FORCE is non-nil, reload the file even if it was loaded before."
  ;; TODO remove condition case (after some time)
  (condition-case nil
      (when (or force (not tla-bookmarks-loaded))
        (dvc-load-state (dvc-config-file-full-path
                         tla-bookmarks-file-name t))
        (setq tla-bookmarks-loaded t))
    (error (progn
             (tla-bookmarks-load-from-file-OBSOLETE force)))))

(defun tla-bookmarks-save-to-file ()
  "Save `tla-bookmarks-alist' to the file `tla-bookmarks-file-name'."
  (dvc-save-state '(tla-bookmarks-alist)
                  (dvc-config-file-full-path tla-bookmarks-file-name t)
                  t))

(defun tla-bookmarks-toggle-details (&optional val)
  "Toggle the display of bookmark details.
If VAL is positive, enable bookmark details.
If VAL is negative, disable bookmark details."
  (interactive "P")
  (let ((current-bookmark (ewoc-locate tla-bookmarks-cookie)))
    (setq tla-bookmarks-show-details
          (if val
              (if (> val 0) t
                (if (< val 0) nil
                  (not tla-bookmarks-show-details)))
            (not tla-bookmarks-show-details)))
    (ewoc-refresh tla-bookmarks-cookie)
    (tla-bookmarks-cursor-goto current-bookmark)))

(defvar tla-bookmarks-align 19
  "Position, in chars, of the `:' when displaying the bookmarks buffer.")

(defun tla-bookmarks-printer (element)
  "Pretty print ELEMENT, an entry of the bookmark list.
This is invoked by ewoc when displaying the bookmark list."
  (insert (if (member element tla-bookmarks-marked-list)
              (concat " " dvc-mark " ") "   "))
  (tla--insert-right-justified (concat (car element) ": ")
                               (- tla-bookmarks-align 3)
                               'dvc-bookmark-name)
  (insert (dvc-face-add (tla--name-construct
                         (cdr (assoc 'location (cdr element))))
                        'dvc-revision-name
                        'tla-bookmarks-entry-map
                        tla-bookmarks-entry-menu
                        ))
  (when tla-bookmarks-show-details
    (newline)
    (insert-char ?\  tla-bookmarks-align)
    (insert (cdr (assoc 'timestamp (cdr element))))
    (newline)
    (let ((notes (assoc 'notes (cdr element))))
      (when notes
        (insert-char ?\  tla-bookmarks-align)
        (insert (cdr notes))
        (newline)))
    (let ((nickname (assoc 'nickname (cdr element))))
      (when nickname
        (tla--insert-right-justified "nickname: " tla-bookmarks-align)
        (insert (cadr nickname))
        (newline)))
    (let ((partners (assoc 'partners (cdr element))))
      (when partners
        (tla--insert-right-justified "partners: " tla-bookmarks-align)
        (insert (cadr partners))
        (dolist (x (cddr partners))
          (insert ",\n")
          (insert-char ?\  tla-bookmarks-align)
          (insert x))
        (newline)))
    (let ((local-tree (assoc 'local-tree (cdr element))))
      (when local-tree
        (tla--insert-right-justified "local trees: " tla-bookmarks-align)
        (insert (cadr local-tree))
        (dolist (x (cddr local-tree))
          (insert ", " x ))
        (newline)))
    (let ((groups (assoc 'groups (cdr element))))
      (when groups
        (tla--insert-right-justified "Groups: " tla-bookmarks-align)
        (insert (cadr groups))
        (dolist (x (cddr groups))
          (insert ", " x ))
        (newline)))
    (let ((summary-format (assoc 'summary-format (cdr element))))
      (when summary-format
        (tla--insert-right-justified "Summary format: " tla-bookmarks-align)
        (insert "\"" (cadr summary-format) "\"")
        (newline)))))

(defun tla-bookmarks-read-local-tree (&optional bookmark arg)
  "Read a local tree for BOOKMARK, and possibly add it to the bookmarks.
If ARG is non-nil, user will be prompted anyway.  Otherwise, just use the
default if it exists."
  (let* ((loc (ewoc-locate tla-bookmarks-cookie))
         (bookmark (or bookmark
                       (and loc (ewoc-data loc))))
         (local-trees (assoc 'local-tree (cdr bookmark))))
    (cond
     ((not loc) nil)
     ((not local-trees)
      (let ((dir (dvc-read-directory-name
                  (format "Local tree for \"%s\": "
                          (car bookmark)))))
        (when (y-or-n-p "Add this tree in your bookmarks? ")
          (tla-bookmarks-add-tree bookmark dir))
        dir))
     (arg
      ;; multiple local trees.
      (let ((dir (dvc-completing-read
                  (format "Local tree for \"%s\": "
                          (car bookmark))
                  (mapcar (lambda (x) (cons x nil))
                          (cdr local-trees))
                  nil nil nil nil (cadr local-trees))))
        (when (and (not (member dir (cdr local-trees)))
                   (y-or-n-p "Add this tree in your bookmarks? "))
          (tla-bookmarks-add-tree bookmark dir))
        (when (and (not (string=
                         dir (cadr local-trees)))
                   (y-or-n-p "Make this the default? "))
          (tla-bookmarks-delete-tree bookmark dir)
          (tla-bookmarks-add-tree bookmark dir))
        dir))
     (t (cadr local-trees)))))

(defun tla-bookmarks-missing (&optional arg)
  "Show the missing patches from your partners.
The missing patches are received via tla missing.
Additionally the local changes in your working copy are also shown.

If prefix argument ARG is specified, the local tree is prompted even
if already set in the bookmarks."
  (interactive "P")
  (unless tla-bookmarks-cookie
    (error "Please, run this command from the bookmarks buffer%s"
           " (M-x tla-bookmarks RET)"))
  (let ((list (or tla-bookmarks-marked-list
                  (list (ewoc-data (ewoc-locate
                                    tla-bookmarks-cookie))))))
    (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
          (tla-bookmarks-missing-buffer-list-elem
           (mapcar
            (lambda (elem)
              (cons
               elem
               (tla-bookmarks-read-local-tree elem arg)))
            list)))
      (set-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing))
      (tla-revision-list-mode)
      (setq dvc-buffer-refresh-function 'tla-missing-refresh)
      (set (make-local-variable 'tla-missing-buffer-todolist)
           (reverse
            (apply 'append
                   (mapcar (lambda (elem)
                             (tla-bookmarks-missing-elem
                              (car elem) arg (cdr elem) t t))
                           tla-bookmarks-missing-buffer-list-elem))))
      (tla-missing-refresh))))

(defvar tla--nb-active-processes 1
  "Number of active processes in this buffer.

Used internally as a counter to launch a global handler when all
processes have finished.")

(defun tla-missing-refresh ()
  "Refreshed a *{tla|baz}-missing* buffer.

Process the variable `tla-missing-buffer-todolist' and launches the
tla processes with the appropriate handlers to fill in the ewoc."
  (interactive)
  (set (make-local-variable 'tla--nb-active-processes) 1)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (set (make-local-variable 'dvc-revlist-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'dvc-revlist-printer)))
    (dvc-kill-process-maybe (current-buffer))
    (dolist (item tla-missing-buffer-todolist)
      (case (car item)
        (missing
         ;; This item is a version that we want to check for missing patches.
         ;; ITEM is of the form:
         ;; (missing <local tree> <fully qualified version> [bookmark name])
         (let* ((local-tree (nth 1 item))
                (version (nth 2 item))
                (bookmark-name (nth 3 item))
                (shorttext (or bookmark-name version))
                (text (if bookmark-name
                          (format "Missing patches from partner %s:"
                                  bookmark-name)
                        (concat "Missing patches from archive " version)))
                (node (ewoc-enter-last dvc-revlist-cookie
                                       (list 'separator (concat
                                                         text)
                                             'partner))))
           (ewoc-enter-last dvc-revlist-cookie
                            '(message "Checking for missing patches..."))
           (let ((default-directory local-tree))
             ;; Set the default-directory for the *{tla|baz}-missing* buffer.
             (cd default-directory)
             (setq tla--nb-active-processes
                   (+ tla--nb-active-processes 1))
             (tla--run-tla-async
              `("missing"
                ,(when (tla-revisions-has-complete-log-option) "--complete-log")
                ,(when (tla-missing-has-full-option) "--full")
                ,(when tla-use-skip-present-option "--skip-present")
                ,version)
              :finished
              (dvc-capturing-lambda (output error status arguments)
                (when (and (dvc-get-buffer tla-arch-branch 'missing)
                           (buffer-live-p (dvc-get-buffer
                                           tla-arch-branch 'missing)))
                  (with-current-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing)
                    (when (ewoc-p dvc-revlist-cookie)
                      (let* ((cookie dvc-revlist-cookie)
                             (to-delete (ewoc-next cookie (capture node)))
                             (prev (ewoc-prev
                                    dvc-revlist-cookie
                                    to-delete))
                             (cur (ewoc-locate
                                   dvc-revlist-cookie))
                             (deleted (eq cur to-delete)))
                        (tla--revisions-parse-list
                         'missing nil
                         nil
                         output (capture node) cookie
                         'tla-revision-compute-merged-by
                         )
                        (dvc-ewoc-delete cookie to-delete)
                        (ewoc-refresh dvc-revlist-cookie)
                        (let ((loc (if deleted
                                       (ewoc-next
                                        dvc-revlist-cookie
                                        prev)
                                     cur)))
                          (when loc
                            (goto-char (ewoc-location loc)))))))))
              :error
              (dvc-capturing-lambda (output error status arguments)
                (when (and (dvc-get-buffer tla-arch-branch 'missing)
                           (buffer-live-p (dvc-get-buffer
                                           tla-arch-branch 'missing)))
                  (with-current-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing)
                    (when (ewoc-p dvc-revlist-cookie)
                      (let* ((cookie dvc-revlist-cookie)
                             (to-delete (ewoc-next cookie (capture node))))
                        (setf (ewoc-data to-delete)
                              (list 'message
                                    (concat
                                     "Error in "
                                     (tla-arch-branch-name)
                                     " process for "
                                     (capture shorttext)
                                     ":\n"
                                     (dvc-buffer-content
                                      error))))))))
                (message "Abnormal exit with code %d!\n%s" status
                         (dvc-buffer-content error)))))))
        (separator
         ;; This item is a separator -- the name of a bookmark.
         ;; ITEM is of the form:
         ;; (separator <text> bookmark <local tree>)
         (let* ((text (nth 1 item))
                (local-tree (nth 3 item)))
           (ewoc-enter-last dvc-revlist-cookie
                            (list 'separator
                                  text
                                  'bookmark
                                  local-tree))))
        (changes
         ;; This item is a local-tree that should be checked for changes.
         ;; ITEM is of the form:
         ;; (changes <local tree>)
         (let ((to-delete
                (ewoc-enter-last dvc-revlist-cookie
                                 '(message "Checking for local changes...")))
               (cur-buf (current-buffer))
               (parent-node (ewoc-nth dvc-revlist-cookie -1)))
           (setq default-directory (nth 1 item))
           (tla--run-tla-async
            '("changes")
            :error (dvc-capturing-lambda (output error status arguments)
                     (with-current-buffer (capture cur-buf)
                       (let* ((prev (ewoc-prev
                                     dvc-revlist-cookie
                                     (capture to-delete)))
                              (cur (ewoc-locate
                                    dvc-revlist-cookie))
                              (deleted (eq cur (capture to-delete))))
                         (tla-bookmarks-missing-parse-changes
                          output (capture parent-node))
                         (dvc-ewoc-delete dvc-revlist-cookie (capture to-delete))
                         (ewoc-refresh dvc-revlist-cookie)
                         (let ((loc (if deleted
                                        (ewoc-next
                                         dvc-revlist-cookie
                                         prev)
                                      cur)))
                           (when loc
                             (goto-char (ewoc-location loc)))))))
            :finished (dvc-capturing-lambda (output error status arguments)
                        (with-current-buffer (capture cur-buf)
                          (let* ((prev (ewoc-prev
                                        dvc-revlist-cookie
                                        (capture to-delete)))
                                 (cur (ewoc-locate
                                       dvc-revlist-cookie))
                                 (deleted (eq cur (capture to-delete))))
                            (dvc-ewoc-delete dvc-revlist-cookie (capture to-delete))
                            (ewoc-refresh dvc-revlist-cookie)
                            (let ((loc (if deleted
                                           (ewoc-next
                                            dvc-revlist-cookie
                                            prev)
                                         cur)))
                              (when loc
                                (goto-char (ewoc-location loc)))))))
            ))))
      (ewoc-set-hf dvc-revlist-cookie ""
                   (concat "\n" (dvc-face-add "end."
                                              'dvc-separator)))))
  (goto-char (point-min))
  ;; If all processes have been run synchronously,
  ;; tla--nb-active-processes is 1 now, and we should run the
  ;; callback.
  (setq tla--nb-active-processes
        (- tla--nb-active-processes 1))
  (when (zerop tla--nb-active-processes)
    (tla-revision-compute-merged-by))
  )

(defun tla--revision-ewoc-map (function ewoc-list)
  "Invoke FUNCTION on 'entry-patch nodes of EWOC-LIST.
Like (ewoc-map FUNCTION EWOC-LIST), but call FUNCTION only on
'entry-patch nodes.  The argument passed to FUNCTION is the element of
the ewoc."
  (ewoc-map (lambda (elem)
              (when (eq (car elem) 'entry-patch)
                (funcall function elem)))
            ewoc-list))

(defun tla--revision-ewoc-map-struct (function ewoc-list)
  "Invoke FUNCTION on 'entry-patch nodes of EWOC-LIST.
Like (ewoc-map FUNCTION EWOC-LIST), but call FUNCTION only on
'entry-patch nodes.  The argument passed to FUNCTION is a struct of
type tla--revisions."
  (ewoc-map (lambda (elem)
              (when (eq (car elem) 'entry-patch)
                (funcall function (nth 3 elem))))
            ewoc-list))


(defvar tla-revision-merge-by-computed nil
  "Non-nil when the \"merged-by\" field have been computed.")

(defvar tla--merged-table nil
  "Lint trap. (global value never used, always defined in a let)

A hashtable
  Revision (as string) -> (cons patches merged by this revision . nil)
We use a cons to be able to use setcar on it.
")

(defun tla-revision-compute-merged-by ()
  "Computes the field \"merged-by:\" for a revision.

In a revision list buffer, with revisions containing the \"merges:\"
information, compute another field \"merged-by:\", containing the
reverse information. If revision-A is a merge of revision-B, then,
you'll get revision-A merges: revision-B revision-B merged-by:
revision-A"
  (interactive)
  (let ((tla--merged-table (make-hash-table :test 'equal)))
    (tla--revision-ewoc-map
     (lambda (elem)
       (setf (dvc-revlist-entry-patch-merged-by (nth 1 elem)) nil))
     dvc-revlist-cookie)
    (tla--revision-ewoc-map 'tla--revision-fill-in-table
                            dvc-revlist-cookie)
    (tla--revision-ewoc-map 'tla--revision-set-merged-patches
                            dvc-revlist-cookie)
    (set (make-local-variable 'tla-revision-merge-by-computed) t)
    ))

(defun tla--revision-fill-in-table (elem)
  "Fills in `tla--merged-table' for ELEM."
  (let* ((struct (dvc-revlist-entry-patch-struct (nth 1 elem)))
         (current-list (tla--revision-revision struct))
         (current (tla--name-construct current-list)))
    (dolist (merged-rev (tla--revision-merges struct))
      (let ((hash-elem (gethash merged-rev tla--merged-table)))
        (if hash-elem
            ;; Add current to the list
            (setcar hash-elem (cons current (cdr hash-elem)))
          ;; Create the list with only current in it
          (puthash merged-rev (cons current nil)
                   tla--merged-table))))))

(eval-when-compile
  (defvar tla--merged-rev))

(defun tla--revision-set-merged-patches (elem)
  "Set the \"merged-by\" field for other revisions according to ELEM.

Adds ELEM to the list of all patches merged by ELEM."
  (let* ((struct (dvc-revlist-entry-patch-struct (nth 1 elem)))
         (current-list (tla--revision-revision struct))
         (current (tla--name-construct current-list))
         (merged-patches (gethash current tla--merged-table)))
    (dvc-trace "let")
    (setf (dvc-revlist-entry-patch-merged-by (nth 1 elem))
          (or (car merged-patches) 'nobody))))

(defun tla-bookmarks-missing-elem (data arg local-tree header
                                        &optional changes-too)
  "Show missing patches for DATA.
ARG is currently ignored but is present for backwards compatibility.
LOCAL-TREE is the local tree for which missing patches should be shown.
HEADER is currently ignored but is present for backwards compatibility.
If CHANGES-TOO is non-nil, show changes for DATA as well as missing patches."
  (let* ((default-directory local-tree)
         (partners (assoc 'partners (cdr data)))
         (location (cdr (assoc 'location (cdr data)))))
    (dvc-switch-to-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing))
    ;; The buffer was created in a context where we didn't know the
    ;; path to use. Set it now.
    (cd local-tree)
    (let ((item '()))
      (add-to-list 'item
                   `(separator
                     ,(format "Bookmark %s (%s):"
                              (car data)
                              (tla--name-construct location))
                     bookmark
                     ,local-tree))
      (when changes-too
        (add-to-list 'item `(changes ,local-tree)))
      (dolist (partner (cons (tla--name-construct
                              (cdr (assoc 'location (cdr data)))) ; Me
                             (cdr partners))) ; and my partners
        (let* ((bookmark-list
                (mapcar (lambda (bookmark)
                          (and (string= partner
                                        (tla--name-construct
                                         (cdr (assoc 'location bookmark))))
                               (car bookmark)))
                        tla-bookmarks-alist))
               (bookmark-name (progn (while (and (not (car bookmark-list))
                                                 (cdr bookmark-list))
                                       (setq bookmark-list
                                             (cdr bookmark-list)))
                                     (car bookmark-list))))
          (add-to-list 'item `(missing ,local-tree ,partner ,bookmark-name))))
      item)))

(defun tla--revisions-parse-list (type details merges buffer
                                       parent-node cookie
                                       &optional callback)
  "Parse a list of revisions.
TYPE can be either 'log, 'missing, but
could be extended in the future.

DETAILS must be non-nil if the buffer contains date, author and
summary.
MERGES must be non-nil if the buffer contains list of merged patches
for each revision.
BUFFER is the buffer to parse.

PARENT-NODE is an ewoc node to which the new items will be appened.  If
nil, append at the end of the ewoc list.
COOKIE must be the ewoc list containing PARENT-NODE.

If CALLBACK is given, it should be a function (or symbol naming a
function) that will be called once the revision list has been fully
parsed."
  (with-current-buffer (ewoc-buffer cookie)
    (set (make-local-variable 'tla-revision-merge-by-computed) nil))
  (let ((last-node parent-node)
        (buffer-to-parse (with-current-buffer buffer
                           (clone-buffer)))
        (parent-buffer (ewoc-buffer cookie))
        revision)
    (with-current-buffer buffer-to-parse
      (goto-char (point-min))
      (re-search-forward ".*/.*--.*--.*--.*" nil t)
      (beginning-of-line)
      (while (progn (> (point-max) (point)))
        (setq revision (buffer-substring-no-properties
                        (point) (line-end-position)))
        (forward-line 1)
        (let* ((rev-struct (make-tla--revision
                            :revision (tla--name-split revision)))
               (elem (list 'entry-patch
                           (make-dvc-revlist-entry-patch
                            :dvc 'tla
                            :struct rev-struct
                            :rev-id `(tla (revision ,(tla--name-split revision)))))))
          (when (or dvc-revisions-shows-summary
                    dvc-revisions-shows-creator
                    dvc-revisions-shows-date
                    tla-revisions-shows-merges
                    tla-revisions-shows-merged-by)
            (with-current-buffer parent-buffer
              (if (tla-revisions-has-complete-log-option)
                  (let* ((rev-list (tla--name-split revision))
                         (tree-str (apply 'tla--archive-tree-get-revision-struct
                                          rev-list))
                         (log-str (or tree-str
                                      (tla--read-complete-log-struct
                                       buffer-to-parse))))
                    (if tree-str (tla--skip-complete-log
                                  buffer-to-parse)
                      (tla--archive-tree-add-revision
                       (nth 0 rev-list)
                       (nth 1 rev-list)
                       (nth 2 rev-list)
                       (nth 3 rev-list)
                       (nth 4 rev-list)
                       log-str))
                    (setf (dvc-revlist-entry-patch-struct (nth 1 elem))
                          log-str)
                    (when (and callback
                               (zerop tla--nb-active-processes))
                      (funcall callback)))
                (setq tla--nb-active-processes
                      (+ tla--nb-active-processes 1))
                (tla--revlog-any
                 (tla--name-split revision)
                 nil
                 (dvc-capturing-lambda (output error status arguments)
                   (with-current-buffer output
                     (setf (tla--revision-date (capture rev-struct))
                           (tla--read-field "Standard-date"))
                     (setf (tla--revision-creator (capture rev-struct))
                           (tla--read-field "Creator"))
                     (setf (tla--revision-summary (capture rev-struct))
                           (tla--read-field "Summary"))
                     (setf (tla--revision-merges (capture rev-struct))
                           (remove (capture revision)
                                   (split-string (tla--read-field
                                                  "New-patches")))))
                   (dvc-trace "rev-struct=%s" (capture rev-struct))
                   (dvc-trace "elem=%s" (capture elem))
                   (with-current-buffer (capture parent-buffer)
                     (setq tla--nb-active-processes
                           (- tla--nb-active-processes 1))
                     (when (and (capture callback)
                                (zerop tla--nb-active-processes))
                       (funcall (capture callback))))
                   (let* ((cur (and
                                dvc-revlist-cookie
                                (ewoc-locate dvc-revlist-cookie))))
                     (ewoc-refresh (capture cookie))
                     (when cur (goto-char (ewoc-location cur))))))
                )))
          (if last-node
              (setq last-node
                    (condition-case nil
                        (ewoc-enter-after cookie last-node elem)
                      (error nil)))     ; ignore bad data
            (ewoc-enter-last cookie elem)))
        (beginning-of-line))
      (kill-buffer (current-buffer)))
    (with-current-buffer (ewoc-buffer cookie)
      (setq tla--nb-active-processes (- tla--nb-active-processes 1))
      (when (and callback
                 (zerop tla--nb-active-processes))
        (funcall callback))))
  (ewoc-refresh cookie))

(defun tla-bookmarks-missing-parse-changes (buffer parent-node)
  "Parse the output of `tla changes' from BUFFER and update PARENT-NODE."
  (with-current-buffer buffer
    (let ((changes
           (progn (goto-char (point-min))
                  (when (re-search-forward "^[^\\*]" nil t)
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (point-max)))))
          (local-tree default-directory))
      (when changes
        (with-current-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing)
          (ewoc-enter-after dvc-revlist-cookie
                            parent-node
                            (list 'entry-change
                                  changes
                                  local-tree)))))))

(defun tla-bookmarks-open-tree ()
  "Open a local tree in a dired buffer."
  (interactive)
  (dired-other-window (tla-bookmarks-read-local-tree)))

(defun tla-bookmarks-find-file ()
  "Find a file starting from the local tree of the current bookmark.
This way, you can type C-x C-f in the bookmarks buffer to open a file
of a bookmarked project."
  (interactive)
  (let ((default-directory (dvc-uniquify-file-name
                            (tla-bookmarks-read-local-tree))))
    (call-interactively 'find-file)))

(defun tla-bookmarks-tag (arg)
  "Run `tla tag' on the current bookmark.

If multiple bookmarks are marked, create a tag for each of them. If a
prefix argument ARG is given, explicitly ask for the revision to tag
from."
  (interactive "P")
  (unless tla-bookmarks-cookie
    (error "Please, run this command from the bookmarks buffer%s"
           " (M-x tla-bookmarks RET)"))
  (let ((list (or tla-bookmarks-marked-list
                  (list (ewoc-data (ewoc-locate tla-bookmarks-cookie))))))
    (let ((tags (mapcar
                 (lambda (bookmark)
                   (let ((location
                          (tla--name-construct
                           (if arg
                               (apply 'tla-name-read "Tag from revision: "
                                      (append (cdr (assoc 'location bookmark))
                                              '(prompt)))
                             (cdr (assoc 'location bookmark))))))
                     (list location
                           (tla--name-construct
                            (tla-name-read (format "Tag version for '%s': "
                                                   location)
                                           'prompt 'prompt 'prompt 'prompt))
                           (read-string
                            "Name of the bookmark for this tag: "))))
                 list)))
      (dolist (tag tags)
        (destructuring-bind (src destination name) tag
          (tla--run-tla-async
           (list "tag" (when (tla-tag-has-setup-option) "--setup")
                 src destination)
           :finished
           (dvc-capturing-lambda (output error status arguments)
             (tla-bookmarks-add (capture name) (tla--name-split (capture destination)))
             (tla-bookmarks-add-partner (assoc (capture name) tla-bookmarks-alist)
                                        (capture src) t))
           :error
           (dvc-capturing-lambda (output error status arguments)
             (error "Fail to create a tag for %s" (capture src)))))))
    (setq tla-bookmarks-marked-list nil)
    (ewoc-refresh tla-bookmarks-cookie)))

(defun tla-bookmarks-inventory ()
  "Run `tla inventory' on a local tree."
  (interactive)
  (let ((default-directory (tla-bookmarks-read-local-tree)))
    (tla-inventory nil t)))

(defun tla-bookmarks-changes ()
  "Run `tla-changes' on a local tree."
  (interactive)
  (let ((default-directory (tla-bookmarks-read-local-tree)))
    (tla-changes nil nil)))

;;;###autoload
(defun tla-bookmarks (&optional arg)
  "Display xtla bookmarks in a buffer.
With prefix argument ARG, reload the bookmarks file from disk."
  (interactive "P")
  (tla-bookmarks-load-from-file arg)
  (pop-to-buffer (dvc-get-buffer-create tla-arch-branch 'bookmark))
  (let ((pos (point)))
    (toggle-read-only -1)
    (erase-buffer)
    (set (make-local-variable 'tla-bookmarks-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'tla-bookmarks-printer)))
    (set (make-local-variable 'tla-bookmarks-marked-list) nil)
    (dolist (elem tla-bookmarks-alist)
      (ewoc-enter-last tla-bookmarks-cookie elem))
    (tla-bookmarks-mode)
    (if (equal pos (point-min))
        (if (ewoc-nth tla-bookmarks-cookie 0)
            (tla-bookmarks-cursor-goto (ewoc-nth tla-bookmarks-cookie 0))
          (message "You have no bookmarks, create some in the other buffers"))
      (goto-char pos))))


(defun tla-bookmarks-mode ()
  "Major mode to show xtla bookmarks.

You can add a bookmark with '\\<tla-bookmarks-mode-map>\\[tla-bookmarks-add]', and remove one with '\\[tla-bookmarks-delete]'.  After
marking a set of files with '\\[tla-bookmarks-mark]', make them partners with '\\[tla-bookmarks-marked-are-partners]', and
you will then be able to use '\\[tla-bookmarks-missing]' to view the missing patches.

Commands:
\\{tla-bookmarks-mode-map}"
  (interactive)
  (use-local-map tla-bookmarks-mode-map)
  (setq major-mode 'tla-bookmarks-mode)
  (setq mode-name "tla-bookmarks")
  (toggle-read-only 1)
  (run-hooks 'tla-bookmarks-mode-hook))

(defun tla-bookmarks-cursor-goto (ewoc-bookmark)
  "Move cursor to the ewoc location of EWOC-BOOKMARK."
  (interactive)
  (goto-char (ewoc-location ewoc-bookmark))
  (search-forward ":"))

(defun tla-bookmarks-next ()
  "Move the cursor to the next bookmark."
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla-bookmarks-cursor-goto next)))

(defun tla-bookmarks-previous ()
  "Move the cursor to the previous bookmark."
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla-bookmarks-cursor-goto previous)))

(defun tla-bookmarks-move-down ()
  "Move the current bookmark down."
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (data (ewoc-data elem))
         (oldname (car data))
         (next (ewoc-next cookie elem)))
    (unless next
      (error "Can't go lower"))
    (dvc-ewoc-delete cookie elem)
    (goto-char (ewoc-location
                (ewoc-enter-after cookie next data)))
    (let ((list tla-bookmarks-alist)
          newlist)
      (while list
        (if (string= (caar list) oldname)
            (progn
              (setq newlist (cons (car (cdr list)) newlist))
              (setq newlist (cons (car      list)  newlist))
              (setq list (cdr list)))
          (setq newlist (cons (car list) newlist)))
        (setq list (cdr list)))
      (setq tla-bookmarks-alist (reverse newlist)))
    (search-forward ":")))

(defun tla-bookmarks-move-up ()
  "Move the current bookmark up."
  (interactive)
  (let* ((cookie tla-bookmarks-cookie)
         (elem (ewoc-locate cookie))
         (data (ewoc-data elem))
         (oldname (car data))
         (previous (ewoc-prev cookie elem)))
    (unless previous
      (error "Can't go upper"))
    (dvc-ewoc-delete cookie elem)
    (goto-char (ewoc-location
                (ewoc-enter-before cookie previous data)))
    (let ((list tla-bookmarks-alist)
          newlist)
      (while list
        (if (string= (caar (cdr list)) oldname)
            (progn
              (setq newlist (cons (car (cdr list)) newlist))
              (setq newlist (cons (car      list)  newlist))
              (setq list (cdr list)))
          (setq newlist (cons (car list) newlist)))
        (setq list (cdr list)))
      (setq tla-bookmarks-alist (reverse newlist)))
    (search-forward ":")))

(defun tla--get-location-as-string ()
  "Construct an a/c--b--v--r string from the current bookmark."
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem))))
    (tla--name-construct location)))

(defun tla-bookmarks-get (directory)
  "Run `tla get' on the bookmark under point, placing the tree in DIRECTORY."
  (interactive (list (expand-file-name
                      (dvc-read-directory-name
                       (format "Get %s in directory: " (tla--get-location-as-string))))))
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem))))
    (tla-get directory t
             (tla--name-archive  location)
             (tla--name-category location)
             (tla--name-branch  location)
             (tla--name-version location))))

(defun tla-bookmarks-goto ()
  "Browse the archive of the current bookmark."
  (interactive)
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem)))
         (archive  (tla--name-archive  location))
         (category (tla--name-category location))
         (branch   (tla--name-branch  location))
         (version  (tla--name-version location)))
    (cond (version  (tla-revisions archive category branch version))
          (branch   (tla-versions  archive category branch))
          (category (tla-branches  archive category))
          (archive  (tla-categories archive))
          (t (error "Nothing specified for this bookmark")))))

(dvc-make-bymouse-function tla-bookmarks-goto)

(defun tla-bookmarks-star-merge (arg)
  "Star-merge the current bookmark to a local tree.
Accepts prefix argument ARG for future extension."
  (interactive "P")
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (cdr (assoc 'location elem)))
         (local-tree (dvc-read-directory-name "Star-merge into: ")))
    (tla-star-merge (tla--name-construct location)
                    local-tree)))

(defun tla-bookmarks-replay (arg)
  "Replay the current bookmark to some local tree.
Accepts prefix argument ARG for future extension."
  (interactive "P")
  (let* ((elem (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
         (location (tla--name-construct (cdr (assoc 'location elem))))
         (local-tree (dvc-read-directory-name
                      (format "Replay %s into: " location))))
    (tla-replay location local-tree)))

(defun tla-bookmarks-update (arg)
  "Update the local tree of the current bookmark.
Accepts prefix argument ARG for future extension."
  (interactive "P")
  (let* ((buf (current-buffer))
         (work-list (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate tla-bookmarks-cookie)))))
         (update-trees
          (mapcar (lambda (bookmark)
                    (let ((local-trees (cdr (assoc 'local-tree bookmark))))
                      (dvc-uniquify-file-name
                       (cond ((null local-trees)
                              (dvc-read-directory-name
                               (format "Local tree for '%s'?: "
                                       (car bookmark)) nil nil t))
                             ((not (null (cdr local-trees)))
                              (dvc-completing-read
                               (format "Local tree for '%s'?: "
                                       (car bookmark))
                               local-trees nil t))
                             (t (car local-trees))))))
                  work-list)))
    (mapc 'tla-update update-trees)
    (with-current-buffer buf
      (setq tla-bookmarks-marked-list '())
      (ewoc-refresh tla-bookmarks-cookie))))

(defun tla-bookmarks-add-elem (name info)
  "Add the association (NAME . INFO) to the list of bookmarks, and save it.
This is an internal function."
  (when (assoc name tla-bookmarks-alist)
    (error (concat "Already got a bookmark " name)))
  (let ((elem (cons name info)))
    (dvc-add-to-list 'tla-bookmarks-alist elem t)
    (tla-bookmarks-save-to-file)
    (ewoc-enter-last tla-bookmarks-cookie elem)
    ))

(defun tla-bookmarks-add (name revision-spec)
  "Add a bookmark named NAME for REVISION-SPEC."
  (interactive (let* ((fq (tla-name-read "Version: "
                                         'prompt 'prompt 'prompt 'prompt))
                      (n  (read-string (format "Name of the bookmark for `%s': "
                                               (tla--name-construct fq)))))
                 (list n fq)))
  (unless (dvc-get-buffer tla-arch-branch 'bookmark)
    (tla-bookmarks))
  (with-current-buffer (dvc-get-buffer-create tla-arch-branch 'bookmark)
    (let* ((info (list (cons 'location
                             revision-spec)
                       (cons 'timestamp (current-time-string)))))
      (tla-bookmarks-add-elem name info))))

(defun tla-bookmarks-mark ()
  "Mark the bookmark at point."
  (interactive)
  (let ((pos (point)))
    (add-to-list 'tla-bookmarks-marked-list
                 (ewoc-data (ewoc-locate tla-bookmarks-cookie)))
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos))
  (tla-bookmarks-next))

(defun tla-bookmarks-unmark ()
  "Unmark the bookmark at point."
  (interactive)
  (let ((pos (point)))
    (setq tla-bookmarks-marked-list
          (delq (ewoc-data (ewoc-locate tla-bookmarks-cookie))
                tla-bookmarks-marked-list))
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos))
  (tla-bookmarks-next))

(defun tla-bookmarks-unmark-all ()
  "Unmark all bookmarks in current buffer."
  (interactive)
  (let ((pos (point)))
    (setq tla-bookmarks-marked-list nil)
    (ewoc-refresh tla-bookmarks-cookie)
    (goto-char pos)))

(defun tla-bookmarks-marked-are-partners ()
  "Make marked bookmarks mutual partners."
  (interactive)
  (let ((list-arch (mapcar
                    (lambda (x)
                      (format "%s"
                              (tla--name-construct
                               (cdr (assoc 'location x)))))
                    tla-bookmarks-marked-list)))
    (dolist (book tla-bookmarks-marked-list)
      (let ((myloc (tla--name-construct
                    (cdr (assoc 'location book)))))
        (message myloc)
        (dolist (arch list-arch)
          (unless (string= myloc arch)
            (tla-bookmarks-add-partner book arch t))))))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))

(defun tla-bookmarks-cleanup-local-trees ()
  "Remove LOCAL-TREE field from bookmarks if they don't exist."
  (interactive)
  (dolist (book tla-bookmarks-alist)
    (let ()
      (dolist (local-tree (cdr (assoc 'local-tree book)))
        (when (and (not (file-exists-p local-tree))
                   (or tla-bookmarks-cleanup-dont-prompt
                       (y-or-n-p
                        (format
                         "Remove tree %s from bookmarks %s? "
                         local-tree
                         (car book)))))
          (tla-bookmarks-delete-tree book local-tree t)))))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))

(defun tla-bookmarks-delete (elem &optional force)
  "Delete the bookmark entry ELEM.
If FORCE is non-nil, don't ask for confirmation."
  (interactive (list (ewoc-locate tla-bookmarks-cookie)))
  (let* ((data (ewoc-data elem)))
    (when (or force
              (yes-or-no-p (format "Delete bookmark \"%s\"? " (car data))))
      (dvc-ewoc-delete tla-bookmarks-cookie elem)
      (let ((list tla-bookmarks-alist)
            newlist)
        (while list
          (unless (string= (caar list) (car data))
            (setq newlist (cons (car list) newlist)))
          (setq list (cdr list)))
        (setq tla-bookmarks-alist (reverse newlist)))
      ;; TODO could be optimized
      (tla-bookmarks-save-to-file)
      )))

(defun tla-bookmarks-find-bookmark (location)
  "Find the bookmark whose location is LOCATION (a string)."
  (let ((list tla-bookmarks-alist)
        result)
    (while list
      (when (string= (tla--name-construct
                      (cdr (assoc 'location (cdar list))))
                     location)
        (setq result (car list))
        (setq list nil))
      (setq list (cdr list)))
    result))

(defun tla-bookmarks-get-field (version field default)
  "Return VERSION'S value of FIELD, or DEFAULT if there is no value."
  (tla-bookmarks-load-from-file)
  (block dolist
    (dolist (elem tla-bookmarks-alist)
      (let ((location (cdr (assoc 'location elem))))
        (when (and (string= (tla--name-archive location)
                            (tla--name-archive version))
                   (string= (tla--name-category location)
                            (tla--name-category version))
                   (string= (tla--name-branch location)
                            (tla--name-branch version))
                   (string= (tla--name-version location)
                            (tla--name-version version)))
          (return-from dolist (or (cadr (assoc field (cdr elem))) default)))))
    default))

(defmacro tla--bookmarks-make-edit-fn (name field read-fn)
  "Define an interactive function called NAME for editing FIELD of a bookmark
entry."
  (declare (indent 2) (debug (&define name form function-form)))
  `(defun ,name (bookmarks value &optional dont-save)
     "Adds the directory VALUE to the list of local trees of bookmark
BOOKMARK.
Unless DONT-SAVE is non-nil, save the bookmark file."
     (interactive
      (let* ((bookmarks (or tla-bookmarks-marked-list
                            (list (ewoc-data (ewoc-locate
                                              tla-bookmarks-cookie)))))
             (bookmark (car bookmarks)))
        (list bookmarks nil)))
     (dolist (bookmark bookmarks)
       (let* ((field-contents (assoc ,field (cdr bookmark)))
              (value (or value
                         (,read-fn
                          (car bookmark)
                          (cadr field-contents)))))
         (if field-contents
             (setcdr (assoc ,field (cdr bookmark))
                     (list value))
           (setcdr bookmark (cons (list ,field value)
                                  (cdr bookmark))))))
     (unless dont-save
       (tla-bookmarks-save-to-file)
       (save-window-excursion
         (tla-bookmarks)))))

(tla--bookmarks-make-edit-fn
    tla-bookmarks-edit-summary
    'summary-format
  (lambda (prompt val)
    (read-string (format
                  "Summary for %s (use %%s for the merge string): "
                  prompt)
                 val)))

(defmacro tla-bookmarks-make-add-fn (name field message-already message-add)
  "Define a function called NAME for adding FIELD to a bookmark entry.
This function will display MESSAGE-ALREADY if the user tries to add a field
twice, and will display MESSAGE-ADD when a new field is successfully added."
  (declare (indent 2) (debug (&define name form stringp stringp)))
  `(defun ,name (bookmark value &optional dont-save)
     "Adds the directory VALUE to the list of local trees of bookmark
BOOKMARK.
Unless DONT-SAVE is non-nil, save the bookmark file."
     (let ((field-contents (assoc ,field (cdr bookmark))))
       (if field-contents
           (if (member value (cdr field-contents))
               (message ,message-already)
             (progn
               (message ,message-add)
               (setcdr field-contents (cons value
                                            (cdr field-contents)))))
         (progn
           (message ,message-add)
           (setcdr bookmark (cons (list ,field value)
                                  (cdr bookmark)))))
       (unless dont-save
         (tla-bookmarks-save-to-file)
         (save-window-excursion
           (tla-bookmarks)))))
  )

(tla-bookmarks-make-add-fn tla-bookmarks-add-tree
    'local-tree
  "Local tree already in the list"
  "Local tree added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-partner
    'partners
  "Partner already in the list"
  "Partner added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-group
    'groups
  "Group already in the list"
  "Group added to your bookmarks")

(tla-bookmarks-make-add-fn tla-bookmarks-add-nickname
    'nickname
  "Nickname already in the list"
  "Nickname added to your bookmark")

(defmacro tla-bookmarks-make-delete-fn (name field)
  "Define a function called NAME for removing FIELD from bookmark entries."
  (declare (indent 2) (debug (&define name form)))
  `(defun ,name (bookmark value &optional dont-save)
     "Deletes the directory VALUE to the list of local trees of bookmark
BOOKMARK."
     (let ((local-trees (assoc ,field (cdr bookmark))))
       (when local-trees
         (let ((rem-list (delete value (cdr (assoc ,field
                                                   bookmark)))))
           (if rem-list
               (setcdr local-trees rem-list)
             ;; Remove the whole ('field ...)
             (setcdr bookmark (delq local-trees (cdr bookmark))))))
       (unless dont-save
         (tla-bookmarks-save-to-file)
         (save-window-excursion
           (tla-bookmarks)))))
  )

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-tree
    'local-tree)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-partner
    'partners)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-group
    'groups)

(tla-bookmarks-make-delete-fn tla-bookmarks-delete-nickname
    'nickname)

(defun tla-bookmarks-add-partner-interactive ()
  "Add a partner to the current or marked bookmarks."
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie)))))
        (partner (tla--name-construct
                  (tla-name-read "Add partner version: "
                                 'prompt 'prompt 'prompt 'prompt))))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-partner bookmark partner t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-add-partners-from-file ()
  "Add a partner to the current or marked bookmarks."
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie))))))
    (dolist (bookmark bookmarks)
      (let ((partners (tla-partner-list
                       (tla-bookmarks-read-local-tree bookmark))))
        (dolist (partner partners)
          (tla-bookmarks-add-partner bookmark partner t))))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-write-partners-to-file ()
  "Add the partners recorded in the bookmarks to the partner file."
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie))))))
    (dolist (bookmark bookmarks)
      (let* ((local-tree (tla-bookmarks-read-local-tree bookmark))
             (partners (tla-partner-list local-tree)))
        (with-current-buffer
            (tla-partner-find-partner-file local-tree)
          (goto-char (point-max))
          (when (not (eq (point) (line-beginning-position)))
            (newline))
          (dolist (partner (cdr (assoc 'partners (cdr bookmark))))
            (unless (member partner partners)
              (insert partner "\n")))
          (and (buffer-modified-p)
               (progn (switch-to-buffer (current-buffer))
                      (y-or-n-p (format "Save file %s? "
                                        (buffer-file-name))))
               (write-file (buffer-file-name))))))))


(defun tla-bookmarks-delete-partner-interactive ()
  "Delete a partner from the current or marked bookmarks."
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (choices (apply 'append
                         (mapcar (lambda (x)
                                   (cdr (assoc 'partners
                                               (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar (lambda (x) (list x)) choices))
         (partner (dvc-completing-read "Partner to remove: " choices-alist)))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-delete-partner bookmark partner t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-add-tree-interactive ()
  "Add a local tree to the current or marked bookmarks."
  (interactive)
  (let ((bookmarks (or tla-bookmarks-marked-list
                       (list (ewoc-data (ewoc-locate
                                         tla-bookmarks-cookie)))))
        (local-tree (dvc-read-directory-name "Local tree to add: ")))
    (unless (file-exists-p (concat (file-name-as-directory local-tree) "{arch}"))
      (error (concat local-tree " is not an arch local tree.")))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-tree bookmark local-tree t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-delete-tree-interactive ()
  "Add a local tree to the current or marked bookmarks."
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (choices (apply 'append
                         (mapcar (lambda (x)
                                   (cdr (assoc 'local-tree
                                               (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar (lambda (x) (list x)) choices))
         (local-tree (dvc-completing-read "Local tree to remove: "
                                          choices-alist)))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-delete-tree bookmark local-tree t))
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-list-groups ()
  "Return the list of groups currently used by bookmarks."
  (let ((list (apply 'append
                     (mapcar (lambda (x)
                               (cdr (assoc 'groups
                                           (cdr x))))
                             tla-bookmarks-alist)))
        result)
    ;; Make elements unique
    (dolist (elem list)
      (add-to-list 'result elem))
    result))

(defun tla-bookmarks-add-group-interactive ()
  "Add a group entry in the current or marked bookmarks."
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (group (dvc-completing-read "Group of bookmarks: "
                                     (mapcar (lambda (x) (list x))
                                             (tla-bookmarks-list-groups)))))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-add-group bookmark group t)))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))


(defun tla-bookmarks-delete-group-interactive ()
  "Delete a group of bookmark entry from the current or marked bookmarks."
  (interactive)
  (let* ((bookmarks (or tla-bookmarks-marked-list
                        (list (ewoc-data (ewoc-locate
                                          tla-bookmarks-cookie)))))
         (choices (apply 'append
                         (mapcar (lambda (x)
                                   (cdr (assoc 'groups
                                               (cdr x))))
                                 bookmarks)))
         (choices-alist (mapcar (lambda (x) (list x)) choices))
         (group (dvc-completing-read "Group to remove: " choices-alist)))
    (dolist (bookmark bookmarks)
      (tla-bookmarks-delete-group bookmark group t)))
  (tla-bookmarks-save-to-file)
  (save-window-excursion
    (tla-bookmarks)))

(defun tla-bookmarks-select-by-group (group)
  "Select all bookmarks in GROUP."
  (interactive (list (dvc-completing-read
                      "Group to select: "
                      (mapcar (lambda (x) (list x))
                              (tla-bookmarks-list-groups)))))
  (dolist (bookmark tla-bookmarks-alist)
    (when (member group (cdr (assoc 'groups bookmark)))
      (add-to-list 'tla-bookmarks-marked-list bookmark))
    )
  (ewoc-refresh tla-bookmarks-cookie))

(defun tla-bookmarks-add-nickname-interactive ()
  "Add a nickname to the current bookmark."
  (interactive)
  (let* ((bookmark (ewoc-data (ewoc-locate
                               tla-bookmarks-cookie)))
         (prompt (format "Nickname for %s: " (tla--name-construct
                                              (cdr (assoc 'location bookmark))))))
    (tla-bookmarks-add-nickname bookmark (read-string prompt) t)
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defun tla-bookmarks-delete-nickname-interactive ()
  "Delete the nickname of the current bookmark."
  (interactive)
  (let* ((bookmark (ewoc-data (ewoc-locate
                               tla-bookmarks-cookie)))
         (nickname (cadr (assoc 'nickname bookmark))))
    (tla-bookmarks-delete-nickname bookmark nickname t)
    (tla-bookmarks-save-to-file)
    (save-window-excursion
      (tla-bookmarks))))

(defvar tla-buffer-bookmark nil
  "The bookmark manipulated in the current buffer.")

(defun tla-bookmarks-edit ()
  "Edit the bookmark at point."
  (interactive)
  (let* ((elem (ewoc-locate tla-bookmarks-cookie))
         (data (ewoc-data elem)))
    (pop-to-buffer (concat "*xtla bookmark " (car data) "*"))
    (erase-buffer)
    (emacs-lisp-mode)
    (make-local-variable 'tla-buffer-bookmark)
    (setq tla-buffer-bookmark elem)
    (insert ";; Edit the current bookmark. C-c C-c to finish\n\n")
    (pp data (current-buffer))
    (goto-char (point-min)) (forward-line 2) (forward-char 2)
    (local-set-key [(control ?c) (control ?c)]
                   (lambda () (interactive)
                     (goto-char (point-min))
                     (let* ((newval (read (current-buffer)))
                            (elem tla-buffer-bookmark)
                            (oldname (car (ewoc-data elem))))
                       (kill-buffer (current-buffer))
                       (pop-to-buffer (dvc-get-buffer-create tla-arch-branch
                                                             'bookmark))
                       (setcar (ewoc-data elem) (car newval))
                       (setcdr (ewoc-data elem) (cdr newval))
                       (let ((list tla-bookmarks-alist)
                             newlist)
                         (while list
                           (if (string= (caar list) oldname)
                               (setq newlist (cons newval newlist))
                             (setq newlist (cons (car list) newlist)))
                           (setq list (cdr list)))
                         (setq tla-bookmarks-alist (reverse newlist)))
                       (tla-bookmarks-save-to-file)
                       (save-excursion (tla-bookmarks)))))))

(defun tla-bookmarks-get-partner-versions (version)
  "Return version lists of partners in bookmarks for VERSION.
Each version in the returned list has a list form.
If no partner, return nil.
VERSION is a fully qualified version string or a list."
  (tla-bookmarks-load-from-file)
  (when (consp version)
    (setq version (tla--name-mask version t
                                  t t t t)))
  (let* ((bookmark (tla-bookmarks-find-bookmark version))
         (partners (cdr (assoc 'partners bookmark))))
    (mapcar 'tla--name-split partners)))

;;
;; Archives
;;

(defvar tla-archives-list-cookie nil)

;;;###autoload
(defun tla-archives ()
  "Start the archive browser."
  (interactive)
  (dvc-switch-to-buffer (dvc-get-buffer-create tla-arch-branch 'archives))
  (tla--archive-tree-build-archives)
  (let ((a-list (reverse tla--archive-tree))
        (inhibit-read-only t)
        (my-default-archive (tla-my-default-archive))
        defaultp
        archive-name
        archive-locations
        p)
    (toggle-read-only -1)
    (tla-archive-list-mode)
    (set (make-local-variable 'tla-archives-list-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'tla-archives-list-printer)))
    (erase-buffer)
    (while a-list
      (setq archive-name (caar a-list)
            archive-locations (car (cdar a-list))
            a-list (cdr a-list)
            defaultp (string= archive-name my-default-archive))
      (if defaultp (setq p (point)))
      (ewoc-enter-last tla-archives-list-cookie
                       (list archive-name
                             archive-locations
                             defaultp)))
    (let ((inhibit-read-only 1))
      (ewoc-refresh tla-archives-list-cookie)
      (if (> (point) (point-min))
          (delete-backward-char 1)))
    (when p (goto-char p))))

(defun tla-archives-list-printer (item)
  "Add an entry for ARCHIVE at LOCATIONS to the archive list.
If DEFAULTP is non-nil, this item will be rendered as the default
archive."
  (let ((archive   (nth 0 item))
        (locations (nth 1 item))
        (defaultp  (nth 2 item))
        (start-pos (point))
        overlay)
    (insert (if defaultp dvc-mark " ")
            "  "
            (dvc-face-add-with-condition
             defaultp
             archive 'dvc-marked 'tla-archive-name))
    (newline)
    (dolist (location locations)
      (insert "      " location "\n"))
    (backward-delete-char 1)
    (setq overlay (make-overlay start-pos (point)))
    (overlay-put overlay 'category 'tla-default-button)
    (overlay-put overlay 'keymap tla-archive-archive-map)
    (overlay-put overlay 'tla-archive-info archive)))

(defun tla-archives-goto-archive-by-name (name)
  "Jump to the archive named NAME."
  (unless (eq (current-buffer) (dvc-get-buffer tla-arch-branch
                                               'archives))
    (error "`tla-archives-goto-archive-by-name' can only be called in *{tla|baz}-archives* buffer"))
  (goto-char (point-min))
  (search-forward name)
  (beginning-of-line))

(defun tla-get-archive-info (&optional property)
  "Get some PROPERTY of the archive at point in an archive list buffer."
  (unless property
    (setq property 'tla-archive-info))
  (let ((overlay (car (overlays-at (point)))))
    (when overlay
      (overlay-get overlay property))))

(defun tla-my-default-archive (&optional new-default)
  "Set or get the default archive.
When called with a prefix argument NEW-DEFAULT: Ask the user for the new
default archive.
If NEW-DEFAULT IS A STRING: Set the default archive to this string.
When called with no argument: return the name of the default argument.
When called interactively, with no argument: Show the name of the default archive."
  (interactive "P")
  (when (or (numberp new-default) (and (listp new-default) (> (length new-default) 0)))
    (setq new-default (car (tla-name-read nil 'prompt))))
  (let ((i-p (interactive-p)))
    (cond ((stringp new-default)
           (message "Setting arch default archive to: %s" new-default)
           (tla--run-tla-sync (list "my-default-archive" new-default)
                              :finished 'dvc-null-handler))
          (t
           (tla--run-tla-sync '("my-default-archive")
                              :finished
                              (dvc-capturing-lambda (output error status arguments)
                                (let ((result (dvc-buffer-content output)))
                                  (when (capture i-p)
                                    (message "Default arch archive: %s"
                                             result))
                                  result))
                              :error
                              (dvc-capturing-lambda (output error status arguments)
                                (if (eq status 1)
                                    (if (capture i-p)
                                        (message "default archive not set")
                                      "")
                                  (dvc-default-error-function
                                   output error status arguments))))))))

(defun tla-whereis-archive (&optional archive)
  "Call tla whereis-archive on ARCHIVE."
  (interactive "P")
  (let (location)
    (unless archive
      (setq archive (tla--name-mask (tla-name-read "Archive: " 'prompt)
                                    t
                                    :archive)))
    (setq location
          (tla--run-tla-sync (list "whereis-archive" archive)
                             :finished
                             (lambda (output error status arguments)
                               (dvc-buffer-content output))))
    (when (interactive-p)
      (message "archive location for %s: %s" archive location))
    location))

(defvar tla--ffap-url-regexp
  (if (ffap-url-p "sftp://host")
      ffap-url-regexp
    (concat
     "\\`\\("
     "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
     "\\|"
     "\\(s?ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
     "\\)."                             ; require one more character
     ))
  "If ffap-url-regexp doesn't match sftp URL, use another value that
matches it.")

(defun tla--read-location (prompt)
  "Read the location for an archive operation, prompting with PROMPT.
The following forms are supported:
* local path: e.g.: ~/archive2004
* ftp path: e.g.: ftp://user:passwd@host.name.com/remote-path
* sftp path: e.g.: sftp://user:passwd@host.name.com/remote-path
* HTTP/WebDAV path: e.g.: http://user:passwd@host.name.com/remote-path"
  (let* ((ffap-url-regexp tla--ffap-url-regexp)
         (l (ffap-read-file-or-url prompt (ffap-url-at-point))))
    (if (string-match "^~" l)
        (expand-file-name l)
      l)))

;;;###autoload
(defun tla-register-archive ()
  "Call `tla--register-archive' interactively and `tla-archives' on success."
  (interactive)
  (let* ((result (call-interactively 'tla--register-archive))
         (archive-registered (nth 0 result))
         (archive (nth 1 result))
         (tla-response (nth 3 result)))
    (when archive-registered
      (tla-archives)
      (tla-archives-goto-archive-by-name
       (progn
         (message tla-response)  ; inform the user about the response from tla
         (if (string-match ".+: \\(.+\\)" tla-response)
             (match-string-no-properties 1 tla-response)
           archive)))
      (dvc-flash-line))))

(defun tla--register-archive (location &optional archive)
  "Register arch archive.
LOCATION should be either a local directory or a remote path.
When ffap is available the url at point is suggested for LOCATION.
ARCHIVE is the name is archive.  If ARCHIVE is not given or an empty string,
the default name is used.
The return value is a list.
- The first element shows whether the archive is registered or not; t means that
  it is registered, already means that the archive was already
  registered, and nil means that it is not registered.
- The second element shows archive name.
- The third element shows archive location.
- The fourth element is the command output string."
  (interactive (list (tla--read-location "Location: ")
                     (when (eq tla-arch-branch 'tla)
                       (read-string "Archive (empty for default): "))))
  (if (and archive (eq 0 (length archive)))
      (setq archive nil))
  (let ((archive-registered nil)
        (tla-response nil))
    (tla--run-tla-sync (list "register-archive" archive location)
                       :finished
                       (lambda (output error status arguments)
                         (setq tla-response (dvc-get-process-output))
                         (setq archive-registered t)
                         (message "%s (=> %s)"
                                  (dvc-buffer-content output) location))
                       :error
                       (lambda (output error status arguments)
                         (setq tla-response (dvc-get-error-output))
                         (if (eq status 2) ;; already registered
                             (setq archive-registered 'already)
                           (dvc-default-error-function output error
                                                       status
                                                       arguments))))
    (list archive-registered archive location tla-response)))

(defun tla--unregister-archive (archive ask-for-confirmation)
  "Delete the registration of ARCHIVE.
When ASK-FOR-CONFIRMATION is non nil, ask the user for confirmation."
  (unless (tla--archive-tree-get-archive archive)
    (tla--archive-tree-build-archives))
  (let ((location (cadr (tla--archive-tree-get-archive archive))))
    (when (or (not ask-for-confirmation)
              (yes-or-no-p (format "Delete the registration of %s(=> %s)? " archive location)))
      (tla--run-tla-sync
       (list "register-archive" "--delete" archive)
       :finished
       (lambda (output error status arguments)
         (message "Deleted the registration of %s (=> %s)" archive location))))))

(defun tla--edit-archive-location (archive)
  "Edit the location of ARCHIVE."
  (let* ((old-location (tla-whereis-archive archive))
         (new-location (read-string (format "New location for %s: " archive) old-location)))
    (unless (string= old-location new-location)
      (tla--unregister-archive archive nil)
      (tla--register-archive new-location archive))))

;;;###autoload
(defun tla-make-archive ()
  "Call `tla--make-archive' interactively  then call `tla-archives'."
  (interactive)
  (call-interactively 'tla--make-archive)
  (tla-archives))

(defun tla--make-archive-read-location ()
  (let ((path-ok nil)
        location)
    (while (not path-ok)
      (setq location (tla--read-location "Location: "))
      (setq path-ok t)
      (when (eq 'local (tla--location-type location))
        (setq location (expand-file-name location))
        (when (file-directory-p location)
          (message "directory already exists: %s" location)
          (setq path-ok nil)
          (sit-for 1))
        (when (not (file-directory-p
                    (file-name-directory location)))
          (message "parent directory doesn't exists for %s"
                   location)
          (setq path-ok nil)
          (sit-for 1))))
    location))

(defun tla--make-archive (name location &optional signed listing)
  "Create a new arch archive.
NAME is the global name for the archive.  It must be an
email address with a fully qualified domain name, optionally
followed by \"--\" and a string of letters, digits, periods
and dashes.
LOCATION specifies the path, where the archive should be created.

Examples for name are:
foo.bar@flups.com--public
foo.bar@flups.com--public-2004

If SIGNED is non-nil, the archive will be created with --signed.
If LISTING is non-nil, the archive will be created with --listing
 (Usefull for http mirrors)."
  (interactive
   (list (read-string "Archive name: ")
         (tla--make-archive-read-location)
         (y-or-n-p "Sign the archive? ")
         (y-or-n-p "Create .listing files? ")))
  (tla--run-tla-sync (list "make-archive"
                           (when listing "--listing")
                           (when signed "--signed")
                           name location)
                     :error
                     (lambda (output error status arguments)
                       (dvc-show-error-buffer error)
                       (dvc-show-last-process-buffer)
                       (error (format "%s failed: exits-status=%s"
                                      (tla-arch-branch-name)
                                      status)))))

(defun tla-mirror-archive (&optional archive location mirror signed
                                     listing)
  "Create a mirror for ARCHIVE, at location LOCATION, named MIRROR.
If SIGNED is non-nil, the archive will be signed.
If LISTING is non-nil, .listing files will be created (useful for HTTP
mirrors)."
  (interactive)
  (let* ((archive (or archive (car (tla-name-read "Archive to mirror: " 'prompt))))
         (location (or location (tla--read-location
                                 (format "Location of the mirror for %s: " archive))))
         ;;todo: take a look ath the mirror-list, when suggesting a mirror name
         ;;(mirror-list (tla--get-mirrors-for-archive archive))
         (mirror (unless (tla-use-baz-archive-registration)
                   (or mirror (read-string "Name of the mirror: "
                                           (concat archive
                                                   "-MIRROR")))))
         (signed (or signed (y-or-n-p "Sign mirror? ")))
         (listing (or listing (y-or-n-p "Create .listing files? "))))
    (tla--run-tla-sync (list "make-archive"
                             (when listing "--listing")
                             (when signed "--signed")
                             "--mirror"
                             archive mirror location))))

(defun tla-mirror-from-archive (&optional from-archive location)
  "Create a mirror-from archive for FROM-ARCHIVE, at location LOCATION.
The archive name FROM-ARCHIVE must end with \"-SOURCE\"."
  (interactive)
  (let* ((from-archive (or from-archive
                           (car (tla-name-read "Mirror from archive: " 'prompt))))
         (location (or location (read-string
                                 (format "Location of the mirror for %s : " from-archive)))))
    (unless (eq (tla--archive-type from-archive) 'source)
      (error "%s is not SOURCE archive" from-archive))
    (tla--run-tla-sync (list "make-archive"
                             "--mirror-from"
                             from-archive location))))

(defun tla--get-mirrors-for-archive (archive)
  "Get a list of all mirrors for the given ARCHIVE."
  (tla--archive-tree-build-archives)
  (delete nil (mapcar '(lambda (elem)
                         (let ((a-name (car elem)))
                           (when (and (eq (tla--archive-type a-name) 'mirror)
                                      (string= archive
                                               (substring a-name 0 (length archive))))
                             a-name)))
                      tla--archive-tree)))

;; in tla-browse use: (tla--name-archive (tla--widget-node-get-name))
;; to get the name of an archive.
;; in tla-archives: use (tla-get-archive-info)

;; (tla--get-mirrors-for-archive (tla-get-archive-info))
;; (tla--get-mirrors-for-archive "xsteve@nit.at--public")

(defun tla--mirror-base-name (archive)
  "Return the base name of the mirror ARCHIVE."
  (when (eq (tla--archive-type archive) 'mirror)
    (substring archive 0 (string-match "-MIRROR.*$" archive))))

(defun tla-use-as-default-mirror (archive)
  "Use the ARCHIVE as default mirror.
This function checks, if ARCHIVE is a mirror (contains -MIRROR).
The default mirror ends with -MIRROR.  Other mirrors have some
other characters after -MIRROR (e.g.: -MIRROR-2.
This function swaps the location of that -MIRROR and the -MIRROR-2.
The effect of the swapping is, that the mirroring functions work
per default on the default mirror."
  (interactive (list (tla--name-archive (tla-name-read "Mirror archive name: " 'prompt))))
  (unless (eq (tla--archive-type archive) 'mirror)
    (error "%s is not a mirror" archive))
  (if (string-match "-MIRROR$" archive)
      (message "%s is already the default mirror." archive)
    (let* ((archive-base-name (tla--mirror-base-name archive))
           (mirror-list (tla--get-mirrors-for-archive archive-base-name))
           (default-mirror (concat archive-base-name "-MIRROR"))
           (default-mirror-present (member default-mirror mirror-list))
           (archive-location (tla-whereis-archive archive))
           (default-mirror-location (and default-mirror-present
                                         (tla-whereis-archive default-mirror))))
      (if default-mirror-present
          (message "swapping mirrors %s <-> %s." archive default-mirror)
        (message "using %s as default mirror." archive))
      (tla--unregister-archive archive nil)
      (when default-mirror-present
        (tla--unregister-archive default-mirror nil))
      (tla--register-archive archive-location default-mirror)
      (when default-mirror-present
        (tla--register-archive default-mirror-location archive)))))


(defun tla--archive-convert-to-source-archive (archive &optional location)
  "Change the name of ARCHIVE to ARCHIVE-SOURCE.
Sets the archive location to LOCATION."
  (unless location
    (setq location (nth 1 (tla--archive-tree-get-archive archive))))
  (unless location
    (error "Location for `%s' is unknown" archive))
  (when (eq 'source (tla--archive-type archive))
    (error "%s is already source" archive))
  ;; (unless (eq 'http (tla--location-type location))
  ;;   (error "Read only archive is supported in xtla: " location))
  (tla--unregister-archive archive nil)
  (tla--register-archive location (concat archive "-SOURCE")))

;;
;; Categories
;;
(defun tla-categories (archive)
  "List the categories of ARCHIVE."
  (interactive (list (tla--name-archive
                      (tla-name-read nil 'prompt))))
  (unless archive
    (setq archive (tla-my-default-archive)))
  (tla--archive-tree-build-categories archive)
  (dvc-switch-to-buffer (dvc-get-buffer-create tla-arch-branch 'categories archive))
  (let ((list (cddr (tla--archive-tree-get-archive archive)))
        category start-pos overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-archives.
    (insert (format "Archive: %s\n%s\n" archive
                    (make-string (+ (length archive)
                                    (length "Archive: ")) ?=)))
    (save-excursion
      (while list
        (setq category (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (dvc-face-add category 'tla-category-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'keymap   tla-category-category-map)
        (overlay-put overlay 'tla-category-info category)
        )
      (delete-backward-char 1)))
  (tla-category-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name)
       archive))

(defun tla-make-category (archive category)
  "In ARCHIVE, create CATEGORY."
  (interactive (let ((l (tla-name-read "New Category: " 'prompt 'prompt)))
                 (list (tla--name-archive l)
                       (tla--name-category l))))
  (tla--run-tla-sync (list "make-category"
                           (tla--name-construct archive category)))
  (let ((tla-buffer-archive-name archive))
    (run-hooks 'tla-make-category-hook)))

;;
;; Branches
;;
(defun tla-branches (archive category)
  "Display the branches of ARCHIVE/CATEGORY."
  (interactive (let ((l (tla-name-read nil 'prompt 'prompt)))
                 (list (tla--name-archive l)
                       (tla--name-category l))))
  (tla--archive-tree-build-branches archive category)
  (dvc-switch-to-buffer (dvc-get-buffer-create tla-arch-branch
                                               'branches (tla--name-construct
                                                          archive category)))
  (let ((list (cdr (tla--archive-tree-get-category archive category)))
        alength
        clength
        branch
        start-pos
        overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-categories and tla-archives
    (setq alength (+ (length archive)  (length "Archive: "))
          clength (+ (length category) (length "Category: ")))
    (insert (format "Archive: %s\nCategory: %s\n%s\n" archive category
                    (make-string (max alength clength) ?=)))
    (save-excursion
      (while list
        (setq branch (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (dvc-face-add (if (string= branch "")
                                        "<empty>" branch)
                                    'tla-branch-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'keymap    tla-branch-branch-map)
        (overlay-put overlay 'tla-branch-info branch))
      (delete-backward-char 1)))
  (tla-branch-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name)
       archive)
  (set (make-local-variable 'tla-buffer-category-name)
       category))

(defun tla-make-branch (archive category branch)
  "Make a new branch in ARCHIVE/CATEGORY called BRANCH."
  (interactive (let ((l (tla-name-read "New Branch: "
                                       'prompt 'prompt 'prompt)))
                 (list (tla--name-archive l)
                       (tla--name-category l)
                       (tla--name-branch l))))
  (tla--run-tla-sync (list "make-branch"
                           (tla--name-construct
                            archive category branch)))
  (let ((tla-buffer-archive-name archive)
        (tla-buffer-category-name category))
    (run-hooks 'tla-make-branch-hook)))

;;
;; Versions
;;
(defun tla-versions (archive category branch)
  "Display the versions of ARCHIVE/CATEGORY in BRANCH."
  (interactive (let ((l (tla-name-read nil
                                       'prompt 'prompt 'prompt)))
                 (list (tla--name-archive l)
                       (tla--name-category l)
                       (tla--name-branch l))))
  (tla--archive-tree-build-versions archive category branch)
  (dvc-switch-to-buffer (dvc-get-buffer-create tla-arch-branch
                                               'versions (tla--name-construct archive
                                                                              category branch)))
  (let ((list (cdr (tla--archive-tree-get-branch
                    archive category branch)))
        alength
        clength
        blength
        version
        start-pos
        overlay)
    (toggle-read-only -1)
    (erase-buffer)
    ;; TODO: button to invoke tla-categories and tla-archives
    (setq alength (+ (length archive)  (length "Archive: "))
          clength (+ (length category) (length "Category: "))
          blength (+ (length branch)   (length "Branch: ")))
    (insert (format "Archive: %s\nCategory: %s\nBranch: %s\n%s\n"
                    archive category branch
                    (make-string (max alength clength blength) ?=)))
    (save-excursion
      (while list
        (setq version (car (car list))
              start-pos (point)
              list (cdr list))
        (insert "   " (dvc-face-add version 'tla-version-name))
        (newline)
        (setq overlay (make-overlay start-pos (point)))
        (overlay-put overlay 'category 'tla-default-button)
        (overlay-put overlay 'keymap   tla-version-version-map)
        (overlay-put overlay 'tla-version-info version))
      (delete-backward-char 1)))
  (tla-version-list-mode)
  (set (make-local-variable 'tla-buffer-archive-name) archive)
  (set (make-local-variable 'tla-buffer-category-name) category)
  (set (make-local-variable 'tla-buffer-branch-name) branch))

(defun tla-make-version (archive category branch version)
  "In ARCHIVE/CATEGORY, add a version to BRANCH called VERSION."
  (interactive (let ((l (tla-name-read "Version: "
                                       'prompt 'prompt 'prompt 'prompt)))
                 (list (tla--name-archive l)
                       (tla--name-category l)
                       (tla--name-branch l)
                       (tla--name-version l))))

  (tla--run-tla-sync (list "make-version"
                           (tla--name-construct
                            archive category branch version)))
  (let ((tla-buffer-archive-name archive)
        (tla-buffer-category-name category)
        (tla-buffer-branch-name branch))
    (run-hooks 'tla-make-version-hook)))

;;
;; Revisions
;;
(defun tla-revision-list-entry-patch-printer (elem)
  "Print an element ELEM of the revision list."
  (let* ((struct (dvc-revlist-entry-patch-struct elem))
         (merged-by (dvc-revlist-entry-patch-merged-by elem))
         (unmerged (eq merged-by 'nobody)))
    (insert (if (dvc-revlist-entry-patch-marked elem)
                (concat " " dvc-mark) "  ")
            ;; The revision is in library?
            (if (and tla-revisions-shows-library
                     (apply 'tla--revlib-tree-get-revision
                            (tla--revision-revision struct)))
                ;;
                ;; (apply 'tla-library-find
                ;;       (append (car (cddr elem) '(t)))

                "L " "  ")
            (dvc-face-add (tla--name-construct
                           (tla--revision-revision struct))
                          (if unmerged 'dvc-unmerged
                            'dvc-revision-name)
                          'tla-revision-revision-map
                          tla-revision-revision-menu)
            (if unmerged (dvc-face-add "  [NOT MERGED]"
                                       'dvc-unmerged)
              ""))
    (let ((summary (tla--revision-summary struct))
          (creator (tla--revision-creator struct))
          (date (tla--revision-date struct)))
      (when (and summary dvc-revisions-shows-summary)
        (insert "\n      " summary))
      (when (and creator dvc-revisions-shows-creator)
        (insert "\n      " creator))
      (when (and date dvc-revisions-shows-date)
        (insert "\n      " date)))
    (when (and tla-revisions-shows-merges
               (tla--revision-merges struct)
               (not (null (car (tla--revision-merges struct)))))
      (insert "\n      Merges:")
      (dolist (elem (tla--revision-merges struct))
        (insert "\n        " elem)))
    (when tla-revisions-shows-merged-by
      (cond ((null merged-by) nil)
            ((listp merged-by)
             (insert "\n      Merged-by:")
             (dolist (elem merged-by)
               (insert "\n        " elem)))))))

;;;###autoload
(defun tla-tree-revisions-goto (root)
  "Goto tree revisions buffer or call `tla-tree-revisions'."
  (interactive (list (dvc-read-project-tree-maybe
                      "Revisions for tree: ")))
  (let* ((default-directory root)
         (buffer (dvc-get-buffer tla-arch-branch 'revisions
                                 (tla-tree-version))))
    (if buffer
        (dvc-switch-to-buffer buffer)
      (tla-tree-revisions root))))

;;;###autoload
(defun tla-tree-revisions (root)
  "Call `tla-revisions' in the current tree."
  (interactive (list (dvc-read-project-tree-maybe
                      "Revisions for tree: ")))
  (let* ((default-directory root)
         (version (tla-tree-version-list default-directory)))
    (unless version
      (error "Not in a project tree"))
    (apply 'tla-revisions version)))

;;;###autoload
(defun tla-revisions (archive category branch version
                              &optional unused from-revlib)
  "List the revisions of ARCHIVE/CATEGORY--BRANCH--VERSION.

UNUSED is left here to keep the position of FROM-REVLIB"
  (interactive (let ((l (tla-name-read "Version: " 'prompt 'prompt 'prompt 'prompt)))
                 (list
                  (tla--name-archive l)
                  (tla--name-category l)
                  (tla--name-branch l)
                  (tla--name-version l))))
  (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc))
        (output-buf (dvc-get-buffer-create tla-arch-branch
                                           'revisions
                                           (tla--name-construct
                                            archive category branch version)))
        separator)
    (with-current-buffer output-buf
      (tla-revision-list-mode)
      (toggle-read-only -1)
      (setq dvc-buffer-refresh-function 'tla-revision-refresh)
      (set (make-local-variable 'tla-buffer-archive-name) archive)
      (set (make-local-variable 'tla-buffer-category-name) category)
      (set (make-local-variable 'tla-buffer-branch-name) branch)
      (set (make-local-variable 'tla-buffer-version-name) version)
      (setq separator (dvc-face-add
                       (make-string
                        (max (+ (length archive)
                                (length category)
                                (length branch)
                                (length version)
                                (length "    /------patch-4242")))
                        ?\ )
                       'dvc-separator))
      (ewoc-set-hf dvc-revlist-cookie
                   (tla--revisions-header archive category branch version
                                          from-revlib separator)
                   (concat "\n" separator)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer output-buf))
    ;; TODO: Consider the case where (and update-display from-revlib)
    ;; is t.
    (funcall
     (if from-revlib 'tla--revlib-tree-build-revisions
       'tla--archive-tree-build-revisions)
     archive category branch version nil nil nil
     (dvc-capturing-lambda ()
       (unless dvc-switch-to-buffer-first
         (dvc-switch-to-buffer (capture output-buf)))
       (let ((list (cdr (funcall (if (capture from-revlib)
                                     'tla--revlib-tree-get-version
                                   'tla--archive-tree-get-version)
                                 (capture archive)
                                 (capture category)
                                 (capture branch)
                                 (capture version)))))
         (with-current-buffer (capture output-buf)
           (if tla-revisions-shows-library
               (tla--revlib-tree-build-revisions
                (capture archive)
                (capture category)
                (capture branch)
                (capture version) nil t))
           (while list
             (let* ((rev-list (list (capture archive)
                                    (capture category)
                                    (capture branch)
                                    (capture version)
                                    (caar list)))
                    (rev-struct (or (cdar list)
                                    (make-tla--revision
                                     :revision rev-list))))
               (ewoc-enter-last dvc-revlist-cookie
                                (list 'entry-patch
                                      (make-dvc-revlist-entry-patch
                                       :dvc 'tla
                                       :struct rev-struct
                                       :rev-id `(tla (revision ,rev-list))))))
             (setq list (cdr list)))
           (ewoc-refresh dvc-revlist-cookie)
           (toggle-read-only t)))))))

(defun tla--revisions-header (archive category branch version from-revlib separator)
  "Construct a header for the revision ARCHIVE/CATEGORY--BRANCH--VERSION.
Mark the revision as contained in FROM-REVLIB and use SEPARATOR to separate
the entries."
  (concat
   "Version: "
   (dvc-face-add archive  'tla-archive-name)  "/"
   (dvc-face-add category 'tla-category-name) "--"
   (dvc-face-add branch   'tla-branch-name)   "--"
   (dvc-face-add version  'tla-version-name)  "\n"
   "In Revision Library: " (dvc-face-add (if from-revlib "Yes" "No")  'bold)
   "\n"
   separator "\n"))

(defun tla-revisions-string (string)
  (let* ((list (tla--name-split string))
         (archive  (nth 0 list))
         (category (nth 1 list))
         (branch   (nth 2 list))
         (version  (nth 3 list)))
    (tla-revisions archive category branch version)))

(defun tla-versions-string (string)
  (let* ((list (tla--name-split string))
         (archive  (nth 0 list))
         (category (nth 1 list))
         (branch   (nth 2 list)))
    (tla-versions archive category branch)))

(defun tla-branches-string (string)
  (let* ((list (tla--name-split string))
         (archive  (nth 0 list))
         (category (nth 1 list)))
    (tla-branches archive category)))

(defun tla-categories-string (string)
  (let* ((list (tla--name-split string))
         (archive  (nth 0 list)))
    (tla-categories archive)))

;;;###autoload
(defun tla-missing-1 (local-tree location)
  "Search in directory LOCAL-TREE for missing patches from LOCATION.
If the current buffers default directory is in an arch managed tree use that
one unless called with a prefix arg.  In all other cases prompt for the local
tree and the location."
  (interactive (let ((dir
                      (or (if (not current-prefix-arg)
                              (tla-tree-root nil t))
                          (expand-file-name
                           (dvc-read-directory-name
                            "Search missing patches in directory: "
                            default-directory default-directory t nil)))))
                 (list dir
                       (let ((default-directory dir))
                         (if current-prefix-arg
                             (tla-name-read
                              "From location: "
                              'prompt 'prompt 'prompt 'prompt)
                           (tla-tree-version))))))
  (let ((dir (tla-tree-root)))
    (pop-to-buffer (dvc-get-buffer-create tla-arch-branch 'tla-missing))
    (cd dir))
  (let ((dvc-temp-current-active-dvc (dvc-current-active-dvc)))
    (tla-revision-list-mode))
  (setq dvc-buffer-refresh-function 'tla-missing-refresh)
  (set (make-local-variable 'tla-missing-buffer-todolist)
       `((missing ,local-tree ,(tla--name-construct location) nil)))
  (tla-missing-refresh))

(defun tla-missing-show-all-revisions ()
  "Show all revisions for the current entry in the *{tla|baz}-missing* buffer."
  (interactive)
  (if tla-missing-buffer-todolist ;; we are in a tla-missing buffer
      (apply 'tla-revisions (tla--name-split (cadr (tla--revision-get-version-info-at-point))))
    (message "Not in the *%s-missing* buffer, already all revisions visible."
             (tla-arch-branch-name))))
;;
;; Rbrowse interface
;;
(defun tla-browse-archive (archive)
  "Browse ARCHIVE.

The interface is rather poor, but tla-browse does a better job
anyway ..."
  (interactive (let ((l (tla-name-read nil 'prompt)))
                 (list (tla--name-archive l))))
  (unless archive
    (setq archive (tla-my-default-archive)))
  (tla--run-tla-sync (list "rbrowse" archive)))

(defun tla--read-config-file (prompt-file)
  "Interactively read the arguments of `tla-build-config'and `tla-cat-config'.

The string PROMPT-FILE will be used when prompting the user for a file."
  (let* ((file (read-file-name prompt-file
                               nil
                               (when (not (eq major-mode 'tla-bconfig-mode))
                                 default-directory)
                               t
                               (when (eq major-mode 'tla-bconfig-mode)
                                 (tla-file-name-relative-to-root
                                  (buffer-file-name)))))
         (buffer (get-file-buffer file))
         (relative-conf-file
          (tla-file-name-relative-to-root file)))
    (when (and buffer
               (buffer-modified-p buffer)
               (y-or-n-p (format "Save buffer %s"
                                 (buffer-name buffer))))
      (save-buffer buffer))
    (list (tla-tree-root file) relative-conf-file)))

(defun tla-build-config (tree-root config-file)
  "Run tla build-config in TREE-ROOT, outputting to CONFIG-FILE.
CONFIG-FILE is the relative path-name of the configuration.

When called interactively, arguments are read with the function
`dvc-read-project-tree-maybe'."
  (interactive (tla--read-config-file "Build configuration: "))
  (let ((default-directory tree-root))
    (tla--run-tla-async (list "build-config" config-file))))

(defun tla-cat-config (tree-root config-file snap)
  "Run tla cat-config in TREE-ROOT, showing CONFIG-FILE.
If SNAP is non-nil, then the --snap option of tla is used.

When called interactively, arguments TREE-ROOT and CONFIG-FILE are
read with the function `dvc-read-project-tree-maybe'."
  (interactive (append (tla--read-config-file "Cat configuration: ")
                       (list (y-or-n-p "Include revision number? "))))
  (let ((default-directory tree-root))
    (tla--run-tla-async
     (list "cat-config" (when snap "--snap") config-file))))

;;
;; Get
;;
;;;###autoload
(defun tla-get (directory run-dired-p archive category branch
                          &optional version revision synchronously)
  "Run tla get in DIRECTORY.
If RUN-DIRED-P is non-nil, display the new tree in dired.
ARCHIVE, CATEGORY, BRANCH, VERSION and REVISION make up the revision to be
fetched.
If SYNCHRONOUSLY is non-nil, run the process synchronously.
Else, run the process asynchronously."
  ;; run-dired-p => t, nil, ask
  (interactive (let* ((l (tla-name-read "Get: "
                                        'prompt 'prompt 'prompt 'maybe 'maybe))
                      (name (tla--name-construct l))
                      (d (dvc-read-directory-name (format "Store \"%s\" to: " name))))
                 (cons d (cons 'ask l))))
  (setq directory (expand-file-name directory))
  (if (file-exists-p directory)
      (error "Directory %s already exists" directory))
  (let* ((name (tla--name-construct
                (if (or
                     ;; the name element are given in interactive form
                     (interactive-p)
                     ;; not interactive, but revision(and maybe version) is
                     ;; passed tothis function.
                     (and revision (stringp revision)))
                    (list archive category branch version revision)
                  (tla-name-read "Version--Revision for Get(if necessary): "
                                 archive category branch
                                 (if version version 'maybe)
                                 'maybe)))))
    (funcall (if synchronously 'tla--run-tla-sync 'tla--run-tla-async)
             (list "get" name directory)
             :finished (dvc-capturing-lambda (output error status arguments)
                         (let ((i (dvc-status-handler output error status arguments)))
                           (when (zerop i)
                             (tla--get-do-bookmark (capture directory) (capture archive) (capture category) (capture branch) (capture version))
                             (tla--do-dired (capture directory) (capture run-dired-p))))))))

(defun tla--get-do-bookmark (directory archive category branch version)
  "Add DIRECTORY to the bookmark for ARCHIVE/CATEGORY--BRANCH--VERSION."
  (let ((bookmark (tla-bookmarks-find-bookmark
                   (tla--name-construct
                    archive category branch version))))
    (when bookmark
      (tla-bookmarks-add-tree bookmark directory))))

(defun tla--do-dired (directory run-dired-p)
  "Possible run dired in DIRECTORY.
If RUN-DIRED-P is 'ask, ask the user whether to run dired.
If RUN-DIRED-P is nil, do not run dired.
Otherwise, run dired."
  (setq directory (expand-file-name directory))
  (case run-dired-p
    (ask (when (y-or-n-p (format "Run dired at %s? " directory))
           (dired directory)))
    ('nil nil)
    (t (dired directory))))

;;
;; Cacherev
;;
;; TODO:
;; - provide the way to run interactively
;; - show progress
;;
(defun tla-cache-revision (archive category branch version revision)
  "Cache the revision named by ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION."
  (interactive (tla-name-read "Revision to cache: "
                              'prompt 'prompt 'prompt 'prompt 'prompt))
  (let ((result (tla--run-tla-async (list "cacherev"
                                          (tla--name-construct
                                           archive category branch version revision)))))
    ;;    (dvc-show-last-process-buffer)
    result))

;;
;; Add
;;
(defun tla-add (id &rest files)
  "Using ID, add FILES to this tree.
When called interactively, ask for the file to add.
When called interactively with a prefix argument, ask additionally for the ID."
  (interactive (let ((name
                      (read-file-name "Add file as source: "
                                      nil nil t
                                      (file-name-nondirectory (or
                                                               (buffer-file-name) ""))))
                     (id (if current-prefix-arg (read-string "id (empty for default): ") "")))
                 (list id name)))
  (if (and id (string= id ""))
      (setq id nil))
  (setq files (mapcar 'expand-file-name files))
  (tla--run-tla-sync `(,(if (tla-has-add-id-command) "add-id" "add")
                       ,@(when id (list "--id" id)) . ,files)))

;;;###autoload
(defun tla-dvc-add-files (&rest files)
  "Run tla add."
  (message "tla-add-files: %s" files)
  (dvc-run-dvc-sync tla-arch-branch (append (list (if (tla-has-add-id-command) "add-id" "add")) files)
                    :finished (dvc-capturing-lambda
                                  (output error status arguments)
                                (message "tla add finished"))))
;;
;; Remove
;;
(defun tla-remove (only-id &rest files)
  "Remove the ids of FILES, possibly also deleting the files.
If ONLY-ID is non-nil, remove the files as well as their ids.  Otherwise,
just remove the ids."
  (interactive (let* ((name
                       (read-file-name "Remove file from archive: "
                                       nil nil t
                                       (file-name-nondirectory (or
                                                                (buffer-file-name) ""))))
                      (only-id (not (y-or-n-p (format
                                               "Delete the \"%s\" locally also? "
                                               name)))))
                 (list only-id name)))
  (setq files (mapcar 'expand-file-name files))
  (dolist (f files)
    (when (equal 0 (tla--run-tla-sync (list "id" "--explicit" f)
                                      :finished 'dvc-status-handler
                                      :error 'dvc-status-handler))
      (tla--run-tla-sync (list "delete-id" f)
                         :finished 'dvc-status-handler))
    (unless only-id
      (delete-file f))))

;;
;; Move
;;
(defun tla-move (from to only-id)
  "Move the file FROM to TO.
If ONLY-ID is non-nil, move only the ID file."
  (interactive
   (list (read-file-name "Move file: "
                         nil nil t
                         (file-name-nondirectory
                          (or (dvc-get-file-info-at-point) "")))
         nil nil))
  (setq to (or to (read-file-name (format "Move file %S to: " from)
                                  nil nil nil (file-name-nondirectory from)))
        only-id (if (eq only-id 'ask)
                    (not (y-or-n-p "Move the file locally also? "))
                  only-id)
        from (expand-file-name from)
        to   (expand-file-name to))
  (let ((buffer (get-file-buffer from))
        (cmd (if only-id "move-id" "mv")))
    (if buffer
        (save-excursion
          (set-buffer buffer)
          (set-visited-file-name to)))
    (tla--run-tla-sync (list cmd from to)
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (let ((buf (find-buffer-visiting (capture from))))
                           (when buf
                             (with-current-buffer buf
                               (rename-buffer (file-name-nondirectory
                                               (capture to)))
                               (set-visited-file-name (capture to)))))
                         status))))

(defalias 'tla-mv 'tla-move)

;; ----------------------------------------------------------------------------
;; Xtla partner stuff
;; ----------------------------------------------------------------------------
(defvar tla-partner-file-precious "{arch}/+partner-versions"
  "Precious version of the partner file.
We strongly suggest keeping the default value since this is a
convention used by other tla front-ends like Aba.")

(defvar tla-partner-file-source "{arch}/=partner-versions"
  "Source version of the partner file.
We strongly suggest keeping the default value since this is
a convention used by other tla front-ends like Aba.")

(defun tla-partner-find-partner-file (&optional local-tree)
  "Do `find-file' tla-partners file and return the buffer.
If the file `tla-partner-file-precious' exists, it is used in priority.
Otherwise,use `tla-partner-file-source'.  The precious one is meant for user
configuration, whereas the source one is used for project-wide
configuration.  If LOCAL-TREE is not managed by arch, return nil."
  (interactive)
  (let ((default-directory (or local-tree
                               (tla-tree-root default-directory t))))
    (let* ((partner-file
            (cond ((not default-directory) nil)
                  ((file-exists-p (concat (tla-tree-root)
                                          tla-partner-file-precious))
                   (concat (tla-tree-root) tla-partner-file-precious))
                  (t (concat (tla-tree-root)
                             tla-partner-file-source))))
           (buffer-visiting (and partner-file (find-buffer-visiting partner-file))))
      (if buffer-visiting
          (with-current-buffer buffer-visiting
            (if (buffer-modified-p)
                (if (progn (switch-to-buffer (current-buffer))
                           (y-or-n-p (format "Save file %s? "
                                             (buffer-file-name))))
                    (save-buffer)
                  (revert-buffer)))
            buffer-visiting)
        (when partner-file
          (find-file-noselect partner-file))))))


(defun tla-partner-add (partner &optional local-tree)
  "Add a partner for this xtla working copy.
Return nil if PARTNER is alerady in partners file.
Look for the parners file in LOCAL-TREE.
For example: Franz.Lustig@foo.bar--public/tla--main--0.1"
  (interactive (list (tla--name-construct
                      (tla-name-read
                       "Version to Add Partner File: "
                       'prompt 'prompt 'prompt 'prompt))))
  (let ((list (tla-partner-list local-tree)))
    (if (member partner list)
        nil
      (with-current-buffer (tla-partner-find-partner-file)
        (goto-char (point-min))
        (insert partner)
        (newline)
        (save-buffer))
      partner)))

(defun tla-partner-list (&optional local-tree)
  "Read the partner list from partner files in LOCAL-TREE.
If LOCAL-TREE is nil, use the `tla-tree-root' of `default-directory' instead.
If LOCAL-TREE is not managed by arch, return nil."
  (let ((buffer (tla-partner-find-partner-file local-tree)))
    (when buffer
      (with-current-buffer buffer
        (let ((partners (split-string (buffer-substring (point-min) (point-max)) "\n")))
          (remove "" partners))))))

(defun tla--partner-member (version &optional local-tree)
  "Predicate to check whether VERSION is in the partners file in LOCAL-TREE."
  (let ((list (tla-partner-list local-tree)))
    (member version list)))

(defun tla--partner-read-version (&optional prompt including-self)
  "Specialized version for `tla-name-read' to read a partner.
- This function displays PROMPT, reads an archive/category--branch--version,
and:
- Return the result in a string form (not in a list form) and
- Ask to the user whether adding the result to the partner file or not
  if the result is not in the partner file.

If INCLUDING-SELF is non-nil, this function asks a question whether
using self as partner or not.  If the user answers `y' as the question,
this function returns a symbol, `self'.  If the user answers `n' as the
question, this function runs as the same as if INCLUDING-SELF is nil."
  (unless prompt (setq prompt "Enter Xtla Partner: "))
  (if (and including-self
           (y-or-n-p "Select `self' as partner? "))
      'self
    (let ((version (tla--name-construct
                    (tla-name-read
                     prompt
                     'prompt 'prompt 'prompt 'prompt))))
      (when (and (not (tla--partner-member version))
                 (y-or-n-p (format "Add `%s' to Partner File? " version)))
        (tla-partner-add version))
      version)))

;; FIXME: Currently does nothing in XEmacs.
(defun tla--partner-create-menu (action &optional prompt)
  "Create the partner menu with ACTION using PROMPT as the menu name."
  (let ((list (tla-partner-list)))
    (dvc-funcall-if-exists
     easy-menu-create-menu prompt
     (mapcar
      (lambda (item)
        (let ((v (make-vector 3 nil)))
          (aset v 0 item)               ; name
          (aset v 1 `(,action ,item))
          (aset v 2 t)                  ; enable
          ;;(aset v 3 :style)
          ;;(aset v 4 'radio)
          ;;(aset v 5 :selected)
          ;;(aset v 6 (if ...))
          v))
      list))))

;; ----------------------------------------------------------------------------
;; tla-inventory-mode:
;; ----------------------------------------------------------------------------

(defun tla-inventory-mode ()
  "Major Mode to show the inventory of a tla working copy.

This allows you to view the list of files in your local tree.  You can
display only some particular kinds of files with 't' keybindings:
'\\<tla-inventory-mode-map>\\[tla-inventory-toggle-source]' to toggle show sources,
'\\[tla-inventory-toggle-precious]' to toggle show precious, ...

Use '\\[tla-inventory-mark-file]' to mark files, and '\\[tla-inventory-unmark-file]' to unmark.
If you commit from this buffer (with '\\[tla-inventory-edit-log]'), then, the list of selected
files in this buffer at the time you actually commit with
\\<tla-log-edit-mode-map>\\[tla-log-edit-done].

Commands:
\\{tla-inventory-mode-map}"
  (interactive)
  ;;  don't kill all local variables : this would clear the values of
  ;;  tla-inventory-display-*, and refresh wouldn't work well anymore.
  ;;  (kill-all-local-variables)
  (use-local-map tla-inventory-mode-map)
  (setq dvc-buffer-refresh-function 'tla-inventory)
  (make-local-variable 'dvc-buffer-marked-file-list)
  (easy-menu-add tla-inventory-mode-menu)
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-inventory-mode)
  (setq mode-name "tla-inventory")
  (setq mode-line-process 'tla-mode-line-process)
  (set (make-local-variable 'dvc-get-file-info-at-point-function)
       'tla-inventory-get-file-info-at-point)
  (set (make-local-variable 'tla-generic-select-files-function)
       'tla--inventory-select-files)
  (toggle-read-only 1)
  (run-hooks 'tla-inventory-mode-hook))

(defun tla-inventory-cursor-goto (ewoc-inv)
  "Move cursor to the ewoc location of EWOC-INV."
  (interactive)
  (if ewoc-inv
      (progn (goto-char (ewoc-location ewoc-inv))
             (forward-char 6))
    (goto-char (point-min))))

(defun tla-inventory-next ()
  "Go to the next inventory item."
  (interactive)
  (let* ((cookie tla-inventory-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla-inventory-cursor-goto next)))

(defun tla-inventory-previous ()
  "Go to the previous inventory item."
  (interactive)
  (let* ((cookie tla-inventory-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla-inventory-cursor-goto previous)))

(defun tla-inventory-edit-log (&optional insert-changelog)
  "Wrapper around `tla-edit-log', setting the source buffer to current buffer.
If INSERT-CHANGELOG is non-nil, insert a changelog too."
  (interactive "P")
  (tla-edit-log insert-changelog (current-buffer)))

(defun tla-inventory-add-files (files)
  "Create explicit inventory ids for FILES."
  (interactive
   (list
    (if dvc-buffer-marked-file-list
        (progn
          (unless (y-or-n-p (if (eq 1 (length dvc-buffer-marked-file-list))
                                (format "Add %s? "
                                        (car dvc-buffer-marked-file-list))
                              (format "Add %s files? "
                                      (length dvc-buffer-marked-file-list))))
            (error "Not adding any file"))
          dvc-buffer-marked-file-list)
      (list (read-file-name "Add file: " default-directory
                            nil nil
                            (dvc-get-file-info-at-point))))))
  (apply 'tla-add nil files)
  (tla-inventory))

(defun tla-inventory-remove-files (files id-only)
  "Remove explicit inventory ids of FILES.
If ID-ONLY is nil, remove the files as well."
  (interactive
   (let ((read-files
          (if dvc-buffer-marked-file-list
              (progn
                (unless (yes-or-no-p
                         (format "Remove %d MARKED file%s from archive? "
                                 (length dvc-buffer-marked-file-list)
                                 (if (< (length dvc-buffer-marked-file-list) 2)
                                     "" "s")))
                  (error "Not removing any file"))
                dvc-buffer-marked-file-list)
            (list (let ((file (dvc-get-file-info-at-point)))
                    (if (yes-or-no-p (format "Remove %s? " file))
                        file
                      (error "Not removing any file")))))))
     (list read-files (not (y-or-n-p (format "Delete %d %sfile%s also locally? "
                                             (length read-files)
                                             (if dvc-buffer-marked-file-list "MARKED " "")
                                             (if (< (length read-files) 2) "" "s")))))))
  (apply 'tla-remove id-only files)
  (tla-inventory))

(defun tla-inventory-delete-files (files no-questions)
  "Delete FILES locally.
This is here for convenience to delete left over, temporary files or files
avoiding a commit or conflicting with tree-lint.

It is not meant to delete tla managed files, i.e. files with IDs will be
passed to `tla-inventory-remove-files'!

When called with a prefix arg NO-QUESTIONS, just delete the files."
  (interactive
   (list
    (if dvc-buffer-marked-file-list
        (progn
          (or current-prefix-arg
              (unless (yes-or-no-p
                       (format "Delete %d files permanently? "
                               (length dvc-buffer-marked-file-list)))
                (error "Not deleting any files")))
          dvc-buffer-marked-file-list)
      (if (or current-prefix-arg
              (yes-or-no-p (format "Delete file %S permanently? "
                                   (dvc-get-file-info-at-point))))
          (list (dvc-get-file-info-at-point))))
    current-prefix-arg))
  (while files
    (let ((f (car files)))
      (if (= 0 (tla--run-tla-sync (list "id" f)
                                  :finished 'dvc-status-handler
                                  :error 'dvc-status-handler))
          (if (or no-questions
                  (y-or-n-p (format (concat "File %s is arch managed! "
                                            "Delete it with its id?") f)))
              (tla-inventory-remove-files (list f) nil))
        (if (file-directory-p f)
            (condition-case nil
                (delete-directory f)
              (file-error
               (if (or no-questions
                       (y-or-n-p (format "Delete non-empty directory %S? " f)))
                   (dired-delete-file f 'always))))
          (delete-file f))))
    (setq files (cdr files)))
  (if dvc-buffer-marked-file-list
      (setq dvc-buffer-marked-file-list nil))
  (tla-inventory))

(defun tla-inventory-move ()
  "Rename file at the current point and update its inventory id if present."
  (interactive)
  (if (eq 0 (tla-move (dvc-get-file-info-at-point) nil 'ask))
      (dvc-generic-refresh)
    (dvc-show-last-process-buffer)))

(defun tla-inventory-revert (files)
  "Reverts file at point or marked files."
  (interactive
   (list (if dvc-buffer-marked-file-list
             (progn
               (unless (yes-or-no-p
                        (format "Revert %d MARKED file%s? "
                                (length dvc-buffer-marked-file-list)
                                (if (< (length dvc-buffer-marked-file-list) 2)
                                    "" "s")))
                 (error "Not reverting any file"))
               dvc-buffer-marked-file-list)
           (list (let ((file (dvc-get-file-info-at-point)))
                   (if (yes-or-no-p (format "Revert %s? " file))
                       file
                     (error "Not reverting any file")))))))
  (mapcar 'tla-inventory-revert-file files))

(defun tla-inventory-revert-file (file)
  "Reverts FILE."
  (let* ((absolute (if (file-name-absolute-p file)
                       file
                     (expand-file-name
                      (concat (file-name-as-directory
                               default-directory) file)))))
    (tla-file-revert absolute)))

(defun tla-inventory-undo (specify-revision)
  "Undo whole local tree associated with the current inventory buffer.
If prefix arg, SPECIFY-REVISION is non-nil, read a revision and use it to undo.
The changes are saved in an ,,undo directory.  You can restore them again via
`tla-inventory-redo'."
  (interactive "P")
  (let* ((tree (tla-tree-root default-directory t))
         (revision (if specify-revision
                       (tla--read-revision-with-default-tree
                        "Undo against archive: "
                        tree)
                     (list nil nil nil nil nil))))
    (apply 'tla--undo-internal tree nil nil revision)))

(defun tla-inventory-maybe-undo-directory ()
  "Return the directory name under point if it may be an ,,undo-? directory.
Return nil otherwise."
  (car (member (expand-file-name (dvc-get-file-info-at-point))
               (tla--get-undo-changeset-names))))

(defun tla-inventory-redo ()
  "Redo whole local tree associated with the current inventory buffer.
This function restores the saved changes from `tla-inventory-undo'."
  (interactive)
  (tla-redo (tla-inventory-maybe-undo-directory)))

;;;###autoload
(defun tla-file-has-conflict-p (file-name)
  "Return non-nil if FILE-NAME has conflicts."
  (let ((rej-file-name (concat default-directory
                               (file-name-nondirectory file-name)
                               ".rej")))
    (file-exists-p rej-file-name)))

(defun tla-inventory-find-file ()
  "Visit the current inventory file."
  (interactive)
  (let* ((file (dvc-get-file-info-at-point)))
    (cond
     ((not file)
      (error "No file at point"))
     ((eq t (car (file-attributes file))) ; file is a directory
      (tla-inventory (expand-file-name file)))
     (t
      (find-file file)))))

(defun tla-inventory-parent-directory ()
  "Go to parent directory in inventory mode."
  (interactive)
  (tla-inventory (expand-file-name "..")))

(defun tla-inventory-mirror ()
  "Create a mirror of version of the current tree."
  (interactive)
  (let ((tree-version (tla-tree-version-list)))
    (tla-archive-mirror (tla--name-archive  tree-version)
                        (tla--name-category tree-version)
                        (tla--name-branch   tree-version)
                        (tla--name-version  tree-version))))

(defun tla-inventory-star-merge (&optional merge-partner)
  "Run tla star-merge.
Either use a partner in the tree's \"++tla-partners\" file or ask the user
for MERGE-PARTNER."
  (interactive (list (tla--partner-read-version "Star-merge with: ")))
  (when (y-or-n-p (format "Star-merge with %s ? " merge-partner))
    (tla-star-merge merge-partner)))

(defun tla-inventory-changes (summary)
  "Run tla changes.
A prefix argument decides whether the user is asked for a diff partner
and whether only a summary without detailed diffs will be shown.

When called without a prefix argument: Show the changes for your tree.
When called with C-u as prefix: Ask the user for a diff partner via `tla--partner-read-version'.
When called with a negative prefix: Show only a summary of the changes.
When called with C-- C-u as prefix: Ask the user for a diff partner, show only change summary."
  (interactive "P")
  (let* ((ask-for-compare-partner (and summary (listp summary)))
         (compare-partner (if ask-for-compare-partner
                              (tla--partner-read-version
                               "Compare with (default is your tree): "
                               t)
                            'self)))
    (if (eq 'self compare-partner)
        (setq compare-partner nil)
      (setq compare-partner (list 'revision (tla--name-split compare-partner))))
    (when (listp summary)
      (setq summary (car summary)))
    (tla-changes summary compare-partner)))

(defun tla-inventory-replay (&optional merge-partner)
  "Run tla replay.
Either use a partner in the tree's ++tla-partners file, or ask the user
for MERGE-PARTNER."
  (interactive (list (tla--partner-read-version "Replay from: ")))
  (when (y-or-n-p (format "Replay from %s ? " merge-partner))
    (tla-replay merge-partner)))

(defun tla-inventory-update ()
  "Run tla update."
  (interactive)
  (tla-update default-directory))

(defun tla-inventory-missing (&optional arg)
  "Run tla missing in `default-directory'.
With an prefix ARG, do this for the archive of one of your partners."
  (interactive "P")
  (if arg
      (let ((missing-partner (tla--partner-read-version "Check missing against: ")))
        (when (y-or-n-p (format "Check missing against %s ? " missing-partner))
          (tla-missing-1 default-directory missing-partner)))
    (tla-missing-1 default-directory (tla-tree-version))))

(defun tla-inventory-file-ediff (&optional file)
  "Run `ediff' on FILE."
  (interactive (list (car (cddr (ewoc-data (ewoc-locate tla-inventory-cookie))))))
  (tla-file-ediff file))

(dvc-make-bymouse-function tla-inventory-find-file)

(defun tla-inventory-delta ()
  "Run tla delta.
Use the head revision of the version associated with the current inventory
buffer as modified tree.  Give the base tree interactively."
  (interactive)
  (let* ((modified (tla-tree-version-list))
         (modified-revision (apply 'tla--version-head modified))
         (modified-fq (tla--name-construct
                       (tla--name-archive modified)
                       (tla--name-category modified)
                       (tla--name-branch modified)
                       (tla--name-version modified)
                       modified-revision))
         (base (tla-name-read
                (format "Revision for delta to %s(HEAD) from: " modified-fq)
                'prompt 'prompt 'prompt 'prompt 'prompt))
         (base-fq (tla--name-construct base)))
    (tla-delta base-fq modified-fq 'ask)))


(defun tla-inventory-apply-changeset (reverse)
  "Apply changeset to the tree visited by the current inventory buffer.
With a prefix argument REVERSE, reverse the changeset."
  (interactive "P")
  (let ((inventory-buffer (current-buffer))
        (target (tla-tree-root))
        (changeset (let ((changeset-dir (or (dvc-get-file-info-at-point) "")))
                     (unless (file-directory-p (expand-file-name changeset-dir))
                       (setq changeset-dir ""))
                     (dvc-uniquify-file-name
                      (dvc-read-directory-name
                       "Changeset directory: "  changeset-dir changeset-dir)))))
    (tla-show-changeset changeset nil)
    (when (yes-or-no-p (format "Apply the changeset%s? "
                               (if reverse " in REVERSE" "")))
      (tla-apply-changeset changeset target reverse)
      (with-current-buffer inventory-buffer
        (dvc-generic-refresh)))))

(defun tla-inventory-apply-changeset-from-tgz (file)
  "Apply the changeset in FILE to the currently visited tree."
  (interactive (list (let ((changeset-tarball (or (dvc-get-file-info-at-point) "")))
                       (read-file-name "Apply changeset from tarball: " nil changeset-tarball t changeset-tarball))))
  (let ((inventory-buffer (current-buffer))
        (target (tla-tree-root)))
    (tla-apply-changeset-from-tgz file target t)
    (with-current-buffer inventory-buffer
      (dvc-generic-refresh))))

;; TODO: Use `tla--inventory-select-file' in other tla-inventory-*.
;; TODO: Mouse event check like `tla--tree-lint-select-files'.
;; TODO: Unify with `tla--tree-lint-select-files'.
(defun tla--inventory-select-files (prompt-singular
                                    prompt-plural msg-err
                                    &optional
                                    msg-prompt no-group ignore-marked
                                    no-prompt y-or-n)
  "Get the list of marked files and ask confirmation of the user.
PROMPT-SINGULAR or PROMPT-PLURAL is used as prompt.  If no file is under
the point MSG-ERR is passed to `error'.

MSG-PROMPT NO-GROUP IGNORE-MARKED NO-PROMPT and Y-OR-N are currently
ignored."
  (let ((files (if dvc-buffer-marked-file-list
                   dvc-buffer-marked-file-list
                 (list (dvc-get-file-info-at-point)))))
    (unless files
      (error msg-err))
    (if (y-or-n-p
         (format
          (if (> (length files) 1)
              prompt-plural
            prompt-singular)
          (if (> (length files) 1)
              (length files)
            (car files))))
        files
      (error msg-err))))

(defun tla-inventory-make-junk (files)
  "Prompts and make the FILES junk.
If marked files are, use them as FIELS.
If not, a file under the point is used as FILES."
  (interactive
   (list
    (tla--inventory-select-files "Make `%s' junk? "
                                 "Make %s files junk? "
                                 "Not making any file junk")))
  (tla-tree-lint-put-file-prefix files ",,"))

(defun tla-inventory-make-precious (files)
  "Prompts and make the FILES precious.
If marked files are, use them as FILES.
If not, a file under the point is used as FILES."
  (interactive
   (list
    (tla--inventory-select-files "Make `%s' precious? "
                                 "Make %s files precious? "
                                 "Not making any file precious")))
  (tla-tree-lint-put-file-prefix files "++"))

(defun tla-generic-add-to-exclude (=tagging-method)
  "Exclude the file/directory under point by adding it to =TAGGING-METHOD.
Adds an entry for the file to .arch-inventory or =tagging-method.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-to-* "exclude" =tagging-method))

(defun tla-generic-add-ext-to-exclude (=tagging-method)
  "Exclude the file/directory with the same extension as the one under
point by adding it to =TAGGING-METHOD. Adds an entry for the file to
.arch-inventory or =tagging-method. If prefix argument =TAGGING-METHOD
is non-nil, the entry is added to \"=tagging-method\" file. Else it is
added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-ext-to-* "exclude" =tagging-method))

(defun tla-generic-add-to-junk (=tagging-method)
  "Add the file/directory under point to =TAGGING-METHOD.
Adds an entry for the file to .arch-inventory or =tagging-method.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-to-* "junk" =tagging-method))

(defun tla-generic-add-ext-to-junk (=tagging-method)
  "Add the file/directory with the same extension as the one under
point to =TAGGING-METHOD. Adds an entry for the file to
.arch-inventory or =tagging-method. If prefix argument =TAGGING-METHOD
is non-nil, the entry is added to \"=tagging-method\" file. Else it is
added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-ext-to-* "junk" =tagging-method))

(defun tla-generic-add-to-backup (=tagging-method)
  "Add the file/directory under the point to =TAGGING-METHOD.
Adds an entry for the file to .arch-inventory or =tagging-method.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-to-* "backup" =tagging-method))

(defun tla-generic-add-ext-to-backup (=tagging-method)
  "Add the file/directory with the same extension as the one under the
point to =TAGGING-METHOD. Adds an entry for the file to
.arch-inventory or =tagging-method. If prefix argument =TAGGING-METHOD
is non-nil, the entry is added to \"=tagging-method\" file. Else it is
added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-ext-to-* "backup" =tagging-method))

(defun tla-generic-add-to-precious (=tagging-method)
  "Add the file/directory under the point to =TAGGING-METHOD.
Adds an entry for the file to .arch-inventory or =tagging-method.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-to-* "precious" =tagging-method))

(defun tla-generic-add-ext-to-precious (=tagging-method)
  "Add files with the same extension as the current to =TAGGING-METHOD.
Adds an entry for the file to .arch-inventory or =tagging-method.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-ext-to-* "precious" =tagging-method))

(defun tla-generic-add-to-unrecognized (=tagging-method)
  "Add the file/directory under the point as an unrecognized entry
of .arch-inventory or =tagging-method file.
If prefix argument =TAGGING-METHOD is non-nil, the entry is added to
\"=tagging-method\" file. Else it is added to \".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-to-* "unrecognized" =tagging-method))

(defun tla-generic-add-ext-to-unrecognized (=tagging-method)
  "Add the file/directory with the same extension as the one under the
point as an unrecognized entry of .arch-inventory or =tagging-method
file. If prefix argument =TAGGING-METHOD is non-nil, the entry is
added to \"=tagging-method\" file. Else it is added to
\".arch-inventory\" file."
  (interactive "P")
  (tla--generic-add-ext-to-* "unrecognized" =tagging-method))

(defun tla--generic-add-to-* (category =tagging-method)
  "Categorize currently marked files or the file under point.
Each file is categorized as CATEGORY by adding it to =TAGGING-METHOD.
If EXT-ONLY is non-nil, add only the file extension."
  (let ((write-in (if =tagging-method "=tagging-method" ".arch-inventory")))
    (tla-generic-add-files-to-*
     category =tagging-method
     (tla--generic-select-files
      (format "Make `%%s' %s in %s file? " category write-in)
      (format "Make %%s files %s in %s file? " category write-in)
      (format "Not making any file %s in %s file " category write-in)
      (format "Make file %s in %s file: " category write-in))
     nil)))

(defun tla--generic-add-ext-to-* (category =tagging-method)
  "Categorize currently marked files or the file under point.
Each file is categorized as CATEGORY by adding it to =TAGGING-METHOD.
If EXT-ONLY is non-nil, add only the file extension."
  (let ((write-in (if =tagging-method "=tagging-method" ".arch-inventory")))
    (tla-generic-add-files-to-*
     category =tagging-method
     (tla--generic-select-files
      (format "Make files with same extension as `%%s' %s in %s file? " category write-in)
      (format "Make %%s file extensions %s in %s file? " category write-in)
      (format "Not making any file extensions %s in %s file " category write-in)
      (format "Make file extension %s in %s file: " category write-in))
     t)))

(defun tla-generic-add-files-to-* (category =tagging-method files
                                            &optional ext-only)
  "Categorize FILES as CATEGORY in =TAGGING-METHOD.
If =TAGGING-METHOD is t, entries for the files are added to =tagging-method.
Else, they are added to .arch-inventory.
CATEGORY is one of the following strings: \"unrecognized\", \"precious\",
\"backup\",\"junk\" or \"exclude\".
If EXT-ONLY is non-nil, add only the file extension."
  (let ((point (point))
        (basedir (expand-file-name default-directory)))
    ;; Write down
    (save-excursion
      (mapc (lambda (file)
              (if =tagging-method
                  (tla-edit-=tagging-method-file)
                (tla-edit-.arch-inventory-file
                 (concat basedir (file-name-directory file))))
              (tla--inventory-file-add-file
               category (dvc-regexp-quote
                         (if ext-only
                             (replace-regexp-in-string
                              "^.*\\." "."
                              (file-name-nondirectory file))
                           (file-name-nondirectory file)))
               ext-only)
              (save-buffer)) files))
    ;; Keep the position
    (prog1
        (dvc-generic-refresh)
      (if (< point (point-max))
          (goto-char point)))))


(defun tla-generic-set-id-tagging-method (method)
  "Set the id tagging method of the current tree to METHOD."
  (interactive (list (tla--id-tagging-method-read
                      (tla-id-tagging-method nil))))
  (tla--id-tagging-method-set method)
  (dvc-generic-refresh))

(defun tla-generic-set-id-tagging-method-by-mouse (dummy-event)
  "Interactively set the id tagging method of the current tree.
DUMMY-EVENT is ignored."
  (interactive "e")
  (call-interactively 'tla-generic-set-id-tagging-method))

(defun tla-generic-set-tree-version (&optional version)
  "Run tla set-tree-version, setting the tree to VERSION."
  (interactive)
  (if version
      (tla-set-tree-version version)
    (call-interactively 'tla-set-tree-version))
  (dvc-generic-refresh))

;; ----------------------------------------------------------------------------
;; tla-revlog-mode:
;; ----------------------------------------------------------------------------
(defun tla-revlog-mode ()
  "Major Mode to show a specific log message.
Commands:
\\{tla-revlog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-revlog-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-revlog-font-lock-keywords t))
  (set (make-local-variable 'tla-button-marker-list)
       nil)
  (set (make-local-variable 'tla-current-revision)
       (save-excursion
         (concat
          (progn
            (goto-char (point-min))
            (re-search-forward "^Archive: ")
            (buffer-substring-no-properties (point)
                                            (line-end-position)))
          "/"
          (progn
            (goto-char (point-min))
            (re-search-forward "^Revision: ")
            (buffer-substring-no-properties (point)
                                            (line-end-position))))))
  (setq major-mode 'tla-revlog-mode)
  (setq mode-name "tla-revlog")
  (toggle-read-only 1)
  (tla-add-buttons)
  (run-hooks 'tla-revlog-mode-hook))

(defun tla-annotate-mode ()
  "Major Mode to show a specific annotate message.

Mostly similar to `tla-annotate-mode'.
Commands:
\\{tla-revlog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-revlog-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-revlog-font-lock-keywords t))
  (set (make-local-variable 'tla-button-marker-list)
       nil)
  (setq major-mode 'tla-revlog-mode)
  (setq mode-name "tla-revlog")
  (toggle-read-only 1)
  (tla-add-buttons)
  (run-hooks 'tla-revlog-mode-hook))

(defun tla-dvc-revlog-get-revision (rev-id)
  (let* ((buf (tla--revlog-any (car (dvc-revision-get-data rev-id))))
         (str (with-current-buffer buf (buffer-string))))
    str))


;;
;; Copied and adapted from gnus-art.el
;;

(defvar tla-button-alist
  `((,(tla-make-name-regexp 0 t t) 1 t
     tla-categories-string 1)
    (,(tla-make-name-regexp 1 t t) 1 t
     tla-branches-string 1)
    (,(tla-make-name-regexp 2 t t) 1 t
     tla-versions-string 1)
    (,(tla-make-name-regexp 3 t t) 1 t
     tla-revisions-string 1)
    (,(tla-make-name-regexp 4 t t) 1 t
     tla--button-revision-fn 1)
    ("Creator: \\(.*\\)$" 1 t
     tla-revlog-send-comments 1)
    ("Archive: \\(.*\\)$" 1 t
     tla-categories-string 1)))


(defvar tla-button-marker-list '())

(defun tla--button-revision-fn (revision)
  (funcall tla-button-revision-fn revision))

(defun tla-revlog-send-comments (email)
  (interactive (list (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "^Creator: \\(.*\\)$")
                       (match-string-no-properties 1))))
  (tla-revision-send-comments (tla--archive-tree-get-revision-struct
                               (tla--name-archive  tla-current-revision)
                               (tla--name-category tla-current-revision)
                               (tla--name-branch   tla-current-revision)
                               (tla--name-version  tla-current-revision)
                               (tla--name-revision tla-current-revision))
                              email))

(defun tla-button-entry ()
  "Return the first entry in `tla-button-alist' matching this place."
  (let ((alist tla-button-alist)
        (entry nil))
    (while alist
      (setq entry (pop alist))
      (if (looking-at (eval (car entry)))
          (setq alist nil)
        (setq entry nil)))
    entry))

(defun tla-button-in-region-p (b e prop)
  "Say whether PROP exists in the region."
  (text-property-not-all b e prop nil))

;;; Copied from gnus-article-add-buttons
(defun tla-add-buttons (&optional buffer force)
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `tla-button-alist'."
  (interactive (list (current-buffer) 'force))
  (with-current-buffer (or buffer (current-buffer))
    (let ((inhibit-read-only t)o
          (inhibit-point-motion-hooks t)
          (case-fold-search t)
          (alist tla-button-alist)
          beg entry regexp)
      ;; Remove all old markers.
      (let (marker entry new-list)
        (while (setq marker (pop tla-button-marker-list))
          (if (or (< marker (point-min)) (>= marker (point-max)))
              (push marker new-list)
            (goto-char marker)
            (when (setq entry (tla-button-entry))
              (put-text-property (match-beginning (nth 1 entry))
                                 (match-end (nth 1 entry))
                                 'tla-callback nil))
            (set-marker marker nil)))
        (setq tla-button-marker-list new-list))
      ;; We skip the headers.
      (goto-char (point-min))
      (setq beg (point))
      (while (setq entry (pop alist))
        (setq regexp (eval (car entry)))
        (goto-char beg)
        (while (re-search-forward regexp nil t)
          (let* ((start (and entry (match-beginning (nth 1 entry))))
                 (end (and entry (match-end (nth 1 entry))))
                 (from (match-beginning 0)))
            (when (and (or (eq t (nth 2 entry))
                           (eval (nth 2 entry)))
                       (not (tla-button-in-region-p
                             start end 'tla-callback)))
              ;; That optional form returned non-nil, so we add the
              ;; button.
              (tla-add-button
               start end 'tla-button-push
               (car (push (set-marker (make-marker) from)
                          tla-button-marker-list))))))))))

(defun tla-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (goto-char marker)
    (let* ((entry (tla-button-entry))
           (inhibit-point-motion-hooks t)
           (fun (nth 3 entry))
           (args (mapcar (lambda (group)
                           (let ((string (match-string group)))
                             (set-text-properties
                              0 (length string) nil string)
                             string))
                         (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
        (apply fun args))
       ((and (boundp fun)
             (fboundp (symbol-value fun)))
        (apply (symbol-value fun) args))
       (t
        (message "You must define `%S' to use this button"
                 (cons fun args)))))))

(defun tla-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (when dvc-button-face
    (dvc-overlay-put (dvc-make-overlay from to)
                     'face dvc-button-face))
  (dvc-add-text-properties
   from to
   (nconc (and dvc-mouse-face
               (list dvc-mouse-face-prop dvc-mouse-face))
          (list 'tla-callback fun)
          (and data (list 'tla-data data))))
  (widget-convert-button 'link from to :action 'tla-widget-press-button
                         :button-keymap nil))

(defun tla-widget-press-button (elems el)
  (goto-char (widget-get elems :from))
  (tla-press-button))

(defun tla-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `tla-callback' property,
call it with the value of the `tla-data' text property."
  (interactive "e")
  (unless event (error "Event is nil"))
  (let ((buffer
         (or (let ((window (dvc-funcall-if-exists
                            posn-window (dvc-funcall-if-exists
                                         event-start event))))
               ;; XEmacs
               (and window (window-buffer window)))
             ;; GNU Emacs
             (dvc-funcall-if-exists event-buffer event))))
    (pop-to-buffer buffer)
    (set-buffer buffer)
    (let* ((pos (or (dvc-funcall-if-exists posn-point (event-start event))
                    (dvc-funcall-if-exists event-point event)
                    (error "No way to determine point")))
           (data (get-text-property pos 'tla-data))
           (fun (get-text-property pos 'tla-callback)))
      (when pos (goto-char pos))
      (when fun
        (funcall fun data)))))

(defun tla-press-button ()
  "Check text at point for a callback function.
If the text at point has a `tla-callback' property,
call it with the value of the `tla-data' text property."
  (interactive)
  (let ((data (get-text-property (point) 'tla-data))
        (fun (get-text-property (point) 'tla-callback)))
    (when fun
      (funcall fun data))))

;;
;; End copied and adapted from gnus-art.el
;;
;;;###autoload
(defun tla-revlog (revision-spec)
  "Show the log for REVISION-SPEC."
  (interactive (list (tla--name-construct
                      (tla-name-read "Revision spec: "
                                     'prompt 'prompt 'prompt 'prompt 'prompt))))
  (tla--run-tla-sync (list "cat-log" revision-spec)
                     :finished 'dvc-finish-function-without-buffer-switch)
  (dvc-show-last-process-buffer 'revlog 'tla-revlog-mode revision-spec))
(defalias 'tla-cat-log 'tla-revlog)

(defun tla-cat-archive-log (revision-spec)
  "Run cat-archive-log for REVISION-SPEC."
  (interactive (list (tla--name-construct
                      (tla-name-read "Revision spec: "
                                     'prompt 'prompt 'prompt 'prompt 'prompt))))
  (tla--run-tla-sync (list "cat-archive-log" revision-spec)
                     :finished 'dvc-finish-function-without-buffer-switch)
  (dvc-show-last-process-buffer 'revlog 'tla-revlog-mode revision-spec))

(defun tla--maybe-save-log (revision)
  "Must be called from the buffer containing the log for REVISION.
Saves this buffer to the corresponding file in the log-library if
`tla-log-library-greedy' is non-nil."
  (if tla-log-library-greedy
      (let ((dir (expand-file-name
                  (concat (file-name-as-directory tla-log-library)
                          (car revision))))
            (file (tla--name-construct-semi-qualified (cdr revision))))
        (unless (file-directory-p dir)
          (make-directory dir))
        (let ((name (concat " *tla-log-rev-" (tla--name-construct
                                              revision) "*"))
              make-backup-files)
          (write-file (concat (file-name-as-directory dir) file))
          (set-visited-file-name
           (concat (file-name-as-directory dir) file))
          (set-buffer-modified-p nil)
          (rename-buffer name)
          (current-buffer)))
    (clone-buffer)))

(defun tla--revlog-any (revision &optional tree async-handler)
  "Create a buffer containing the log file for REVISION.

Either call cat-log, cat-archive-log, or read the log from the log library.

REVISION must be specified as a list.  If TREE is provided, try a
cat-log in TREE preferably.  Otherwise, try a cat-log in the local
directory.  If both are impossible, run cat-archive-log.  (same result,
but needs to retrieve something from the archive).

Call the function ASYNC-HANDLER in the created buffer, with arguments
 (output error status arguments)."
  ;;  (message "tla-revlog-any %S" revision)
  ;; See if the log is in the log library
  (when tla-log-library-greedy
    (if (not (file-directory-p tla-log-library))
        (make-directory tla-log-library)))
  (let* ((rev-str (if (stringp revision) revision
                    (tla--name-construct revision)))
         (rev-list (if (listp revision) revision
                     (tla--name-split revision)))
         (lib-log (concat (file-name-as-directory tla-log-library)
                          rev-str))
         (buffer
          (or (get-file-buffer lib-log)
              (when (file-exists-p lib-log)
                (let* ((name (concat " *tla-log("
                                     rev-str ")*")))
                  (or (get-buffer name)
                      ;; Surprisingly, (rename-buffer) didn't rename
                      ;; anything here. Solution: Create a buffer with
                      ;; the right name, and simulate a find-file.
                      (with-current-buffer
                          (get-buffer-create name)
                        (insert-file-contents lib-log)
                        (set-visited-file-name lib-log)
                        (rename-buffer name)
                        (set-buffer-modified-p nil)
                        (current-buffer))))))))
    (if buffer
        (if async-handler
            (funcall async-handler buffer nil 0 "cat-log")
          buffer)
      ;; Try a revlog
      (let ((run-mode (if async-handler 'tla--run-tla-async 'tla--run-tla-sync))
            (handler (if async-handler
                         (dvc-capturing-lambda (output error status arguments)
                           (with-current-buffer output
                             (tla--maybe-save-log (capture rev-list)))
                           (funcall (capture async-handler) output error status
                                    arguments))
                       (dvc-capturing-lambda (output error status arguments)
                         (with-current-buffer output
                           (tla--maybe-save-log (capture rev-list)))))))
        (tla--run-tla-sync ;; Anyway, tla revlog is fast, so, no
         ;; need for an asynchronous process. For some reason,
         ;; running it asynchronously caused a random bug when
         ;; running tla remotely.
         (list "revlog" rev-str)
         :finished handler
         ;; revlog failed: cat-archive-log is needed
         :error (dvc-capturing-lambda (output error status arguments)
                  (funcall (capture run-mode)
                           (list "cat-archive-log"
                                 (capture rev-str))
                           :finished (capture handler))))))))

(defun tla-log-get-changeset ()
  "Get and show the changeset whose log is being displayed."
  (interactive)
  (tla-get-changeset tla-current-revision t))

;; ----------------------------------------------------------------------------
;; tla-log-edit-mode:
;; ----------------------------------------------------------------------------
(defun tla-log-edit-next-field (&optional notab)
  "Go to next field in a log edition."
  (interactive)
  (let ((in-field (string-match "^\\([A-Z][A-Za-z]*\\(: ?\\)?\\)?$"
                                (buffer-substring
                                 (line-beginning-position) (point))))
        (oldpoint (point)))
    (if (and in-field
             (string-match "^[A-Z][A-Za-z]*: $"
                           (buffer-substring
                            (line-beginning-position) (point))))
        (forward-line))
    (if in-field (beginning-of-line) (forward-line 1))
    (or (and (looking-at "^[A-Z][a-zA-Z]*: ")
             (goto-char (match-end 0)))
        (and (looking-at "^[A-Z][a-zA-Z]*:$")
             (goto-char (match-end 0))
             (progn (insert " ") t))
        (let ((body
               (save-excursion
                 (when (search-forward
                        (concat "\n"
                                dvc-log-edit-file-list-marker
                                "\n") nil t)
                   (progn (goto-char (point-min))
                          (re-search-forward "^$" nil t)
                          (forward-line 1)
                          (point))))))
          (when (and body (> body (point)))
            (goto-char body)))
        (progn (goto-char oldpoint)
               (unless notab (insert "\t"))))))

(defun tla-log-goto-field (field)
  "Go to FIELD in a log file."
  (goto-char (point-min))
  (re-search-forward field)
  (save-excursion
    (if (not (looking-at " "))
        (insert " ")))
  (forward-char 1))

(defun tla-log-goto-summary ()
  "Go to the Summary field in a log file."
  (interactive)
  (tla-log-goto-field "^Summary:"))

(defun tla-log-goto-keywords ()
  "Go to the Keywords field in a log file."
  (interactive)
  (tla-log-goto-field "^Keywords:"))

(defun tla-log-goto-body ()
  "Go to the Body in a log file."
  (interactive)
  (goto-char (point-min))
  (forward-line 3))

(defun tla-log-kill-body ()
  "Kill the content of the log file body."
  (interactive)
  (tla-log-goto-body)
  (kill-region (point) (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\+\\+log\\." . tla-log-edit-mode))

;;;###autoload
(define-derived-mode tla-log-edit-mode dvc-log-edit-mode "tla-log-edit"
  "Major Mode to edit xtla log messages.
Commands:
\\{tla-log-edit-mode-map}
"
  (use-local-map tla-log-edit-mode-map)
  (easy-menu-add tla-log-edit-mode-menu)
  (dvc-install-buffer-menu)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-log-edit-font-lock-keywords t))
  (setq fill-column 73)
  (run-hooks 'tla-log-edit-mode-hook))

(defun tla-log-edit-abort ()
  "Abort the current log edit."
  (interactive)
  (bury-buffer)
  (set-window-configuration tla-pre-commit-window-configuration))

(autoload 'dvc-tips-popup-maybe "dvc-tips")

(defun tla-log-edit-done ()
  "Finish the current log edit and commit."
  (interactive)
  (tla-log-edit-done-internal nil))

(defun tla-log-edit-done-with-sealing ()
  "Finish the current log edit and commit with sealing(--seal)."
  (interactive)
  (if (yes-or-no-p
       (format "Do you really want to seal: \"%s\" ?"
               (tla-tree-version)))
      (tla-log-edit-done-internal 'seal)
    (error "Abort to seal this version")))

(defun tla-log-edit-done-with-fixing ()
  "Finish the current log edit and commit with fixing(--fix)."
  (interactive)
  (if (yes-or-no-p
       (format "Do you really want to fix: \"%s\" ?"
               (tla-tree-version)))
      (tla-log-edit-done-internal 'fix)
    (error "Abort to fix this version")))

(defun tla-log-edit-done-internal (version-flag)
  "Finish the current log edit and commit.
`nil' or a symbol(`seal' or `fix') is acceptable as VERSION-FLAG."
  (tla-edit-log-delete-file-list)
  (save-buffer)
  (let ((dir default-directory)
        (log-buffer (current-buffer)))
    (dvc-tips-popup-maybe)
    (let ((default-directory dir))
      (tla-commit
       (dvc-capturing-lambda (output error status args)
         (kill-buffer (capture log-buffer)))
       version-flag))))

(defun tla-archive-maintainer-name (version)
  "Return the maintainer name for a given VERSION.
This function looks in the bookmarks file for the nickname field and
returns it.
If the nickname field is not present, just return VERSION as string."
  (tla-bookmarks-get-field version 'nickname (tla--name-mask version t t t t t)))

(defun tla-archive-maintainer-id (archive &optional shorter)
  "Return my-id substring from ARCHIVE.
If SHORTER is non-nil, return login name part of the my-id substring.
E.g. If ARCHIVE is x@y.z--a, the result is x@y.z.
If SHORTER is non-nil, the result is x."
  (if (string-match "\\(\\(.+\\)@.+\\)--.+" archive)
      (if shorter
          (match-string 2 archive)
        (match-string 1 archive))))

(defun tla-archive-default-maintainer-name (version)
  "Return a suitable maintainer name or version name for VERSION.
Either the nickname if defined in the bookmarks, or the left hand side
of the email in the archive name."
  (or (tla-archive-maintainer-name version)
      (tla-archive-maintainer-id (tla--name-archive version) t)))

(defun tla--merge-summary-end-of-sequence (string low high)
  "Pretty-print a range of merged patches.
STRING is an identifier for this merge, while LOW and HIGH are the lowest
and highest patches that were merged."
  (let ((elem
         (if (= low high)
             ;; singleton
             (int-to-string low)
           (format "%d-%d" low high))))
    (if (string= string "")
        (concat "patch " elem)
      (concat string ", " elem))))


(defun tla-merge-summary-line (mergelist)
  "Create a suitable log summary line for a list of merges.
MERGELIST is an alist in the form
\((maintainer1 12 13 14 25 26)
  ...
  (maintainerN num42))
The return value is a string in the form
\"maintainer1 (patch 12-14, 25-26), maintainerN (patch-num42)\""
  (let ((res ""))
    (while mergelist
      (let ((patch-list (sort (cdar mergelist) '<))
            (list-string "")
            last-patch-number-low
            last-patch-number-high)
        ;; patch-list is the list of patch numbers.
        (while patch-list
          (unless last-patch-number-low
            (setq last-patch-number-low (car patch-list))
            (setq last-patch-number-high (- (car patch-list) 1)))
          (if (= (1+ last-patch-number-high) (car patch-list))
              ;; normal sequence
              (setq last-patch-number-high (car patch-list))
            (setq list-string
                  (tla--merge-summary-end-of-sequence
                   list-string
                   last-patch-number-low
                   last-patch-number-high))
            (setq last-patch-number-low (car patch-list)))
          (setq last-patch-number-high (car patch-list))
          (setq patch-list (cdr patch-list)))
        (setq list-string
              (tla--merge-summary-end-of-sequence
               list-string
               last-patch-number-low
               last-patch-number-high))
        (setq last-patch-number-low nil)
        (setq res
              (let ((maint (format "%s (%s)" (caar mergelist)
                                   list-string)))
                (if (string= res "")
                    maint
                  (concat res ", " maint)))))
      (setq mergelist (cdr mergelist)))
    res))

(defun tla--merge-summary-default-format-function (string)
  "Return an appropriate \"Merged from\" summary line for STRING.

Gets the 'summary-format field for that version in the bookmarks (or
use \"Merged from %s\" by default), and calls
\(format summary-format S)."
  (let ((format-string (tla-bookmarks-get-field
                        (tla-tree-version-list)
                        'summary-format
                        "Merged from %s")))
    (format format-string string)))

(defun tla-merge-summary-line-for-log ()
  "Generate an appropriate summary line after a merge.
The generated line is of the form
\"Merged from Robert (167-168, 170), Masatake (209, 213-215, 217-218)\".
The names \"Robert\" and \"Masatake\" in this example are nicknames
defined in the bookmarks for the corresponding versions.

First, an alist A like
\((\"Robert\" 167 168 170) (\"Masatake\" 209 213 214 215 217 218)) is
generated. If `tla-version-to-name-function' is non-nil, then it must
be a function that is called with the version as an argument, and must
return a string that will be used to instead of the nickname.

Then, a string S like
\"Robert (167-168, 170), Masatake (209, 213-215, 217-218)\"
is generated. This is done by default by `tla-merge-summary-line',
which can be overridden by `tla-generate-line-function'.

Then, the function `tla-format-line-function' is called with this
string S as an argument. If `tla-format-line-function' is nil, then,
`tla--merge-summary-default-format-function' is called. It retrieves
the fields summary-format from the bookmark for the tree version, and
calls (format summary-format S)."
  (save-excursion
    (let ((rev-list)
          (maintainer)
          (rev)
          (patch-list))
      (goto-char (point-min))
      (while (re-search-forward "^ \\* \\(.+@.+--.+/.+--.+\\)$" nil t)
        (setq rev-list (tla--name-split (match-string-no-properties 1)))
        (setq maintainer (funcall (or tla-version-to-name-function
                                      'tla-archive-default-maintainer-name)
                                  rev-list))
        (setq rev (cadr (split-string (tla--name-revision rev-list) "-")))
        (add-to-list 'patch-list (list maintainer rev)))
      ;; patch-list has now the form
      ;; ((maintainer1 num1) (maintainer1 num2) ... (maintainerN num42))
      (let ((alist))
        (while patch-list
          (let* ((elem (car patch-list))
                 (patch-number-list (assoc (car elem) alist)))
            (if patch-number-list
                ;; This maintainer already has a patch in the list
                (setcdr patch-number-list
                        (cons (string-to-number (cadr elem))
                              (cdr patch-number-list)))
              ;; First patch for this maintainer. add
              ;; (maintainer patch-number) to the alist.
              (setq alist (cons (list (car elem)
                                      (string-to-number (cadr elem)))
                                alist))))
          (setq patch-list (cdr patch-list)))
        ;; alist now has the form
        ;; ((maintainer1 num1 num2)
        ;;  ...
        ;;  (maintainerN num42))
        ;; where numX are of type integer.
        (funcall (or tla-format-line-function
                     'tla--merge-summary-default-format-function)
                 (funcall (or tla-generate-line-function
                              'tla-merge-summary-line) alist))))))

(defun tla-log-edit-insert-log-for-merge-and-headers ()
  "Call `tla-log-edit-insert-log-for-merge' with a prefix arg."
  (interactive)
  (tla-log-edit-insert-log-for-merge t))

(defun tla-log-edit-insert-log-for-merge (arg)
  "Insert the output of tla log-for-merge at POINT.

When called with a prefix argument ARG, create a standard Merged from
line as Summary with `tla-merge-summary-line-for-log'."
  (interactive "P")
  (let ((cur-buf (current-buffer)))
    (tla--run-tla-sync '("log-for-merge")
                       :finished
                       (dvc-capturing-lambda (output error status arguments)
                         (let ((content (dvc-buffer-content
                                         output)))
                           (if (= 0 (length content))
                               (error "There was no merge!"))
                           (with-current-buffer (capture cur-buf)
                             (let ((on-summary-line
                                    (= 1 (count-lines (point-min) (point))))
                                   (old-pos (point)))
                               (if on-summary-line
                                   (tla-log-goto-body)
                                 (goto-char old-pos))
                               (insert content)))
                           (when arg
                             (tla-log-goto-summary)
                             (delete-region (point) (line-end-position))
                             (insert
                              (with-current-buffer output
                                (tla-merge-summary-line-for-log)))
                             (tla-log-goto-keywords)
                             (delete-region (point) (line-end-position))
                             (insert "merge")
                             (tla-log-goto-summary)))))))


(defun tla-log-edit-insert-memorized-log ()
  "Insert a memorized log message."
  (interactive)
  (when dvc-memorized-log-header
    (tla-log-goto-summary)
    (delete-region (point) (line-end-position))
    (insert dvc-memorized-log-header))
  (when dvc-memorized-log-message
    (tla-log-goto-body)
    (when dvc-memorized-patch-sender
      (if (looking-at "Patch from ")
          (forward-line 1)
        (progn
          (undo-boundary)
          (insert (format "Patch from %s\n" dvc-memorized-patch-sender)))))
    (when (looking-at "\* .+: ") ;; e.g.: "* lisp/xtla.el: "
      (end-of-line)
      (newline))
    (insert dvc-memorized-log-message)))


;; ----------------------------------------------------------------------------
;; tla-log-edit-insert-keywords:
;; ----------------------------------------------------------------------------

(defvar tla-log-edit-keywords-marked-list)
(defvar tla-log-edit-keywords-cookie)
(defvar tla-log-edit-keywords-log-buffer)

(defun tla-log-edit-keywords-printer (elem)
  "If ELEM is a keyword, print it differently."
  (insert (if (member elem tla-log-edit-keywords-marked-list)
              (concat dvc-mark " ") "  ")
          elem))

(defun tla-log-edit-keywords (arg)
  "Add keywords listed in variable `tla-log-edit-keywords'.
When called with a prefix argument ARG, delete all current keywords."
  (interactive "P")
  (let ((current-keywords
         (save-excursion
           (tla-log-goto-keywords)
           (buffer-substring (point) (line-end-position))))
        (log-buffer (current-buffer))
        keywords)
    (setq current-keywords (replace-regexp-in-string "," " " current-keywords nil t)
          current-keywords (mapcar (lambda (k) (format "%s" k))
                                   (read (concat "(" current-keywords ")"))))
    (switch-to-buffer (concat " *" (tla-arch-branch-name) "log-keywords*"))
    (toggle-read-only 0)
    (erase-buffer)
    (make-local-variable 'tla-log-edit-keywords)
    (make-local-variable 'tla-log-edit-keywords-marked-list)
    (make-local-variable 'tla-log-edit-keywords-cookie)
    (make-local-variable 'tla-log-edit-keywords-log-buffer)
    (setq tla-log-edit-keywords-log-buffer
          log-buffer
          tla-log-edit-keywords-marked-list
          current-keywords
          tla-log-edit-keywords-cookie
          (ewoc-create (dvc-ewoc-create-api-select
                        #'tla-log-edit-keywords-printer)
                       "List of keywords from `tla-log-edit-keywords':\n"
                       (format "type C-c C-c to insert the marked keywords to the buffer\n%s"
                               (buffer-name log-buffer))))

    (while current-keywords
      (add-to-list 'tla-log-edit-keywords (car current-keywords))
      (setq current-keywords (cdr current-keywords)))

    (setq keywords tla-log-edit-keywords)

    (while keywords
      (add-to-list 'tla-log-edit-keywords (car keywords))
      (ewoc-enter-last tla-log-edit-keywords-cookie (car keywords))
      (setq keywords (cdr keywords))))

  (use-local-map tla-log-edit-keywords-mode-map)
  (setq major-mode 'tla-log-edit-keywords-mode)
  (setq mode-name "tla-log-keywords")
  (toggle-read-only 1)
  (message "Type C-c C-c to finish.")
  (goto-char (point-min))
  (forward-line 1))

(defun tla-log-edit-keywords-cursor-goto (elem)
  "Jump to the location of ELEM."
  (interactive)
  (goto-char (ewoc-location elem))
  (re-search-forward "^"))

(defun tla-log-edit-keywords-next ()
  "Go to the next keyword."
  (interactive)
  (let* ((cookie tla-log-edit-keywords-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla-log-edit-keywords-cursor-goto next)))

(defun tla-log-edit-keywords-previous ()
  "Go to the previous keyword."
  (interactive)
  (let* ((cookie tla-log-edit-keywords-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla-log-edit-keywords-cursor-goto previous)))

(defun tla-log-edit-keywords-mark ()
  "Mark the current keyword."
  (interactive)
  (let ((pos (point)))
    (add-to-list 'tla-log-edit-keywords-marked-list
                 (ewoc-data (ewoc-locate tla-log-edit-keywords-cookie)))
    (ewoc-refresh tla-log-edit-keywords-cookie)
    (goto-char pos))
  (tla-log-edit-keywords-next))

(defun tla-log-edit-keywords-unmark ()
  "Unmark the current keyword."
  (interactive)
  (let ((pos (point)))
    (setq tla-log-edit-keywords-marked-list
          (delete (ewoc-data (ewoc-locate tla-log-edit-keywords-cookie))
                  tla-log-edit-keywords-marked-list))
    (ewoc-refresh tla-log-edit-keywords-cookie)
    (goto-char pos))
  (tla-log-edit-keywords-next))

(defun tla-log-edit-keywords-unmark-all ()
  "Unmark all marked keywords."
  (interactive)
  (let ((pos (point)))
    (setq tla-log-edit-keywords-marked-list nil)
    (ewoc-refresh tla-log-edit-keywords-cookie)
    (goto-char pos)))

(defun tla-log-edit-keywords-mark-all ()
  "Mark all keywords."
  (interactive)
  (let ((pos (point)))
    (setq tla-log-edit-keywords-marked-list tla-log-edit-keywords)
    (ewoc-refresh tla-log-edit-keywords-cookie)
    (goto-char pos)))

(defun tla-log-edit-keywords-toggle-mark ()
  "Toggle marking of the current keyword."
  (interactive)
  (let ((pos (point)))
    (if (member (ewoc-data (ewoc-locate tla-log-edit-keywords-cookie))
                tla-log-edit-keywords-marked-list)
        (tla-log-edit-keywords-unmark)
      (tla-log-edit-keywords-mark))
    (ewoc-refresh tla-log-edit-keywords-cookie)
    (goto-char pos)))

(defun tla-log-edit-keywords-insert ()
  "Insert marked keywords into log buffer."
  (interactive)
  (let ((keywords tla-log-edit-keywords-marked-list))
    (switch-to-buffer tla-log-edit-keywords-log-buffer)
    (kill-buffer (concat " *" (tla-arch-branch-name) "log-keywords*"))
    (save-excursion
      (tla-log-goto-keywords)
      (delete-region (point) (line-end-position))
      (insert (mapconcat 'identity (reverse keywords) ", ")))))

;; ----------------------------------------------------------------------------
;; tla-archive-list-mode:
;; ----------------------------------------------------------------------------
(defun tla-archive-mirror-archive ()
  "Mirror the archive at point."
  (interactive)
  (let ((archive-info (tla-get-archive-info)))
    (when archive-info
      (tla-mirror-archive archive-info)
      (tla-archives))))

(defun tla-archive-synchronize-archive ()
  "Synchronizes the mirror for the archive at point."
  (interactive)
  (let ((archive-info (tla-get-archive-info)))
    (when archive-info
      (tla-archive-mirror archive-info))))

(defun tla-archive-list-mode ()
  "Major Mode to show arch archives:
\\{tla-archive-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-archive-list-mode-map)
  (easy-menu-add tla-archive-list-mode-menu)
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-archive-list-mode)
  (setq mode-name "tla-archives")

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'tla--get-archive-info-at-point)
  (run-hooks 'tla-archive-list-mode-hook))

(defun tla--get-archive-info-at-point ()
  "Get archive information."
  (list 'archive (tla-get-archive-info)))

(defun tla-archive-select-default ()
  "Select the default archive."
  (interactive)
  (when (tla-get-archive-info)
    (let ((pos (point)))
      (tla-my-default-archive (tla-get-archive-info))
      (tla-archives)
      (goto-char pos))))

(defun tla-archive-unregister-archive ()
  "Delete the registration of the selected archive."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (progn (tla--unregister-archive archive t)
               (tla-archives))
      (error "No archive under the point"))))

(defun tla-archive-edit-archive-location ()
  "Edit the archive location for a archive.
This is done by unregistering the archive, followed by a new registration with
the new location."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (tla--edit-archive-location archive)
    (save-excursion
      (tla-archives))))

(defun tla-archive-use-as-default-mirror ()
  "Use the mirror archive as default mirror."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (tla-use-as-default-mirror archive)
    (save-excursion
      (tla-archives))))

(defun tla-archive-list-categories ()
  "List the categories for the current archive."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (tla-categories archive)
      (error "No archive under the point"))))

(dvc-make-bymouse-function tla-archive-list-categories)

(defun tla-archive-browse-archive ()
  "Browse the current archive."
  (interactive)
  (let ((archive (tla-get-archive-info)))
    (if archive
        (tla-browse-archive archive)
      (error "No archive under the point"))))

(dvc-make-move-fn ewoc-next tla-archive-next
  tla-archives-list-cookie)

(dvc-make-move-fn ewoc-prev tla-archive-previous
  tla-archives-list-cookie)

(defun tla-save-archive-to-kill-ring ()
  "Save the name of the current archive to the kill ring."
  (interactive)
  (let ((archive (or (tla-get-archive-info)
                     tla-buffer-archive-name
                     (tla--name-archive (tla-tree-version-list nil 'no-error)))))
    (unless archive
      (error "No archive name associated with current buffer"))
    (kill-new archive)
    (if (interactive-p)
        (message "%s" archive))
    archive))

(defun tla-save-version-to-kill-ring ()
  "Save tla tree-version to the kill ring."
  (interactive)
  (let ((version (tla-tree-version)))
    (kill-new version)
    (if (interactive-p)
        (message "%s" version))
    version))

(defun tla-save-revision-to-kill-ring ()
  "Save tla tree-version to the kill ring."
  (interactive)
  (let ((revision (tla-tree-id)))
    (kill-new revision)
    (if (interactive-p)
        (message "%s" revision))
    revision))

;; ----------------------------------------------------------------------------
;; tla-category-list-mode:
;; ----------------------------------------------------------------------------
(defun tla-category-list-mode ()
  "Major Mode to show arch categories:
\\{tla-category-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-category-list-mode-map)
  (easy-menu-add tla-category-list-mode-menu)
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-category-list-mode)
  (setq mode-name "tla-category")
  (add-hook 'tla-make-category-hook 'tla-category-refresh)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'tla--get-category-info-at-point)
  (run-hooks 'tla-category-list-mode-hook))

(defun tla--get-category-info-at-point ()
  "Get archive/category--branch information."
  (let ((buffer-version (tla--name-construct
                         tla-buffer-archive-name
                         (tla-get-archive-info 'tla-category-info))))
    (list 'category buffer-version)))

(defun tla-category-list-branches ()
  "List branches of the current category."
  (interactive)
  (let ((category (tla-get-archive-info 'tla-category-info)))
    (if category
        (tla-branches tla-buffer-archive-name category)
      (error "No category under the point"))))

(dvc-make-bymouse-function tla-category-list-branches)

(defun tla-category-make-category (category)
  "Create a new category named CATEGORY."
  (interactive "sCategory name: ")
  (tla-make-category tla-buffer-archive-name category))

(defun tla-category-refresh ()
  "Refresh the current category list."
  (interactive)
  (tla-categories tla-buffer-archive-name))

(defun tla-category-next ()
  "Move to the next category."
  (interactive)
  (forward-line 1)
  (beginning-of-line))

(defun tla-category-previous ()
  "Move to the previous category."
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (unless (looking-at "^   ")
    (forward-line 1)))

(defun tla-category-mirror-archive ()
  "Mirror the current category."
  (interactive)
  (let ((category (tla-get-archive-info 'tla-category-info)))
    (unless category
      (error "No category at point"))
    (tla-archive-mirror tla-buffer-archive-name
                        category)))


(defun tla-category-bookmarks-add-here (name)
  "Add a bookmark named NAME for this category."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name
                           (tla-get-archive-info 'tla-category-info)
                           nil nil nil))
  (message "bookmark %s added." name))

(defun tla-category-bookmarks-add (name)
  "Add a bookmark named NAME for this category."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name nil nil nil))
  (message "bookmark %s added." name))

;; ----------------------------------------------------------------------------
;; tla-branch-list-mode
;; ----------------------------------------------------------------------------
(defun tla-branch-list-mode ()
  "Major Mode to show arch branches:
\\{tla-branch-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-branch-list-mode-map)
  (easy-menu-add tla-branch-list-mode-menu)
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-branch-list-mode)
  (setq mode-name "tla-branch")
  (add-hook 'tla-make-branch-hook 'tla-branch-refresh)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'tla--get-branch-info-at-point)
  (run-hooks 'tla-branch-list-mode-hook))

(defun tla--get-branch-info-at-point ()
  "Get archive/category--branch--version information."
  (let ((buffer-version (tla--name-construct
                         tla-buffer-archive-name
                         tla-buffer-category-name
                         (tla-get-archive-info 'tla-branch-info))))
    (list 'branch buffer-version)))

(defun tla-branch-make-branch (branch)
  "Create a new branch named BRANCH."
  (interactive "sBranch name: ")
  (tla-make-branch tla-buffer-archive-name
                   tla-buffer-category-name
                   branch))

(defun tla-branch-refresh ()
  "Refresh the current branch list."
  (interactive)
  (tla-branches
   tla-buffer-archive-name
   tla-buffer-category-name))

(defun tla-branch-list-parent-category ()
  "List the parent category of the current branch."
  (interactive)
  (tla-categories tla-buffer-archive-name))

(defun tla-branch-list-versions ()
  "List the versions of the current branch."
  (interactive)
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (if branch
        (tla-versions tla-buffer-archive-name
                      tla-buffer-category-name
                      branch)
      (error "No branch under the point"))))

(dvc-make-bymouse-function tla-branch-list-versions)

(defun tla-branch-mirror-archive ()
  "Mirror the current branch."
  (interactive)
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (unless branch
      (error "No branch under the point"))
    (tla-archive-mirror tla-buffer-archive-name
                        tla-buffer-category-name
                        branch)))

(defun tla-branch-get-branch (directory)
  "Get the current branch and place it in DIRECTORY."
  (interactive (list (expand-file-name
                      (dvc-read-directory-name
                       (format "Restore \"%s\" to: "
                               (let ((branch
                                      (tla-get-archive-info 'tla-branch-info)))
                                 (unless branch
                                   (error "No branch under the point"))
                                 (tla--name-construct
                                  tla-buffer-archive-name
                                  tla-buffer-category-name
                                  branch)))))))
  (let ((branch (tla-get-archive-info 'tla-branch-info)))
    (if branch
        (tla-get directory
                 t
                 tla-buffer-archive-name
                 tla-buffer-category-name
                 branch)
      (error "No branch under the point"))))

(defun tla-branch-bookmarks-add-here (name)
  "Add a bookmark named NAME for the current branch."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name
                           tla-buffer-category-name
                           (tla-get-archive-info 'tla-branch-info)
                           nil nil))
  (message "bookmark %s added." name))

(defun tla-branch-bookmarks-add (name)
  "Add a bookmark named NAME for the current branch."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name
                           tla-buffer-category-name
                           nil nil nil))
  (message "bookmark %s added." name))




;; ----------------------------------------------------------------------------
;; tla-version-list-mode
;; ----------------------------------------------------------------------------
(defun tla-version-list-mode ()
  "Major Mode to show arch versions:
\\{tla-version-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tla-version-list-mode-map)
  (easy-menu-add tla-version-list-mode-menu)
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-version-list-mode)
  (setq mode-name "tla-version")
  (add-hook 'tla-make-version-hook 'tla-version-refresh)

  (toggle-read-only 1)
  (set-buffer-modified-p nil)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'tla--get-version-info-at-point)
  (run-hooks 'tla-version-list-mode-hook))

(defun tla--get-version-info-at-point ()
  "Get archive/category--branch--version--revision information."
  (let ((buffer-version (tla--name-construct
                         tla-buffer-archive-name
                         tla-buffer-category-name
                         tla-buffer-branch-name
                         (tla-get-archive-info 'tla-version-info))))
    (list 'version buffer-version)))

(defun tla-version-refresh ()
  "Refresh the current version list."
  (interactive)
  (tla-versions
   tla-buffer-archive-name
   tla-buffer-category-name
   tla-buffer-branch-name))

(defun tla-version-list-parent-branch ()
  "List the parent branch of this version."
  (interactive)
  (tla-branches tla-buffer-archive-name
                tla-buffer-category-name))

(defun tla-version-list-revisions ()
  "List the revisions of this version."
  (interactive)
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-revisions tla-buffer-archive-name
                       tla-buffer-category-name
                       tla-buffer-branch-name
                       version)
      (error "No version under the point"))))

(dvc-make-bymouse-function tla-version-list-revisions)

(defun tla-version-make-version (version)
  "Create a new version named VERSION."
  (interactive "sVersion name: ")
  (tla-make-version tla-buffer-archive-name
                    tla-buffer-category-name
                    tla-buffer-branch-name
                    version))

(defun tla-version-bookmarks-add-here (name)
  "Add a bookmark named NAME for the current version."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name
                           tla-buffer-category-name
                           tla-buffer-branch-name
                           (tla-get-archive-info 'tla-version-info)
                           nil))
  (message "bookmark %s added." name))

(defun tla-version-bookmarks-add (name)
  "Add a bookmark named NAME for the current version."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (list tla-buffer-archive-name
                           tla-buffer-category-name
                           tla-buffer-branch-name
                           nil nil))
  (message "bookmark %s added." name))

(defun tla-version-save-version-to-kill-ring ()
  "Save the version to the kill-ring."
  (interactive)
  (let ((version (cadr (tla--get-version-info-at-point))))
    (kill-new version)
    (if (interactive-p)
        (message "%s" version))
    version))

(defun tla-version-get-version (directory)
  "Get a version and place it in DIRECTORY."
  (interactive (list (expand-file-name
                      (dvc-read-directory-name
                       (format "Restore \"%s\" to: "
                               (let ((version
                                      (tla-get-archive-info 'tla-version-info)))
                                 (unless version
                                   (error "No version under the point"))
                                 (tla--name-construct
                                  tla-buffer-archive-name
                                  tla-buffer-category-name
                                  tla-buffer-branch-name
                                  version)))))))
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-get directory
                 t
                 tla-buffer-archive-name
                 tla-buffer-category-name
                 tla-buffer-branch-name
                 version)
      (error "No version under the point"))))


(defun tla-version-mirror-archive ()
  "Mirror the current version."
  (interactive)
  (let ((version (tla-get-archive-info 'tla-version-info)))
    (if version
        (tla-archive-mirror tla-buffer-archive-name
                            tla-buffer-category-name
                            tla-buffer-branch-name
                            version))))

(defun tla-version-tag (to-archive to-category to-branch to-version)
  "Run tla tag from the current location in version buffer.
The tag is created in TO-ARCHIVE/TO-CATEGORY--TO-BRANCH--TO-VERSION."
  (interactive
   (let ((l (tla-name-read "Tag to: " 'prompt 'prompt 'prompt 'prompt)))
     (list
      (tla--name-archive l)
      (tla--name-category l)
      (tla--name-branch l)
      (tla--name-version l))))
  (let ((to-fq (tla--name-construct to-archive
                                    to-category
                                    to-branch
                                    to-version))
        from-fq
        (from-version (tla-get-archive-info 'tla-version-info)))
    (unless from-version
      (error "No version under the point"))
    (setq from-fq (tla--name-construct
                   tla-buffer-archive-name
                   tla-buffer-category-name
                   tla-buffer-branch-name
                   from-version))
    (tla--version-tag-internal from-fq to-fq)))


(defun tla--version-tag-internal (from-fq to-fq &optional synchronously)
  "Create a tag from FROM-FQ to TO-FQ.
If SYNCHRONOUSLY is non-nil, internal `tla-tag' and `tla-get' runs synchronously.
Else it runs asynchronously."
  ;; `baz-branch' supports operations on both local working tree and
  ;; archive side. If the first argument, from-fq of baz-branch not given,
  ;; The operation on local working tree is taken. Here what we want is
  ;; archive side operation. So check from-fq heavily.
  (cond
   ((null from-fq) (error "from-fq is not specified"))
   ((not (stringp from-fq)) (error "from-fq is not string"))
   ((string= from-fq "") (error "from-fq is an empty string")))
  (when (yes-or-no-p (format "Create a tag from `%s' to `%s'? " from-fq to-fq))
    (unless (funcall (if (tla-has-branch-command)
                         'baz-branch
                       'tla-tag)
                     from-fq to-fq (tla--tag-does-cacherev) synchronously)
      (error "Fail to create a tag"))
    (when (y-or-n-p "Tag created.  Get a copy of this revision? ")
      (let* ((prompt "Get a copy in: ")
             dir parent
             to-fq-split)
        (while (not dir)
          (setq dir (dvc-read-directory-name prompt dir)
                parent (expand-file-name
                        (concat (file-name-as-directory dir) "..")))
          (cond
           ;; Parent directoy must be.
           ((not (file-directory-p parent))
            (message "`%s' is not directory" parent)
            (sit-for 2)
            (setq dir nil))
           ;; dir itself must not be.
           ((file-exists-p dir)
            (message "`%s' exists already" dir)
            (sit-for 2)
            (setq dir nil))))
        (setq to-fq-split (tla--name-split to-fq))
        (tla-get dir 'ask
                 (nth 0 to-fq-split)
                 (nth 1 to-fq-split)
                 (nth 2 to-fq-split)
                 (nth 3 to-fq-split)
                 (nth 4 to-fq-split)
                 synchronously)))))

;; ----------------------------------------------------------------------------
;; tla-revision-list-mode
;; ----------------------------------------------------------------------------
(require 'dvc-revlist)
(define-derived-mode tla-revision-list-mode dvc-revlist-mode
  "tla-revisions"
  "Major mode to show Arch revision lists:
\\{tla-revision-list-mode-map}."
  (use-local-map tla-revision-list-mode-map)
  (set (make-local-variable 'dvc-get-revision-info-at-point-function)
       'tla--revision-get-revision-at-point))

(defun tla--revision-get-revision-at-point ()
  "Get archive/category--branch--version--revision--patch information.
Returns nil if not on a revision list, or not on a revision entry in a
revision list."
  (let ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie))))
    (when (eq (car elem) 'entry-patch)
      (let ((full (tla--revision-revision
                   (dvc-revlist-entry-patch-struct (nth 1 elem)))))
        (tla--name-construct full)))))

(defun tla--revision-get-version-info-at-point ()
  "Get archive/category--branch--version--revision information.
Returns nil if not on a revision list, or not on a revision entry in a
revision list."
  (list 'version
        (tla--name-mask (tla--name-split
                         (tla--revision-get-revision-at-point)) t
                         t t t t)))

(defun tla-revision-save-revision-to-kill-ring ()
  "Save the name of the current revision to the kill ring."
  (interactive)
  (let ((rev (tla--revision-get-revision-at-point)))
    (unless rev
      (error "No revision at point"))
    (kill-new rev)
    (if (interactive-p)
        (message "%s" rev))
    rev))

(defun tla-revision-save-version-to-kill-ring ()
  "Save the name of the current version to the kill ring."
  (interactive)
  (let ((rev (cadr (tla--revision-get-version-info-at-point))))
    (unless rev
      (error "No version at point"))
    (kill-new rev)
    (if (interactive-p)
        (message "%s" rev))
    rev))

(defun tla-revision-refresh ()
  "Refresh the current list of revisions."
  (interactive)
  (tla-revisions
   tla-buffer-archive-name
   tla-buffer-category-name
   tla-buffer-branch-name
   tla-buffer-version-name))

(defun tla-revision-list-parent-version ()
  "List the versions of the parent of this revision."
  (interactive)
  (tla-versions tla-buffer-archive-name
                tla-buffer-category-name
                tla-buffer-branch-name))

(defun tla-revision-get-revision (directory archive category branch
                                            version revision)
  "Get a revision and place it in DIRECTORY.
The revision is named by ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION."
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie)))
          (full (tla--revision-revision
                 (dvc-revlist-entry-patch-struct (nth 1 elem))))
          (revision (tla--name-revision full))
          (archive (tla--name-archive full))
          (category (tla--name-category full))
          (branch (tla--name-branch full))
          (version (tla--name-version full))
          dir)
     (unless revision
       (error "No revision under the point"))
     (setq dir (expand-file-name
                (dvc-read-directory-name
                 (format "Restore \"%s\" to: "
                         (tla--name-construct
                          archive category branch version revision)))))
     (if (file-exists-p dir)
         (error "Directory %s already exists" dir))
     (list dir archive category branch version revision)))
  (if revision
      (tla-get directory t archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-cache-revision (archive category branch version revision)
  "Create a cached revision for the revision at point."
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie)))
          (full (tla--revision-revision (car (cddr elem))))
          (archive (tla--name-archive full))
          (category (tla--name-category full))
          (branch (tla--name-branch full))
          (version (tla--name-version full))
          (revision (tla--name-revision full)))
     (unless revision
       (error "No revision under the point"))
     (list archive category branch version revision)))
  (if revision
      (tla-cache-revision archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-add-to-library (archive category branch version revision)
  "Add the revision at point to library."
  (interactive
   (let* ((elem (ewoc-data (ewoc-locate dvc-revlist-cookie)))
          (full (tla--revision-revision (car (cddr elem))))
          (archive (tla--name-archive full))
          (category (tla--name-category full))
          (branch (tla--name-branch full))
          (version (tla--name-version full))
          (revision (tla--name-revision full)))
     (unless revision
       (error "No revision under the point"))
     (list archive category branch version revision)))
  (if revision
      (tla-library-add archive category branch version revision)
    (error "No revision under the point")))

(defun tla-revision-refresh-maybe ()
  "Refresh the revision list if new information is available.
If the current ewoc doesn't contain creator, date, and summary, and
if these values should now be displayed, run the refresh function."
  (when (or dvc-revisions-shows-date
            dvc-revisions-shows-creator
            dvc-revisions-shows-summary
            tla-revisions-shows-merges
            tla-revisions-shows-merged-by)
    (let ((stop nil)
          (ewoc-elem (ewoc-nth dvc-revlist-cookie 0)))
      (while (and ewoc-elem (not stop))
        (let ((elem (ewoc-data ewoc-elem)))
          (if (eq (car elem) 'entry-patch)
              (setq stop t)
            (setq ewoc-elem (ewoc-next dvc-revlist-cookie
                                       ewoc-elem)))))
      (when (and ewoc-elem
                 (null (tla--revision-summary
                        (dvc-revlist-entry-patch-struct (nth 1 (ewoc-data ewoc-elem))))))
        (dvc-generic-refresh)))))

(defun tla-revision-toggle-library ()
  "Toggle display of the revision library in the revision list."
  (interactive)
  (setq tla-revisions-shows-library (not tla-revisions-shows-library))
  (ewoc-refresh dvc-revlist-cookie))

(defun tla-revision-toggle-merges ()
  "Toggle display of the merges in the revision list."
  (interactive)
  (setq tla-revisions-shows-merges (not tla-revisions-shows-merges))
  (tla-revision-refresh-maybe)
  (ewoc-refresh dvc-revlist-cookie))

(defun tla-revision-toggle-merged-by ()
  "Toggle display of merged-by in the revision list."
  (interactive)
  (setq tla-revisions-shows-merged-by
        (not tla-revisions-shows-merged-by))
  (when (and (not tla-revision-merge-by-computed)
             tla-revisions-shows-merged-by)
    (tla-revision-refresh-maybe)
    (tla-revision-compute-merged-by))
  (ewoc-refresh dvc-revlist-cookie))

(defun tla-revision-scroll-or-show-changeset (up-or-down)
  "If file-diff buffer is visible, scroll. Otherwise, show it."
  (interactive)
  (let* ((cookie dvc-revlist-cookie)
         (full (tla--revision-revision
                (caddr (ewoc-data (ewoc-locate cookie)))))
         (revision (tla--name-construct full)))
    (unless revision
      (error "No revision info at point."))
    (let ((buffer (dvc-get-buffer
                   tla-arch-branch 'changeset revision)))
      (dvc-trace "buffer=%S revision=%S tla-arch-branch=%S" buffer
                 revision tla-arch-branch)
      (unless (dvc-scroll-maybe buffer up-or-down)
        (tla-revision-changeset)))))

(defun tla-revision-scroll-up-or-show-changeset ()
  (interactive)
  (tla-revision-scroll-or-show-changeset 'scroll-up))

(defun tla-revision-scroll-down-or-show-changeset ()
  (interactive)
  (tla-revision-scroll-or-show-changeset 'scroll-down))

;;TODO: remove tla-revision-changeset if it is really no longer needed...
(defun tla-revision-changeset (&optional arg)
  "Gets and display the changeset at point in a revision list buffer.
If used with a prefix arg ARG, don't include the diffs from the output."
  (interactive "P")
  (error "tla-revision-changeset should be handled by DVC now...")
  (let* ((window-conf (current-window-configuration))
         (cur-buf (current-buffer))
         (cookie dvc-revlist-cookie)
         (full (tla--revision-revision
                (dvc-revlist-entry-patch-struct
                 (nth 1 (ewoc-data (ewoc-locate cookie))))))
         (revision (tla--name-construct full)))
    (tla-get-changeset revision t nil arg)

    (setq dvc-partner-buffer
          (dvc-get-buffer-create tla-arch-branch
                                 'changeset revision))
    (dvc-trace "before with. dvc-partner-buffer=%S" dvc-partner-buffer)
    (with-current-buffer dvc-partner-buffer
      (setq dvc-partner-buffer cur-buf))

    (when (or (dvc-do-in-xemacs (setq window-conf t)
                window-conf ;; we use window-conf only to get rid of warnings
                )
              (dvc-do-in-gnu-emacs (compare-window-configurations
                                    (current-window-configuration) window-conf)))
      (dvc-scroll-maybe dvc-partner-buffer 'scroll-up))))

(defun tla-revision-store-delta (across-versions)
  "Store a delta between two marked revisions.
If prefix argument ACROSS-VERSIONS is given, read revision details from the
user."
  (interactive "P")
  (tla-revision-delta across-versions t))

(defun tla-revision-delta (across-versions &optional stored-to-directory)
  "Run tla delta from marked revision to revision at point.
If prefix-argument ACROSS-VERSIONS is nil, read a revision
in the current version.  If ACROSS-VERSIONS is non-nil, read an archive,
a category, a branch, a version, and a revision to specify the revision.
If STORED-TO-DIRECTORY is nil, ask the user whether the changeset is stored
to or not.  If STORED-TO-DIRECTORY is non-nil, don't ask the use and the
changeset is stored."
  (interactive "P")
  (let* ((modified
          (tla--revision-revision
           (car (cddr (ewoc-data (ewoc-locate dvc-revlist-cookie))))))
         (modified-fq (tla--name-construct modified))
         (base
          (let ((marked (dvc-revision-marked-revisions)))
            (when (< 1 (length marked))
              (error "Delta can be run against one marked revision as the base revision"))
            (cond ((and marked (null (cdr marked)))
                   ;; use the marked revision
                   ;; (dvc-revision-unmark-all)
                   (tla--revision-revision (car marked)))
                  (t
                   (tla-name-read
                    (format "Revision for delta to %s from: "
                            (if across-versions
                                modified-fq
                              (tla--name-revision modified)))
                    (if across-versions 'prompt (tla--name-archive modified))
                    (if across-versions 'prompt (tla--name-category modified))
                    (if across-versions 'prompt (tla--name-branch modified))
                    (if across-versions 'prompt (tla--name-version modified))
                    'maybe))))))

    (unless (tla--name-archive base)
      (error "Archive for the base is not specified"))
    (unless (tla--name-category base)
      (error "Cateogory for the base is not specified"))
    (unless (tla--name-branch base)
      (error "Branch for the base is not specified"))
    (unless (tla--name-version base)
      (error "Version for the base is not specified"))
    (unless (tla--name-revision base)
      ;; No revision for modified is specified.
      ;; Use HEAD revision.
      (setcar (nthcdr 4 base)
              (tla--version-head
               (tla--name-archive base)
               (tla--name-category base)
               (tla--name-branch base)
               (tla--name-version base))))

    (when (or stored-to-directory
              (and (not stored-to-directory)
                   (y-or-n-p "Store the delta to a directory? ")))
      (setq stored-to-directory 'ask))

    (tla-delta (tla--name-construct base)
               modified-fq
               stored-to-directory)))

(defun tla-revision-bookmarks-add (name)
  "Add a bookmark named NAME for the current revision."
  (interactive "sBookmark name: ")
  (tla-bookmarks-add name
                     (tla--revision-revision
                      (car (cddr (ewoc-data (ewoc-locate dvc-revlist-cookie))))))
  (message "bookmark %s added." name))

(defun tla-revision-sync-tree (arg)
  "Unify a tree's patch log with the current revision.
With prefix argument ARG, use the latest version instead."
  (interactive "P")
  (let* ((last-inventory (tla--last-visited-inventory-buffer))
         (local-tree (or (if last-inventory
                             (with-current-buffer last-inventory
                               default-directory)
                           default-directory)))
         (current (ewoc-locate dvc-revlist-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (car (cddr (ewoc-data current)))
                              'bookmark))))
      (setq current (ewoc-prev dvc-revlist-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (car (cddr (ewoc-data current))) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((to-tree (dvc-read-directory-name "Sync with tree: " local-tree)))
      (let* ((elem (ewoc-data (ewoc-locate
                               dvc-revlist-cookie)))
             (full (tla--revision-revision
                    (dvc-revlist-entry-patch-struct (nth 1 elem)))))
        (tla-sync-tree (tla--name-construct
                        (if arg (butlast full) full))
                       to-tree)))))

(defun tla-revision-star-merge-version ()
  "Run star-merge for the version at point."
  (interactive)
  (tla-revision-star-merge t))

(defun tla-revision-star-merge (arg)
  "Run star-merge from the revision at point.
With prefix argument ARG, merge all missing revisions from this version."
  (interactive "P")
  (let* ((last-inventory (tla--last-visited-inventory-buffer))
         (local-tree (or (if last-inventory
                             (with-current-buffer last-inventory
                               default-directory)
                           default-directory)))
         (current (ewoc-locate dvc-revlist-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (car (cddr (ewoc-data current)))
                              'bookmark))))
      (setq current (ewoc-prev dvc-revlist-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (car (cddr (ewoc-data current))) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((to-tree (dvc-read-directory-name "Merge to tree: "
                                            local-tree local-tree t)))
      (let* ((elem (ewoc-data (ewoc-locate
                               dvc-revlist-cookie)))
             (full (tla--revision-revision
                    (dvc-revlist-entry-patch-struct (nth 1 elem)))))
        (tla-star-merge (tla--name-construct
                         (if arg (butlast full) full))
                        to-tree)))))

(defun tla-revision-replay-version ()
  "Call `tla-revision-replay' with a prefix arg."
  (interactive)
  (tla-revision-replay 'all))

(defun tla-revision-lessp (rev1 rev2)
  "Compares REV1 and REV2 as strings.

Similar to `string-lessp', but sorts numerical substring according to
numerical value instead of lexicographical order.

\(tla-revision-lessp \"patch-2\" \"patch-10\") will be true for
example."
  (let ((s1 (string-to-list rev1))
        (s2 (string-to-list rev2))
        (result 'dont-know))
    (while (eq result 'dont-know)
      (cond ((and (null s1) (null s2))
             (setq result t))
            ((null s1)
             (setq result t))
            ((null s2)
             (setq result nil))
            ((and (dvc-digit-char-p (car s1))
                  (dvc-digit-char-p (car s2)))
             (setq result (tla-revision-lessp-digit s1 s2)))
            ((not (eq (car s1) (car s2)))
             (setq result (< (car s1) (car s2))))
            (t
             (setq s1 (cdr s1)
                   s2 (cdr s2)))))
    result))

(defun tla-revision-lessp-digit (s1 s2)
  "Compare S1 and S2 (as lists of char) starting with a number.

For example, '(?1 ?2 ?f ?o? ?o) and '(?4 ?2 ?b ?a ?r)."
  (let (sub1 sub2)
    (while (and s1 (dvc-digit-char-p (car s1)))
      (setq sub1 (cons (car s1) sub1))
      (setq s1 (cdr s1)))
    (while (and s2 (dvc-digit-char-p (car s2)))
      (setq sub2 (cons (car s2) sub2))
      (setq s2 (cdr s2)))
    (let* ((num1 (string-to-number (concat (nreverse sub1))))
           (num2 (string-to-number (concat (nreverse sub2)))))
      (cond ((equal num1 num2)
             (tla-revision-lessp s1 s2))
            (t (< num1 num2))))))

(defun tla-revision-replay (arg)
  "Run replay from the current location.
If there are marked revisions, these are replayed.
If these are marked revisions and ARG is `reversely', these
are replayed reversely. If ARG is `all', all missing revisions
from this version are replayed. If there are no marked
revisions is given, and ARG is `nil', the revision under the point
is replayed. If you call this function interactively, give a positive
prefix argument to set ARG `all' or give a negative prefix argument
to set ARG `reversely'. If no prefix argument is given, ARG is set to `nil'."
  (interactive (list
                (cond
                 ((eq current-prefix-arg nil) nil)
                 ((or (eq current-prefix-arg '-)
                      (and
                       (numberp current-prefix-arg)
                       (> 0 current-prefix-arg)))
                  'reversely)
                 (current-prefix-arg
                  'all))))
  (let* ((last-inventory (tla--last-visited-inventory-buffer))
         (local-tree (or (if last-inventory
                             (with-current-buffer last-inventory
                               default-directory)
                           default-directory)))
         (current (ewoc-locate dvc-revlist-cookie))
         marked)
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (car (cddr (ewoc-data current)))
                              'bookmark))))
      (setq current (ewoc-prev dvc-revlist-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (car (cddr (ewoc-data current))) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))

    (setq marked (dvc-revision-marked-revisions))
    (let ((to-tree (dvc-read-directory-name
                    (format "Replay%s to tree: "
                            (cond
                             ((eq arg 'reversely)
                              (if marked
                                  (format " %d MARKED revision%s REVERSELY"
                                          (length marked)
                                          (if (eq (length marked) 1) "" "s"))
                                " a revision under the point REVERSELY"))
                             ((eq arg 'all)
                              " ALL missing revisions")
                             (t (if marked
                                    (format " %d MARKED revision%s"
                                            (length marked)
                                            (if (eq (length marked) 1) "" "s"))
                                  " a revision under the point"))))
                    local-tree
                    )))
      (if marked
          (let ((revisions (mapcar 'tla--revision-revision marked)))
            (tla-replay (sort (mapcar (lambda (revision)
                                        (tla--name-construct
                                         revision))
                                      revisions)
                              'tla-revision-lessp)
                        to-tree
                        (when (eq arg 'reversely) t)))
        (let* ((elem (ewoc-data (ewoc-locate
                                 dvc-revlist-cookie)))
               (full (tla--revision-revision (or (car (cddr elem))
                                                 ;; single unmarked item
                                                 (aref (cadr elem) 3)))))
          (tla-replay (tla--name-construct
                       (if (eq arg 'all) (butlast full) full))
                      to-tree
                      (when (eq arg 'reversely) t)))))))

(defun tla-revision-tag-from-head ()
  "Run tla tag from the newest revision in revision buffer."
  (interactive)
  (let* ((from (when tla-buffer-archive-name
                 (tla--name-construct tla-buffer-archive-name
                                      tla-buffer-category-name
                                      tla-buffer-branch-name
                                      tla-buffer-version-name))))
    (unless from (error "No head revision"))
    (tla--revision-tag-internal from)))

(defun tla-revision-tag-from-here ()
  "Run tla tag from the current location in revision buffer."
  (interactive)
  (let ((from (when dvc-revlist-cookie
                (let* ((elem (ewoc-data (ewoc-locate
                                         dvc-revlist-cookie))))
                  (apply 'tla--name-construct (aref (car (cddr elem)) 1))))))
    (unless from (error "No revision here"))
    (tla--revision-tag-internal from)))

(defun tla--revision-tag-internal (from-fq)
  "Tag from FROM-FQ to some destination."
  (let* ((to (tla-name-read "Tag to: "
                            'prompt 'prompt 'prompt 'prompt))
         (to-fq (tla--name-construct to)))
    (tla--version-tag-internal from-fq to-fq)))

(defun tla-revision-revlog ()
  "Show the log entry for the revision at point."
  (interactive)
  (let* ((elem (ewoc-data (ewoc-locate
                           dvc-revlist-cookie)))
         (full (tla--revision-revision
                (dvc-revlist-entry-patch-struct (nth 1 elem))))
         (cur-buf (current-buffer))
         (log-buf (tla--revlog-any full))
         (display-buf (dvc-get-buffer-create tla-arch-branch 'revlog
                                             (tla--name-construct full))))
    (dvc-switch-to-buffer display-buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (with-current-buffer log-buf
                (buffer-string)))
      (goto-char (point-min)))
    (tla-revlog-mode)
    (when (eq dvc-switch-to-buffer-mode 'pop-to-buffer)
      (pop-to-buffer cur-buf))))

;;;###autoload
(defun tla-revlog-any (revision)
  "Show the log entry for REVISION (a string)."
  (interactive (list (tla--name-construct
                      (tla-name-read "Revision spec: "
                                     'prompt 'prompt 'prompt 'prompt 'prompt))))
  (let* ((log-buf (tla--revlog-any revision))
         (display-buf (dvc-get-buffer-create tla-arch-branch 'revlog revision)))
    (dvc-switch-to-buffer display-buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (with-current-buffer log-buf
                (buffer-string)))
      (goto-char (point-min)))
    (tla-revlog-mode)))

(defun tla-revision-update ()
  "Run tla update for this revision."
  (interactive)
  (let ((local-tree default-directory) ;; Default value
        (current (ewoc-locate dvc-revlist-cookie)))
    (while (and current
                (not (and (eq (car (ewoc-data current))
                              'separator)
                          (eq (car (cddr (ewoc-data current)))
                              'bookmark))))
      (setq current (ewoc-prev dvc-revlist-cookie current)))
    (when (and current
               (eq (car (ewoc-data current)) 'separator)
               (eq (car (cddr (ewoc-data current))) 'bookmark))
      (setq local-tree (cadddr (ewoc-data current))))
    (let ((buffer (current-buffer)))
      (tla-update (dvc-read-directory-name "Update tree: "
                                           local-tree)
                  (dvc-capturing-lambda ()
                    (pop-to-buffer (capture buffer))
                    (dvc-generic-refresh))))))

(defun tla-revision-send-comments (revision &optional email)
  "Sends comments to the author of REVISION.

The email is extracted from the archive name.  A new mail message is
opened with a description of the revision.  REVISION must be the same
structure as the elem of `dvc-revlist-cookie', or a string.

When called interactively, REVISION is the revision at point."
  (interactive (list (car (cddr (ewoc-data (ewoc-locate dvc-revlist-cookie))))))
  (let* ((full-rev (tla--revision-revision revision))
         (archive (tla--name-archive full-rev))
         (email (or email
                    (progn (string-match "\\(.*\\)--\\([^-]\\|-[^-]\\)"
                                         archive)
                           (match-string 1 archive))))
         (summary (tla--revision-summary revision))
         (subject tla-send-comments-format))
    (dolist (pair '(("%f" . (tla--name-construct full-rev))
                    ("%a" . archive)
                    ("%c" . (tla--name-category full-rev))
                    ("%b" . (tla--name-branch full-rev))
                    ("%v" . (tla--name-version full-rev))
                    ("%r" . (tla--name-revision full-rev))
                    ("%s" . summary)
                    ("%t" . (if (> (string-width summary)
                                   tla-send-comments-width)
                                (concat (truncate-string-to-width summary 25)
                                        "...")
                              summary))))
      (setq subject
            (replace-regexp-in-string (car pair) (eval (cdr pair))
                                      subject)))
    (compose-mail email subject)
    (save-excursion
      (insert "\n\n" (tla--name-construct full-rev) "\n"
              "  " summary "\n"
              "  " (tla--revision-date revision) "\n"
              "  " (tla--revision-creator revision) "\n"))))

(defun tla--changes-what-changed-original-file (file)
  "Remove what-changed directory part from FILE and return it."
  (if (string-match
       "\\(/,,what-changed[^/]+/new-files-archive\\)"
       file)
      (concat (substring file 0 (match-beginning 1))
              (substring file (match-end 1)))
    file))


(defun dvc-diff-master-buffer ()
  "Jump to the master *{tla|baz}-changes* buffer for a nested changes buffer."
  (interactive)
  (unless tla--changes-buffer-master-buffer
    (error "No master buffer"))
  (dvc-switch-to-buffer tla--changes-buffer-master-buffer))

(defun dvc-diff-view-source (&optional other-file)
  "Show the corresponding file and location of the change.
This function does not switch to the file, but it places the cursor
temporarily at the location of the change and will stay in the changes
buffer.  Thus you can quickly see more context on a specific change without
switching buffers.
The prefix argument OTHER-FILE controls whether the original or new
file is visited."
  (interactive "P")
  (let ((diff-window (selected-window)))
    (save-excursion
      (diff-goto-source other-file)
      (recenter)
      (dvc-flash-line)
      (select-window diff-window))))

(defun dvc-diff-save-current-defun-as-kill ()
  "Copy the function name for the change at point to the kill-ring.
That function uses `add-log-current-defun'"
  (interactive)
  (let ((func-name (add-log-current-defun)))
    (if func-name
        (progn
          (kill-new func-name)
          (message "Copied %S" func-name))
      (message "No current defun detected."))))


(defun dvc-diff-jump-to-change-by-mouse (event &optional other-file)
  "Jump to the changes."
  (interactive "e\nP")
  (mouse-set-point event)
  (dvc-diff-jump-to-change other-file))

(defalias 'tla-changes-revert 'tla-inventory-revert)


;; ----------------------------------------------------------------------------
;; tla-changelog-mode
;; ----------------------------------------------------------------------------

(define-derived-mode tla-changelog-mode change-log-mode "tla-changelog"
  (set (make-local-variable 'font-lock-defaults)
       (list 'tla-changelog-font-lock-keywords
             t nil nil 'backward-paragraph))
  (use-local-map tla-changelog-mode-map)
  (set (make-local-variable 'tla-button-marker-list)
       nil)
  (unless tla-dont-hyperlink-changelog (tla-add-buttons)))

(defconst tla-changelog-start-regexp "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ")
(defun tla-changelog-next-entry (n)
  "Go to the next entry in the changelog.
If called with a prefix argument, skip n entries forward."
  (interactive "p")
  (re-search-forward tla-changelog-start-regexp nil t n)
  (search-forward "Summary:" nil t))

(defun tla-changelog-previous-entry (n)
  "Go to the previous entry in the changelog.
If called with a prefix argument, skip n entries backward."
  (interactive "p")
  (end-of-line)
  (re-search-backward tla-changelog-start-regexp)
  (re-search-backward tla-changelog-start-regexp nil t n)
  (search-forward "Summary:"))

(defun tla-changelog-revision-at-point ()
  "Return the patch number at point in a tla changelog buffer."
  (save-excursion
    (let ((patch-nr (progn
                      (end-of-line)
                      (re-search-backward tla-changelog-start-regexp)
                      (re-search-forward "\\(\\(patch\\|base\\|version\\)-[0-9]+\\)$")
                      (match-string-no-properties 1)))
          (version (progn
                     (goto-char (point-min))
                     (search-forward "arch-tag: automatic-ChangeLog--")
                     (buffer-substring-no-properties (point) (line-end-position)))))
      (concat version "--" patch-nr))))

(defun tla-changelog-version-at-point ()
  "Return the version at point in a tla changelog buffer."
  (tla--name-mask (tla--name-split (tla-changelog-revision-at-point)) t t t t t nil))

(defun tla-changelog-show-changeset ()
  "Show the changeset for the actual changelog entry."
  (interactive)
  (tla-get-changeset (tla-changelog-revision-at-point) t))

(defun tla-changelog-log-summary-at-point ()
  "Return the log message summary string at point as string."
  (save-excursion
    (re-search-backward tla-changelog-start-regexp)
    (re-search-forward "Summary:\n +")
    (buffer-substring-no-properties (point) (line-end-position))))

(defun tla-changelog-log-message-at-point ()
  "Return the log message at point as string."
  (save-excursion
    (let ((start-pos (progn
                       (re-search-backward tla-changelog-start-regexp)
                       (point)))
          (end-pos (progn
                     (re-search-forward "\\(modified\\|new\\) files:")
                     (forward-line -2)
                     (line-end-position))))
      (buffer-substring-no-properties start-pos end-pos))))

(defun tla-changelog-save-log-message-as-kill ()
  "Save the log message for the actual patch."
  (interactive)
  (kill-new (tla-changelog-log-message-at-point))
  (message "Copied log message for %s" (tla-changelog-revision-at-point)))

(defun tla-changelog-save-revision-as-kill ()
  "Save the revision for the actual patch to the kill-ring."
  (interactive)
  (kill-new (tla-changelog-revision-at-point))
  (message "Copied %s" (tla-changelog-revision-at-point)))

(defun tla-changelog-save-version-as-kill ()
  "Save the version for the actual patch to the kill-ring."
  (interactive)
  (kill-new (tla-changelog-version-at-point))
  (message "Copied %s" (tla-changelog-version-at-point)))

;; ----------------------------------------------------------------------------
;; tla-inventory-file-mode
;; ----------------------------------------------------------------------------
;;;###autoload
(defun tla-inventory-file-mode ()
  "Major mode to edit tla inventory files (=tagging-method, .arch-inventory)."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-inventory-file-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "# ")
  (setq major-mode 'tla-inventory-file-mode
        mode-name "tla-inventory-file")
  (run-hooks 'tla-inventory-file-mode-hook))

(defun tla--inventory-file-jump-from-head (category)
  "Search CATEGORY from the head of the buffer."
  (let ((p (save-excursion (goto-char (point-min))
                           (re-search-forward
                            (concat "^" category) nil t))))
    (when p
      (goto-char p))))

(defun tla--inventory-file-jump-from-tail (category)
  "Search CATEGORY from the tail of the buffer.
Return nil if CATEGORY is not found."
  (let ((p (save-excursion (goto-char (point-max))
                           (re-search-backward
                            (concat "^" category) nil t))))
    (when p
      (goto-char p))))

(defun tla--inventory-file-add-file (category file &optional ext-only)
  "Added FILE to CATEGORY."
  (unless (tla--inventory-file-jump-from-tail category)
    (goto-char (point-min)))
  (save-excursion (open-line 1))
  ;; TODO regexp quote FILE
  (insert (format (if ext-only "%s %s$" "%s ^(%s)$") category file)))

;; ----------------------------------------------------------------------------
;; Find file hook
;; ----------------------------------------------------------------------------
;; just 99% cut&paste from vc-follow-link in vc-hook.el, but this way there is
;; no need to load it thus avoiding interfering with VC ...
(defun tla-follow-link ()
  "Follow a symbolic link.
If the current buffer visits a symbolic link, this function makes it
visit the real file instead.  If the real file is already visited in
another buffer, make that buffer current, and kill the buffer
that visits the link."
  (let* ((truename (abbreviate-file-name (file-truename buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
         (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
        (progn
          (kill-buffer this-buffer)
          ;; In principle, we could do something like set-visited-file-name.
          ;; However, it can't be exactly the same as set-visited-file-name.
          ;; I'm not going to work out the details right now. -- rms.
          (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

(defun tla-find-file-hook ()
  "Hook executed when opening a file.
Follow symlinked files/directories to the actual location of a file.
See also `dvc-find-file-hook'."
  (let (link file result)
    (when (and (if (boundp 'vc-ignore-vc-files)
                   (not vc-ignore-vc-files)
                 t)
               (if (fboundp 'file-remote-p)
                   (not (file-remote-p (buffer-file-name)))
                 t)
               tla-follow-symlinks
               (setq file buffer-file-name)
               (not (string= (setq link (file-truename file)) file)))
      (setq file link
            result (cond ((equal tla-follow-symlinks 'tree)
                          (tla-tree-root file t))
                         ((equal tla-follow-symlinks 'id)
                          (= 0 (tla--run-tla-sync
                                (list "id" file)
                                :finished 'dvc-status-handler
                                :error 'dvc-status-handler)))))

      (if result
          (cond ((eq tla-follow-symlinks-mode 'warn)
                 (message
                  "Warning: symbolic link to arch-controlled source file: %s"
                  file))
                ((or (eq tla-follow-symlinks-mode 'follow)
                     (find-buffer-visiting file))
                 (tla-follow-link)
                 (message "Followed link to arch-controlled %s"
                          buffer-file-name))
                ((eq tla-follow-symlinks-mode 'ask)
                 (if (y-or-n-p "Follow symbolic link to arch-controlled source file? ")
                     (progn
                       (tla-follow-link)
                       (message "Followed link to arch-controlled %s"
                                buffer-file-name))
                   (message
                    "Warning: editing through the link bypasses version control")))
                (t (error "Unknown mode for tla-follow-symlinks-mode=%s"
                          tla-follow-symlinks-mode)))
        ))))

;; ----------------------------------------------------------------------------
;; Misc functions
;; ----------------------------------------------------------------------------
(defvar tla--insert-arch-tag-functions
  '((autoconf-mode . tla--insert-arch-tag-for-autoconf-mode)
    (makefile-mode . tla--insert-arch-tag-for-makefile-mode)
    (texinfo-mode  . tla--insert-arch-tag-for-texinfo-mode)
    )
  "Alist containing per mode specialized functions for inserting arch-tag.
Key stands for a major mode.  Value is a function which inserts arch-tag.
The function takes two arguments.  The first argument is an uuid string.
The second argument is a boolean showing whether the point is in a comment
or not." )

(defconst tla--arch-tag-string (concat "arch-ta" "g: ")
  "To avoid having the string a-r-c-h--t-a-g: in this buffer ;-).")

(defun tla-tag-uuid ()
  "Candidate for `tla-tag-function'.
Returns a unique string using uuidgen"
  (dvc-strip-final-newline (shell-command-to-string "uuidgen")))

(defun tla-tag-name-date-filename ()
  "Candidate for `tla-tag-function'.
Returns a string containing the name of the user, the precise date,
and the name of the current file.  This should be unique worldwide,
has the advantage of containing usefull information in addition to
the unique identifier.  The inconvenient in comparison to
`tla-tag-uuid' is that an unfortunate modification of the tag is more
easily made (sed script or manual modification)"
  (concat (user-full-name) ", "
          (format-time-string "%c")
          " (" (file-name-nondirectory (buffer-file-name)) ")"))

;;;###autoload
(defun tla-tag-string ()
  "Return a suitable string for an arch-tag.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after).

Interactively, you should call `tla-tag-insert', but this function can
be usefull to write template files."
  (funcall tla-tag-function))

;;;###autoload
(defun tla-tag-insert ()
  "Insert a unique arch-tag in the current file.
Actually calls `tla-tag-function', which defaults to `tla-tag-uuid' to generate
string (and possibly add a comment-end after)"
  (interactive)
  (let ((the-tag-itself (tla-tag-string))
        (in-comment-p (nth 4 (parse-partial-sexp (point) (point-min))))
        (header "")
        (footer "")
        (handler (assoc major-mode tla--insert-arch-tag-functions)))
    (if (cdr handler)
        (funcall (cdr handler) the-tag-itself in-comment-p)
      (unless in-comment-p
        (setq header (if comment-start
                         (concat comment-start
                                 (if (string-match " $" comment-start)
                                     "" " "))
                       "")
              footer (if (and comment-end (not (string= "" comment-end)))
                         (format "\n%s(do not change this comment)%s%s"
                                 (make-string (length header) ?\ )
                                 comment-end
                                 (if (string-match "^ " comment-end)
                                     "" " "))
                       "")))
      (insert (concat header tla--arch-tag-string the-tag-itself
                      footer)))))

;;;###autoload
(defun tla-tag-regenerate ()
  "Find an arch tag in the current buffer and regenerates it.
This means changing the ID of the file, which will usually be done after
copying a file in the same tree to avoid duplicates ID.

Raises an error when multiple tags are found or when no tag is found."
  (interactive)
  (let ((second-tag
         (save-excursion
           (goto-char (point-min))
           (unless (search-forward tla--arch-tag-string nil t)
             (error "No arch tag in this buffer"))
           (delete-region (point) (progn (end-of-line) (point)))
           (insert (funcall tla-tag-function))
           (if (search-forward tla--arch-tag-string nil t)
               (point)
             nil))))
    (when second-tag
      (goto-char second-tag)
      (beginning-of-line)
      (error "Multiple tag in this buffer"))))

(defun tla-regenerate-id-for-file (file)
  "Create a new id for the file FILE.
Does roughly

$ tla delete file
$ tla add file

But also works for the tagline method.  When the tagline method is
used, the file is opened in a buffer.  If the file had modifications,
the tag is modified in the buffer, and the user is prompted for
saving.  If the file had no unsaved modifications, the modification is
done in the buffer and the file is saved without prompting.

FILE must be an absolute filename.  It can also be a directory"
  (interactive "f")
  (cond
   ((file-directory-p file)
    (progn
      (delete-file (concat (file-name-as-directory file)
                           ".arch-ids/=id"))
      (tla-add nil file)))
   ((string-match "^\\(.*\\)/\\.arch-ids/=id" file) ;; file is an =id file.
    (tla-regenerate-id-for-file (match-string 1 file)))
   ((string-match "^\\(.*\\)/\\.arch-ids/\\([^/]*\\)\\.id" file)
    ;; file is an id file.
    (tla-regenerate-id-for-file
     (concat (match-string 1 file) "/" (match-string 2 file))))
   (t
    (let* ((dir (file-name-directory file))
           (basename (file-name-nondirectory file))
           (id-file (concat dir
                            (file-name-as-directory ".arch-ids")
                            basename ".id")))
      (if (file-exists-p id-file)
          (progn (delete-file id-file)
                 (tla-add nil file))
        (with-current-buffer
            (find-file-noselect file)
          (let ((modif (buffer-modified-p)))
            (tla-tag-regenerate)
            (if modif
                (when (y-or-n-p (format "Save buffer %s? " (buffer-name)))
                  (save-buffer))
              ;; No modif. We can safely save without prompting.
              (save-buffer)))))))))

(defun tla--insert-arch-tag-for-autoconf-mode (uuid in-comment-p)
  "Insert arch-tag, UUID to the current `autoconf-mode' buffer.
IN-COMMENT-P indicates whether we are currently inside a comment."
  (when in-comment-p
    ;; In current GNU Emacs's autoconf-mode implementation,
    ;; next line is never executed.
    (error "Comment prefix \"dnl\" is not suitable for gnuarch"))
  (let ((header "m4_if(dnl      Do not change this comment\n")
        (footer "\n)dnl\n"))
    (insert (concat header "    " tla--arch-tag-string uuid footer))))

(defun tla--insert-arch-tag-for-makefile-mode (uuid in-comment-p)
  "Insert arch-tag, UUID to the current `makefile-mode' buffer.
If the file is Makefile.am, input for automake, use `##' as `comment-start'.
Comment started with `##' in Makefile.am is automatically stripped by automake.
IN-COMMENT-P indicates whether we are currently inside a comment."
  (let ((tla--insert-arch-tag-functions
         (assq-delete-all 'makefile-mode
                          (copy-sequence tla--insert-arch-tag-functions)))
        (comment-start (if (and (buffer-file-name)
                                (string-match "Makefile.am$" (buffer-file-name)))
                           "##"
                         comment-start)))
    (tla-tag-insert)))

(defun tla--insert-arch-tag-for-texinfo-mode (uuid in-comment-p)
  "Insert arch-tag, UUID to the current `texinfo-mode' buffer.
IN-COMMENT-P indicates whether we are currently inside a comment."
  (when in-comment-p
    (error "Comment prefix \"@c\" is not suitable for gnuarch"))
  (let ((header "@ignore\n")
        (footer "\n@end ignore\n"))
    (insert (concat header "    " tla--arch-tag-string uuid footer))))

;;;###autoload
(defun tla-ediff-add-log-entry ()
  "Add a log entry."
  (interactive)
  (pop-to-buffer ediff-buffer-A)
  (dvc-add-log-entry))

;;
;; Tree-lint mode
;;
(defvar tla--tree-lint-cookie nil
  "Ewoc cookie used in tree-lint mode.")

(define-derived-mode tla-tree-lint-mode fundamental-mode
  "tla-tree-lint"
  "Major mode to view tree-lint warnings.
Commands:
\\{tla-tree-lint-mode-map}
"
  (dvc-install-buffer-menu)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (setq dvc-buffer-refresh-function
        (lexical-let ((lex-default-directory default-directory))
          (lambda () (interactive) (tla-tree-lint default-directory))))
  (set (make-local-variable 'tla--tree-lint-cookie)
       (ewoc-create (dvc-ewoc-create-api-select
                     #'tla--tree-lint-printer)))
  (set (make-local-variable 'dvc-get-file-info-at-point-function)
       'tla-tree-lint-get-file-at-point)
  (set (make-local-variable 'dvc-buffer-marked-file-list)
       nil)
  (set (make-local-variable 'dvc-buffer-all-marked-file-list)
       nil)
  (set (make-local-variable 'tla-generic-select-files-function)
       'tla--tree-lint-select-files)
  (toggle-read-only t))

(defun tla-tree-lint-get-file-at-point ()
  "Find file at point in *{tla|baz}-tree-lint*.  Error when not on a file."
  (let ((data (ewoc-data (ewoc-locate tla--tree-lint-cookie))))
    (if (eq (car data) 'message)
        nil
      (cadr data))))

(defun tla--tree-lint-prepare-buffer (root &optional function)
  "Prepare the buffer to display the tree-lint warnings for tree ROOT.

If FUNCTION is provided, it will be ran when all warnings will have
been eliminated."
  (let* ((buffer (dvc-get-buffer-create tla-arch-branch 'tree-lint root))
         (function (or function tla--tree-lint-nowarning-fn)))
    (with-current-buffer buffer
      (tla-tree-lint-mode)
      (set (make-local-variable 'tla--tree-lint-nowarning-fn)
           function)
      (ewoc-enter-last
       tla--tree-lint-cookie
       (list 'message (format "Running tree-lint in %s ..."
                              root)))
      buffer)))

(defun tla-tree-lint-goto (root)
  "Goto tree-lint buffer or run `tla-tree-lint'."
  (interactive
   (list (dvc-read-project-tree-maybe "Run tla tree-lint in: ")))
  (let* ((default-directory root)
         (buffer (dvc-get-buffer tla-arch-branch 'tree-lint default-directory)))
    (if buffer
        (dvc-switch-to-buffer buffer)
      (tla-tree-lint root))))

;;;###autoload
(defun tla-tree-lint (root)
  "Run tla tree-lint in directory ROOT."
  (interactive
   (list (dvc-read-project-tree-maybe "Run tla tree-lint in: ")))
  (setq tla-pre-tree-lint-window-configuration (current-window-configuration))
  (let ((default-directory root)
        (buffer (tla--tree-lint-prepare-buffer root)))
    (when dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer))
    (tla--run-tla-async
     (list (if (tla-has-lint-command) "lint" "tree-lint"))
     :related-buffer buffer
     :finished
     (dvc-capturing-lambda (output error status arguments)
       (if (> (buffer-size output) 0)
           (progn
             (save-excursion
               (tla--tree-lint-parse-buffer output (capture buffer)))
             (with-current-buffer (capture buffer)
               (tla--tree-lint-cursor-goto
                (ewoc-nth tla--tree-lint-cookie 0))))
         (message "No tree-lint warnings for %s." (capture default-directory))
         (with-current-buffer (capture buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (ewoc-enter-last
              tla--tree-lint-cookie
              (list 'message (format "No tree-lint warnings for %s."
                                     (capture default-directory))))))
         (set-window-configuration
          tla-pre-tree-lint-window-configuration)
         (when tla--tree-lint-nowarning-fn
           (funcall tla--tree-lint-nowarning-fn)
           (setq tla--tree-lint-nowarning-fn nil))))
     :error
     (dvc-capturing-lambda (output error status arguments)
       (if (equal status 2)
           (with-current-buffer (capture buffer)
             (set 'tla--tree-lint-cookie
                  (ewoc-create (dvc-ewoc-create-api-select
                                #'tla--tree-lint-printer)))
             (let ((inhibit-read-only t))
               (erase-buffer))
             (ewoc-enter-last
              tla--tree-lint-cookie
              (list 'message
                    (concat "* Error running lint:\n"
                            (dvc-buffer-content output)
                            (dvc-buffer-content error))))
             (ewoc-refresh tla--tree-lint-cookie)
             (goto-char (point-min)))
         (save-excursion
           (tla--tree-lint-parse-buffer output (capture buffer)))
         (with-current-buffer (capture buffer)
           (tla--tree-lint-cursor-goto
            (ewoc-nth tla--tree-lint-cookie 0))))))))

(defconst tla--tree-lint-message-alist
  '(("^These files would be source but lack inventory ids"
     missing-file)
    ("^These explicit ids have no corresponding file:"
     id-without-file)
    ("^These files violate naming conventions:"
     unrecognized)
    ("^These symlinks point to nonexistent files:"
     broken-link)
    ("^Duplicated ids among each group of files listed here:"
     duplicate-id)
    ))

(defun tla--tree-lint-message-type (message)
  "Return a symbol saying which type of message the string MESSAGE is."
  (let ((result nil)
        (iterator tla--tree-lint-message-alist))
    (while (and iterator (not result))
      (when (string-match (caar iterator) message)
        (setq result (car (cdar iterator))))
      (setq iterator (cdr iterator)))
    (or result 'unknown)))

(defun tla--tree-lint-parse-buffer (buffer output-buffer)
  "Parse the output of tla tree-lint in BUFFER.
Show in in the tree-lint-mode buffer OUTPUT-BUFFER."
  (with-current-buffer output-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (dvc-face-add (format "Tree lint warnings in %s\n"
                                    default-directory)
                            'dvc-messages)))
    (setq tla--tree-lint-cookie
          (ewoc-create (dvc-ewoc-create-api-select
                        #'tla--tree-lint-printer))))
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((cookie (with-current-buffer output-buffer
                    tla--tree-lint-cookie)))
      (while (re-search-forward "^." nil t)
        (goto-char (line-beginning-position))
        (let* ((message (buffer-substring-no-properties
                         (point) (line-end-position)))
               (type (tla--tree-lint-message-type message)))
          (ewoc-enter-last cookie (list 'message message))
          (forward-line 2)
          (if (eq type 'duplicate-id)
              (progn
                (while (looking-at "\\([^ \t]*\\)[ \t]+\\(.*\\)")
                  (let* ((file (match-string 1))
                         (id (match-string 2)))
                    ;; Format: (duplicate-id "filename" "id" first? last?)
                    (ewoc-enter-last
                     cookie (list 'duplicate-id (tla-unescape file) id
                                  t nil))
                    (forward-line 1)
                    (while (not (eq (char-after) ?\n))
                      (let ((file (buffer-substring-no-properties
                                   (point) (line-end-position))))
                        (forward-line 1)
                        (ewoc-enter-last cookie
                                         (list 'duplicate-id
                                               (tla-unescape file)
                                               id nil
                                               (eq (char-after) ?\n)))))
                    (forward-line 1)
                    )))
            (while (not (eq (char-after) ?\n))
              (ewoc-enter-last cookie
                               (list type (tla-unescape
                                           (buffer-substring-no-properties
                                            (point)
                                            (line-end-position)))))
              (forward-line 1)))))
      (let ((inhibit-read-only t))
        (ewoc-refresh cookie)))))

(defvar tla--tree-lint-printer-first-duplicate nil
  "Internal variable.
non-nil when the ewoc printer is printing the first group of duplicate ID's")

(defun tla--tree-lint-printer (elem)
  "Ewoc printer for the tree-lint buffer.
Displays ELEM."
  (when (not (eq (car elem) 'message))
    (insert (if (member (cadr elem)
                        dvc-buffer-marked-file-list)
                (concat " " dvc-mark " ") "   ")))
  (case (car elem)
    (message (insert "\n" (dvc-face-add (cadr elem) 'dvc-messages)
                     "\n")
             (setq tla--tree-lint-printer-first-duplicate t))
    (missing-file (insert
                   (dvc-face-add (cadr elem) 'dvc-to-add
                                 'tla-tree-lint-file-map
                                 tla-tree-lint-file-menu)))
    (id-without-file (insert
                      (dvc-face-add (cadr elem) 'dvc-to-add
                                    'tla-tree-lint-file-map
                                    tla-tree-lint-file-menu)))
    (unrecognized (insert
                   (dvc-face-add (cadr elem)
                                 'dvc-unrecognized
                                 'tla-tree-lint-file-map
                                 tla-tree-lint-file-menu)))
    (broken-link (insert (dvc-face-add (cadr elem)
                                       'dvc-broken-link
                                       'tla-tree-lint-file-map
                                       tla-tree-lint-file-menu)))
    (unknown (insert (dvc-face-add (cadr elem)
                                   'dvc-unrecognized
                                   'tla-tree-lint-file-map
                                   tla-tree-lint-file-menu)))
    (duplicate-id
     (insert (dvc-face-add (cadr elem)
                           'dvc-duplicate
                           'tla-tree-lint-file-map
                           tla-tree-lint-file-menu))
     (when (nth 3 elem) (insert "\t"
                                (dvc-face-add (car (cddr elem))
                                              'dvc-id)))
     (when (nth 4 elem) (insert "\n")))
    (t (error "Unimplemented type of tree-lint error")))
  )

(defun tla--tree-lint-cursor-goto (ewoc-tree-lint)
  "Move cursor to the ewoc location of EWOC-TREE-LINT."
  (interactive)
  (if ewoc-tree-lint
      (progn (goto-char (ewoc-location ewoc-tree-lint))
             (re-search-forward "." nil t)
             (backward-char 1))
    (goto-char (point-min))))

(defun tla-tree-lint-next ()
  "Move to the next tree lint item."
  (interactive)
  (let* ((cookie tla--tree-lint-cookie)
         (elem (ewoc-locate cookie))
         (next (or (ewoc-next cookie elem) elem)))
    (tla--tree-lint-cursor-goto next)))

(defun tla-tree-lint-previous ()
  "Move to the previous tree lint item."
  (interactive)
  (let* ((cookie tla--tree-lint-cookie)
         (elem (ewoc-locate cookie))
         (previous (or (ewoc-prev cookie elem) elem)))
    (tla--tree-lint-cursor-goto previous)))

(defun tla-tree-lint-mark-file ()
  "Mark the current tree-lint file."
  (interactive)
  (let ((current (ewoc-locate tla--tree-lint-cookie))
        (files (tla--tree-lint-select-files nil nil nil nil nil t t)))
    (when files
      (dolist (file files)
        (add-to-list 'dvc-buffer-marked-file-list file)
        (add-to-list 'dvc-buffer-all-marked-file-list file))
      (ewoc-refresh tla--tree-lint-cookie))
    (tla--tree-lint-cursor-goto
     (if (eq (car (ewoc-data current)) 'message)
         current
       (ewoc-next tla--tree-lint-cookie current)))))

(defun tla-tree-lint-unmark-file ()
  "Unmark the current tree-lint file."
  (interactive)
  (let ((current (ewoc-locate tla--tree-lint-cookie))
        (files (tla--tree-lint-select-files nil nil nil nil nil t t)))
    (when files
      (dolist (file files)
        (setq dvc-buffer-all-marked-file-list
              (delete file dvc-buffer-all-marked-file-list))
        (setq dvc-buffer-marked-file-list
              (delete file dvc-buffer-marked-file-list)))
      (ewoc-refresh tla--tree-lint-cookie))
    (tla--tree-lint-cursor-goto
     (if (eq (car (ewoc-data current)) 'message)
         current
       (ewoc-next tla--tree-lint-cookie current)))))

(defun tla-tree-lint-unmark-all ()
  "Unmark all tree-lint files."
  (interactive)
  (let ((current (ewoc-locate tla--tree-lint-cookie)))
    (setq dvc-buffer-marked-file-list nil)
    (setq dvc-buffer-all-marked-file-list nil)
    (ewoc-refresh tla--tree-lint-cookie)
    (tla--tree-lint-cursor-goto current)))


(defun tla--tree-lint-select-files (msg-singular
                                    msg-plural msg-err
                                    msg-prompt
                                    &optional
                                    no-group ignore-marked
                                    no-prompt
                                    y-or-n)
  "Get the list of files under cursor, and ask confirmation of the user.
Prompt with either MSG-SINGULAR, MSG-PLURAL, MSG-ERR OR MSG-PROMPT.
If NO-GROUP is nil and if the cursor is on a message, all the
files belonging to this message are selected.  If some files are marked
 (i.e. `dvc-buffer-marked-file-list' is non-nil) and IGNORE-MARKED is
non-nil, the list of marked files is returned.  If NO-PROMPT is
non-nil, don't ask for confirmation.  If Y-OR-N is non-nil, then this
function is used instead of `y-or-n-p'."
  (if (and dvc-buffer-marked-file-list
           (not ignore-marked)
           (not (tla--mouse-event-p last-input-event)))
      (let ((list dvc-buffer-marked-file-list))
        (unless (or no-prompt
                    (funcall (or y-or-n 'y-or-n-p)
                             (if (eq 1 (length list))
                                 (format msg-singular
                                         (car list))
                               (format msg-plural
                                       (length list))))
                    (error msg-err)))
        list)
    (let* ((ewoc-elem (ewoc-locate tla--tree-lint-cookie))
           (elem (ewoc-data ewoc-elem)))
      (if (eq (car elem) 'message)
          (progn
            (when no-group (error msg-err))
            (let ((list nil))
              (setq ewoc-elem
                    (ewoc-next tla--tree-lint-cookie ewoc-elem))
              (setq elem (and ewoc-elem (ewoc-data ewoc-elem)))
              (while (and ewoc-elem (not (eq (car elem) 'message)))
                (add-to-list 'list (cadr elem))
                (setq ewoc-elem
                      (ewoc-next tla--tree-lint-cookie ewoc-elem))
                (setq elem (and ewoc-elem (ewoc-data ewoc-elem))))
              (progn
                (unless (or no-prompt
                            (funcall (or y-or-n 'y-or-n-p)
                                     (if (eq 1 (length list))
                                         (format msg-singular
                                                 (car list))
                                       (format msg-plural
                                               (length list)))))
                  (error msg-err))
                list)))
        (list (if (or no-prompt
                      (funcall (or y-or-n 'y-or-n-p)
                               (format msg-singular
                                       (cadr elem))))
                  (cadr elem)
                (error msg-err)))))))

(defun tla-tree-lint-add-files (files)
  "Prompts and add FILES.
If on a message field, add all the files below this message."
  (interactive
   (list
    (tla--tree-lint-select-files "Add %s? "
                                 "Add %s files? "
                                 "Not adding any file"
                                 "Add file: ")))
  (apply 'tla-add nil files)
  (tla-tree-lint default-directory))

(defun tla-tree-lint-delete-files (files)
  "Prompts and delete FILES.
If on a message field, delete all the files below this message."
  (interactive
   (list
    (tla--tree-lint-select-files "Delete %s? "
                                 "Delete %s files? "
                                 "Not deleting any file"
                                 "Delete file: "
                                 nil nil nil
                                 'yes-or-no-p)))
  (mapcar 'delete-file files)
  (tla-tree-lint default-directory))

(defun tla-tree-lint-regenerate-id (files)
  "Prompts and regenerate an ID (either explicit or tagline) for FILES."
  (interactive
   (list
    (tla--tree-lint-select-files "Regenerate ID for %s? "
                                 "Regenerate ID for %s files? "
                                 "Not regenerating ID for any file"
                                 "Regenerate ID for file: "
                                 t)))
  (mapcar 'tla-regenerate-id-for-file files)
  (tla-tree-lint default-directory))

(defun tla-tree-lint-make-junk (files)
  "Prompts and make the FILES junk.
If marked files are, use them as FIELS.
If not, a file under the point is used as FILES.
If on a message field, make all the files below this message junk."
  (interactive
   (list
    (tla--tree-lint-select-files "Make %s junk(prefixing \",,\")? "
                                 "Make %s files junk? "
                                 "Not making any file junk"
                                 "Make file junk: "
                                 nil nil nil
                                 'yes-or-no-p)))
  (tla-tree-lint-put-file-prefix files ",,"))

(defun tla-tree-lint-make-precious (files)
  "Prompts and make the FILES precious.
If marked files are, use them as FIELS.
If not, a file under the point is used as FILES.
If on a message field, make all the files below this message precious."
  (interactive
   (list
    (tla--tree-lint-select-files "Make %s precious(prefixing \"++\")? "
                                 "Make %s files precious? "
                                 "Not making any file precious? "
                                 "Make file precious: "
                                 nil nil nil
                                 'yes-or-no-p)))
  (tla-tree-lint-put-file-prefix files "++"))

(defun tla-tree-lint-put-file-prefix (files prefix)
  "Rename FILES with adding prefix PREFIX.
Visited buffer associations also updated."
  (mapcar
   (lambda (from)
     (let* ((buf (find-buffer-visiting from))
            (to (concat
                 (file-name-directory from)
                 prefix
                 (file-name-nondirectory from))))
       (rename-file from to)
       (when buf
         (with-current-buffer buf
           (rename-buffer to)
           (set-visited-file-name to)))))
   files)
  (dvc-generic-refresh))


;; end tree-lint-mode

;;
;; Small editor functions
;;
(defun tla-to-kill-ring ()
  "Prompts an archive location and add it to kill ring."
  (interactive)
  (kill-new
   (tla--name-construct
    (tla-name-read "Save to kill ring: "
                   'maybe 'maybe 'maybe 'maybe 'maybe))))

;;;###autoload
(defun tla-insert-location ()
  "Prompts an archive location and insert it on the current point location."
  (interactive)
  (insert
   (tla--name-construct
    (tla-name-read "Insert string: "
                   'maybe 'maybe 'maybe 'maybe 'maybe))))

(defun tla-insert-description (patch-id)
  "Prompts an archive location and insert its description at point.

LOCATION is a list."
  (interactive (list (tla-name-read "Insert description for: "
                                    'maybe 'maybe 'maybe 'maybe 'maybe)))
  (dolist (element tla-description-format)
    (cond ((eq element 'patch-id)
           (insert (tla--name-construct patch-id)))
          ((eq element 'summary)
           (when (tla--name-revision patch-id)
             (tla--archive-tree-build-revisions
              (tla--name-archive patch-id)
              (tla--name-category patch-id)
              (tla--name-branch patch-id)
              (tla--name-version patch-id)
              t nil t)
             (insert (tla--revision-summary
                      (apply 'tla--archive-tree-get-revision-struct
                             patch-id)))))
          ((eq element 'location)
           (insert
            (tla-whereis-archive
             (tla--name-archive patch-id))))
          ((stringp element)
           (insert element)))))

;;
;; Version information
;;
(defvar tla-command-version nil
  "Version of tla version.")

(defun tla-command-version ()
  "Return the TLA (arch) version."
  (interactive)
  (setq tla-command-version
        (tla--run-tla-sync '("-V")
                           :finished
                           (lambda (output error status arguments)
                             (dvc-buffer-content output))))
  (if (interactive-p)
      (message tla-command-version))
  tla-command-version)

(defvar tla-version nil "Version of xtla")
(defun tla-version ()
  "Return the Xtla version."
  (interactive)
  (let ((version
         (or (when (locate-library "dvc-version")
               (load-library "dvc-version")
               (when (boundp 'tla-version)
                 tla-version))
             (let ((default-directory
                     (file-name-directory (locate-library "tla"))))
               (setq tla-version (tla-tree-id))))))
    (if (not version)
        (progn
          (message "We did not find dvc-version.el nor the arch-tree containing xtla.el!")
          (sit-for 2)
          (message "Are you using a developer version of Xtla?")
          (sit-for 2))
      (if (interactive-p)
          (message tla-version))
      tla-version)))

(defvar tla-patch-data nil)
;;;###autoload
(defun tla-prepare-patch-submission (tla-tree-root tarball-base-name email version-string
                                                   &optional description subject)
  "Submit a patch to a tla working copy (at TLA-TREE-ROOT) via email.
With this feature it is not necessary to tag an tla archive.
You simply edit your checked out copy from your project and call this function.
The function will create a patch as *.tar.gz file (based on TARBALL-BASE-NAME)
and send it to the given email address EMAIL.
VERSION-STRING should indicate the version of tla that the patch applies to.
DESCRIPTION is a brief descsription of the patch.
SUBJECT is the subject for the email message.
For an example, how to use this function see: `tla-submit-patch'."
  (interactive)

  ;; create the patch
  (let* ((default-directory tla-tree-root)
         (tarball-full-base-name (concat default-directory tarball-base-name))
         (tarball-full-name (concat tarball-full-base-name ".tar.gz")))
    (tla-changes-save-as-tgz tarball-full-base-name)

    (require 'reporter)
    (delete-other-windows)
    (reporter-submit-bug-report
     email
     nil
     nil
     nil
     nil
     description)

    (set (make-local-variable 'tla-patch-data) (list tla-tree-root tarball-full-name))

    (insert "[VERSION] " version-string)
    (goto-char (point-max))
    (mml-attach-file tarball-full-name "application/octet-stream")
    (tla-show-changeset-from-tgz tarball-full-name)
    (other-window 1)

    (goto-char (point-min))
    (mail-position-on-field "Subject")
    (insert (or subject "[PATCH] "))))

;;;###autoload
(defun tla-submit-patch-done ()
  "Clean up after sending a patch via mail.
That function is usually called via `message-sent-hook'. Its purpose is to revert
the sent changes or to delete sent changeset tarball (see: `tla-patch-sent-action'."
  (when tla-patch-data
    (when (member tla-patch-sent-action '(keep-tarball keep-none))
      (message "Reverting the sent changes in %s" (car tla-patch-data))
      (tla--undo-internal (car tla-patch-data) t t))
    (when (member tla-patch-sent-action '(keep-changes keep-none))
      (message "Deleting the sent tarball %s" (cadr tla-patch-data))
      (delete-file (cadr tla-patch-data)))
    (when (member tla-patch-sent-action '(keep-both))
      (message "Keeping the sent changes and the sent tarball %s" (cadr tla-patch-data)))))

(defun tla-submit-patch ()
  "Submit a patch for the current arch project.
With this feature it is not necessary to tag an arch archive.
You simply edit your checked out copy and call this function.
The function will create a patch as *.tar.gz file and prepare a buffer to
send the patch via email.

The variable `tla-submit-patch-mapping' allows to specify the
target email address and the base name of the sent tarball.

After the user has sent the message, `tla-submit-patch-done' is called."
  (interactive)
  (tla-command-version)
  (let* ((submit-patch-info (tla--name-match-from-list
                             (tla--name-split (tla-tree-version)) tla-submit-patch-mapping))
         (mail-address (or (nth 0 submit-patch-info) ""))
         (patch-base-file-name (or (nth 1 submit-patch-info) "arch")))
    (tla-prepare-patch-submission (dvc-uniquify-file-name (tla-tree-root))
                                  (concat "++" patch-base-file-name "-patch-"
                                          (format-time-string "%Y-%m-%d_%H-%M-%S" (current-time)))
                                  mail-address
                                  (tla-tree-id)
                                  dvc-patch-email-message-body-template
                                  )))

(defun tla-send-commit-notification ()
  "Send a commit notification email for the changelog entry at point.

`tla-mail-notification-destination' can be used to specify a prefix for
the subject line, the rest of the subject line contains the summary line
of the commit. Additionally the destination email address can be specified."
  (interactive)
  (let ((dest-specs (tla--name-match-from-list
                     (tla--name-split (tla-changelog-revision-at-point))
                     tla-mail-notification-destination))
        (rev (tla-changelog-revision-at-point))
        (summary (tla-changelog-log-summary-at-point))
        (log-message (tla-changelog-log-message-at-point)))
    (message "Preparing commit email for %s" rev)
    (compose-mail (if dest-specs (cadr dest-specs) "")
                  (if dest-specs (car dest-specs) ""))
    (message-goto-subject)
    (insert summary)
    (message-goto-body)
    (insert (concat "Committed " rev "\n\n"))
    (insert log-message)
    (message-goto-body)))

;; Local Variables:
;; End:

(provide 'tla)

;;; tla.el ends here

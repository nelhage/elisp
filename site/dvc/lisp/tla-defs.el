;;; tla-defs.el --- UI Xtla's element definitions

;; Copyright (C) 2003-2008 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; This file is part of Xtla.
;;
;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; In order to keep UI consistency, especially about key binding,
;; we gather all UI definition in this separated file.
;;


;;; History:
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (autoload 'ad-add-advice "advice")
  (require 'ediff)
  (require 'diff-mode)
  (require 'font-lock)
  (require 'add-log)
  (require 'ffap)
  (require 'dvc-log)
  (require 'dvc-utils)
  (require 'dvc-core)
  (require 'dvc-ui)
  (require 'dvc-defs))

(condition-case nil
    (progn
      ;; Contains the site-specific config info. Must remain
      ;; optional.
      (require 'dvc-site))
  (error nil))

;;;###autoload
(eval-and-compile
  (require 'easymenu)
  (require 'dvc-core))

;;
;; Minibuffer(for reading engine)
;;
(defvar xtla--name-read-partner-menu (cons "Insert Partner Version" nil))
(fset 'xtla--name-read-partner-menu (cons 'keymap xtla--name-read-partner-menu))
(defvar xtla--name-read-bookmark-menu (cons "Insert Version in Bookmarks" nil))
(fset 'xtla--name-read-bookmark-menu (cons 'keymap xtla--name-read-bookmark-menu))

(defvar tla--name-read-extension-keydefs
  '(([(control r)] . tla-name-read-refresh-cache)
    ([(meta *)]    . tla-name-read-insert-default-archive)
    ([(meta \.)]   . tla-name-read-insert-info-at-point)
    ([(meta \;)]   . tla-name-read-insert-version-associated-with-default-directory)
    ([(control n)] . tla-name-read-insert-partner-next)
    ([(control p)] . tla-name-read-insert-partner-previous)
    ([(control v)] . tla-name-read-insert-bookmark-next)
    ([(meta v)]    . tla-name-read-insert-bookmark-previous)
    ([(meta ^)]    . tla-name-read-insert-ancestor)
    ([(control h)] . tla-name-read-help)
    ([(meta \?)]    . tla-name-read-inline-help))
  "Key definitions table for `tla--name-read-minibuf-map'.
The reason these definitions are defined separately from
`tla--name-read-minibuf-map' is that to reuse these definitions
in `tla-name-read-help'. Don't forget to evalute
`tla--name-read-minibuf-map' again after updating this value.")

(defun tla-name-read-minibuf-map-fn ()
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    ;; Keys
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (func (cdr pair)))
         (define-key map key func)))
     tla--name-read-extension-keydefs)
    ;; Menus
    (define-key map [menu-bar xtla]
      (cons "Xtla" (make-sparse-keymap "Xtla")))
    (define-key map [menu-bar xtla refresh]
      (list 'menu-item "Refresh Completion Cache"
            'tla-name-read-refresh-cache))
    (define-key map [menu-bar xtla ancestor]
      (list 'menu-item "Insert Ancestor"
            'tla-name-read-insert-ancestor
            :enable '(and
                      (minibufferp)
                      (equal "" (minibuffer-contents))
                      (member archive '(prompt maybe))
                      (not (eq this-command 'tla-revision-direct-ancestor))
                      )))
    (define-key map [menu-bar xtla default]
      (list 'menu-item "Insert Default Archive"
            'tla-name-read-insert-default-archive
            :enable '(and
                      (minibufferp)
                      (equal "" (minibuffer-contents))
                      (member archive '(prompt maybe)))))
    (define-key map [menu-bar xtla here]
      (list 'menu-item "Insert Thing at Point"
            'tla-name-read-insert-info-at-point
            :enable '(and (minibufferp)
                          (equal "" (minibuffer-contents))
                          tla-name-read-insert-info-at-point)))
    (define-key map [menu-bar xtla bookmark]
      (list 'menu-item "Insert Version in Bookmark" 'xtla--name-read-bookmark-menu
            :enable '(let* ((l (condition-case nil
                                   (let ((default-version (tla-tree-version-list default-directory)))
                                     (tla-bookmarks-get-partner-versions default-version))
                                 (error nil))))
                       (and l (< 0 (length l))))))
    (define-key map [menu-bar xtla partner]
      (list 'menu-item "Insert Partner Version" 'xtla--name-read-partner-menu
            :enable '(let* ((l (condition-case nil (tla-partner-list)
                                 (error nil))))
                       (and l (< 0 (length l))))))
    map))

(defvar tla--name-read-minibuf-map (tla-name-read-minibuf-map-fn)
  "Keymap to input a gnuarch revision at the minibuffer.")

(defvar tla--tree-lint-nowarning-fn nil
  "Function to run when all lint warnings have been eliminated.

Must be buffer-local, in a tree-lint mode buffer.")


;;
;; Bookmarks mode
;;
(defvar tla-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    ;; Move
    (define-key map dvc-keyvec-next       'tla-bookmarks-next)
    (define-key map dvc-keyvec-previous   'tla-bookmarks-previous)
    (define-key map [?N] 'tla-bookmarks-move-down)
    (define-key map [?P] 'tla-bookmarks-move-up)
    ;; Actions
    (define-key map (dvc-prefix-merge dvc-key-star-merge)
      'tla-bookmarks-star-merge)
    (define-key map (dvc-prefix-merge dvc-key-replay)
      'tla-bookmarks-replay)
    (define-key map (dvc-prefix-merge dvc-key-update)
      'tla-bookmarks-update)
    (define-key map (dvc-prefix-merge dvc-key-missing)
      'tla-bookmarks-missing)
    (define-key map (dvc-prefix-merge dvc-key-tag)
      'tla-bookmarks-tag)
    (define-key map [?o] 'tla-bookmarks-open-tree)
    (define-key map [(control x) (control f)] 'tla-bookmarks-find-file)
    (define-key map dvc-keyvec-diff 'tla-bookmarks-changes)
    (define-key map dvc-keyvec-get  'tla-bookmarks-get)
    (define-key map "\C-m"           'tla-bookmarks-goto)
    ;; Marks
    (define-key map dvc-keyvec-mark       'tla-bookmarks-mark)
    (define-key map dvc-keyvec-unmark     'tla-bookmarks-unmark)
    (define-key map dvc-keyvec-unmark-all 'tla-bookmarks-unmark-all)
    (define-key map (dvc-prefix-mark ?g)  'tla-bookmarks-select-by-group)
    ;; Partners
    (define-key map [(meta p)] 'tla-bookmarks-marked-are-partners)
    (define-key map (dvc-prefix-add    ?p)
      'tla-bookmarks-add-partner-interactive)
    (define-key map (dvc-prefix-remove ?p)
      'tla-bookmarks-delete-partner-interactive)
    (define-key map (dvc-prefix-partner-file ?r)
      'tla-bookmarks-add-partners-from-file)
    (define-key map (dvc-prefix-partner-file ?w)
      'tla-bookmarks-write-partners-to-file)
    ;; Bookmark manipulation
    (define-key map (dvc-prefix-add    ?b) 'tla-bookmarks-add)
    (define-key map (dvc-prefix-remove ?b) 'tla-bookmarks-delete)
    (define-key map [?e] 'tla-bookmarks-edit)
    (define-key map dvc-keyvec-toggle  'tla-bookmarks-toggle-details)
    ;; Fields
    (define-key map (dvc-prefix-add    ?t)
      'tla-bookmarks-add-tree-interactive)
    (define-key map (dvc-prefix-remove ?t)
      'tla-bookmarks-delete-tree-interactive)
    (define-key map (dvc-prefix-add    ?g)
      'tla-bookmarks-add-group-interactive)
    (define-key map (dvc-prefix-remove ?g)
      'tla-bookmarks-delete-group-interactive)
    (define-key map (dvc-prefix-add    ?n)
      'tla-bookmarks-add-nickname-interactive)
    (define-key map (dvc-prefix-remove ?n)
      'tla-bookmarks-delete-nickname-interactive)
    (define-key map [?s] 'tla-bookmarks-edit-summary)
    ;; Switch to other buffers
    (define-key map dvc-keyvec-inventory 'tla-bookmarks-inventory)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `tla-bookmarks-mode' buffers.")

(defvar tla-bookmarks-entry-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'tla-bookmarks-goto-by-mouse)
    map)
  "Keymap used on entries in `tla-bookmarks-mode' buffers.")

;;
;; Inventory mode
;;
(defvar tla-inventory-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help    'describe-mode)
    (define-key map dvc-keyvec-refresh 'dvc-generic-refresh)
    (define-key map dvc-keyvec-add     'tla-inventory-add-files)
    (define-key map dvc-keyvec-remove  'tla-inventory-remove-files)
    (define-key map dvc-keyvec-quit    'dvc-buffer-quit)
    (define-key map dvc-keyvec-next     'tla-inventory-next)
    (define-key map dvc-keyvec-previous 'tla-inventory-previous)
    (define-key map dvc-keyvec-parent   'tla-inventory-parent-directory)
    (define-key map [(control x) (control j)] 'dvc-dired-jump)
    ;;
    ;;
    ;;
    (define-key map [?X] 'tla-inventory-delete-files)
    (define-key map (dvc-prefix-move dvc-key-move) 'tla-inventory-move)
    (define-key map dvc-keyvec-commit 'tla-inventory-edit-log)
    (define-key map [?l] 'tla-changelog)
    (define-key map dvc-keyvec-logs 'tla-logs)
    ;;
    ;; Find file group
    ;;
    (define-key map [?f] 'tla-inventory-find-file)
    (define-key map [return] 'tla-inventory-find-file)
    (define-key map "\C-m" 'tla-inventory-find-file)
    (define-key map [?o] 'dvc-find-file-other-window)
    (define-key map [?v] 'dvc-view-file)
    ;;
    ;; Diffs group
    ;;
    (define-key map (dvc-prefix-merge dvc-key-missing)
      'tla-inventory-missing)
    (define-key map (dvc-prefix-diff dvc-key-diff)
      'tla-inventory-changes)
    (define-key map (dvc-prefix-diff ?l) 'tla-changes-last-revision)
    (define-key map (dvc-prefix-diff dvc-key-ediff)
      'tla-inventory-file-ediff)
    (define-key map (dvc-prefix-diff dvc-key-get)
      'tla-inventory-delta)
    ;; Alias for above bindings
    (define-key map dvc-keyvec-diff    'tla-inventory-changes)
    (define-key map dvc-keyvec-ediff   'tla-inventory-file-ediff)
    ;;
    (define-key map dvc-keyvec-reflect 'tla-inventory-mirror)
    ;;
    ;; Merge group
    ;;
    (define-key map (dvc-prefix-merge dvc-key-star-merge)
      'tla-inventory-star-merge)
    (define-key map (dvc-prefix-merge dvc-key-replay)
      'tla-inventory-replay)
    (define-key map (dvc-prefix-merge dvc-key-update)
      'tla-inventory-update)
    (define-key map (dvc-prefix-merge dvc-key-reflect)
      'tla-inventory-apply-changeset)
    ;;
    ;; Buffers group
    ;;
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)
    ;;
    ;; Undo and redo group
    ;;
    (define-key map dvc-keyvec-revert        'tla-inventory-revert)
    (define-key map (dvc-prefix-working-copy dvc-key-revert) 'tla-inventory-undo)
    (define-key map (dvc-prefix-working-copy ?R) 'tla-inventory-redo)
    ;;
    ;; Patches group
    ;;
    (define-key map (dvc-prefix-working-copy ?S) 'tla-changes-save)
    (define-key map (dvc-prefix-working-copy ?s) 'tla-changes-save-as-tgz)
    (define-key map (dvc-prefix-working-copy ?V) 'tla-show-changeset)
    (define-key map (dvc-prefix-working-copy ?v) 'tla-show-changeset-from-tgz)
    (define-key map (dvc-prefix-working-copy ?A) 'tla-inventory-apply-changeset)
    (define-key map (dvc-prefix-working-copy ?a) 'tla-inventory-apply-changeset-from-tgz)
    ;;
    ;; Kill ring group
    ;;
    (define-key map (dvc-prefix-kill-ring ?a) 'tla-save-archive-to-kill-ring)
    (define-key map (dvc-prefix-kill-ring ?v) 'tla-save-version-to-kill-ring)
    (define-key map (dvc-prefix-kill-ring ?r) 'tla-save-revision-to-kill-ring)

    ;;
    ;; Tree lint
    ;;
    (define-key map (dvc-prefix-working-copy dvc-key-tree-lint)
      'tla-tree-lint)
    ;;
    ;; Mark group
    ;;
    (define-key map (dvc-prefix-mark dvc-key-mark) 'tla-inventory-mark-file)
    (define-key map (dvc-prefix-mark dvc-key-unmark) 'tla-inventory-unmark-file)
    ;; (define-key map dvc-keyvec-mark-all      'tla-inventory-mark-all)
    (define-key map dvc-keyvec-unmark-all    'tla-inventory-unmark-all)
    ;; Alias for above bindings
    (define-key map dvc-keyvec-mark          'tla-inventory-mark-file)
    (define-key map dvc-keyvec-unmark        'tla-inventory-unmark-file)
    ;;
    ;; Tagging method
    ;;
    (define-key map (dvc-prefix-tagging-method ?=) 'tla-edit-=tagging-method-file)
    (define-key map (dvc-prefix-tagging-method ?.) 'tla-edit-.arch-inventory-file)
    ;;
    ;; Exclude, junk, precious, unrecognized...
    ;;
    (define-key map (dvc-prefix-move ?j) 'tla-inventory-make-junk)
    (define-key map (dvc-prefix-move ?,) 'tla-inventory-make-junk)
    (define-key map (dvc-prefix-move ?p) 'tla-inventory-make-precious)
    (define-key map (dvc-prefix-move ?+) 'tla-inventory-make-precious)
    (define-key map (dvc-prefix-tagging-method ?M) 'tla-generic-set-id-tagging-method)
    (define-key map (dvc-prefix-tagging-method ?V) 'tla-generic-set-tree-version)
    (define-key map (dvc-prefix-tagging-method ?x) 'tla-generic-add-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?e) 'tla-generic-add-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?j) 'tla-generic-add-to-junk)
    (define-key map (dvc-prefix-tagging-method ?b) 'tla-generic-add-to-backup)
    (define-key map (dvc-prefix-tagging-method ?~) 'tla-generic-add-to-backup) ; alias
    (define-key map (dvc-prefix-tagging-method ?p) 'tla-generic-add-to-precious)
    (define-key map (dvc-prefix-tagging-method ?u) 'tla-generic-add-to-unrecognized)
    (define-key map (dvc-prefix-tagging-method ?X) 'tla-generic-add-ext-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?E) 'tla-generic-add-ext-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?J) 'tla-generic-add-ext-to-junk)
    (define-key map (dvc-prefix-tagging-method ?B) 'tla-generic-add-ext-to-backup)
    (define-key map (dvc-prefix-tagging-method ?P) 'tla-generic-add-ext-to-precious)
    (define-key map (dvc-prefix-tagging-method ?U) 'tla-generic-add-ext-to-unrecognized)
    ;;
    ;; Toggles
    ;;
    (define-key map dvc-keyvec-toggle-set    'tla-inventory-set-all-toggle-variables)
    (define-key map dvc-keyvec-toggle-reset  'tla-inventory-reset-all-toggle-variables)
    (define-key map dvc-keyvec-toggle-invert 'tla-inventory-toggle-all-toggle-variables)
    map)
  "Keymap used in `tla-inventory-mode' buffers.")

(defvar tla-inventory-item-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'tla-inventory-find-file-by-mouse)
    map)
  "Keymap used on items in `tla-inventory-mode' buffers.")

(defvar tla-inventory-default-version-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map [return] 'tla-generic-set-tree-version)
    (define-key map "\C-m" 'tla-generic-set-tree-version)
    map)
  "Keymap used on the default version field in `tla-inventory-mode' buffers.")

(defvar tla-inventory-tagging-method-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'tla-generic-set-id-tagging-method-by-mouse)
    (define-key map [return] 'tla-generic-set-id-tagging-method)
    (define-key map "\C-m" 'tla-inventory-id-tagging-method)
    map)
  "Keymap used on the tagging method field in `tla-inventory-mode' buffers.")

(defconst tla-inventory-file-types-manipulators
  '((?S tla-inventory-display-source
        tla-inventory-toggle-source ?s "source")
    (?P tla-inventory-display-precious
        tla-inventory-toggle-precious ?p "precious")
    (?J tla-inventory-display-junk
        tla-inventory-toggle-junk ?j "junk")
    (?B tla-inventory-display-backup
        tla-inventory-toggle-backup ?b "backup")
    (?T tla-inventory-display-tree
        tla-inventory-toggle-tree ?t "tree root")
    (?U tla-inventory-display-unrecognized
        tla-inventory-toggle-unrecognized ?u "unrecognized"))
  "List of possible file types in inventory.")

(dolist (type-arg tla-inventory-file-types-manipulators)
  (define-key tla-inventory-mode-map `[?t ,(cadr (cddr type-arg))]
    (car (cddr type-arg))))

(dolist (type-arg tla-inventory-file-types-manipulators)
  (eval `(defcustom ,(cadr type-arg) t
           ,(concat "Wether " (nth 4 type-arg)
                    " should be printed in inventory")
           :group 'tla-inventory
           :type 'boolean)))

(defvar tla-tree-lint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help    'describe-mode)
    (define-key map dvc-keyvec-refresh 'dvc-generic-refresh)
    (define-key map dvc-keyvec-add     'tla-tree-lint-add-files)
    (define-key map dvc-keyvec-remove  'tla-tree-lint-delete-files)
    (define-key map dvc-keyvec-quit    'dvc-buffer-quit)
    (define-key map [(control x) (control j)] 'dvc-dired-jump)
    (define-key map dvc-keyvec-commit  'tla-edit-log)
    (define-key map dvc-keyvec-next    'tla-tree-lint-next)
    (define-key map dvc-keyvec-previous 'tla-tree-lint-previous)
    (define-key map [down]              'tla-tree-lint-next)
    (define-key map [up]                'tla-tree-lint-previous)
    (define-key map dvc-keyvec-id      'tla-tree-lint-regenerate-id)
    (define-key map (dvc-prefix-move ?j) 'tla-tree-lint-make-junk)
    (define-key map (dvc-prefix-move ?,) 'tla-tree-lint-make-junk)
    (define-key map (dvc-prefix-move ?p) 'tla-tree-lint-make-precious)
    (define-key map (dvc-prefix-move ?+) 'tla-tree-lint-make-precious)
    ;;
    (define-key map (dvc-prefix-tagging-method ?=) 'tla-edit-=tagging-method-file)
    (define-key map (dvc-prefix-tagging-method ?.) 'tla-edit-.arch-inventory-file)
    (define-key map (dvc-prefix-tagging-method ?M) 'tla-generic-set-id-tagging-method)
    (define-key map (dvc-prefix-tagging-method ?V) 'tla-generic-set-tree-version)
    (define-key map (dvc-prefix-tagging-method ?x) 'tla-generic-add-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?X) 'tla-generic-add-ext-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?e) 'tla-generic-add-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?E) 'tla-generic-add-ext-to-exclude) ; alias
    (define-key map (dvc-prefix-tagging-method ?j) 'tla-generic-add-to-junk)
    (define-key map (dvc-prefix-tagging-method ?J) 'tla-generic-add-ext-to-junk)
    (define-key map (dvc-prefix-tagging-method ?b) 'tla-generic-add-to-backup)
    (define-key map (dvc-prefix-tagging-method ?B) 'tla-generic-add-ext-to-backup)
    (define-key map (dvc-prefix-tagging-method ?~) 'tla-generic-add-to-backup) ; alias
    (define-key map (dvc-prefix-tagging-method ?p) 'tla-generic-add-to-precious)
    (define-key map (dvc-prefix-tagging-method ?P) 'tla-generic-add-ext-to-precious)
    (define-key map (dvc-prefix-tagging-method ?u) 'tla-generic-add-to-unrecognized)
    (define-key map (dvc-prefix-tagging-method ?U) 'tla-generic-add-ext-to-unrecognized)
    ;; Other commands
    (define-key map dvc-keyvec-diff      'tla-changes)
    (define-key map dvc-keyvec-inventory 'tla-inventory)
    ;;
    (define-key map [return]            'dvc-find-file-at-point)
    (define-key map "\C-m"              'dvc-find-file-at-point)
    (define-key map [?o]                'dvc-find-file-other-window)
    (define-key map [?v]                'dvc-view-file)
    ;;
    ;; Mark group
    ;;
    (define-key map (dvc-prefix-mark dvc-key-mark) 'tla-tree-lint-mark-file)
    (define-key map (dvc-prefix-mark dvc-key-unmark) 'tla-tree-lint-unmark-file)
    ;; TODO
    ;; (define-key map dvc-keyvec-mark-all      'tla-tree-lint-mark-all)
    (define-key map dvc-keyvec-unmark-all    'tla-tree-lint-unmark-all)
    ;; Alias for above bindings
    (define-key map dvc-keyvec-mark          'tla-tree-lint-mark-file)
    (define-key map dvc-keyvec-unmark        'tla-tree-lint-unmark-file)
    ;;
    ;; Buffers group
    ;;
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)
    map)
  "Keymap used in `tla-tree-lint-mode' buffers.")

(defvar tla-tree-lint-file-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'dvc-find-file-at-point-by-mouse)
    map)
  "Keymap used on files in tla-lint-mode buffers.")

;;
;; Revlog mode
;;
(defvar tla-revlog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map dvc-keyvec-inventory 'dvc-pop-to-inventory)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map dvc-keyvec-diff 'tla-log-get-changeset)
    (define-key map "\C-m" 'tla-press-button)
    (define-key map dvc-mouse-2 'tla-push-button)
    map)
  "Keymap used in `tla-revlog-mode' buffers.")

(defvar tla-current-revision nil
  "Revision displayed in a `tla-revlog-mode' buffer.")

(defcustom tla-button-revision-fn 'tla-revlog-any
  "*Function to call when clicking a revision button.

Buttons appear in Gnus Article buffer if `tla-insinuate-gnus' has
been run, and in log buffers.

The function must take a string as argument."
  :type 'function
  :group 'xtla)

;;
;; Log edit mode
;;
(defvar tla-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'tla-log-edit-done)
    (define-key map [(control ?c) (control ?d)] 'tla-changes)
    (define-key map [(control ?c) (control ?l)] 'tla-changelog)
    (define-key map [(control ?c) (control ?m)] 'tla-log-edit-insert-log-for-merge)
    (define-key map [(control ?c)          ?m ]
      'tla-log-edit-insert-log-for-merge-and-headers)
    (define-key map [(control ?c) (control ?p)] 'tla-log-edit-insert-memorized-log)
    (define-key map [(control ?c) (control ?q)] 'tla-log-edit-abort)
    (define-key map [(control ?c) (control ?s)] 'tla-log-goto-summary)
    (define-key map [(control ?c) (control ?b)] 'tla-log-goto-body)
    (define-key map [(control ?c) (control ?k)] 'tla-log-edit-keywords)
    (define-key map "\t" 'tla-log-edit-next-field)
    map)
  "Keymap used in `tla-log-edit-mode' buffers.")

;;
;; Archive list mode
;;
(defvar tla-archive-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map (dvc-prefix-kill-ring ?a) 'tla-save-archive-to-kill-ring)
    (define-key map "\C-m" 'tla-archive-list-categories)
    (define-key map [return] 'tla-archive-list-categories)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    (define-key map dvc-keyvec-add-bookmark 'tla-bookmarks-add)
    (define-key map [?o] 'tla-archive-browse-archive)
    (define-key map [?*] 'tla-archive-select-default)
    (define-key map (dvc-prefix-add ?r) 'tla-register-archive)
    (define-key map (dvc-prefix-add ?a) 'tla-make-archive)
    (define-key map (dvc-prefix-add ?m) 'tla-archive-mirror-archive)
    (define-key map dvc-keyvec-remove   'tla-archive-unregister-archive)
    (define-key map [?g] 'tla-archives)
    (define-key map [?s] 'tla-archive-synchronize-archive)
    (define-key map [?e] 'tla-archive-edit-archive-location)
    (define-key map [down] 'tla-archive-next)
    (define-key map [up] 'tla-archive-previous)
    (define-key map [?n] 'tla-archive-next)
    (define-key map [?p] 'tla-archive-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `tla-archive-list-mode' buffers.")

(defvar tla-archive-archive-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-mouse-2 'tla-archive-list-categories-by-mouse)
    map)
  "Keymap used archives in `tla-archive-list-mode' buffers.")

;;
;; Category list mode
;;
(defvar tla-category-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-category-list-branches)
    (define-key map [return] 'tla-category-list-branches)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    (define-key map dvc-keyvec-add-bookmark 'tla-category-bookmarks-add-here)
    (define-key map [?^] 'tla-archives)
    (define-key map (dvc-prefix-add ?c) 'tla-category-make-category)
    (define-key map [?g] 'tla-category-refresh)
    (define-key map [?s] 'tla-category-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `tla-category-list-mode' buffers.")

(defvar tla-category-category-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-mouse-2 'tla-category-list-branches-by-mouse)
    map)
  "Keymap used categories in `tla-category-list-mode' buffers.")

;;
;; Branch list mode section
;;
(defvar tla-branch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-branch-list-versions)
    (define-key map [return] 'tla-branch-list-versions)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    (define-key map dvc-keyvec-parent 'tla-branch-list-parent-category)
    (define-key map (dvc-prefix-add ?b) 'tla-branch-make-branch)
    (define-key map [?>] 'tla-branch-get-branch)
    (define-key map [?g] 'tla-branch-refresh)
    (define-key map [?s] 'tla-branch-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map dvc-keyvec-add-bookmark 'tla-branch-bookmarks-add-here)
    map)
  "Keymap used in `tla-branch-list-mode' buffers.")

(defvar tla-branch-branch-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-mouse-2 'tla-branch-list-versions-by-mouse)
    map)
  "Keymap used branches in `tla-branch-list-mode' buffers.")

;;
;; Version list mode
;;
(defvar tla-version-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map "\C-m" 'tla-version-list-revisions)
    (define-key map [return] 'tla-version-list-revisions)

    ;; Buffers group
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)

    (define-key map (dvc-prefix-kill-ring ?v) 'tla-version-save-version-to-kill-ring)
    (define-key map dvc-keyvec-parent 'tla-version-list-parent-branch)
    (define-key map (dvc-prefix-add ?v) 'tla-version-make-version)
    (define-key map [?>] 'tla-version-get-version)
    (define-key map [?g] 'tla-version-refresh)
    (define-key map [?s] 'tla-version-mirror-archive)
    (define-key map [down] 'tla-category-next)
    (define-key map [up] 'tla-category-previous)
    (define-key map [?n] 'tla-category-next)
    (define-key map [?p] 'tla-category-previous)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map dvc-keyvec-add-bookmark 'tla-version-bookmarks-add-here)
    (define-key map dvc-keyvec-tag 'tla-version-tag)
    map)
  "Keymap used in `tla-version-list-mode' buffers.")

(defvar tla-version-version-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-mouse-2 'tla-version-list-revisions-by-mouse)
    map)
  "Keymap used versions in `tla-version-list-mode' buffers.")

;;
;; Revision list mode
;;
(defvar tla-revision-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-parent 'tla-revision-list-parent-version)
    (define-key map [?> ?g] 'tla-revision-get-revision)
    (define-key map [?> ?C] 'tla-revision-cache-revision)
    (define-key map [?> ?L] 'tla-revision-add-to-library)

    ;; Buffers group
    (define-key map (dvc-prefix-kill-ring ?r) 'tla-revision-save-revision-to-kill-ring)
    (define-key map (dvc-prefix-kill-ring ?v) 'tla-revision-save-version-to-kill-ring)

    (define-key map dvc-keyvec-add-bookmark 'tla-bookmarks-add)

    (define-key map (dvc-prefix-toggle ??) 'tla-revision-toggle-help)
    (define-key map (dvc-prefix-toggle ?l) 'tla-revision-toggle-library)
    (define-key map (dvc-prefix-toggle ?m) 'tla-revision-toggle-merges)
    (define-key map (dvc-prefix-toggle ?b) 'tla-revision-toggle-merged-by)
    (define-key map (dvc-prefix-toggle ?r) 'tla-revision-toggle-reverse)

    ;;
    ;; Star merge
    ;; from here
    (define-key map dvc-keyvec-star-merge 'tla-revision-star-merge)
    ;; from head
    (define-key map (dvc-prefix-merge dvc-key-star-merge)
      'tla-revision-star-merge-version)

    ;;
    ;; Replay
    ;; from here
    (define-key map dvc-keyvec-replay 'tla-revision-replay)
    ;; from head
    (define-key map (dvc-prefix-merge dvc-key-replay)
      'tla-revision-replay-version)

    ;;
    ;; Sync tree
    (define-key map  [?y] 'tla-revision-sync-tree)
    ;;
    ;; Update
    (define-key map (dvc-prefix-merge dvc-key-update)
      'tla-revision-update)
    ;;
    ;; Tag
    ;; from here
    (define-key map dvc-keyvec-tag 'tla-revision-tag-from-here)

    (define-key map [?l] 'tla-revision-revlog)
    (define-key map (dvc-prefix-merge dvc-key-missing) 'tla-missing-show-all-revisions)
    (define-key map (dvc-prefix-diff dvc-key-diff) 'tla-revision-delta)
    (define-key map (dvc-prefix-diff dvc-key-get)
      'tla-revision-store-delta)
    ;; Moved to DVC now.
    ;;    (define-key map [?=] 'tla-revision-changeset)
    ;; (define-key map [(meta ?=)] 'tla-revision-scroll-up-or-show-changeset)
    (define-key map dvc-keyvec-add-bookmark 'tla-revision-bookmarks-add)
    map)
  "Keymap used in `tla-revision-list-mode' buffers.")

(defstruct (tla--revision)
  revision ;; The revision, as a list
  summary creator date
  merges ;; List of patches merged by this revision
  body   ;; Body of the log file (after headers)
  log    ;; full log (redundant with other fields)
  )

(defvar tla-revision-revision-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map dvc-mouse-2 'dvc-revlist-show-item-by-mouse)
    map)
  "Keymap used on revisions in `tla-revision-list-mode' buffers.")


;;
;; ChangeLog mode section
;;
(defvar tla-changelog-mode-map
  (let ((map (copy-keymap change-log-mode-map)))
    (suppress-keymap map)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map [?n] 'tla-changelog-next-entry)
    (define-key map [?p] 'tla-changelog-previous-entry)
    (define-key map [?=] 'tla-changelog-show-changeset)
    (define-key map [?M] 'tla-send-commit-notification)
    (define-key map "\C-m" 'tla-press-button)
    (define-key map dvc-mouse-2 'tla-push-button)
    ;;
    ;; Kill ring group
    ;;
    (define-key map dvc-keyvec-kill-ring nil)
    (define-key map (dvc-prefix-kill-ring ?l) 'tla-changelog-save-log-message-as-kill)
    (define-key map (dvc-prefix-kill-ring ?r) 'tla-changelog-save-revision-as-kill)
    (define-key map (dvc-prefix-kill-ring ?v) 'tla-changelog-save-version-as-kill)
    map)
  "Keymap used in `tla-changelog-mode'.")


;;
;; Log edit buffer mode section
;;

(defvar tla-log-edit-keywords-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?n] 'tla-log-edit-keywords-next)
    (define-key map [?p] 'tla-log-edit-keywords-previous)
    (define-key map [?m] 'tla-log-edit-keywords-mark)
    (define-key map [?u] 'tla-log-edit-keywords-unmark)
    (define-key map [?t] 'tla-log-edit-keywords-toggle-mark)
    (define-key map [?* ?!] 'tla-log-edit-keywords-unmark-all)
    (define-key map [?* ?*] 'tla-log-edit-keywords-mark-all)
    (define-key map "\C-c\C-c" 'tla-log-edit-keywords-insert)
    map)
  "Keymap used in tla-log-edit-keywords-mode buffers.")


;; ----------------------------------------------------------------------------
;; Menu entries
;; ----------------------------------------------------------------------------
;;
;; Conventions
;;
;; 1. Each Nouns and verbs in menu items are should be capitalized.
;; 2. TODO: Consider menu items order.

;;
;; Common submenus
;;

(defconst tla-.arch-inventory-menu-list
  '("Put to .arch-inventory"
    ["Junk"         tla-generic-add-to-junk         t]
    ["Backup"       tla-generic-add-to-backup       t]
    ["Precious"     tla-generic-add-to-precious     t]
    ["Unrecognized" tla-generic-add-to-unrecognized t]))

(defconst tla-=tagging-method-menu-list
  '("Put to =tagging-method"
    ["Junk"         (tla-generic-add-to-junk t)     t]
    ["Backup"       (tla-generic-add-to-backup t)   t]
    ["Precious"     (tla-generic-add-to-precious t) t]
    ["Unrecognized" (tla-generic-add-to-junk t)     t]))

;;
;; Bookmarks mode
;;
(defconst tla-bookmarks-entry-menu-list
  '("Bookmark Entry"
    ["Delete"         tla-bookmarks-delete    t]
    ["Goto Location"  tla-bookmarks-goto      t]
    ("File Tree"
     ["Find File"      tla-bookmarks-find-file t]
     ["Run Dired"      tla-bookmarks-open-tree t]
     ["Run Inventory"  tla-bookmarks-inventory t]
     ["View Changes"   tla-bookmarks-changes t]
     )
    ("Merge/Tag"
     ["View Missing Patches" tla-bookmarks-missing t]
     ["Replay"       tla-bookmarks-replay  t]
     ["Update"       tla-bookmarks-update  t]
     ["Star-merge"   tla-bookmarks-star-merge t]
     ["Tag"          tla-bookmarks-tag     t]
     )
    ("Edit"
     ["Edit Bookmark"    tla-bookmarks-edit t]
     ["Add Nickname"     tla-bookmarks-add-nickname-interactive    t]
     ["Remove Nickname"  tla-bookmarks-delete-nickname-interactive t]
     ["Add Local Tree"   tla-bookmarks-add-tree-interactive        t]
     ["Remove Local Tree" tla-bookmarks-delete-tree-interactive    t]
     ["Add Group"        tla-bookmarks-add-group-interactive       t]
     ["Remove Group"     tla-bookmarks-delete-group-interactive    t]
     ["Add Partner"      tla-bookmarks-add-partner-interactive     t]
     ["Remove Partner"   tla-bookmarks-delete-partner-interactive  t]
     )
    ("Partners"
     ["Add Partner"      tla-bookmarks-add-partner-interactive     t]
     ["Remove Partner"   tla-bookmarks-delete-partner-interactive  t]
     ["Write to Partner File" tla-bookmarks-write-partners-to-file t]
     ["Load from Partner File" tla-bookmarks-add-partners-from-file t]
     ["View Missing Patches" tla-bookmarks-missing t]
     ))
  "Used both for the local and the global menu."
  )

(easy-menu-define tla-bookmarks-mode-menu tla-bookmarks-mode-map
  "`tla-bookmarks-mode' menu"
  `("Xtla-Bookmarks"
    ["Add Bookmark" tla-bookmarks-add t]
    ["Show Details" tla-bookmarks-toggle-details
     :style toggle :selected tla-bookmarks-show-details]
    ["Select by Group" tla-bookmarks-select-by-group t]
    ["Cleanup 'local-tree fields" tla-bookmarks-cleanup-local-trees t]
    ,tla-bookmarks-entry-menu-list
    ))

(easy-menu-define tla-bookmarks-entry-menu nil
  "Menu used on a tla bookmark entry."
  tla-bookmarks-entry-menu-list)

;;
;; Inventory mode
;;
(easy-menu-define tla-inventory-mode-partners-menu tla-inventory-mode-map
  "`tla-inventory-mode' partners menu"
  '("Partners"
    ["Add Partner..." tla-partner-add t]
    ("Set Tree Version" :filter (lambda (x)
                                  (tla--partner-create-menu
                                   'tla-generic-set-tree-version)))
    "--"
    ("Show Changes" :filter (lambda (x)
                              (tla--partner-create-menu
                               '(lambda (x)
                                  (tla-changes current-prefix-arg
                                               (list tla-arch-branch
                                                     (list 'revision
                                                           (tla--name-split x))))))))
    ("Show Missing" :filter (lambda (x)
                              (tla--partner-create-menu
                               '(lambda (x)
                                  (tla-missing-1 default-directory x)))))
    ("Show ChangeLog" :filter (lambda (x)
                                (tla--partner-create-menu
                                 '(lambda (x)
                                    (tla-changelog x)))))
    "--"
    ("Replay" :filter (lambda (x)
                        (tla--partner-create-menu
                         'tla-inventory-replay)))
    ("Star-merge" :filter (lambda (x)
                            (tla--partner-create-menu
                             'tla-inventory-star-merge)))))

(defconst tla-inventory-item-menu-list
  `("Inventory Item"
    ["Open File" tla-inventory-find-file t]
    ["View File" dvc-view-file t]
    "--"
    ["Add"    tla-inventory-add-files    t]
    ["Move"   tla-inventory-move         t]
    ["Revert" tla-inventory-revert       t]
    ["Remove" tla-inventory-remove-files t]
    ["Delete" tla-inventory-delete-files t]
    "--"
    ["Make Junk"     tla-inventory-make-junk     t]
    ["Make Precious" tla-inventory-make-precious t]
    ,tla-.arch-inventory-menu-list
    ,tla-=tagging-method-menu-list)
  "Used both in the context and the global menu for inventory.")

(easy-menu-define tla-inventory-mode-menu tla-inventory-mode-map
  "`tla-inventory-mode' menu"
  `("Inventory"
    ["Edit Log" tla-inventory-edit-log t]
    "--"
    ["Show Changes"   tla-inventory-changes t]
    ["Show Changelog" tla-changelog t]
    ["Show Logs"      tla-logs t]
    ["Show Missing"   tla-inventory-missing t]
    "--"
    ,tla-inventory-item-menu-list
    "--"
    ["Update"     tla-inventory-update t]
    ["Replay"     tla-inventory-replay t]
    ["Star-merge" tla-inventory-star-merge t]
    ("Changesets"
     ["Save actual changes in directory" tla-changes-save t]
     ["Save actual changes in tarball" tla-changes-save-as-tgz t]
     ["View changeset from directory" tla-show-changeset t]
     ["View changeset from tarball" tla-show-changeset-from-tgz t]
     ["Apply changeset from directory" tla-inventory-apply-changeset t]
     ["Apply changeset from tarball" tla-inventory-apply-changeset-from-tgz t]
     )
    "--"
    ["Undo" tla-inventory-undo t]
    ["Redo" tla-inventory-redo t]
    "--"
    ["Synchronize Mirror" tla-inventory-mirror t]
    ("Taging Method"
     ["Edit .arch-inventory" tla-edit-.arch-inventory-file t]
     ["Edit =tagging-method" tla-edit-=tagging-method-file t]
     ["Set Tagging Method"   tla-generic-set-id-tagging-method t]
     ["Set Tree Version From Scratch" tla-generic-set-tree-version t]
     )
    ["Tree-lint" tla-tree-lint t]
    "--"
    ("Toggles"
     ["Set All Toggle Variables" tla-inventory-set-all-toggle-variables t]
     ["Reset All Toggle Variables" tla-inventory-reset-all-toggle-variables t]
     ["Toggle All Toggle Variables" tla-inventory-toggle-all-toggle-variables t] .
     ,(mapcar '(lambda (elem) `[,(concat "Toggle " (car (cddddr elem)))
                                ,(car (cddr elem))
                                :style toggle
                                :selected ,(cadr elem)])
              tla-inventory-file-types-manipulators))))

(easy-menu-define tla-inventory-item-menu nil
  "Menu used on a inventory item."
  tla-inventory-item-menu-list)

(easy-menu-define tla-inventory-tagging-method-menu nil
  "Menu used on the taggine method line in a inventory buffer."
  '("Switch Taggine Method"
    ["Tagline"  (tla-generic-set-id-tagging-method "tagline") t]
    ["Explicit" (tla-generic-set-id-tagging-method "explicit") t]
    ["Names"    (tla-generic-set-id-tagging-method "names") t]
    ["Implicit" (tla-generic-set-id-tagging-method "implicit") t]))

;;
;; revlog mode
;;
(easy-menu-define tla-revlog-mode-menu tla-revlog-mode-map
  "'tla-revlog-mode' menu"
  '("Revlog"
    ["Inventory" dvc-pop-to-inventory t]
    ["Show Changeset" tla-log-get-changeset t]
    ["Quit" dvc-buffer-quit t]
    ))

;;
;; Log edit mode
;;
(easy-menu-define tla-log-edit-mode-menu tla-log-edit-mode-map
  "`tla-log-edit-mode' menu"
  '("Log"
    ["Insert tla log-for-merge" tla-log-edit-insert-log-for-merge t]
    ["log-for-merge and headers"
     tla-log-edit-insert-log-for-merge-and-headers t]
    ["Insert memorized log"     tla-log-edit-insert-memorized-log t]
    ["Show changes"             tla-changes                       t]
    ["Commit"                   tla-log-edit-done                 t]
    ("Manage Version"
     ["Commit with Sealing"     tla-log-edit-done-with-sealing    t]
     ["Commit with Fixing"      tla-log-edit-done-with-fixing     t])
    ["Show Changelog"           tla-changelog                     t]
    "--"
    ["Goto Summary Field"       tla-log-goto-summary              t]
    ["Goto Body"                tla-log-goto-body                 t]
    ["Edit Keywords Field"      tla-log-edit-keywords             t]
    ["Kill Body"                tla-log-kill-body                 t]
    "--"
    ["Tree Lint"                tla-tree-lint                     t]
    ["Abort"                    tla-log-edit-abort                t]))

;;
;; Archive list mode
;;
(easy-menu-define tla-archive-list-mode-menu tla-archive-list-mode-map
  "`tla-archive-list-mode' menu"
  '("Archives"
    ["Register New Archive"        tla-register-archive t]
    ["Add a Bookmark"              tla-bookmarks-add t]
    ["Update Archives List"        tla-archives t]
    ["Set Default Archive"         tla-archive-select-default t]
    ["Remove Archive Registration" tla-archive-unregister-archive t]
    ["Edit Archive Location"       tla-archive-edit-archive-location t]
    ["Make New Archive..."         tla-make-archive t]
    ["Create a Mirror"             tla-archive-mirror-archive t]
    ["Use as default Mirror"       tla-archive-use-as-default-mirror t]
    ["Synchronize Mirror"          tla-archive-synchronize-archive t]
    ))

;;
;; Category list mode
;;
(easy-menu-define tla-category-list-mode-menu tla-category-list-mode-map
  "`tla-category-list-mode' menu"
  '("Categories"
    ["List Archives"          tla-archives                t]
    ["Update Categories List" tla-category-refresh         t]
    ["Make New Category..."   tla-category-make-category  t]
    ["Add a Bookmark"         tla-bookmarks-add           t]
    ["Synchronize Mirror"     tla-category-mirror-archive t]
    ))


;;
;; Branch list mode
;;
(easy-menu-define tla-branch-list-mode-menu tla-branch-list-mode-map
  "`tla-branch-list-mode' menu"
  '("Branches"
    ["Update Branches List" tla-branch-refresh               t]
    ["List Parent Category" tla-branch-list-parent-category t]
    ["Make New Branch..."   tla-branch-make-branch          t]
    ["Synchronize Mirror"   tla-branch-mirror-archive       t]
    ["Bookmark Branch under Point"    tla-branch-bookmarks-add        t]
    ["Get..."               tla-branch-get-branch           t]
    ))

;;
;; Version list mode
;;
(easy-menu-define tla-version-list-mode-menu tla-version-list-mode-map
  "`tla-version-list-mode' menu"
  '("Versions"
    ["Update Versions List" tla-version-refresh             t]
    ["Get..."               tla-version-get-version        t]
    ["Make New Version..."  tla-version-make-version       t]
    ["List Parent Branch"   tla-version-list-parent-branch t]
    ["Synchronize Mirror"   tla-version-mirror-archive     t]
    ["Bookmark Version under Point"    tla-version-bookmarks-add      t]
    ["Tag This Version"     tla-version-tag      t]))

;;
;; Revision list mode
;;
(easy-menu-define tla-revision-list-mode-menu tla-revision-list-mode-map
  "`tla-revision-list-mode' menu"
  '("Revisions"
    ["Refresh Revisions List" dvc-generic-refresh t]
    ["List Parent Version"    tla-revision-list-parent-version t]
    ["Show all revisions" tla-missing-show-all-revisions t]
    "--"
    ["Bookmark Revision under Point"      tla-revision-bookmarks-add t]
    ("Mark"
     ["Mark Revision"   dvc-revision-mark-revision t]
     ["Unmark Revision" dvc-revision-unmark-revision t])
    "--"
    ["Show Log"                            tla-revision-revlog t]
    ["Unify Patch Logs with This Revision" tla-revision-sync-tree t]
    ["View changeset"                      tla-revision-changeset t]
    ("Delta"
     ["View"  (tla-revision-delta t) t]
     ["Store to Directory" (tla-revision-store-delta t) t])
    "--"
    ["Update" tla-revision-update t]
    ("Replay"
     ["From Head Revision" tla-revision-replay-version t]
     ["From Revision under Point" tla-revision-replay t]
     ["Revision under Point Reversely" (tla-revision-replay 'reversely) t])
    ("Star-Merge"
     ["From Head Revision" tla-revision-star-merge-version t]
     ["From Revision under Point" tla-revision-star-merge t])
    ("Get"
     ["Get a Local Copy" tla-revision-get-revision t]
     ["Make Cache"       tla-revision-cache-revision t]
     ["Add to Library"   tla-revision-add-to-library t])
    ("Tag "
     ["From Head Revision" tla-revision-tag-from-head t]
     ["From Revision under Point" tla-revision-tag-from-here t])
    ["Send comment to author" tla-revision-send-comments t]
    "--"
    ("Filter Display"
     ["Date"    dvc-revlist-toggle-date
      :style toggle :selected dvc-revisions-shows-date]
     ["Creator" dvc-revlist-toggle-creator
      :style toggle :selected dvc-revisions-shows-creator]
     ["Summary" dvc-revlist-toggle-summary
      :style toggle :selected dvc-revisions-shows-summary]
     ["Presence in Revlib" tla-revision-toggle-library
      :style toggle :selected tla-revisions-shows-library]
     ["Merged Patches"   tla-revision-toggle-merges
      :style toggle :selected tla-revisions-shows-merges]
     ["Patches Merging ..." tla-revision-toggle-merged-by
      :style toggle :selected tla-revisions-shows-merged-by])))

(easy-menu-define tla-revision-revision-menu nil
  "Menu used on a revision item in `tla-revision-list-mode' buffer"
  '("Revision"
    ["Show Log"        tla-revision-revlog t]
    ["Unify Patch Logs with This Revision" tla-revision-sync-tree t]
    ["View changeset"  tla-revision-changeset t]
    ["Set Bookmark"    tla-revision-bookmarks-add t]
    ("Mark"
     ["Mark Revision"   dvc-revision-mark-revision t]
     ["Unmark Revision"   dvc-revision-unmark-revision t])
    ("Delta"
     ["In This Version"                     tla-revision-delta t]
     ["With Revision in Another Archive"    tla-revision-store-delta t])
    ("Merge"
     ["Star-Merge"       tla-revision-star-merge t]
     ["Replay"           tla-revision-replay t]
     ["Replay Reversely" (tla-revision-replay 'reversely) t])
    ("Get"
     ["Get a Local Copy" tla-revision-get-revision t]
     ["Make Cache"       tla-revision-cache-revision t]
     ["Add to Library"   tla-revision-add-to-library t])
    ["Send comment to author" tla-revision-send-comments t]
    ["Tag from Here"      tla-revision-tag-from-here]))

;;
;; Lint mode
;;
(defconst tla-tree-lint-file-menu-list
  `("File"
    ["Jump to File"  dvc-find-file-at-point t]
    ("Mark"
     ["Mark File" tla-tree-lint-mark-file t]
     ["Unmark File" tla-tree-lint-unmark-file t])
    "--"
    ["Add File"      tla-tree-lint-add-files        t]
    ["Delete File"   tla-tree-lint-delete-files     t]
    ["Regenerate ID" tla-tree-lint-regenerate-id    t]
    "--"
    ["Make Junk"     tla-tree-lint-make-junk        t]
    ["Make Precious" tla-tree-lint-make-precious     t]
    ,tla-.arch-inventory-menu-list
    ,tla-=tagging-method-menu-list
    )
  "Used both for context and global menu.")

(easy-menu-define tla-tree-lint-file-menu nil
  "Menu used on files listed in `tla-tree-lint'"
  tla-tree-lint-file-menu-list
  )

(easy-menu-define tla-tree-lint-mode-menu tla-tree-lint-mode-map
  "`tla-tree-lint' menu"
  `("Tree Lint"
    ["Refresh Buffer"         dvc-generic-refresh t]
    ,tla-tree-lint-file-menu-list
    ))

;;
;; Event Log buffer
;;
(easy-menu-define dvc-log-buffer-mode-menu dvc-log-buffer-mode-map
  "`dvc-log-buffer' menu"
  '("Logs"
    ["Show Related Buffer" dvc-switch-to-related-buffer t]
    ["Show Output Buffer"  dvc-switch-to-output-buffer  t]
    ["Show Error Buffer"   dvc-switch-to-error-buffer   t]
    ))


;; ----------------------------------------------------------------------------
;; User customization section
;; ----------------------------------------------------------------------------

;;;###autoload
(defgroup xtla nil
  "Arch interface for emacs."
  :group 'dvc
  :prefix "tla-")

(defgroup tla-inventory nil
  "This group contains items used in inventory mode."
  :group 'xtla)

(defgroup tla-revisions nil
  "This group contains items used in revisions mode."
  :group 'xtla)

(defgroup tla-bindings nil
  "This group contains items related to key bindings."
  :group 'xtla)

(defgroup tla-faces nil
  "This group contains faces defined for Xtla."
  :group 'dvc-faces)

(defcustom tla-executable (dvc-first-set
                              dvc-site-tla-executable
                            "tla")
  "*The name of the tla executable."
  :type 'string
  :group 'xtla)

(defcustom baz-executable (dvc-first-set
                              dvc-site-baz-executable
                            "baz")
  "*The name of the baz executable.
baz is the command name for bazaar, a branch of tla."
  :type 'string
  :group 'xtla)

(defcustom tla-arch-branch (dvc-first-set
                               dvc-site-arch-branch
                             (if (executable-find
                                  baz-executable)
                                 'baz
                               'tla))
  "*Branch of GNU Arch to use.
Currently supported are 'tla and 'baz."
  :type '(choice (const baz)
                 (const tla)
                 (const :tag "No tla variant installed" none))
  :group 'xtla)

(defcustom tla-install-command-help-system t
  "*Use f1 to display help for the actual function call during minibuffer input.
Note: this functionality is provided for all minibuffer prompts."
  :type 'boolean
  :group 'xtla)

(defcustom tla-changes-recursive t
  "*Whether or not Xtla will compute changes recursively.

If non nil, `tla-changes' will be applied recursively to subprojects
of the current tree"
  :type 'boolean
  :group 'xtla)

(defcustom tla-update-recursive t
  "*Whether or not Xtla will run update recursively.

If non nil, `tla-update' will be applied recursively to subprojects
of the current tree"
  :type 'boolean
  :group 'xtla)

(defcustom tla-strict-commits nil
  "*If non-nil, commit operations are invoked with the --strict option."
  :type 'boolean
  :group 'xtla)

(defcustom tla-commit-check-log-buffer-functions
  '(tla-commit-check-empty-headers
    tla-commit-check-empty-line
    tla-commit-check-missing-space)
  "*List of functions to check the ++log.. buffer.

Each function is called, from the log buffer, with no argument. It
should raise an error if commit should be canceled."
  :type 'hook
  :group 'xtla)

(defcustom tla-commit-headers-allowed-to-be-empty
  "^\\(Keywords\\)$"
  "*Headers allowed to be empty in the ++log.. buffer.

This should be a regexp matching the header names. Headers not
matching this regexp should not be empty when committing."
  :type 'string
  :group 'xtla)

(defcustom tla-commit-fix-missing-space t
  "*Whether or not Xtla will add missing spaces after header names.

If non-nil, missing spaces after a space will be inserted
automatically instead of raising an error when committing."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-three-way-merge t
  "*If non-nil, merge operations are invoked with --three-way.
\(or without --two-way for branches of arch in which --three-way is the
default)."
  :type 'boolean
  :group 'xtla)

;;;###autoload
(defcustom tla-show-ancestor nil
  "*If non-nil, merge operations are invoked with --show-ancestor.

With this option, conflicts markers will include TREE, MERGE-SOURCE,
and ancestor versions. `smerge-ediff' allows you to view the ancestor
with `ediff-show-ancestor' (usually bound to `/').

Unfortunately, this will also report more conflicts: Conflicts will be
reported even when TREE and MERGE-SOURCE are identical, if they differ
from ANCESTOR."
  :type 'boolean
  :group 'xtla)

(defcustom tla-update-strategy 'merge
  "*Which strategy to apply for `tla-update'.

\"baz merge\" has the advantage of being able to use a 3-way merge.
\"baz replay\" is the fastest: No need to build any reference tree.
\"baz update\" is \"safe\": The local changes are backed-up before
updating.

In the absence of conflicts, the result should be identical. In the
case of conflicts:

\"baz merge\" will leave diff3 inline markers in the code.
\"baz update\" will leave the rejected changes from YOUR modifications
in .rej files.
\"baz replay\" will leave the rejected changes from THE ARCHIVE
modifications in .rej files. It also stops when it encounters
conflicts, so it doesn't always apply every upstream change."
  :type '(choice (const 'update)
                 (const 'merge)
                 (const 'replay))
  :group 'xtla)

;;;###autoload
(defcustom tla-non-recursive-inventory t
  "*If non-nil, inventory is run with --no-recursion (if available)."
  :type 'boolean
  :group 'xtla)

;; --forward is actually a no-op !
;; ;;;###autoload
;; (defcustom tla-use-forward-option nil
;;   "*If non-nil, use the --forward option with commands that allow it."
;;   :type 'boolean
;;   :group 'xtla)

(defcustom tla-tag-does-cacherev 'ask
  "*Whether \"tla tag\" or \"baz branch\" should create a cacherev.

Supported values are:
 'yes
 'no
 'ask"
  :type '(choice (const 'yes)
                 (const 'no)
                 (const 'ask))
  :group 'xtla)

;;;###autoload
(defcustom tla-use-skip-present-option nil
  "*If non-nil, use --skip-present with commands that allow it."
  :type 'boolean
  :group 'xtla)

;; ;;;###autoload
;; (defun tla-toggle-use-forward-option ()
;;   "Toggle the value of `tla-use-forward-option'."
;;   (interactive)
;;   (setq tla-use-forward-option (not tla-use-forward-option)))

(defun tla-toggle-use-skip-present-option ()
  "Toggle the value of `tla-use-skip-present-option'."
  (interactive)
  (setq tla-use-skip-present-option
        (not tla-use-skip-present-option)))

;;;###autoload
(defun tla-toggle-three-way-merge ()
  "Toggle the value of `tla-three-way-merge'."
  (interactive)
  (setq tla-three-way-merge (not tla-three-way-merge)))

;;;###autoload
(defun tla-toggle-show-ancestor ()
  "Toggle the value of `tla-show-ancestor'."
  (interactive)
  (setq tla-show-ancestor (not tla-show-ancestor)))

;;;###autoload
(defun tla-toggle-non-recursive-inventory ()
  "Toggle the value of `tla-toggle-non-recursive-inventory'."
  (interactive)
  (setq tla-non-recursive-inventory
        (not tla-non-recursive-inventory)))

(defgroup tla-bookmarks nil
  "Xtla bookmarks allows you to save places (archive, category,
branch, version) in the archive that you use often. Try M-x
tla-bookmarks RET to see."
  :group 'xtla)

(defcustom tla-bookmarks-file-name "bookmarks.el"
  "*File in which Xtla bookmarks will be saved.
The bookmark file is stored in the `dvc-config-directory'"
  :type 'file
  :group 'tla-bookmarks)

(defcustom tla-tag-function 'tla-tag-uuid
  "Function called to generate the value of the arch-tag.

The function must take no argument, and return a string without a
final newline."
  :type '(choice (const tla-tag-uuid)
                 (const tla-tag-name-date-filename)
                 function)
  :group 'xtla)

(defcustom tla-log-library "~/.arch-log-library/"
  "*Directory in which the log library will be stored."
  :type 'directory
  :group 'xtla)

(defcustom tla-log-library-greedy t
  "*Whether log files are automatically saved in the log library.

If non-nil, then, whenever Xtla needs to access a log file, this file
will be copied to the log library."
  :type 'boolean
  :group 'xtla)

(defcustom tla-bookmarks-cleanup-dont-prompt nil
  "*Whether Xtla should prompt before cleaning a local tree.

non nil means `tla-bookmarks-cleanup-local-trees' shouldn't prompt
before removing a local-tree"
  :type 'boolean
  :group 'tla-bookmarks)

(defcustom tla-send-comments-width 25
  "*Max length for the summary line when using %t in
`tla-send-comments-format'."
  :type 'integer
  :group 'xtla)

(defcustom tla-send-comments-format "Your patch %c--%b--%v--%r (%t)"
  "Format for the Subject line for `tla-revision-send-comments'.

The following substring will be substituted:

%f: Full revision name
%a: The archive name
%c: The category name
%b: The branch name
%v: The version name
%r: The revision name
%s: The summary line
%t: The summary line, truncated to `tla-send-comments-width'
characters."
  :type 'string
  :group 'xtla)

(defcustom tla-switch-to-changes-buffer nil
  "Switch to the changes buffer or stay in the current buffer."
  :type 'boolean
  :group 'xtla)

(defgroup tla-hooks nil
  "This group contains hooks into Xtla."
  :group 'xtla)

(defcustom tla-commit-done-hook '()
  "*Hooks run after a successful commit via `tla-commit'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-archive-list-mode-hook '()
  "*Hooks run after switching to `tla-archive-list-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-bookmarks-mode-hook '()
  "*Hooks run after switching to `tla-bookmarks-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-branch-list-mode-hook '()
  "*Hooks run after switching to `tla-branch-list-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-revlog-mode-hook '()
  "*Hooks run after switching to `tla-revlog-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-category-list-mode-hook '()
  "*Hooks run after switching to `tla-category-list-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-inventory-file-mode-hook '()
  "*Hooks run after switching to `tla-inventory-file-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-inventory-mode-hook '()
  "*Hooks run after switching to `tla-inventory-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-log-edit-mode-hook '()
  "*Hooks run after switching to `tla-log-edit-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-logs-mode-hook '()
  "*Hooks run after switching to `tla-logs-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-revision-list-mode-hook '()
  "*Hooks run after switching to `tla-revision-list-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-version-list-mode-hook '()
  "*Hooks run after switching to `tla-version-list-mode'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-make-branch-hook '()
  "*Hooks run after making a branch."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-make-category-hook '()
  "*Hooks run after making a category."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-make-version-hook '()
  "*Hooks run after making a version."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-make-archive-hook '()
  "*Hooks run after creating a new archive."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-name-read-init-hook '()
  "*Hooks run when the control enters to `tla-name-read'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-name-read-final-hook '()
  "*Hooks run when the control leaves `tla-name-read'.
The name read by `tla-name-read' is passed to functions connected
to this hook as an argument."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-name-read-error-hook '()
  "*Hooks run when an error is occurred in `tla-name-read'."
  :type 'hook
  :group 'tla-hooks)

(defcustom tla-follow-symlinks 'tree
  "*Follow symlinks of this type."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Symlinks into an arch-managed tree" tree)
                 (const :tag "Symlinks to an arch-managed file" id))
  :group 'dvc-file-actions)

(defcustom tla-follow-symlinks-mode 'follow
  "*Before following a symlink do this."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Follow" follow)
                 (const :tag "Warn" warn))
  :group 'dvc-file-actions)

(defcustom tla-use-arrow-keys-for-navigation nil
  "*Enable left/right for navigation.
This works best if `dvc-switch-to-buffer-mode' is set to 'single-window.

It enables binding for navigation allowing you to browse by only using the
cursor keys, which is much faster than n/p/return/^.  Use up/down to move to
an item, right to select it and left to go to its \"logical\" parent!

Got the idea?

See the variable `tla-use-arrow-keys-for-navigation-list' for a list of
bindings that will be installed."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled"  t)
                 (const :tag "Enabled with Shift" shift))
  :group 'tla-bindings)

(defcustom tla-revisions-shows-library t
  "*Display library information in revision lists.

If non-nil the presence of this revision in the library should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-merges nil
  "*Display merge information in revision lists.

If non-nil, the list of merged patches of this revision should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-revisions-shows-merged-by t
  "*Display \"merged-by\" field in revision lists.

If non-nil the list of patches merged by this revision should be
displayed for `tla-revisions'"
  :type 'boolean
  :group 'tla-revisions)

(defcustom tla-log-edit-keywords
  '(
    ;; I am not sure how to group keywords ...
    "bugfix"                            ; should it be bugfix=BUGNO
    "docfix"
    "warnfix"
    "linting"                           ; whitespace only change
    ;;
    "newfeature"
    ;;
    "merge"
    "update"
    "rename"
    "delete"
    "newfile"
    )
  "A list of keywords for the Keywords field of a log message."
  :type '(repeat (string))
  :group 'xtla)

(defcustom tla-apply-patch-mapping nil
  "*Tree in which patches should be applied.

An alist of rules to map fully qualified revision names to target
directories.

This is used by the `tla-gnus-apply-patch' function.
Example setting: '(((nil \"xtla\" nil nil nil) \"~/work/tla/xtla\")))"
  :type '(repeat (list :tag "Rule"
                       (list :tag "Full revision (regexps)"
                             (choice (const :tag "Any archive" nil)
                                     (regexp :tag "Archive"))
                             (choice (const :tag "Any category" nil)
                                     (regexp :tag "Category"))
                             (choice (const :tag "Any branch" nil)
                                     (regexp :tag "Branch"))
                             (choice (const :tag "Any version" nil)
                                     (regexp :tag "Version"))
                             (choice (const :tag "Any revision" nil)
                                     (string :tag "Revision")))
                       (string :tag "Target directory")))
  :group 'xtla)

(defcustom tla-submit-patch-mapping
  '(((nil "xtla" nil  nil nil) ("xtla-el-dev@gna.org" "xtla")))
  "*Email addresses that should be used to send patches

An alist of rules to map fully qualified revision names to target
email addresses and the base name to use in the attached patch.

This is used by the `tla-submit-patch' function."
  :type '(repeat (list :tag "Rule"
                       (list :tag "Full revision (regexps)"
                             (choice (const :tag "Any archive" nil)
                                     (regexp :tag "Archive"))
                             (choice (const :tag "Any category" nil)
                                     (regexp :tag "Category"))
                             (choice (const :tag "Any branch" nil)
                                     (regexp :tag "Branch"))
                             (choice (const :tag "Any version" nil)
                                     (regexp :tag "Version"))
                             (choice (const :tag "Any revision" nil)
                                     (string :tag "Revision")))
                       (list :tag "Target"
                             (string :tag "Email address")
                             (string :tag "Base name of tarball"))))
  :group 'xtla)

(defcustom tla-patch-sent-action 'keep-tarball
  "*What shall be done, after sending a patch via mail.
The possible values are 'keep-tarball, 'keep-changes, 'keep-both, 'keep-none."
  :type '(choice (const keep-tarball)
                 (const keep-changes)
                 (const keep-both)
                 (const keep-none))
  :group 'xtla)

;;example:
;;(setq tla-mail-notification-destination
;;      '(((nil "xtla" nil  nil nil) ("[commit][xtla 1.2] " "xtla-el-dev@gna.org"))))
(defcustom tla-mail-notification-destination nil
  "*Preset some useful values for commit emails.

An alist of rules to map fully qualified revision names to target
email addresses and the prefix string for the subject line.

This is used by the `tla-send-commit-notification' function."
  :type '(repeat (list :tag "Rule"
                       (list :tag "Full revision (regexps)"
                             (choice (const :tag "Any archive" nil)
                                     (regexp :tag "Archive"))
                             (choice (const :tag "Any category" nil)
                                     (regexp :tag "Category"))
                             (choice (const :tag "Any branch" nil)
                                     (regexp :tag "Branch"))
                             (choice (const :tag "Any version" nil)
                                     (regexp :tag "Version"))
                             (choice (const :tag "Any revision" nil)
                                     (string :tag "Revision")))
                       (list :tag "Target"
                             (string :tag "Email subject prefix")
                             (string :tag "Email address"))))
  :group 'xtla)

(defgroup tla-merge nil
  "Merging with Xtla."
  :group 'xtla)

(defcustom tla-version-to-name-function nil
  "*Function returning a name for a version.

If non-nil, it must be a function that is called with the version as
an argument, and must return a string that will be used to instead of
the nickname.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)

(defcustom tla-generate-line-function nil
  "*Function generating a string summarizing the merge.

If non-nil, it must be a function that is called with a list like
\((\"Robert\" 167 168 170) (\"Masatake\" 209 213 214 215 217 218)) as
an argument, and must return a string.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)


(defcustom tla-format-line-function nil
  "*Function formatting the summary line.

If non-nil, it must be a function that is called with a string as an
argument, and returns another string (typically adding a \"Merges \"
comment in front of it.

See `tla-merge-summary-line-for-log'."
  :type '(choice (const nil)
                 function)
  :group 'tla-merge)

(defcustom tla-description-format
  '(patch-id "\n  " summary "\n  Located at: " location "\n")
  "*Format to use to display description of patch-id.

Must be a list. Each element is either
 - A string to be inserted.
 - The symbol 'patch-id => print the patch-id as entered in the
   prompt.
 - The symbol 'summary => if patch-id is actually a patch level,
   insert its summary line.
 - The symbol 'location => insert the location of the archive."
  :type '(repeat (choice symbol string))
  :group 'xtla)

(defcustom tla-dont-hyperlink-changelog nil
  "*If non-nil, don't insert hyperlink in ChangeLog buffer.

Hyperlink are sometimes long to set up with large ChangeLogs ..."
  :type 'boolean
  :group 'xtla)


;; ----------------------------------------------------------------------------
;; Face
;; ----------------------------------------------------------------------------


(defface tla-archive-name
  '((t (:inherit dvc-repository-name)))
  "Face to highlight Xtla archive names."
  :group 'tla-faces)

(defface tla-source-archive-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight Xtla source archive names."
  :group 'tla-faces)

(defface tla-mirror-archive-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight Xtla mirror archive names."
  :group 'tla-faces)

(defface tla-category-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight Xtla category names."
  :group 'tla-faces)

(defface tla-branch-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight Xtla branch names."
  :group 'tla-faces)

(defface tla-version-name
  '((t (:inherit tla-archive-name)))
  "Face to highlight Xtla version names."
  :group 'tla-faces)

(defface tla-tagging-method
  '((t (:inherit tla-archive-name)))
  "Face to highlight tagging methods."
  :group 'tla-faces)

(defface tla-junk
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight junk entries"
  :group 'dvc-faces)


;; ----------------------------------------------------------------------------
;; Font lock keywords
;; ----------------------------------------------------------------------------

;;
;; Inventory file mode
;;
(defvar tla-inventory-file-font-lock-keywords
  '(
    ("^#.*$" . 'dvc-comment)
    ("^[ \t]*\\(backup\\|exclude\\|junk\\|precious\\|unrecognized\\|source\\)\\>[  ]*\\(.*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^[ \t]*\\(untagged-source\\)"
     (1 font-lock-builtin-face))
    ("^[ \t]*\\(untagged-source\\) \\(precious\\|source\\|backup\\|junk\\|unrecognized\\)\\>"
     (1 font-lock-builtin-face)
     (2 font-lock-keyword-face))
    ("^[ \t]*\\(explicit\\|tagline\\|names\\|implicit\\)\\>"
     (1 font-lock-builtin-face))
    )
  "Keywords in tla-inventory-file mode.")

;;
;; Logs mode
;;
(defvar tla-logs-font-lock-keywords
  '(("^[^ \t]*\\(base\\|patch\\|version\\(fix\\)?\\)-[0-9]+" .
     font-lock-function-name-face))
  "Keywords in tla-logs-mode.")

;;
;; Revlog mode
;;
(defvar tla-revlog-font-lock-keywords
  '(("^\\(Revision\\|Archive\\|Creator\\|Date\\|Standard-date\\|Modified-files\\|New-patches\\|Summary\\|Keywords\\|New-files\\|New-directories\\|Removed-files\\|Removed-directories\\|Renamed-files\\|Renamed-directories\\|Modified-directories\\|Removed-patches\\):" . font-lock-function-name-face))
  "Keywords in `tla-revlog-mode'.")

;;
;; Log edit mode
;;
(defvar tla-log-edit-font-lock-keywords
  `(("^Summary: " . 'dvc-header)
    ("^Keywords: " . 'dvc-header)
    ("^\t?\\* \\([^ ,:([\n]+\\)"
     (1 'change-log-file-face)
     ("\\=, \\([^ ,:([\n]+\\)" nil nil
      (1 'change-log-file-face))
     ("\\= (\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face))
     ("\\=, *\\([^) ,:\n]+\\)" nil nil
      (1 'change-log-list-face)))
    (,(concat "^" (regexp-quote dvc-log-edit-file-list-marker) "$")
     . 'dvc-header))
  "Keywords in tla-log-edit mode.")

;;
;; Changes mode
;;
(defvar tla-changes-font-lock-keywords
  (append
   '(("^\\* looking for .* to compare with$" . font-lock-function-name-face)
     ("^\\* comparing to .*$" . font-lock-function-name-face)
     ("^\\* dir metadata changed$" . font-lock-function-name-face)
     ("^\\* file metadata changed$" . font-lock-function-name-face)
     ("^\\* modified files" . font-lock-function-name-face)
     ("^\\* added files" . font-lock-function-name-face)
     ("^\\* removed files" . font-lock-function-name-face)
     ("^ +-?-/ .*$" . 'dvc-meta-info)
     ("^ +-- .*$" .   'dvc-meta-info)
     ("^ *T. .*$" .  'dvc-nested-tree))
   diff-font-lock-keywords)
  "Keywords in `tla-changes' mode.")

;;
;; ChangeLog mode
;;
(defvar tla-changelog-font-lock-keywords
  (append
   '(("    \\([^ ].+:\\)$" (1 'dvc-keyword))
     ("\t\\(patch-[0-9]+\\)" (1 'dvc-keyword))
     ("\t\\(base-0\\)" (1 'dvc-keyword))
     ("^#.*$" . 'dvc-comment))
   change-log-font-lock-keywords)
  "Keywords in `tla-changelog' mode.")


;; ----------------------------------------------------------------------------
;; Auto-mode-alist entries
;; ----------------------------------------------------------------------------
;;;###autoload
(add-to-list 'auto-mode-alist
             '("/\\(=tagging-method\\|\\.arch-inventory\\)$" .
               tla-inventory-file-mode))

;; ----------------------------------------------------------------------------
;; Hooks into other packages and/or functions
;; ----------------------------------------------------------------------------

;;
;; ediff
;;
(defvar tla-ediff-keymap (copy-keymap dvc-global-keymap)
  "Global keymap used by Xtla in the ediff control buffer.")

(define-key tla-ediff-keymap dvc-keyvec-log-entry 'tla-ediff-add-log-entry)

(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (define-key ediff-mode-map dvc-prefix-key tla-ediff-keymap)))

;;
;; find-file
;;
(autoload 'tla-find-file-hook "tla")
(add-hook 'find-file-hooks 'tla-find-file-hook)

;; ----------------------------------------------------------------------------
;; Enables arrow key navigation for left/right
;; ----------------------------------------------------------------------------
(defvar tla-use-arrow-keys-for-navigation-list
  '((tla-inventory-mode-map right 'tla-inventory-find-file)
    (tla-inventory-mode-map left 'tla-inventory-parent-directory)
    (tla-archive-list-mode-map right 'tla-archive-list-categories)
    (tla-archive-list-mode-map left 'dvc-buffer-quit)
    (tla-category-list-mode-map right 'tla-category-list-branches)
    (tla-category-list-mode-map left 'tla-archives)
    (tla-branch-list-mode-map right 'tla-branch-list-versions)
    (tla-branch-list-mode-map left 'tla-branch-list-parent-category)
    (tla-version-list-mode-map right 'tla-version-list-revisions)
    (tla-version-list-mode-map left 'tla-version-list-parent-branch)
    (tla-revision-list-mode-map left 'tla-revision-list-parent-version)
    (tla-revision-list-mode-map right 'dvc-revlist-show-item)
    (dvc-diff-mode-map left 'dvc-diff-jump-to-change)
    (dvc-diff-mode-map right 'dvc-diff-view-source)
    (tla-changelog-mode-map left 'dvc-buffer-quit)
    (dvc-process-buffer-mode-map left 'dvc-buffer-quit)
    (tla-bookmarks-mode-map right 'tla-bookmarks-inventory)
    ))

(defun tla-use-arrow-keys-for-navigation (&optional uninstall)
  "Bind the left/right keys for navigation.

This function will be called automatically if variable
`tla-use-arrow-keys-for-navigation' is non-nil.

If argument UNINSTALL is non-nil, undefine the keys instead of
defining it."
  (interactive "P")
  ;; eval-after-load would be better.
  (unless (boundp 'dvc-diff-mode-map)
    (load-library "dvc-diff"))
  (let ((bl tla-use-arrow-keys-for-navigation-list) b
        (m tla-use-arrow-keys-for-navigation))
    (while bl
      (setq b (car bl)
            bl (cdr bl))
      (eval
       (append (list 'define-key
                     (car b))
               (cond ((eq nil m)
                      (list (vector (cadr b)) nil))
                     ((eq 'shift m)
                      (if uninstall
                          (list (vector (list 'shift (cadr b))) nil)
                        (list (vector (list 'shift (cadr b))) (car (cddr b)))))
                     ((eq t m)
                      (if uninstall
                          (list (vector (cadr b)) nil)
                        (list (vector (cadr b)) (car (cddr b)))))))))
    (if uninstall
        (message "%sleft/right bindings for Xtla have been removed."
                 (if (eq 'shift m) "Shifted " ""))
      (message "%sleft/right bindings for Xtla have been installed."
               (if (eq 'shift m) "Shifted " "")))))

;; install them if customized
(if tla-use-arrow-keys-for-navigation
    (tla-use-arrow-keys-for-navigation))

(provide 'tla-defs)

;;; tla-defs.el ends here

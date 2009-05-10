;;; dvc-ui.el --- User interface (keybinding, menus) for DVC

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

(eval-and-compile
  (require 'dvc-utils))

(require 'dvc-register)

;;;###autoload
(eval-and-compile
  (require 'easymenu))

(require 'dvc-register)

;; ----------------------------------------------------------------------------
;; Key bindings
;; ----------------------------------------------------------------------------
;;
;; Conventions
;;
;; - Meta Rules:
;; 0. If you feel a binding odd more than 3 times, report it to dvc dev mailing
;;    list. Especially about some danger functions like undo, merge; and functions
;;    taking longer time to be executed.
;;
;; 1. Our key binding should not surprise "general users" even if the
;;    binding is convenience. Instead, provide hooks for customization.
;;    We know it is difficult to define "general users".
;;
;; 2. Write the result of discussion here.
;;
;; 3. See http://mail.gnu.org/archive/html/emacs-devel/2004-03/msg00608.html
;;
;;
;; - Generic Rules:
;;
;; 1. dvc-status should have similar key bindings to pcl-cvs.
;;    If a pcl-cvs's binding is too odd, talk it in dvc dev mailing list.
;;
;; 2. Define common prefix for command groups like '>'.
;;    So a key binding for a grouped command has following structure:
;;
;;    ?{prefix} ?{suffix}
;;
;;    e.g. `get something commands' should have `>' as prefix.
;;
;;    About suffix part, ? should show the help for the groups.
;;
;;    e.g. `help for `get something commands'' is >?.
;;
;;    BTW, The prefix ? is for help related command.
;;    So `? >' can stand for "show the help for get-something related
;;    command". In other word, prefix and suffix is swappable if
;;    prefix or suffix is `?'.
;;
;; 3. Upper case for commands taking longer time to be executed.
;; 4. Lower case for commands taking shorter time to be executed.
;; 5. dired's binding is also helpful.
;;
;;
;; - Concrete Rules:
;;

;; t  ?    list all toggles
;; c       dvc-edit-log
;; RET     Open the thing at point
;;

;;
;; Definitions for key concrete rules
;;

;; common keys
;;;###autoload
(defvar dvc-key-help        ??)         ; help
(defvar dvc-key-mark-prefix ?*)         ; other mark related command prefix
(defvar dvc-key-add-bookmark    ?b)     ; add this to bookmark
(defvar dvc-key-get      ?>)            ; prefix for getting something
(defvar dvc-key-reflect  ?<)            ; mirror, apply, install...
(defvar dvc-key-parent   ?^)            ; visit uppper XXX. e.g. directory
;;;###autoload
(defvar dvc-key-diff     ?=)            ; one shot
;;;###autoload
(defvar dvc-key-status   ?s)            ; one shot

(defvar dvc-key-add      ?a)            ; prefix for adding something
;;;###autoload
(defvar dvc-key-show-bookmark ?b)       ; show bookmark
(defvar dvc-key-diff-prefix ?d)
;;;###autoload
(defvar dvc-key-file-diff ?d)
;;;###autoload
(defvar dvc-key-tree-lint ?l)
;;;###autoload
(defvar dvc-key-logs      ?L)
;;;###autoload
(defvar dvc-key-ediff     ?e)
;;;###autoload
(defvar dvc-key-log-entry ?a)
(defvar dvc-key-refresh   ?g)           ; refresh buffer
;;;###autoload
(defvar dvc-key-inventory ?i)           ; inventory
(defvar dvc-key-mark      ?m)           ; mark
(defvar dvc-key-next      ?n)           ; next item
(defvar dvc-key-previous  ?p)           ; previous item
(defvar dvc-key-quit      ?q)           ; quit
(defvar dvc-key-remove    ?r)           ; prefix for remove something
(defvar dvc-key-move      ?R)           ; prefix for move/rename something
(defvar dvc-key-toggle    ?t)           ; prefix for toggle
(defvar dvc-key-unmark    ?u)           ; unmark
(defvar dvc-key-popup-menu ?\C-j)
;;;###autoload
(defvar dvc-key-kill-ring-prefix ?w)
;;;###autoload
(defvar dvc-key-commit    ?c)           ; actually edit-log, but
                                        ; that's what you do when you
                                        ; want to commit.
;;;###autoload
(defvar dvc-key-update     ?u)          ; to run dvc update
(defvar dvc-key-replay     ?r)          ; to run dvc replay
(defvar dvc-key-star-merge ?s)          ; to run dvc star-merge
;;;###autoload
(defvar dvc-key-missing    ?m)          ; to run dvc missing

;;;###autoload
(defvar dvc-key-buffer-prefix ?B)       ; prefix to switch to XXX buffer
(defvar dvc-key-view-buffer-prefix ?v)  ; prefix to view XXX buffer
(defvar dvc-key-directory-prefix ?D)

;;;###autoload
(defvar dvc-key-file-prefix ?f)         ; file specific functions
(defvar dvc-key-branch-prefix ?o)       ; branch specific functions
(defvar dvc-key-merge-prefix ?M)
(defvar dvc-key-tag ?T)
(defvar dvc-key-revert ?U)
(defvar dvc-key-working-copy ?W)        ; Affecting on working copy
(defvar dvc-key-partner-file-prefix ?f) ; Only used in the bookmarks buffer
(defvar dvc-key-tagging-method-prefix ?#)
(defvar dvc-key-id ?t)                  ; `t' for `t'ag.

;; functions for creating key groups
;;;###autoload
(progn
  (defun dvc-key-group (prefix &rest keys)
    (apply 'vector prefix keys)))

(defun  dvc-prefix-toggle (&rest keys)
  (dvc-key-group dvc-key-toggle keys))

(defun dvc-prefix-add (&rest keys)
  (dvc-key-group dvc-key-add keys))

(defun dvc-prefix-remove (&rest keys)
  (dvc-key-group dvc-key-remove keys))

(defun dvc-prefix-move (&rest keys)
  (dvc-key-group dvc-key-move keys))

(defun dvc-prefix-mark (&rest keys)
  (dvc-key-group dvc-key-mark-prefix keys))

(defun dvc-prefix-diff (&rest keys)
  (dvc-key-group dvc-key-diff-prefix keys))

(defun dvc-prefix-merge (&rest keys)
  (dvc-key-group dvc-key-merge-prefix keys))

(defun dvc-prefix-directory (&rest keys)
  (dvc-key-group dvc-key-directory-prefix keys))

;;;###autoload
(progn
  (defun dvc-prefix-file (&rest keys)
    (dvc-key-group dvc-key-file-prefix keys)))

;;;###autoload
(progn
  (defun dvc-prefix-branch (&rest keys)
    (dvc-key-group dvc-key-branch-prefix keys)))

;;;###autoload
(progn
  (defun dvc-prefix-kill-ring (&rest keys)
    (dvc-key-group dvc-key-kill-ring-prefix keys)))

;;;###autoload
(progn
  (defun dvc-prefix-view-buffer (&rest keys)
    (dvc-key-group dvc-key-view-buffer-prefix keys)))

;;;###autoload
(progn
  (defun dvc-prefix-buffer (&rest keys)
    (dvc-key-group dvc-key-buffer-prefix keys)))

(defun dvc-prefix-working-copy (&rest keys)
  (dvc-key-group dvc-key-working-copy keys))

(defun dvc-prefix-partner-file (&rest keys)
  (dvc-key-group dvc-key-partner-file-prefix keys))

(defun dvc-prefix-tag (&rest keys)
  (dvc-key-group dvc-key-tag keys))

(defun dvc-prefix-tagging-method (&rest keys)
  (dvc-key-group dvc-key-tagging-method-prefix keys))

;; predefined key vectors
(defvar dvc-keyvec-toggle-set     (dvc-prefix-toggle ?+))
(defvar dvc-keyvec-toggle-reset   (dvc-prefix-toggle ?-))
(defvar dvc-keyvec-toggle-invert  (dvc-prefix-toggle ?~))

;;;###autoload
(defvar dvc-keyvec-help    (vector dvc-key-help))
(defvar dvc-keyvec-parent  (vector dvc-key-parent))
(defvar dvc-keyvec-add     (vector dvc-key-add))
(defvar dvc-keyvec-remove  (vector dvc-key-remove))
(defvar dvc-keyvec-get     (vector dvc-key-get))
(defvar dvc-keyvec-refresh (vector dvc-key-refresh))

(defvar dvc-keyvec-next     (vector dvc-key-next))
(defvar dvc-keyvec-previous (vector dvc-key-previous))

(defvar dvc-keyvec-mark     (vector dvc-key-mark))
(defvar dvc-keyvec-unmark   (vector dvc-key-unmark))
(defvar dvc-keyvec-mark-all (dvc-prefix-mark ?*))
(defvar dvc-keyvec-unmark-all (dvc-prefix-mark ?!))
(defvar dvc-keyvec-quit (vector dvc-key-quit))
(defvar dvc-keyvec-popup-menu   (vector dvc-key-popup-menu))


;;;###autoload
(defvar dvc-keyvec-ediff (vector dvc-key-ediff))
;;;###autoload
(defvar dvc-keyvec-tree-lint (vector dvc-key-tree-lint))
;;;###autoload
(defvar dvc-keyvec-logs      (vector dvc-key-logs))
;;;###autoload
(defvar dvc-keyvec-log-entry (vector dvc-key-log-entry))
;;;###autoload
(defvar dvc-keyvec-diff (vector dvc-key-diff))
;;;###autoload
(defvar dvc-keyvec-status (vector dvc-key-status))
;;;###autoload
(defvar dvc-keyvec-file-diff (vector dvc-key-file-diff))
;;;###autoload
(defvar dvc-keyvec-file-diff (vector dvc-key-file-diff))
;;;###autoload
(defvar dvc-keyvec-commit (vector dvc-key-commit))
;;;###autoload
(defvar dvc-keyvec-update     (vector dvc-key-update))
;;;###autoload
(defvar dvc-keyvec-missing     (vector dvc-key-missing))
(defvar dvc-keyvec-replay     (vector dvc-key-replay))
(defvar dvc-keyvec-star-merge (vector dvc-key-star-merge))

(defvar dvc-keyvec-reflect  (vector dvc-key-reflect))
(defvar dvc-keyvec-revert   (vector dvc-key-revert))

;;;###autoload
(defvar dvc-keyvec-inventory (vector dvc-key-inventory))

;;;###autoload
(defvar dvc-keyvec-show-bookmark (vector dvc-key-show-bookmark))
(defvar dvc-keyvec-add-bookmark (vector dvc-key-add-bookmark))

(defvar dvc-keyvec-tag (vector dvc-key-tag))
(defvar dvc-keyvec-kill-ring (vector dvc-key-kill-ring-prefix))

(defvar dvc-keyvec-id (vector dvc-key-id))
(defvar dvc-keyvec-toggle (vector dvc-key-toggle))


;;
;; Global
;;
;; FIXME: replace all those tla-... by dvc-... !!!
;;;###autoload
(defvar dvc-global-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [?U]                     'tla-undo)
    (define-key map [?R]                     'tla-redo)
    (define-key map [?t]                     'tla-tag-insert)
    (define-key map [?r]                     'tla-tree-revisions)
    (define-key map [(meta ?l)]              'tla-tree-lint)
    ;;(define-key map [(meta ?o)]            'tla-file-view-original)
    (define-key map [?p]                     'dvc-submit-patch)
    (define-key map dvc-keyvec-log-entry     'dvc-add-log-entry)
    (define-key map [?A] 'tla-archives)
    (define-key map dvc-keyvec-file-diff     'dvc-file-diff)
    (define-key map dvc-keyvec-ediff         'dvc-file-ediff)
    (define-key map dvc-keyvec-diff          'dvc-diff)
    (define-key map dvc-keyvec-status        'dvc-status)
    (define-key map dvc-keyvec-commit        'dvc-log-edit)
    (define-key map dvc-keyvec-inventory     'dvc-inventory)
    (define-key map dvc-keyvec-logs          'dvc-log)
    ;; dvc: l runs changelog, M-l runs tree-lint for Arch
    (define-key map [?l]                     'dvc-changelog)
    (define-key map [?I]                     'dvc-init)
    (define-key map [?C]                     'dvc-clone)
    (define-key map [?F]                     'dvc-pull)
    (define-key map [?P]                     'dvc-push)
    (define-key map dvc-keyvec-update        'dvc-update)
    (define-key map [?m]                     'dvc-missing)
    (define-key map [?M]                     'dvc-merge)
    (define-key map dvc-keyvec-show-bookmark 'dvc-bookmarks)
    (define-key map dvc-keyvec-help          'tla-help)

    ;; branch handling
    (define-key map (dvc-prefix-branch ?c)   'dvc-create-branch)
    (define-key map (dvc-prefix-branch ?s)   'dvc-select-branch)
    (define-key map (dvc-prefix-branch ?l)   'dvc-list-branches)

    ;; file specific functionality
    (define-key map (dvc-prefix-file ?a) 'dvc-add-files)
    (define-key map (dvc-prefix-file ?D) 'dvc-remove-files)
    (define-key map (dvc-prefix-file ?R) 'dvc-revert-files)
    (define-key map (dvc-prefix-file ?M) 'dvc-rename)
    (define-key map (dvc-prefix-file ?X) 'dvc-purge-files)
    (define-key map (dvc-prefix-file ?=) 'dvc-file-diff)

    (define-key map (dvc-prefix-view-buffer
                     ?p)                     'dvc-show-process-buffer)
    (define-key map (dvc-prefix-view-buffer
                     ?e)                     'dvc-show-last-error-buffer)
    (define-key map (dvc-prefix-view-buffer
                     ?l)                     'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-view-buffer
                     dvc-key-diff)           'tla-changes-goto)
    (define-key map (dvc-prefix-view-buffer
                     dvc-key-status)         'baz-status-goto)
    (define-key map (dvc-prefix-view-buffer
                     dvc-key-inventory)      'tla-inventory-goto)
    (define-key map (dvc-prefix-view-buffer
                     ?L)                     'tla-tree-lint-goto)
    (define-key map (dvc-prefix-view-buffer ?r)   'tla-tree-revisions-goto)

    (define-key map (dvc-prefix-kill-ring ?a) 'tla-save-archive-to-kill-ring)
    (define-key map (dvc-prefix-kill-ring ?v) 'tla-save-version-to-kill-ring)
    (define-key map (dvc-prefix-kill-ring ?r) 'tla-save-revision-to-kill-ring)

    map)
  "Global keymap used by DVC.")


;;;###autoload
(defcustom dvc-prefix-key [(control x) ?V]
  "Prefix key for the DVC commands in the global keymap.

If you wish to disable the prefix key, set this variable to nil."
  :type '(choice (const [(control x) ?V])
                 (const [(control x) ?T])
                 (const [(control x) ?t])
                 (const [(control x) ?v ?t])
                 (const [(super v)])
                 (const [(hyper v)])
                 (const [(super t)])
                 (const [(hyper t)])
                 (const :tag "None" nil)
                 (sexp))
  :group 'tla-bindings
  :set  (lambda (var value)
          (if (boundp var)
              (global-unset-key (symbol-value var)))
          (set var value)
          (global-set-key (symbol-value var) dvc-global-keymap)))

;;;###autoload
(defun dvc-enable-prefix-key ()
  "Install the DVC prefix key globally."
  (interactive)
  (when dvc-prefix-key
    (global-set-key dvc-prefix-key dvc-global-keymap)))

;;;###autoload
(dvc-enable-prefix-key)

;; It is important that DVC has this key, so steal it from other
;; programs, but give the user a chance to override this.
;;;###autoload
(add-hook 'after-init-hook 'dvc-enable-prefix-key t)

;;;###autoload
(define-key ctl-x-4-map [?T] 'dvc-add-log-entry)

(defvar dvc-cmenu-map-template
  (let ((map (make-sparse-keymap)))
    ;; TODO: [return], "\C-m" => tla--generic-context-action
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [down-mouse-3] 'dvc-cmenu-popup-by-mouse)
    (define-key map dvc-keyvec-popup-menu 'dvc-cmenu-popup)
    map)
  "Template for keymaps used in items, files, changes, etc.")

;;
;; Global
;;
;;;###autoload
(easy-menu-add-item
 (and (boundp 'menu-bar-tools-menu) (dvc-do-in-gnu-emacs menu-bar-tools-menu))
 (dvc-do-in-xemacs '("Tools"))
 '("DVC"
   ;; ["Browse Archives" tla-archives t] ;; obsolete
   ["Show Bookmarks" dvc-bookmarks t]
   "---"
   "Tree Commands:"
   ["View Diff" dvc-diff t]
   ["View Status" dvc-status t]
   ["View Missing" dvc-missing t]
   ["View Log" dvc-log t]
   ["View ChangeLog" dvc-changelog t]
   ;; ["View Inventory" tla-inventory t]
   ;; ["View Tree Lint" tla-tree-lint t]
   ;; ["Show Tree Revisions" tla-tree-revisions t]
   ["Edit Commit Log" dvc-log-edit t]
   "---"
   "File Commands:"
   ["Add Files"  dvc-add-files t]
   ["Revert Files"  dvc-revert-files t]
   ["Remove Files"  dvc-remove-files t]
   ["Add Log Entry"  dvc-add-log-entry t]
   ;; ["Insert Arch Tag" tla-tag-insert t]
   ;; ["View File Diff" tla-file-diff t]
   ;; ["View File Ediff" tla-file-ediff t]
   ;; ["View Original" tla-file-view-original t]
   ;; ["View Conflicts" tla-view-conflicts t]
   "---"
   ["Initialize repository" dvc-init t]
   "---"
   ("Tla Goto Buffer"
    ["View Changes" tla-changes-goto t]
    ["View Status"  baz-status-goto t]
    ["View Inventory" tla-inventory-goto t]
    ["View Tree Lint" tla-tree-lint-goto t]
    ["Show Tree Revisions" tla-tree-revisions-goto t])
   ("Tla Quick Configuration"
    ["Three Way Merge" tla-toggle-three-way-merge
     :style toggle :selected tla-three-way-merge]
    ["Show Ancestor in Conflicts" tla-toggle-show-ancestor
     :style toggle :selected tla-show-ancestor]
    ["Non Recursive Inventory" tla-toggle-non-recursive-inventory
     :style toggle :selected tla-non-recursive-inventory]
    ;; ["Use --forward" tla-toggle-use-forward-option
    ;;  :style toggle :selected tla-use-forward-option]
    ["Use --skip-present" tla-toggle-use-skip-present-option
     :style toggle :selected tla-use-skip-present-option]
    )
   )
 "PCL-CVS")


;; Show the selected DVC in the modeline: M-x dvc-show-active-dvc
(defvar dvc-show-active-dvc nil)
(defvar dvc-show-active-dvc-string "")
(make-variable-buffer-local 'dvc-show-active-dvc-string)

(add-to-list 'minor-mode-alist '(dvc-show-active-dvc dvc-show-active-dvc-string))

(add-hook 'find-file-hooks 'dvc-find-file-hook)
(add-hook 'dired-mode-hook 'dvc-dired-hook)


(defun dvc-show-active-dvc (arg)
  "Toggle displaying a DVC string in the modeline.

If ARG is null, toggle displaying
If ARG is a number and is greater than zero, turn on visualization; otherwise,
turn off visualization."
  (interactive "P")
  (setq dvc-show-active-dvc (if arg
                                (> (prefix-numeric-value arg) 0)
                              (not dvc-show-active-dvc)))
  (when dvc-show-active-dvc
    (dvc-actualize-modeline)))

(defun dvc-dvc-file-has-conflict-p (filename)
  nil)

(defun dvc-find-file-hook ()
  "Set dvc-show-active-dvc-string, after loading a file.  Enter
smerge mode if the file has conflicts (detected by
<dvc>-dvc-file-has-conflict-p)."
  (when (dvc-current-active-dvc)
    (dvc-actualize-modeline)
    (when (dvc-call "dvc-file-has-conflict-p" (buffer-file-name))
      (dvc-funcall-if-exists smerge-mode 1)
      (message
       "Conflicts in file%s. Use M-x dvc-resolved RET when done."
       (if (boundp 'smerge-mode) ", entering SMerge mode" "")))))

(defun dvc-dired-hook ()
  "Set dvc-show-active-dvc-string for dired buffers."
  (dvc-actualize-modeline))

(defun dvc-actualize-modeline ()
  (let ((dvc (dvc-current-active-dvc)))
    ;;(when dvc-show-active-dvc (dvc-trace "dvc-actualize-modeline: %S %S" default-directory dvc))
    (setq dvc-show-active-dvc-string (if dvc (concat " DVC:" (symbol-name dvc))
                                       ""))))


(provide 'dvc-ui)
;;; dvc-ui.el ends here

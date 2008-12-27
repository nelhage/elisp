;;; dvc-buffers.el --- Buffer management for DVC

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

(eval-when-compile (require 'cl))
(eval-and-compile (require 'dvc-utils))
(require 'dvc-ui)
(require 'dvc-defs)
(require 'dvc-register)

(defvar dvc-buffers-tree nil
  "Tree containing all dvc buffers.

Must be of the form
 ((tla
   (type1 (\"path1\" buffer \"original name of buffer\")
          (\"path2\" buffer2 \"original name of buffer2\"))
   (type2 (\"path1\" buffer3 \"original name of buffer3\")
          (\"path3\" buffer4 \"original name of buffer4\")))
  (bzr
   (type1 (\"path4\" buffer5 \"original name of buffer5\")))
  (xhg
   (type3 (...))))
Used to keep track of all the dvc related buffers.")

(defvar dvc-buffer-type-alist
  '(
    (alog       "log*"      path)
    ;; alog for "absolute log", i.e., assume path supplied is already
    ;; the root path
    (annotate  "annotate*"  path)
    (archives  "archives*"  single)
    (bookmark  "bookmarks*" single)
    (branches  "branches(%s)*" string)
    (browse    "browse*"    single)
    (categories "categories(%s)*" string)
    (changelog "changelog*" root)
    (changeset "changeset(%s)*" string)
    (commit    "commit*"    root)
    (conflicts "conflicts*" root)
    (diff      "diff*"      root)
    (errors    "error*"     multiple)
    (file-diff "file-diff*" path)
    (generic   "process*"   multiple)
    (info      "info*"  root)
    (inventory "inventory*" path)
    (log       "log*"       root)
    (log-edit  "log-edit*"  root)
    (manifest  "manifest*"  root)
    (missing   "missing*"   root)
    (patch-queue "patch-queue*"  root)
    (pull      "pull*"  root)
    (remote-log "log(%s)*"  string)
    (revision-diff "diff(%s)*" string)
    (revisions "revisions(%s)*" string)
    (revlog    "revlog(%s)*" string-multiple)
    (status    "status*"    root)
    (tips      "tips*"      single)
    (tla-missing "missing*" single)
    (tree-lint "tree-lint*" root)
    (unknowns  "unknowns*"  root)
    (versions  "versions(%s)*" string)
    )
  "List of (type name mode) used to generate a name for a buffer.

TYPE is the type of buffer to create, passed as the first argument to
`dvc-get-buffer-create'.

NAME is a string, used as a name for the returned buffer.

MODE is a symbol defining the way to manage (value of
`default-directory' in the created buffer) paths for this type of
buffers. It can have the following values:

 * 'root: `default-directory' will be the tree-root of the specified
   directory.

 * 'path: `default-directory' will be the path specified. Can also be
   a file.

For 'root and 'path, `dvc-get-buffer-create' will return the existing
buffer for this type and this path if it exists, or create a new one
otherwise.

 * 'single: There is only one buffer of this type for each Emacs
   instance. If a path is provided, `default-directory' is set to that
   path. Otherwise, the path is left unchanged when a buffer is
   reused, and set to the current directory on buffer creation.

 * 'multiple: `default-directory' is set to the path specified. A new
   buffer is returned anyway. (No buffer reuse).

 * 'string: The path specified is actually a string. It won't be used
   to set `default-directory'. The name of the created buffer will be
   (format name string).

 * 'string-multiple: combination of 'string and 'multiple.")

(defun dvc-buffers-tree-remove (buffer)
  "Remove BUFFER from the buffers tree."
  (dolist (dvc-cons dvc-buffers-tree)
    (dolist (type-cons (cdr dvc-cons))
      (dolist (path-buffer (cdr type-cons))
        (when (eq (cadr path-buffer) buffer)
          (setcdr type-cons (delete path-buffer (cdr type-cons))))))))

(defun dvc-buffers-tree-add (dvc type path buffer)
  "Add a buffer for back-end DVC, of TYPE visiting PATH to the buffers tree.
BUFFER should be the buffer to add."
  (let* ((to-add (list path buffer (buffer-name buffer)))
         (dvc-assoc (assoc dvc dvc-buffers-tree))
         (tree-assoc (assoc type dvc-assoc)))
    (if dvc-assoc
        (if tree-assoc
            (push to-add
                  (cdr tree-assoc))
          (push (list type to-add)
                (cdr dvc-assoc)))
      (push (list dvc (list type to-add))
            dvc-buffers-tree))))

(defun dvc-create-buffer (name)
  "Create a buffer for a dvc-mode.
`create-file-buffer' is used to allow uniquify to modify the name."
  (with-current-buffer (create-file-buffer name)
    (setq list-buffers-directory (concat default-directory name))
    (current-buffer)))

(defun dvc-get-buffer-create (dvc type &optional path)
  "Get a buffer of type TYPE for the path PATH.

Maybe reuse one if it exists, according to the value of
`dvc-buffer-type-alist' (see its docstring), or, call
`generate-new-buffer' to create the buffer.

See also `dvc-get-buffer'"
  ;; Inspired from `cvs-get-buffer-create'
  (let ((return-buffer
         (let* ((path (or path default-directory))
                (elem (assoc type dvc-buffer-type-alist))
                (mode (car (cddr elem))))
           (or (dvc-get-buffer dvc type path mode)
               ;; Buffer couldn't be reused. Create one
               (let ((path (cond
                            ((eq mode 'root)
                             (dvc-uniquify-file-name
                              (dvc-tree-root path)))
                            ((or (eq mode 'string)
                                 (eq mode 'string-multiple))
                             path)
                            (t (dvc-uniquify-file-name path))))
                     (name (concat "*" (symbol-name dvc) "-"
                                   (cadr (assoc type dvc-buffer-type-alist)))))
                 (let ((buffer
                        (if (or (eq mode 'string)
                                (eq mode 'string-multiple))
                            (generate-new-buffer (format name path))
                          (let ((default-directory
                                  (or (file-name-directory path)
                                      default-directory)))
                            (dvc-create-buffer name)))))
                   (with-current-buffer buffer
                     (if (featurep 'xemacs)
                         (dvc-install-buffer-menu))
                     (dvc-buffers-tree-add dvc type path buffer)
                     buffer)))))))
    (with-current-buffer return-buffer
      ;; We do not set dvc-buffer-current-active-dvc here, because any
      ;; subsequent mode function will call kill-all-local-variables.
      (dvc-trace "create buffer %S with back-end %S in %S"
                 return-buffer dvc default-directory)
      return-buffer)))

(defun dvc-get-matching-buffers (dvc type path)
  "Return the list of all dvc-buffers-tree entries matching DVC, TYPE, PATH.

If DVC is nil, it matches any back-end. TYPE must match exactly.
PATH matches if the entry in dvc-buffers-tree is a prefix of
PATH."
  (let ((result nil)
        tree)

    (if dvc
        (setq tree (cdr (assoc type (cdr (assoc dvc dvc-buffers-tree)))))
      ;; flatten tree to cover all back-ends
      (let ((temp dvc-buffers-tree)
            buffers)
        (while temp
          (setq buffers (cdr (assoc type (cdar temp))))
          (setq tree (append tree buffers))
          (setq temp (cdr temp)))))

    ;; Filter for path
    (while tree
      (let* ((root (caar tree))
             (index (string-match root path)))
        (if (and index (= 0 index))
            (setq result (cons (car tree) result)))
        (setq tree (cdr tree))))
    result))

(defun dvc-get-buffer (dvc type &optional path mode)
  "Get a buffer of type TYPE for the path PATH.

Maybe reuse one if it exists, depending on the value of MODE (see
`dvc-buffer-type-alist' 's third element), otherwise, return nil.  See
also `dvc-get-buffer-create'."
  (let ((mode (or mode (car (cddr (assoc type dvc-buffer-type-alist)))))
        (path (or path default-directory))
        (subtree (cdr (assoc dvc dvc-buffers-tree))))
    (if (eq mode 'single)
        ;; nothing to do about PATH. Reuse anyway
        (let* ((dvc-path subtree)
               (list-path (cdr (assoc type dvc-path)))
               (first-elem (car list-path)))
          (if list-path
              (if (string= (buffer-name (cadr first-elem))
                           (car (cddr first-elem)))
                  (cadr first-elem)
                (setcdr (assoc type subtree) nil)
                nil)
            nil))
      ;; not 'single
      (let ((path (and path
                       (cond
                        ((eq mode 'root)
                         (dvc-uniquify-file-name
                          (dvc-tree-root path)))
                        ((or (eq mode 'string)
                             (eq mode 'string-multiple))
                         path)
                        (t (dvc-uniquify-file-name path))))))
        (if (or (eq mode 'multiple)
                (eq mode 'string-multiple))
            ;; no need to search an existing buffer
            nil
          (let* ((list-path (assoc type subtree))
                 (elem (assoc path (cdr list-path)))
                 (buffer (cadr elem)))
            (when buffer
              (if (buffer-live-p buffer)
                  ;; This used to check for buffer not renamed, but
                  ;; that conflicts with uniquify.
                  buffer
                ;; remove the buffer and try again
                (setcdr list-path
                        (delq (assoc path (cdr list-path))
                              (cdr list-path)))
                (dvc-get-buffer type path mode)))))))))

(defun dvc-add-buffer-type (type name)
  "Define a new TYPE of buffer whose buffer will be named NAME."
  (unless (assoc type dvc-buffer-type-alist)
    (push (list type name) dvc-buffer-type-alist)))

;; ----------------------------------------------------------------------------
;; Process buffers
;; ----------------------------------------------------------------------------

;; TODO unify with above alist.
(defcustom dvc-process-buffer " *%s-process*"
  "*Name of the process buffer."
  :type 'string
  :group 'dvc-internal)

(defcustom dvc-error-buffer " *%s-errors*"
  "*Name of the buffer to which the process's stderr is redirected."
  :type 'string
  :group 'dvc-internal)

(defcustom dvc-number-of-dead-process-buffer 0
  "*Number of process buffers to keep after process termination.
When the number of process buffers exceeds this number, the most ancient
is killed.  This includes both the process buffer and the error
buffer (to which stderr is redirected).

A nil value here means \"Never kill any process buffer\". Useful for
debugging, but this will eat the memory of your computer ;-)"
  :type 'integer
  :group 'dvc-internal)

(defcustom dvc-show-internal-buffers-on-menu nil
  "Toggle display of dead process buffers in the buffer menu."
  :type 'boolean
  :group 'dvc-internal)

(defcustom dvc-other-frame-width 80
  "Width of frame created by `dvc-switch-to-buffer' when `other-frame' arg is non-nil."
  :type 'integer
  :group 'dvc)

(defcustom dvc-other-frame-height 20
  "Height of frame created by `dvc-switch-to-buffer' when `other-frame' arg is non-nil."
  :type 'integer
  :group 'dvc)

(defvar dvc-dead-process-buffer-queue nil
  "List of process buffers belonging to terminated processes.
When the list is greater than `dvc-number-of-dead-process-buffer', the last
ones are killed.")

(defun dvc-kill-process-buffer (buffer)
  "Don't actually kill BUFFER, but add it to `dvc-dead-process-buffer-queue'.
It will eventually be killed when the number of buffers in
`dvc-dead-process-buffer-queue'exceeds `dvc-number-of-dead-process-buffer'."
  (dvc-add-to-list 'dvc-dead-process-buffer-queue buffer t)
  (when dvc-number-of-dead-process-buffer
    (while (> (length dvc-dead-process-buffer-queue)
              (max 2 dvc-number-of-dead-process-buffer))
      (let ((buf (car dvc-dead-process-buffer-queue)))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (setq dvc-dead-process-buffer-queue
            (cdr dvc-dead-process-buffer-queue)))))

(defvar dvc-last-process-buffer nil
  "The last created process buffer.")

(defvar dvc-last-error-buffer nil
  "The last created process buffer.")

(defun dvc-new-process-buffer (to-be-deleted back-end)
  "Create a new process buffer.
If TO-BE-DELETED is non-nil, make this buffer a candidate for eventually
being deleted."
  (let ((buffer (generate-new-buffer
                 (format dvc-process-buffer
                         back-end))))
    (setq dvc-last-process-buffer buffer)
    (when to-be-deleted (dvc-kill-process-buffer buffer))
    buffer))

(defun dvc-new-error-buffer (to-be-deleted back-end)
  "Create a new error buffer.
If TO-BE-DELETED is non-nil, make this buffer a candidate for eventually
being deleted."
  (let ((buffer (generate-new-buffer
                 (format dvc-error-buffer
                         back-end))))
    (setq dvc-last-error-buffer buffer)
    (when to-be-deleted (dvc-kill-process-buffer buffer))
    buffer))

;;
;; Process buffer mode section
;;
(defvar dvc-process-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in dvc's log buffer.")

(define-derived-mode dvc-process-buffer-mode fundamental-mode
  "DVC Process"
  "Major mode for process buffers. Mainly defines \\[bury-buffer]
to quit the buffer"
  (dvc-install-buffer-menu)
  (toggle-read-only 1))


(defvar dvc-switched-buffer nil)
(defvar dvc-switched-from-buffer nil)

(defun dvc-switch-to-buffer (buffer &optional other-frame)
  "Switch to BUFFER using the user's preferred method.
See `dvc-switch-to-buffer-mode' for possible settings."
  (setq dvc-switched-from-buffer (current-buffer))
  (cond
   (other-frame
    (let ((display-reuse-frames t)
          (pop-up-frames t)
          (pop-up-frame-alist `((width . ,dvc-other-frame-width)
                                (height . ,dvc-other-frame-height)
                                (minibuffer . nil))))
      (pop-to-buffer buffer)))
   ((eq dvc-switch-to-buffer-mode 'pop-to-buffer)
    (pop-to-buffer buffer))
   ((eq dvc-switch-to-buffer-mode 'single-window)
    (switch-to-buffer buffer))
   ((eq dvc-switch-to-buffer-mode 'show-in-other-window)
    (pop-to-buffer buffer)
    (setq dvc-switched-buffer (current-buffer))
    (pop-to-buffer dvc-switched-from-buffer))
   (t
    (error "Switch mode %s not implemented" dvc-switch-to-buffer-mode))))

(defun dvc-switch-to-buffer-maybe (buffer &optional setup-as-partner)
  "Either switch to buffer BUFFER or just set-buffer.
Depends on the value of `dvc-switch-to-buffer-first'.

When SETUP-AS-PARTNER, set the `dvc-partner-buffer' variable in BUFFER to current-buffer and vice versa."
  ;; (message "dvc-switch-to-buffer-maybe, curr-buff: %s switch-to: %s" (current-buffer) buffer)
  (when setup-as-partner
    (setq dvc-partner-buffer buffer)
    (let ((cur-buff (current-buffer)))
      (with-current-buffer buffer
        (setq dvc-partner-buffer cur-buff))))
  (if dvc-switch-to-buffer-first
      (dvc-switch-to-buffer buffer)
    (set-buffer buffer)))

(defun dvc-post-switch-to-buffer ()
  "Executed when showing a changeset.

If `dvc-switched-buffer' is non-nil, show this buffer, but keep
cursor position in previous buffer."
  (when dvc-switched-buffer
    (pop-to-buffer dvc-switched-buffer)
    (setq dvc-switched-buffer nil)
    (goto-char (point-min))
    (pop-to-buffer dvc-switched-from-buffer)))


(defun dvc-show-process-buffer ()
  "Show the process buffer of the last started DVC command."
  (interactive)
  (dvc-switch-to-buffer dvc-last-process-buffer)
  (unless (member dvc-last-process-buffer
                  (mapcar (lambda (p)
                            (process-buffer (car p)))
                          dvc-process-running))
    (dvc-process-buffer-mode)))

(defun dvc-show-last-error-buffer ()
  "Show the error buffer of the last started DVC command."
  (interactive)
  (dvc-switch-to-buffer dvc-last-error-buffer)
  (dvc-process-buffer-mode))

(defun dvc-show-last-process-buffer (&optional type mode path)
  "Switch to the last used process buffer in a new buffer of TYPE.
If MODE is specified, it is a function that will be run in the
new buffer.  Otherwise, the buffer will remain in fundamental mode, in
read-only.

If PATH is specified, it will be passed to `dvc-get-buffer-create'."
  (when (buffer-live-p dvc-last-process-buffer)
    (let ((content (with-current-buffer dvc-last-process-buffer
                     (buffer-string))))
      (dvc-switch-to-buffer (dvc-get-buffer-create
                             'dvc (or type 'generic) path))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (if mode
        (funcall mode)
      (dvc-process-buffer-mode))))

(defun dvc-show-error-buffer (buffer &optional type mode)
  "Pops up a new buffer displaying contents of BUFFER.
New buffer has type TYPE (default 'errors), mode MODE (default
`dvc-process-buffer-mode')."
  (when (buffer-live-p buffer)
    (let ((content (with-current-buffer buffer
                     (buffer-string))))
      (dvc-switch-to-buffer (dvc-get-buffer-create
                             'dvc (or type 'errors)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)))
    (if mode
        (funcall mode)
      (dvc-process-buffer-mode))))

;; ----------------------------------------------------------------------------
;; Buffers menu
;; ----------------------------------------------------------------------------
(defun dvc-buffers-menu ()
  "Return menus for buffers managed in DVC."
  (let ((menu (make-sparse-keymap (concat "DVC-Buffers")))
        (submenu (make-sparse-keymap "Queue"))
        (i dvc-number-of-dead-process-buffer))
    ;; Debug QUEUE
    (mapcar
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (define-key submenu (vector (make-symbol (buffer-name buffer)))
           `(menu-item ,(format "%d: %s%s"
                                i
                                (if (zerop (buffer-size buffer)) "[empty] " "")
                                (buffer-name buffer))
                       (lambda () (interactive) (switch-to-buffer ,buffer))
                       :enable t)))
       (setq i (1- i)))
     dvc-dead-process-buffer-queue)
    (define-key menu [queue]
      `(menu-item "Queue(DEBUG)"
                  ,submenu
                  :enable dvc-show-internal-buffers-on-menu))
    (mapcar
     (lambda (item)
       (let* ((dvc (car item))
              (type-list (cdr item))
              (dvc-label (capitalize (symbol-name dvc)))
              (submenu (make-sparse-keymap dvc-label)))
         (mapcar
          (lambda (type-list)
            (let* ((type-label
                    (concat dvc-label "-"
                            (capitalize (symbol-name (car type-list)))))
                   (type-submenu (make-sparse-keymap type-label)))
              (mapcar
               (lambda (subitem)
                 (let ((path (car subitem))
                       (buffer (cadr subitem)))
                   (when (buffer-live-p buffer)
                     (unless path
                       (setq path (buffer-name buffer)))
                     (define-key type-submenu (vector (make-symbol path))
                       `(menu-item ,path
                                   (lambda () (interactive)
                                     (switch-to-buffer ,buffer))
                                   :enable t)))))
               (cdr type-list))
              (define-key submenu (vector (car type-list))
                `(menu-item ,type-label
                            ,type-submenu
                            :enable t))))
          type-list)
         (when type-list
           (define-key menu (vector dvc)
             `(menu-item ,dvc-label
                         ,submenu
                         :enable t))
           )))
     dvc-buffers-tree)
    (define-key menu [list-separator]
      '(menu-item "--"))
    (define-key menu [process-buffer]
      '(menu-item "Show Process Bufffer" dvc-show-process-buffer))
    (define-key menu [error-buffer]
      '(menu-item "Show Error Bufffer" dvc-show-last-error-buffer))
    (define-key menu [log-buffer]
      '(menu-item "Open Log Bufffer" dvc-open-internal-log-buffer))
    menu))

(eval-when-compile
  (unless (functionp 'add-submenu)
    (defun add-submenu (&rest arg)
      "Avoids a byte-compiler warning for GNU Emacs")))

(defun dvc-install-buffer-menu ()
  "Install the buffer menu."
  (if (featurep 'xemacs)
      ;; See dvc-xemacs-buffers-menu in dvc-xemacs.el
      (dvc-do-in-xemacs
        (add-submenu nil (list "DVC-Buffers"
                               :filter 'dvc-xemacs-buffers-menu) nil))
    ;; GNU Emacs
    (dvc-do-in-gnu-emacs
      (let ((dvc-menu (or (lookup-key global-map [menu-bar tools dvc])
                          (lookup-key global-map [menu-bar tools DVC]))))
        (when (and dvc-menu (not (integerp dvc-menu)))
          (define-key-after
            dvc-menu
            [dvc-buffers]
            (cons "DVC-Buffers"
                  (dvc-buffers-menu)))))
      (let ((map (and
                  (current-local-map)
                  (or (lookup-key (current-local-map) [menu-bar])
                      (define-key (current-local-map) [menu-bar]
                        (make-keymap))))))
        (when map
          (apply (if (functionp 'define-key-after)
                     'define-key-after
                   'define-key)
                 map
                 [dvc-buffers]
                 (cons "DVC-Buffers"
                       (dvc-buffers-menu))
                 nil)))
      (add-hook 'menu-bar-update-hook 'dvc-install-buffer-menu nil t))))

(defvar dvc-buffer-previous-window-config nil
  "Window-configuration to return to on buffer quit.

If nil, nothing is done special.  Otherwise, must be a
window-configuration.  `dvc-buffer-quit' will restore this
window-configuration.")

(make-variable-buffer-local 'dvc-buffer-previous-window-config)

;; TODO: eventually implement dvc-buffer-previous-window-config as list
;;       That does not work at the moment, because it is buffer local.
;;       I (Stefan) will play a bit with a global list
(defun dvc-buffer-push-previous-window-config (&optional window-config)
  "Store WINDOW-CONFIG in `dvc-buffer-previous-window-config'.
When WINDOW-CONFIG is nil, store `current-window-configuration' instead."
  (setq dvc-buffer-previous-window-config (or window-config (current-window-configuration))))

(defun dvc-buffer-quit ()
  "Quit the current buffer.

If `dvc-buffer-quit-mode' is 'kill, then kill the buffer.  Otherwise,
just bury it."
  (interactive)
  ;; Value is buffer local => keep it before killing the buffer!
  (let ((prev-wind-conf dvc-buffer-previous-window-config))
    (if (eq dvc-buffer-quit-mode 'kill)
        (kill-buffer (current-buffer))
      (bury-buffer))
    (when prev-wind-conf
      (set-window-configuration prev-wind-conf))))

(defun dvc-kill-all-buffers ()
  "Kill all dvc buffers."
  (interactive)
  (let ((number 0))
    (dolist (dvc-kind dvc-buffers-tree)
      (dolist (type-cons (cdr dvc-kind))
        (dolist (path-buffer (cdr type-cons))
          (setq number (1+ number))
          (kill-buffer (cadr path-buffer)))))
    (message "Killed %d buffer%s" number
             (if (> number 1) "s" "")))
  (setq dvc-buffers-tree nil))

(defvar dvc-save-some-buffers-ignored-modes '(dvc-log-edit-mode))
(defun dvc-save-some-buffers (&optional tree)
  "Save all buffers visiting a file in TREE."
  (interactive)
  (let ((ok t)
        (tree (or (dvc-tree-root tree t)
                  tree)))
    (unless tree
      (error "Not in a project tree"))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-modified-p) (not (member major-mode dvc-save-some-buffers-ignored-modes)))
          (let ((file (buffer-file-name)))
            (when file
              (let ((root (dvc-uniquify-file-name
                           (dvc-tree-root (file-name-directory file) t)))
                    (tree-exp (dvc-uniquify-file-name tree)))
                (when (and root
                           (string= root tree-exp)
                           ;; buffer is modified and in the tree TREE.
                           (or dvc-do-not-prompt-for-save
                               (y-or-n-p (concat "Save buffer "
                                                 (buffer-name)
                                                 "? "))
                               (setq ok nil)))
                  (save-buffer))))))))
    ok))

(defun dvc-revert-some-buffers (&optional tree)
  "Reverts all buffers visiting a file in TREE that aren't modified.
To be run after an update or a merge."
  (interactive)
  (let ((tree (dvc-tree-root tree)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (not (buffer-modified-p))
          (let ((file (buffer-file-name)))
            (when file
              (let ((root (dvc-uniquify-file-name
                           (dvc-tree-root (file-name-directory file) t)))
                    (tree-exp (dvc-uniquify-file-name
                               (expand-file-name tree))))
                (when (and (string= root tree-exp)
                           ;; buffer is modified and in the tree TREE.
                           dvc-automatically-revert-buffers)
                  ;; Keep the buffer if the file doesn't exist
                  (if (file-exists-p file)
                      (revert-buffer t t)))))))))))

(defun dvc-buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible in frame."
  (save-window-excursion
    (let ((buf (current-buffer))
          (window-conf (current-window-configuration)))
      (pop-to-buffer buffer)
      (pop-to-buffer buf)
      (dvc-do-in-xemacs
        (and (setq window-conf (get-buffer-window buffer))
             window-conf ;; we use window-conf only to get rid of warnings
             (equal (window-frame (get-buffer-window buffer))
                    (selected-frame))))
      (dvc-do-in-gnu-emacs
        (compare-window-configurations window-conf
                                       (current-window-configuration))))))

(defun dvc-buffer-show-or-scroll (buffer &optional down)
  "If BUFFER is visible, scroll it. Otherwise, show it.

if DOWN is non-nil, scroll down, otherwise, scroll up."
  (if (dvc-buffer-visible-p buffer)
      (progn
        (pop-to-buffer buffer)
        (condition-case nil
            (if down
                (scroll-down 2)
              (save-excursion
                (move-to-window-line -1)
                (if (> (point-max) (point))
                    (scroll-up 2)
                  (message "end of buffer"))))
          (error (message "Can't scroll anymore."))
          ))
    (dvc-switch-to-buffer buffer)))

(provide 'dvc-buffers)
;;; dvc-buffers.el ends here

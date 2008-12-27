;;; tla-browse.el --- Arch archives/library browser

;; Copyright (C) 2004 by all contributors

;; Author: Masatake YAMATO <jet@gyve.org>

;; Contributions from:
;;    Stefan Reichoer, <stefan@xsteve.at>
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; This is a part of xtla.
;;
;; xtla is free software; you can redistribute it and/or modify
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
;; 1. Load tla-browse.el
;; 2. M-x tla-browse RET

;;; TODO:
;; - Generic refresh
;;

;;; History:
;;

;;; Code:
;; runtime use of 'cl package is discourraged. Please keep this
;; "eval-when-compile"
;;       ^^^^
(eval-when-compile (require 'cl))
(eval-when-compile (require 'dvc-core))
(eval-when-compile (require 'dvc-utils))
(require 'tree-widget)
(require 'tla)
(require 'dvc-ui)

(defvar tla--browse-buffer-name "*tla-browse*")
(defvar tla--browse-buffer-type 'browse)
(dvc-add-buffer-type tla--browse-buffer-type
                     tla--browse-buffer-name)

;; ----------------------------------------------------------------------------
;; Open node tracking
;; ----------------------------------------------------------------------------
(defvar tla--browse-open-list '()
  "List holding the name of open nodes.")

(defun tla--browse-open-list-member (archive
                                     &optional category branch version)
  "Return a node, ARCHIVE/CATEGORY--BRANCH--VERSION is opend or not.
CATEGORY, BRANCH, VERSION are optional."
  (let ((name (list archive category branch version nil)))
    (member name tla--browse-open-list)))

(defun tla--browse-open-list-add (archive
                                  &optional category branch version)
  "Add a node specified by the arguments to 'tla--browse-open-list'.
ARCHIVE/CATEGORY--BRANCH--VERSION,  ARCHIVE/CATEGORY--BRANCH,
ARCHIVE/CATEGORY, ARCHIVE are added.  CATEGORY, BRANCH, VERSION
are optional."
  (tla--browse-open-list-add-internal (list archive category branch version nil))
  (tla--browse-open-list-add-internal (list archive category branch nil nil))
  (tla--browse-open-list-add-internal (list archive category nil nil nil))
  (tla--browse-open-list-add-internal (list archive nil nil nil nil))
  (tla--browse-open-list-add-internal (list nil nil nil nil nil)))

(defun tla--browse-open-list-add-internal (name)
  "Add NAME to `tla--browse-open-list'."
  (unless (tla--browse-open-list-member (tla--name-archive name)
                                        (tla--name-category name)
                                        (tla--name-branch name)
                                        (tla--name-version name))
    (push name tla--browse-open-list)))

(defun tla--browse-open-list-remove (archive
                                     &optional category branch version)
  "Remove ARCHIVE/CATEGORY--BRANCH--VERSION from `tla--browse-open-list'.
CATEGORY, BRANCH and VERSION are optional."
  (let ((name (list archive category branch version nil)))
    (setq tla--browse-open-list (delete name tla--browse-open-list))))

(defun tla--browse-open-tracker (tree)
  "Add or remove a node represented by TREE to/from `tla--browse-open-list'.
If TREE is opened, it is added.  Else it is removed."
  (let* ((node (widget-get tree :node))
         (a (widget-get node :archive))
         (c (widget-get node :category))
         (b (widget-get node :branch))
         (v (widget-get node :version)))
    (if (widget-get tree :open)
        (tla--browse-open-list-add a c b v)
      (tla--browse-open-list-remove a c b v))))

(defun tla--browse-find-archives-root-widget ()
  "Return the root widget of archives tree."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward " Archives$")
    (backward-char 1)
    (tla--widget-node-get-at)))

(defun tla--browse-find-named-widget (parent name type)
  "Find a widget specified with arguments.
PARENT specifies the parent widget.
NAME is the name of the widget.
TYPE is the type of widget.  You can specify :archive, :category,
:branch, or :version."
  (let* ((args (widget-get parent :args))
         (largs (length args))
         (index (dvc-position name args (lambda (e w)
                                          (let ((node (widget-get w :node)))
                                            ;; Next line is hack for version node.
                                            (unless node (setq node w))
                                            (string= e (widget-get node type))))))
         (children (widget-get parent :children))
         (lchildren (length children))
         ;; The internal data structure of tree-widget bundled to develoment
         ;; version of GNU Emacs may by changed; :children list becomes longer
         ;; than :args list.
         (tree (when index (nth (+ index (if (eq largs lchildren) 0 1))
                                children)))
         (node (when tree (save-excursion (goto-char (widget-get tree :from))
                                          (goto-char (next-single-property-change (point) 'widget))
                                          (tla--widget-node-get-at)))))
    node))


(defun tla--browse-find-widget (archive
                                &optional category branch version)
  "Return a list of widgets: (root archive category branch version)
root is always the root of the tree, of type `tla--widget-root-node'.
archive is the widget representing ARCHIVE, of type
`tla--widget-archive-node'.  The last items are potentially nil if
CATEGORY, BRANCH or VERSION is nil.  Otherwise, they are respectively
of type `tla--widget-category-node', `tla--widget-revision-node' and
`tla--widget-version-node'."
  (let* ((root (tla--browse-find-archives-root-widget))
         (a    (tla--browse-find-named-widget
                (widget-get root :parent) archive :archive))
         (c    (and a category
                    (tla--browse-find-named-widget
                     (widget-get a :parent) category :category)))
         (b    (and c branch
                    (tla--browse-find-named-widget
                     (widget-get c :parent) branch :branch)))
         (v    (and b version
                    (tla--browse-find-named-widget
                     (widget-get b :parent) version :version))))
    (list root a c b v)))

(defun tla--browse-find-single-widget (archive
                                       &optional category branch
                                       version)
  "Similar to `tla--browse-find-widget'.
Difference is it returns only the widget representing the last non-nil
widget of the list.  The means of ARCHIVE, CATEGORY, BRANCH and VERSION
are the same as that of `tla--browse-find-widget'."
  (let ((widgets (tla--browse-find-widget archive category branch
                                          version)))
    (or (nth 4 widgets)
        (nth 3 widgets)
        (nth 2 widgets)
        (nth 1 widgets)
        (error "Widget not found.  Please fill-in a bug report"))))

(defun tla--browse-find-real-widget (widget)
  "Find real(complete) widget from incomplete WIDGET.
When trying to find widgets using (widget-get ... :args), we
sometimes find an incomplete widget, having no :from or :to
information for example.  This function takes as an argument an
incomplete widget, and finds the corresponding full widget.

WIDGET must be of type tla--widget-*-node."
  (case (widget-type widget)
    (tla--widget-archive-node
     (tla--browse-find-single-widget
      (widget-get widget :archive)))
    (tla--widget-category-node
     (tla--browse-find-single-widget
      (widget-get widget :archive)
      (widget-get widget :category)))
    (tla--widget-branch-node
     (tla--browse-find-single-widget
      (widget-get widget :archive)
      (widget-get widget :category)
      (widget-get widget :branch)))
    (tla--widget-version-node
     (tla--browse-find-single-widget
      (widget-get widget :archive)
      (widget-get widget :category)
      (widget-get widget :version)))))

(defun* tla--browse-open (flash archive
                                &optional category branch version)
  (let (widgets root a c b v)

    (unless archive
      (return-from tla--browse-open nil))
    (setq widgets (tla--browse-find-widget archive category branch nil))
    (setq root (nth 0 widgets))
    (unless root
      (error "Cannot find root archives node"))
    (tla--widget-node-toggle-subtree-internal root 'open)

    (setq widgets (tla--browse-find-widget archive category branch nil))
    (setq a (nth 1 widgets))
    (unless category
      (if a
          (progn (when flash
                   (goto-char (widget-get a :from))
                   (dvc-flash-line))
                 (return-from tla--browse-open nil))
        (error "Cannot find archive node for: %s" archive)))
    (tla--widget-node-toggle-subtree-internal a 'open)

    (setq widgets (tla--browse-find-widget archive category branch nil))
    (setq c (nth 2 widgets))
    (unless branch
      (if c
          (progn (when flash
                   (goto-char (widget-get c :from))
                   (dvc-flash-line))
                 (return-from tla--browse-open nil))
        (error "Cannot find category node for: %s/%s" archive category)))
    (tla--widget-node-toggle-subtree-internal c 'open)

    (setq widgets (tla--browse-find-widget archive category branch nil))
    (setq b (nth 3 widgets))
    (unless version
      (if b
          (progn (when flash
                   (goto-char (widget-get b :from))
                   (dvc-flash-line))
                 (return-from tla--browse-open nil))
        (error "Cannot find branch node for: %s/%s--%s" archive category branch)))
    (tla--widget-node-toggle-subtree-internal b 'open)

    (setq widgets (tla--browse-find-widget archive category branch version))
    (setq v (nth 4 widgets))
    (if v
        (progn (when flash
                 (goto-char (widget-get v :from))
                 (dvc-flash-line))
               (return-from tla--browse-open nil))
      (error "Cannot find branch node for: %s/%s--%s--%s" archive category branch version)))
  )

;; ----------------------------------------------------------------------------
;; Abstract Super Widget
;; ----------------------------------------------------------------------------
(define-widget 'tla--widget-node 'item
  "Abstract super widget for tla--widget-*-node."
  :tla-type nil
  :format "%[ %t%]%{%v%}\n"
  :face nil
  :keymap nil
  :menu nil
  :marks " "
  :keep '(:marks :open)
  :open-subtree 'tla--tree-widget-node-open-subtree
  :close-subtree 'tla--tree-widget-node-close-subtree)

(defvar tla--widget-node-map
  (let ((map (copy-keymap dvc-cmenu-map-template)))
    (define-key map [return]
      'tla--widget-node-toggle-subtree)
    (define-key map [down-mouse-2]
      'tla--widget-node-toggle-subtree-by-mouse)
    (define-key map "\C-m"
      'tla--widget-node-toggle-subtree)
    (define-key map (dvc-prefix-buffer ?p)
      'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L)
      'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark)
      'tla-bookmarks)
    (define-key map dvc-keyvec-kill-ring
      'tla--widget-node-save-name-to-kill-ring)
    (define-key map dvc-keyvec-add-bookmark
      'tla--widget-node-add-bookmark)
    map)
  "Keymap commonly used in tla--widget-*-node.")

(defun tla--widget-node-value-create (widget keyword)
  "Create value for WIDGET.
KEYWORD is used to get the base string to create the value."
  (insert (let* ((marks (widget-get widget :marks))
                 (string (widget-get widget keyword))
                 (value (tla--widget-node-install-ui-element
                         widget (if (string= string "") "<empty>"
                                  string))))
            (concat marks value))))

(defun tla--widget-node-install-ui-element (widget value &optional face)
  "Create a string with keymap, menu and face properties.
The keymap and menu are retrieved from WIDGET.
The string is copied from VALUE.
FACE is useds as the face."
  (let ((prop-value (dvc-face-add value
                                  (if face face (widget-get widget :face))
                                  (widget-get widget :keymap)
                                  (widget-get widget :menu))))
    (put-text-property 0 (length value)
                       'widget widget
                       prop-value)
    prop-value))

(defun tla--widget-node-get-at (&optional point)
  "Get widget at POINT."
  (get-text-property (if point point (point)) 'widget))

(defun tla--widget-node-get-name (&optional point)
  "Get name list associated widget under the POINT."
  (let ((widget (tla--widget-node-get-at point)))
    (list (widget-get widget :archive)
          (widget-get widget :category)
          (widget-get widget :branch)
          (widget-get widget :version)
          nil)))

(defun tla--widget-node-get-type (&optional point)
  "Get type of widget under the POINT.

Can be either 'archive, 'category, 'branch, 'version or nil for the
root of the tree."
  (let ((widget (tla--widget-node-get-at point)))
    (widget-get widget :tla-type)))

(defun tla--widget-get-ancestor (widget level)
  "Get the ancestor widget of WIDGET.
\"ancestor\" widget stands for the LEVEL upper widget
in the archives tree."
  (let ((i 0)
        (parent widget))
    (while (< i level)
      (setq parent (widget-get parent :parent)
            i (1+ i)))
    parent))

(defun tla--widget-node-refresh (&optional level point
                                           archive
                                           category
                                           branch)
  "Refresh node and LEVEL subnode at the POINT.
Before refreshing node, names cache are also refreshed if
ARCHIVE, CATEGORY, and/or BRANCH are specified.
If POINT is a symbol, `name', node is specified by ARCHIVE,
CATEGORY, and/or BRANCH."
  (interactive)
  (unless level (setq level 1))
  (setq point (cond
               ((null point) (point))
               ((eq 'name point)
                (save-excursion
                  (goto-char
                   (next-single-property-change
                    (widget-get
                     (tla--browse-find-single-widget
                      archive
                      category
                      branch)
                     :from)
                    'widget))))
               (t point)))
  (if branch
      (tla--archive-tree-build-versions archive
                                        category
                                        branch
                                        nil t)
    (if category
        (tla--archive-tree-build-branches archive
                                          category
                                          nil t)
      (if archive
          (tla--archive-tree-build-categories archive
                                              nil
                                              t)
        (tla--archive-tree-build-archives nil t))))

  (let* ((widget (tla--widget-node-get-at point))
         (tree (tla--widget-get-ancestor widget level)))
    (widget-put tree :args nil)
    (widget-value-set tree (widget-value tree))
    (widget-setup)))

(defun tla--widget-node-synchronize-mirror-to-remote ()
  "Synchronizes the mirror for the archive at point to remote from local."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (type (tla--archive-type archive))
         mirror source)
    (cond
     ((eq type 'normal)
      (setq mirror (tla--archive-name-mirror archive t))
      (unless mirror
        (error "No mirror archive for `%s'" archive)))
     ((eq type 'mirror)
      (setq source (tla--archive-name-source archive t))
      (if source
          (setq archive source)
        (error "No source archive for `%s'" archive)))
     (t (error "Cannot mirror to a source archive: `%s'" archive)))
    (tla-archive-mirror archive
                        (tla--name-category name)
                        (tla--name-branch name)
                        (tla--name-version name)
                        nil)))

(defun tla--widget-node-synchronize-mirror-to-local ()
  "Synchronizes the mirror for the archive at point to local from remote."
  (interactive)
  ;; TODO
  )

(defun tla--widget-node-save-name-to-kill-ring ()
  "Save the name under point to `kill-ring'."
  (interactive)
  (let ((name (tla--name-construct (tla--widget-node-get-name))))
    (when (equal "" name)
      (error "No widget under the point"))
    (kill-new name)
    (message "Name: %s" name)))

(defun tla--widget-node-add-bookmark ()
  "Add a name associated with a widget at point to xtla's bookmarks."
  (interactive)
  (let* ((target (tla--widget-node-get-name))
         (target-fq (tla--name-construct target))
         (bookmark (read-from-minibuffer (format "Name of Bookmark for `%s': "
                                                 target-fq))))
    (tla-bookmarks-add bookmark target)
    (when (y-or-n-p "View bookmarks? ")
      (tla-bookmarks))
    (message "bookmark %s(=> %s) added." bookmark target-fq)))

(defun tla--widget-node-toggle-subtree (&optional point force)
  "Toggle between closing and opening the node at POINT.
You can specify a symbol, `open' or `close' to FORCE to force
the node to open or to close."
  (interactive)
  (tla--widget-node-toggle-subtree-internal
   (tla--widget-node-get-at point) force))

(defun tla--widget-node-toggle-subtree-recursive (&optional point
                                                            force)
  "Same as `tla--widget-node-toggle-subtree'.
The difference is that when the node is expanded, expands it
recursively, which means all the children will also be expanded.  (this
may take looong).
Meaning of POINT and FORCE are the same as that of
`tla--widget-node-toggle-subtree'."
  (interactive)
  (tla--widget-node-toggle-subtree-internal
   (tla--widget-node-get-at point) force t))

(defun tla--widget-node-toggle-subtree-internal (widget force
                                                        &optional
                                                        recursive)
  "Toggle between closing and opening the WIDGET.
You can specify a symbol, `open' or `close' to FORCE to force
the node to open or to close.  If RECURSIVE is non-nil, the opening
or closing are applied recursively."
  (let* ((open-subtree (widget-get widget :open-subtree))
         (close-subtree (widget-get widget :close-subtree)))
    (cond
     ((or (eq force 'open)
          (and (not force)
               (not (widget-get (widget-get widget :parent) :open))))
      (when open-subtree (funcall open-subtree widget))
      (when recursive
        (tla--widget-node-toggle-subtree-recursion widget 'open)))
     ((or (eq force 'close)
          (and (not force)
               (widget-get (widget-get widget :parent) :open)))
      (when (and recursive
                 (widget-get (widget-get widget :parent) :open))
        (when open-subtree (funcall open-subtree widget))
        (tla--widget-node-toggle-subtree-recursion widget 'close))
      (when close-subtree (funcall close-subtree widget))))))

(defun tla--widget-node-toggle-subtree-recursion (widget force)
  "A helper function for 'tla--widget-node-toggle-subtree-internal'.
Apply all sub node of WIDGET opening or closing which is specified
by FORCE."
  (let ((args (widget-get (widget-get widget :parent) :args)))
    (dolist (arg args)
      (let* ((t-widget (widget-get arg :node))
             ;; surprisingly, t-widget doesn't have all the
             ;; necessary fields. Look for the _real_ widget.
             (full-widget
              (tla--browse-find-real-widget t-widget)))
        (unless (eq (widget-type t-widget)
                    (widget-type full-widget))
          (error "Incorrect widget.  Please contact the developers"))
        (when full-widget
          (tla--widget-node-toggle-subtree-internal
           full-widget force t))))))

(defun tla--tree-widget-node-open-subtree (widget)
  "Open tree node function used in `tla-browse'."
  (cond
   ((fboundp 'tree-widget-action)
    (let ((parent (widget-get widget :parent)))
      (unless (widget-get parent :open)
        (tree-widget-action parent))))
   ((fboundp 'tree-widget-open-node)
    'tree-widget-open-node)
   (t
    'tla--tree-widget-node-toggle-subtree-for-tree-widget-v1)))

(defun tla--tree-widget-node-close-subtree (widget)
  "Close tree node function used in `tla-browse'."
  (cond
   ((fboundp 'tree-widget-action)
    (let ((parent (widget-get widget :parent)))
      (when (widget-get parent :open)
        (tree-widget-action parent))))
   ((fboundp 'tree-widget-open-node)
    'tree-widget-close-node)
   (t
    'tla--tree-widget-node-toggle-subtree-for-tree-widget-v1)))

(defun tla--tree-widget-node-toggle-subtree-for-tree-widget-v1 (widget)
  "Toggle tree node function used in `tla-browse' with tree-widget ver.1.0.5.
The code is the almost same as in tree-widget-toggle-folding tree-widget version
1.0.5.

Original documents say:
  \"Toggle a `tree-widget' folding.
WIDGET is a `tree-widget-node-handle-widget' and its parent the
`tree-widget' itself.  IGNORE other arguments.\""
  (let* ((parent (widget-get widget :parent))
         ;; Original code
         ;;(open   (widget-value widget))
         ;; Here `parent' is used instead of `widget'.
         (open   (widget-value parent)))
    (if open
        (tree-widget-children-value-save parent))
    (widget-put parent :open (not open))
    (widget-value-set parent (not open))
    (run-hook-with-args 'tree-widget-after-toggle-functions parent)))

(dvc-make-bymouse-function tla--widget-node-toggle-subtree)

;; ----------------------------------------------------------------------------
;; My-id
;; ----------------------------------------------------------------------------
(define-widget 'tla--widget-my-id 'push-button
  "Widget to control tla's my-id."
  :format "%{My-id:%} %[%t%]"
  :sample-face 'bold
  :button-face 'widget-field-face
  :notify 'tla--widget-my-id-set
  :help-echo "Click here to change my-id")

(defun tla--widget-my-id-set (self changed event)
  "Set my-id to my-id-widget.
SELF is not used.  CHANGED is just passed to `widget-value-set'.
EVENT is also not used."
  (let ((new-id (tla-my-id t)))
    (widget-value-set changed new-id)
    (widget-setup)))

;; ----------------------------------------------------------------------------
;; Root node
;; ----------------------------------------------------------------------------
(define-widget 'tla--widget-root-node 'tla--widget-node
  "Root node widget for trees in tla-browse buffer."
  :value-create 'tla--widget-root-node-value-create
  :format " %v\n"
  :face 'bold)

(defun tla--widget-root-node-value-create (widget)
  "Create a value for root node represented by WIDGET."
  (insert (tla--widget-node-install-ui-element
           widget
           (widget-get widget :tag))))

(defvar tla--widget-archives-root-node-map
  (let ((map (copy-keymap tla--widget-node-map)))
    (define-key map dvc-keyvec-refresh
      'tla--widget-node-refresh)
    (define-key map (dvc-prefix-add ?a)
      'tla--widget-archives-root-node-make-archive)
    (define-key map (dvc-prefix-add ?r)
      'tla--widget-archives-root-node-register-archive)
    map)
  "Keymap used on the archives root node.")

(easy-menu-define tla--widget-archives-root-node-menu nil
  "Menu used on the root archives item in `tla-browse-mode' buffer."
  '("Archives Root"
    ["Update Archives List"
     tla--widget-node-refresh t]
    ["Make New Archive..."
     tla--widget-archives-root-node-make-archive t]
    ["Register Archive"
     tla--widget-archives-root-node-register-archive t]))

(defun tla--widget-archives-root-node-make-archive ()
  "Call `tla--make-archive' interactively  then update the tree of `tla-browse'."
  (interactive)
  (call-interactively 'tla--make-archive)
  (tla--widget-node-refresh 1))

(defun tla--widget-archives-root-node-goto (name)
  "Move the point to beginning of line in where the NAME is.
This may be useful to search an archive named NAME."
  (goto-char (point-min))
  (search-forward name)
  (beginning-of-line))

(defun tla--widget-archives-root-node-register-archive ()
  "Call `tla--register-archive' interactively ; then update the tree of `tla-browse'."
  (interactive)
  (let* ((result (call-interactively 'tla--register-archive))
         (archive-registered (nth 0 result))
         (archive (nth 1 result))
         (tla-response (nth 3 result)))
    (when archive-registered
      (tla--widget-node-refresh 1)
      (message tla-response)
      (tla--widget-archives-root-node-goto
       (if (string-match ".+: \\(.+\\)" tla-response)
           (match-string-no-properties 1 tla-response)
         archive))
      (dvc-flash-line))))


;; ----------------------------------------------------------------------------
;; Archive
;; ----------------------------------------------------------------------------
(defface tla-location
  '((((type tty) (class color)) (:weight light))
    (((class color) (background light)) (:foreground "gray"))
    (((class color) (background dark)) (:foreground "gray"))
    (t (:weight bold)))
  "Face to highlight xtla's archive location."
  :group 'tla-faces)

(defface tla-location-ftp
  '((t (:inherit tla-location)))
  "Face to highlight xtla's archive ftp location."
  :group 'tla-faces)

(defface tla-location-sftp
  '((t (:inherit tla-location :foreground "gray50")))
  "Face to highlight xtla's archive sftp location."
  :group 'tla-faces)

(defface tla-location-http
  '((t (:inherit tla-location :foreground "gray60")))
  "Face to highlight xtla's archive sftp location."
  :group 'tla-faces)

(defface tla-location-local
  '((t (:inherit tla-location :foreground "gray30")))
  "Face to highlight xtla's local archive."
  :group 'tla-faces)

(defvar tla--widget-archive-node-map
  (let ((map (copy-keymap tla--widget-node-map)))
    (define-key map dvc-keyvec-refresh
      'tla--widget-archive-node-refresh)
    (define-key map "*" 'tla--widget-archive-node-select-default)
    (define-key map dvc-keyvec-remove
      'tla--widget-archive-node-unregister-archive)
    (define-key map (dvc-prefix-add ?c)
      'tla--widget-archive-node-make-category)
    (define-key map (vector ?. dvc-key-reflect)
      'tla--widget-archive-node-start-project)
    (define-key map dvc-keyvec-reflect
      'tla--widget-node-synchronize-mirror-to-remote)
    (define-key map dvc-keyvec-get
      'tla--widget-node-synchronize-mirror-to-local)
    (define-key map (dvc-prefix-add dvc-key-reflect)
      'tla--widget-archive-node-make-mirror-at-remote)
    (define-key map (dvc-prefix-add dvc-key-get)
      'tla--widget-archive-node-make-mirror-at-local)
    map)
  "Keymap used on tla--widget-archive-node.")

(easy-menu-define tla--widget-archive-node-menu nil
  "Menu used on a archive item in `tla-browse-mode' buffer."
  '("Archive"
    ["Update Categories List"      tla--widget-archive-node-refresh t]
    ["Set Default Archive"         tla--widget-archive-node-select-default t]
    ["Remove Archive Registration" tla--widget-archive-node-unregister-archive t]
    ["Make New Category..."        tla--widget-archive-node-make-category t]
    ["Start Project from Here"     tla--widget-archive-node-start-project t]
    ["Add a Bookmark"              tla--widget-node-add-bookmark t]
    ("Remote Mirror"
     ["Synchronize Mirror to Remote From Local"
      tla--widget-node-synchronize-mirror-to-remote
      (let* ((archive (tla--name-archive (tla--widget-node-get-name)))
             (type (tla--archive-type archive)))
        (or (and (eq type 'normal)
                 (tla--archive-name-mirror archive t))
            (and (eq type 'mirror)
                 (tla--archive-name-source archive t))))]
     ["Create a Mirror at Remote"
      tla--widget-archive-node-make-mirror-at-remote
      (eq (tla--archive-type (tla--name-archive (tla--widget-node-get-name)))
          'normal)])
    ("Local Mirror"
     ["Synchronize Mirror to Local[TODO]"
      ;; TODO
      tla--widget-node-synchronize-mirror-to-local nil]
     ["Create a Mirror at Local" tla--widget-archive-node-make-mirror-at-local
      (eq (tla--archive-type (tla--name-archive (tla--widget-node-get-name)))
          'source)]
     "--"
     ["Convert to SOURCE archive" tla--widget-archive-node-convert-to-source
      (eq (tla--archive-type (tla--name-archive (tla--widget-node-get-name)))
          'normal)])
    ["Save Name to Kill Ring" tla--widget-node-save-name-to-kill-ring t]))

(defconst tla--widget-archive-node-tag "a")
(defconst tla--widget-archive-node-default-tag "A")

(define-widget 'tla--widget-archive-node 'tla--widget-node
  "Archive node in tla-browse."
  :tag tla--widget-archive-node-tag
  :value-create 'tla--widget-archive-node-value-create
  :tla-type 'archive
  :face 'tla-archive-name
  :keymap 'tla--widget-archive-node-map
  :menu tla--widget-archive-node-menu
  :archive nil
  :archive-location nil
  :archive-defaultp nil)

(defvar tla--widget-archive-node-list nil)
(defun tla--browse-expand-archives (root)
  "Expand ROOT widget."
  (or (and (not current-prefix-arg) (widget-get root :args))
      (let ((default-archive (tla-my-default-archive)))
        (setq tla--widget-archive-node-list nil)
        (mapcar
         (lambda (archive)
           (let ((res
                  `(tree-widget
                    :open ,(tla--browse-open-list-member (car archive))
                    :has-children t
                    :dynargs tla--browse-expand-categories
                    :node (tla--widget-archive-node
                           :tag ,(if (equal default-archive (car archive))
                                     tla--widget-archive-node-default-tag
                                   tla--widget-archive-node-tag)
                           :archive ,(car archive)
                           ;; TODO(Multiple locations)
                           :archive-location ,(car (cadr archive))
                           :archive-defaultp ,(equal
                                               default-archive
                                               (car
                                                archive))))))
             (widget-put (widget-get res :node) :parent res)
             res))
         (progn
           (tla--archive-tree-build-archives (not current-prefix-arg) t)
           tla--archive-tree)))))

(defun tla--widget-archive-node-value-create (widget)
  "Create values for WIDGET."
  (push widget tla--widget-archive-node-list)
  (insert (let* ((archive  (widget-get widget :archive))
                 (location (widget-get widget :archive-location))
                 (defaultp (widget-get widget :archive-defaultp))
                 (marks    (widget-get widget :marks))
                 (value (progn
                          (case (tla--archive-type archive)
                            (mirror (widget-put widget :face 'tla-mirror-archive-name))
                            (source (widget-put widget :face 'tla-source-archive-name)))
                          ;;
                          ;; It seems that XEmacs's format hides text properties.
                          ;;
                          (concat marks
                                  (tla--widget-node-install-ui-element
                                   widget archive (when defaultp
                                                    'dvc-marked))
                                  " => "
                                  (if location
                                      (tla--widget-archive-put-face-on-location
                                       location)
                                    "*unknown now*")))))
            value)))

(defun tla--widget-archive-put-face-on-location (location)
  "Set face to LOCATION based on the location type(ftp, sftp, http or local)."
  (let ((face (case (tla--location-type location)
                (ftp 'tla-location-ftp)
                (sftp 'tla-location-sftp)
                (http 'tla-location-http)
                (local 'tla-location-local)))
        (location (copy-sequence location)))
    (put-text-property 0 (length location)
                       'face face location)
    location))

(defun tla--widget-archive-node-refresh ()
  "Refresh an archive node under the point."
  (interactive)
  (tla--widget-node-refresh 1 nil
                            (tla--name-archive
                             (tla--widget-node-get-name))))

(defun tla--widget-archive-node-select-default ()
  "Mark a widget associated with the default archive.
Unmark widgets not associated with the default archive.
`:archive-defaultp' keyword is used to mark."
  (interactive)
  (mapc
   (lambda (widget)
     (when (equal tla--widget-archive-node-default-tag
                  (widget-get widget :tag))
       (widget-put widget :tag tla--widget-archive-node-tag)
       (widget-put widget :archive-defaultp nil)
       (widget-value-set widget (widget-value widget))))
   tla--widget-archive-node-list)
  (let* ((widget (tla--widget-node-get-at))
         (archive (tla--name-archive (tla--widget-node-get-name) )))
    (tla-my-default-archive archive)
    (widget-put widget :tag tla--widget-archive-node-default-tag)
    (widget-put widget :archive-defaultp t)
    (widget-value-set widget (widget-value widget))))

(defun tla--widget-archive-node-unregister-archive ()
  "Delete the registration of the archive under the point."
  (interactive)
  (let ((archive (tla--name-archive (tla--widget-node-get-name))))
    (if archive
        (progn (tla--unregister-archive archive t)
               (tla--widget-node-refresh 2))
      (error "No archive under the point"))))

(defun tla--widget-archive-node-make-category ()
  "Make new category in the archive under the point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (l (tla-name-read "New Category: "
                           archive
                           'prompt)))
    (tla-make-category (tla--name-archive l) (tla--name-category l))
    (tla--widget-node-refresh 1 nil (tla--name-archive l))
    (tla--browse-open t
                      (tla--name-archive l)
                      (tla--name-category l))
    ))

(defun tla--widget-archive-node-convert-to-source ()
  "Convert the archive under the point to a source archive."
  (interactive)
  (let* ((widget (tla--widget-node-get-at))
         (archive (widget-get widget :archive))
         (location (widget-get widget :archive-location))
         (result (tla--archive-convert-to-source-archive archive location)))
    (let ((archive-registered (nth 0 result))
          (archive (nth 1 result))
          (tla-response (nth 3 result)))
      (when archive-registered
        (tla--widget-node-refresh 2)
        (message tla-response)
        (tla--widget-archives-root-node-goto
         (if (string-match ".+: \\(.+\\)" tla-response)
             (match-string-no-properties 1 tla-response)
           archive))
        (dvc-flash-line)))))

(defun tla--widget-archive-node-start-project ()
  "Start new project in the archive unde the point."
  (interactive)
  (let* ((archive (tla--name-archive (tla--widget-node-get-name)))
         (buffer (current-buffer))
         (p (point))
         (result (tla-start-project archive 'synchronously))
         (category (tla--name-category (car result)))
         (branch (tla--name-branch (car result)))
         (version (tla--name-version (car result)))
         )
    (with-current-buffer buffer
      (tla--widget-node-refresh 1 p archive)
      (tla--browse-open t
                        archive category branch version))))

(defun tla--widget-archive-node-make-mirror-at-remote ()
  "Create a mirror for the local archive under the point at somewhere remote."
  (interactive)
  (let ((archive (tla--name-archive (tla--widget-node-get-name))))
    (unless archive
      (error "No archive under the point"))
    (tla-mirror-archive archive nil nil nil nil)
    (tla--widget-node-refresh 2)
    (tla--widget-archives-root-node-goto (format
                                          (if (tla-use-baz-archive-registration)
                                              "%s"
                                            "%s-MIRROR")
                                          archive))
    (dvc-flash-line)))

(defun tla--widget-archive-node-make-mirror-at-local ()
  "Create a mirror for the remote archive under the point to local."
  (interactive)
  (let ((archive (tla--name-archive (tla--widget-node-get-name))))
    (unless archive
      (error "No archive under the point"))
    (tla-mirror-from-archive archive nil)
    (tla--widget-node-refresh 2)
    (string-match "\\(.*\\)-SOURCE$" archive)
    (tla--widget-archives-root-node-goto
     ;; Adding a space not to match SOURCE archive.
     (concat (match-string 1 archive) " "))
    (dvc-flash-line)))

;; ----------------------------------------------------------------------------
;; Categories
;; ----------------------------------------------------------------------------
(defvar tla--widget-category-node-map
  (let ((map (copy-keymap tla--widget-node-map)))
    (define-key map dvc-keyvec-refresh
      'tla--widget-category-node-refresh)
    (define-key map (dvc-prefix-add ?b)
      'tla--widget-category-node-make-branch)
    map)
  "Keymap used on tla--widget-category-node.")

(easy-menu-define tla--widget-category-node-menu nil
  "Menu used on a archive item in `tla-browse-mode' buffer."
  '("Category"
    ["Update Branches List" tla--widget-category-node-refresh t]
    ["Remove Category[NOT IMPLEMENTED]" nil t]
    ["Make New Branch..." tla--widget-category-node-make-branch t]
    ["Add a Bookmark" tla--widget-node-add-bookmark t]
    ["Synchronize Mirror to Remote"
     tla--widget-node-synchronize-mirror-to-remote t]
    ["Save Name to Kill Ring" tla--widget-node-save-name-to-kill-ring t]))

(define-widget 'tla--widget-category-node 'tla--widget-node
  "Category node in tla-browse."
  :tag "c"
  :value-create 'tla--widget-category-node-value-create
  :tla-type 'category
  :face 'tla-category-name
  :keymap 'tla--widget-category-node-map
  :menu tla--widget-category-node-menu
  :archive nil
  :category nil)

(defun tla--browse-expand-categories (archive)
  "Expand ARCHIVE widget."
  (or (and (not current-prefix-arg) (widget-get archive :args))
      (let ((archive-name (widget-get
                           (widget-get archive :node)
                           :archive)))
        (mapcar
         (lambda (category)
           (let ((res `(tree-widget
                        :open ,(tla--browse-open-list-member archive-name
                                                             (car category))
                        :has-children t
                        :dynargs tla--browse-expand-branches
                        :node (tla--widget-category-node
                               :archive ,archive-name
                               :category ,(car category)))))
             (widget-put (widget-get res :node) :parent res)
             res))
         (let* ((l (cddr (tla--archive-tree-get-archive
                          archive-name))))
           (when (or (null l) current-prefix-arg)
             (tla--archive-tree-build-categories archive-name nil t))
           (cddr (tla--archive-tree-get-archive archive-name)))))))

(defun tla--widget-category-node-value-create (widget)
  "Create values for category WIDGET."
  (tla--widget-node-value-create widget :category))

(defun tla--widget-category-node-refresh ()
  "Refresh a category widget at the point."
  (interactive)
  (let ((name (tla--widget-node-get-name)))
    (tla--widget-node-refresh 1 nil
                              (tla--name-archive name)
                              (tla--name-category name))))

(defun tla--widget-category-node-make-branch ()
  "Make new branch in the category under the point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (category  (tla--name-category name))
         (l (tla-name-read "New Branch: "
                           archive
                           category
                           'prompt)))
    (tla-make-branch (tla--name-archive l)
                     (tla--name-category l)
                     (tla--name-branch l))
    (tla--widget-node-refresh 1 nil
                              (tla--name-archive l)
                              (tla--name-category l))
    (tla--browse-open t
                      (tla--name-archive l)
                      (tla--name-category l)
                      (tla--name-branch l))))

;; ----------------------------------------------------------------------------
;; Branch
;; ----------------------------------------------------------------------------
(defvar tla--widget-branch-node-map
  (let ((map (copy-keymap tla--widget-node-map)))
    (define-key map dvc-keyvec-refresh
      'tla--widget-branch-node-refresh)
    (define-key map (dvc-prefix-add ?v)
      'tla--widget-branch-node-make-version)
    (define-key map dvc-keyvec-get
      'tla--widget-branch-node-get-branch)
    map)
  "Keymap used on tla--widget-branch-node.")

(easy-menu-define tla--widget-branch-node-menu nil
  "Menu used on a archive item in `tla-browse-mode' buffer."
  '("Branch"
    ["Update Version List" tla--widget-branch-node-refresh t]
    ["Remove Branch Registration[NOT IMPLEMENTED]" nil t]
    ["Make New Version..." tla--widget-branch-node-make-version t]
    ["Get..."              tla--widget-branch-node-get-branch t]
    ["Add a Bookmark" tla--widget-node-add-bookmark t]
    ["Synchronize Mirror to Remote"
     tla--widget-node-synchronize-mirror-to-remote t]
    ["Save Name to Kill Ring" tla--widget-node-save-name-to-kill-ring t]))

(define-widget 'tla--widget-branch-node 'tla--widget-node
  "Branch node in tla-browse."
  :tag "b"
  :value-create 'tla--widget-branch-node-value-create
  :tla-type 'branch
  :face 'tla-branch-name
  :keymap 'tla--widget-branch-node-map
  :menu tla--widget-branch-node-menu
  :archive nil
  :category nil
  :branch nil)

(defun tla--browse-expand-branches (category)
  "Expand CATEGORY widget."
  (or (and (not current-prefix-arg) (widget-get category :args))
      (let* ((parent-node   (widget-get category :node))
             (archive-name  (widget-get parent-node :archive))
             (category-name (widget-get parent-node :category)))
        (mapcar
         (lambda (branch)
           (let ((res
                  `(tree-widget
                    :open ,(tla--browse-open-list-member archive-name
                                                         category-name
                                                         (car branch))
                    :has-children t
                    :leaf-control tla--widget-version-control
                    :dynargs tla--browse-expand-versions
                    :node (tla--widget-branch-node
                           :archive ,archive-name
                           :category ,category-name
                           :branch ,(car branch)))))
             (widget-put (widget-get res :node) :parent res)
             res))
         (let* ((l (cdr (tla--archive-tree-get-category
                         archive-name
                         category-name))))
           (when (or (null l) current-prefix-arg)
             (tla--archive-tree-build-branches archive-name
                                               category-name
                                               nil t))
           (cdr (tla--archive-tree-get-category archive-name
                                                category-name)))))))

(defun tla--widget-branch-node-value-create (widget)
  "Create values for branch WIDGET."
  (tla--widget-node-value-create widget :branch))

(defun tla--widget-branch-node-refresh ()
  "Refresh a branch widget at the point."
  (interactive)
  (let ((name (tla--widget-node-get-name)))
    (tla--widget-node-refresh 1 nil
                              (tla--name-archive name)
                              (tla--name-category name)
                              (tla--name-branch name))))

(defun tla--widget-branch-node-make-version ()
  "Make new version in the branch under the point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (category (tla--name-category name))
         (branch (tla--name-category name))
         (l (tla-name-read "New Version: "
                           archive
                           category
                           branch
                           'prompt)))
    (tla-make-version (tla--name-archive l)
                      (tla--name-category l)
                      (tla--name-branch l)
                      (tla--name-version l))
    (tla--widget-node-refresh 1 nil
                              (tla--name-archive l)
                              (tla--name-category l)
                              (tla--name-branch l))
    (tla--browse-open t
                      (tla--name-archive l)
                      (tla--name-category l)
                      (tla--name-branch l)
                      (tla--name-version l))))

(defun tla--widget-branch-node-get-branch ()
  "Run `tla get' against the branch at point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (category (tla--name-category name))
         (branch (tla--name-branch name))
         (directory (expand-file-name
                     (dvc-read-directory-name
                      (format "Restore \"%s\" to: "
                              (progn
                                (unless branch
                                  (error "No branch under the point"))
                                (tla--name-construct
                                 archive category branch)))))))
    (if branch
        (tla-get directory
                 'ask
                 archive
                 category
                 branch)
      (error "No branch under the point"))))


;; ----------------------------------------------------------------------------
;; Version
;; ----------------------------------------------------------------------------
(defvar tla--widget-version-node-map
  (let ((map (copy-keymap tla--widget-node-map)))
    (define-key map dvc-keyvec-refresh
      'tla--widget-version-node-show-revisions)
    (define-key map dvc-keyvec-get
      'tla--widget-version-node-get-version)
    (define-key map dvc-keyvec-tag
      'tla--widget-version-node-tag)
    (define-key map [?L]
      'tla--widget-version-node-add-to-library)
    map)
  "Keymap used on tla--widget-version-node.")

(easy-menu-define tla--widget-version-node-menu nil
  "Menu used on a archive item in `tla-browse-mode' buffer."
  '("Version"
    ["Show Revisions" tla--widget-version-node-show-revisions t]
    ["Remove Version Registration[NOT IMPLEMENTED]" nil t]
    ["Get..." tla--widget-version-node-get-version t]
    ["Add to Library" tla--widget-version-node-add-to-library t]
    ["Add a Bookmark" tla--widget-node-add-bookmark t]
    ["Synchronize Mirror to Remote"
     tla--widget-node-synchronize-mirror-to-remote t]
    ["Put Tag..." tla--widget-version-node-tag t]
    ["Save Name to Kill Ring" tla--widget-node-save-name-to-kill-ring t]))

(define-widget 'tla--widget-version-node 'tla--widget-node
  "Version node in tla-browse."
  :tag "v"
  :value-create 'tla--widget-version-node-value-create
  :tla-type 'version
  :face 'tla-version-name
  :keymap 'tla--widget-version-node-map
  :menu   tla--widget-version-node-menu
  :archive nil
  :category nil
  :branch nil
  :version nil
  :open-subtree 'tla--widget-version-node-open-subtree
  :close-subtree 'tla--widget-version-node-open-subtree)


(define-widget 'tla--widget-version-control 'tree-widget-empty-control
  "Control widget that represents a leaf version node."
  :tag       "[->]"
  :format    "%[%t%]"
  :action  'tla--widget-version-control-show-revisions)

(defun tla--widget-version-control-show-revisions (widget &optional event)
  "Show revisions in a version associated with WIDGET.
The version is under the point or place where click EVENT is created."
  (if event
      (mouse-set-point event))
  (let ((pos (next-single-property-change (point)
                                          'widget
                                          (current-buffer)
                                          (line-end-position))))
    (when pos
      (tla--widget-version-node-show-revisions pos))))

(defun tla--browse-expand-versions (branch)
  "Expand BRANCH widget."
  (or (and (not current-prefix-arg) (widget-get branch :args))
      (let* ((parent-node   (widget-get branch :node))
             (archive-name  (widget-get parent-node :archive))
             (category-name (widget-get parent-node :category))
             (branch-name (widget-get parent-node :branch)))
        (mapcar (lambda (version)
                  `(tla--widget-version-node
                    :archive  ,archive-name
                    :category ,category-name
                    :branch   ,branch-name
                    :version  ,(car version)))
                (let* ((l (cdr (tla--archive-tree-get-branch archive-name
                                                             category-name
                                                             branch-name))))
                  (when (or (null l) current-prefix-arg)
                    (tla--archive-tree-build-versions archive-name
                                                      category-name
                                                      branch-name
                                                      nil t))
                  (cdr (tla--archive-tree-get-branch archive-name
                                                     category-name
                                                     branch-name)))))))

(defun tla--widget-version-node-value-create (widget)
  "Create values for version WIDGET."
  (tla--widget-node-value-create widget :version))

(defun tla--widget-version-node-show-revisions (&optional point)
  "Show revisions in the version under the POINT.
If POINT is nil, use the point under `point'."
  (interactive)
  (let ((name (tla--widget-node-get-name (or point (point)))))
    (tla-revisions (tla--name-archive name)
                   (tla--name-category name)
                   (tla--name-branch name)
                   (tla--name-version name)
                   nil nil)))

(defun tla--widget-version-node-get-version ()
  "Run \"tla get\" against the version at point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (category (tla--name-category name))
         (branch (tla--name-branch name))
         (version (tla--name-version name))
         (directory (expand-file-name
                     (dvc-read-directory-name
                      (format "Restore \"%s\" to: "
                              (progn
                                (unless version
                                  (error "No version under the point"))
                                (tla--name-construct
                                 archive category branch version)))))))
    (if version
        (tla-get directory
                 'ask
                 archive
                 category
                 branch
                 version)
      (error "No version under the point"))))

(defun tla--widget-version-node-add-to-library ()
  "Run \"tla library-add\" against the version at point."
  (interactive)
  (let* ((name (tla--widget-node-get-name))
         (archive (tla--name-archive name))
         (category (tla--name-category name))
         (branch (tla--name-branch name))
         (version (tla--name-version name)))
    (if version
        (tla-library-add archive category branch version)
      (error "No version under the point"))))

(defun tla--widget-version-node-tag ()
  "Run tla tag from the version under the point."
  (interactive)
  (let* ((from (tla--widget-node-get-name))
         (from-fq (tla--name-construct from))
         (to   (tla-name-read (format "Tag from `%s' to: " from-fq)
                              'prompt 'prompt 'prompt 'prompt))
         (to-fq (tla--name-construct to)))
    (unless from
      (error "No version under the point"))
    (unless to-fq
      (error "Wrong version tagged to is given"))
    (save-excursion
      (tla--version-tag-internal from-fq to-fq 'synchronously))
    ;;
    (tla--browse-open nil
                      (tla--name-archive to-fq))
    (tla--widget-node-refresh 1
                              'name
                              (tla--name-archive to-fq))
    (tla--browse-open nil
                      (tla--name-archive to-fq)
                      (tla--name-category to-fq))

    (tla--widget-node-refresh 1
                              'name
                              (tla--name-archive to-fq)
                              (tla--name-category to-fq))
    (tla--browse-open nil
                      (tla--name-archive to-fq)
                      (tla--name-category to-fq)
                      (tla--name-branch to-fq))
    (tla--widget-node-refresh 1
                              'name
                              (tla--name-archive to-fq)
                              (tla--name-category to-fq)
                              (tla--name-branch to-fq))
    (tla--browse-open t
                      (tla--name-archive to-fq)
                      (tla--name-category to-fq)
                      (tla--name-branch to-fq)
                      (tla--name-version to-fq))))

(defun tla--widget-version-node-open-subtree (widget)
  "List revisions in the version associated with WIDGET."
  (tla-revisions (widget-get widget :archive)
                 (widget-get widget :category)
                 (widget-get widget :branch)
                 (widget-get widget :version)
                 nil nil))

;; ----------------------------------------------------------------------------
;; Entry point
;; ----------------------------------------------------------------------------
;; TODO: Filtered by GROUP in bookmark
;;;###autoload
(defun tla-browse (&optional initial-open-list append)
  "Browse registered archives as trees within one buffer.
You can specify the node should be opened by alist,
INITIAL-OPEN-LIST.  If APPEND is nil, the nodes not in
INITIAL-OPEN-LIST are made closed.  If non-nil, the nodes
already opened are kept open."

  (interactive)
  (switch-to-buffer (dvc-get-buffer-create tla-arch-branch
                                           tla--browse-buffer-type))
  (make-local-variable 'tla--browse-open-list)
  (setq truncate-lines t)

  (let (building)
    (if (zerop (buffer-size))
        (progn (setq building t)
               (tla--browse-set-initial-open-list initial-open-list t))
      (if append
          (progn
            (setq building nil)
            (tla--browse-set-initial-open-list initial-open-list nil))
        (if (y-or-n-p (format "Remove old %s? " (buffer-name)))
            (progn (setq building t)
                   (tla--browse-set-initial-open-list initial-open-list nil))
          (setq building nil)
          (tla--browse-set-initial-open-list initial-open-list t))))

    (if building
        (progn
          (tla--browse-erase-buffer)
          (tla--browse-build-buffer))
      (mapc
       (lambda (elt)
         (tla--browse-open nil
                           (tla--name-archive elt)
                           (tla--name-category elt)
                           (tla--name-branch elt)
                           (tla--name-version elt)))
       tla--browse-open-list)))
  (goto-char (point-min))
  (tla-browse-mode))

(defun tla--browse-set-initial-open-list (list clearp)
  "Insert LIST to `tla--browse-open-list'.
If CLEARP is set, clear `tla--browse-open-list' before insertion.
This is a helper function for `tla-browse'."
  (when clearp
    (setq tla--browse-open-list nil))
  (mapc
   (lambda (elt)
     (tla--browse-open-list-add (tla--name-archive elt)
                                (tla--name-category elt)
                                (tla--name-branch elt)
                                (tla--name-version elt)))
   list))
(defun tla--browse-erase-buffer ()
  "Erase *tla-browse* buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  ;; remove-overlays is not portable enough.
  (mapc #'delete-overlay (overlays-in
                          (point-min) (point-max))))

(defun tla--browse-build-buffer ()
  "Insert contents of *tla-buffer*."
  ;; Tla config
  (widget-create 'tree-widget
                 :open t
                 :node '(item :format "%[%t%]\n"
                              :tag "Personal Configuration")
                 :has-chidren t
                 `(tla--widget-my-id ,(tla-my-id)))

  (widget-insert "\n")

  ;; Archives
  (add-hook 'tree-widget-after-toggle-functions
            'tla--browse-open-tracker)
  (widget-create 'tree-widget
                 :open t
                 :node `(tla--widget-root-node
                         :tla-type archives-root
                         :tag "Archives"
                         :keymap tla--widget-archives-root-node-map
                         :menu ,tla--widget-archives-root-node-menu)
                 :has-children t
                 :dynargs 'tla--browse-expand-archives)
  ;; Libraries
  ;; TODO
  (widget-setup))

(defun tla--browse-toggle-subtree-maybe ()
  "Run `tla--browse-toggle-subtree'.
Before running a widget is searched and move the point to
the widget if it is found.  If no widget is found,
`widget-button-press'."
  (interactive)
  (let ((p (next-single-property-change (line-beginning-position)
                                        'widget
                                        nil
                                        (line-end-position))))
    (if (and p (tla--widget-node-get-type p))
        (tla--widget-node-toggle-subtree p)
      (widget-button-press (point)))))

(defun tla--browse-dash ()
  "Move the point to the place where a widget is in the current line."
  (interactive)
  (let ((p (next-single-property-change (line-beginning-position)
                                        'widget
                                        nil
                                        (line-end-position))))
    (when (and p (tla--widget-node-get-type p))
      (goto-char p)
      (dvc-flash-line))))

(defvar tla-browse-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map (dvc-prefix-buffer ?p) 'dvc-show-process-buffer)
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map (dvc-prefix-buffer dvc-key-show-bookmark) 'tla-bookmarks)
    (define-key map [return] 'tla--browse-toggle-subtree-maybe)
    (define-key map "\C-m" 'tla--browse-toggle-subtree-maybe)
    (define-key map " " 'tla--browse-dash)
    (define-key map dvc-keyvec-next     'next-line)
    (define-key map dvc-keyvec-previous 'previous-line)
    (define-key map dvc-keyvec-quit     'kill-this-buffer)
    (define-key map [?+] 'tla--widget-node-toggle-subtree-recursive)
    map)
  "Keymap used in `tla-browse-mode'.")

(defun tla-browse-mode ()
  "Mode for browsing tla's archives.
Don't use this function.  Instead call `tla-browse'."
  (dvc-install-buffer-menu)
  (setq major-mode 'tla-browse-mode
        mode-name "tla-browse")
  (use-local-map tla-browse-map)
  (set-buffer-modified-p nil)
  (run-hooks 'tla-browse-mode-hook))

(provide 'tla-browse)

;; Local Variables:
;; End:
;;; tla-browse.el ends here

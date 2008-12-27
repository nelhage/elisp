;;; dvc-xemacs.el --- Compatibility stuff for XEmacs
;;;
;;; This file should be loaded when using XEmacs; load
;;; dvc-emacs.el when using Gnu Emacs.

;; Copyright (C) 2004-2006, 2008 by all contributors

;; Author: Robert Widhopf-Fenk <hack@robf.de>

;; This file is part of DVC.
;;
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

;;; Policy: see dvc-emacs.el for policy on what goes in this file.

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'overlay)
  (require 'wid-edit)
  ;; The following require causes a infinite recursion as the (provide ...) is at
  ;; the file end.  Thus we live with the warnings about unknown variables etc.
  ;;(require 'dvc-core)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixes warnings about undefined variables
(unless (boundp 'add-log-buffer-file-name-function)
  (defvar add-log-buffer-file-name-function nil))
(unless (boundp 'add-log-file-name-function)
  (defvar add-log-file-name-function nil))
(unless (boundp 'add-log-keep-changes-together)
  (defvar add-log-keep-changes-together nil))
(unless (boundp 'global-font-lock-mode)
  (defvar global-font-lock-mode nil))
(unless (boundp 'vc-ignore-vc-files)
  (defvar vc-ignore-vc-files nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'add-log-file-name)
  (defun add-log-file-name (buffer-file log-file)
    ;; Never want to add a change log entry for the ChangeLog file itself.
    (unless (or (null buffer-file) (string= buffer-file log-file))
      (if add-log-file-name-function
          (funcall add-log-file-name-function buffer-file)
        (setq buffer-file
              (if (string-match
                   (concat "^" (regexp-quote (file-name-directory log-file)))
                   buffer-file)
                  (substring buffer-file (match-end 0))
                (file-name-nondirectory buffer-file)))
        ;; If we have a backup file, it's presumably because we're
        ;; comparing old and new versions (e.g. for deleted
        ;; functions) and we'll want to use the original name.
        (if (backup-file-name-p buffer-file)
            (file-name-sans-versions buffer-file)
          buffer-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the unless check seems to fail
;;(unless (functionp 'replace-regexp-in-string)
(defun replace-regexp-in-string (regexp rep string
                                        &optional fixedcase literal)
  (replace-in-string string regexp rep literal))
;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'line-end-position)
  (defun line-end-position ()
    (save-excursion (end-of-line) (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'line-beginning-position)
  (defun line-beginning-position (&optional n)
    (save-excursion
      (if n (forward-line n))
      (beginning-of-line)
      (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'mouse-set-point)
  (defun mouse-set-point (event)
    (goto-char (event-point event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'match-string-no-properties)
  (defun match-string-no-properties (arg &optional string)
    (match-string arg string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'clone-buffer)
  (defun clone-buffer (&optional newname display-flag)
    "Create a twin copy of the current buffer.
If NEWNAME is nil, it defaults to the current buffer's name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.

If DISPLAY-FLAG is non-nil, the new buffer is shown with `pop-to-buffer'.
This runs the normal hook `clone-buffer-hook' in the new buffer
after it has been set up properly in other respects."
    (interactive (list (if current-prefix-arg (read-string "Name: "))
                       t))
    (if buffer-file-name
        (error "Cannot clone a file-visiting buffer"))
    (if (get major-mode 'no-clone)
        (error "Cannot clone a buffer in %s mode" mode-name))
    (setq newname (or newname (buffer-name)))
    (if (string-match "<[0-9]+>\\'" newname)
        (setq newname (substring newname 0 (match-beginning 0))))
    (let ((buf (current-buffer))
          (ptmin (point-min))
          (ptmax (point-max))
          (pt (point))
          (mk (mark t))
          (modified (buffer-modified-p))
          (mode major-mode)
          (lvars (buffer-local-variables))
          (process (get-buffer-process (current-buffer)))
          (new (generate-new-buffer (or newname (buffer-name)))))
      (save-restriction
        (widen)
        (with-current-buffer new
          (insert-buffer-substring buf)))
      (with-current-buffer new
        (narrow-to-region ptmin ptmax)
        (goto-char pt)
        (if mk (set-mark mk))
        (set-buffer-modified-p modified)

        ;; Clone the old buffer's process, if any.
        (when process (clone-process process))

        ;; Now set up the major mode.
        (funcall mode)

        ;; Set up other local variables.
        (mapcar (lambda (v)
                  (condition-case ()    ;in case var is read-only
                      (if (symbolp v)
                          (makunbound v)
                        (set (make-local-variable (car v)) (cdr v)))
                    (error nil)))
                lvars)

        ;; Run any hooks (typically set up by the major mode
        ;; for cloning to work properly).
        (run-hooks 'clone-buffer-hook))
      (if display-flag (pop-to-buffer new))
      new)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'make-temp-file)
  (defun make-temp-file (prefix &optional dir-flag)
    "Create a temporary file.
The returned file name (created by `make-temp-name', is guaranteed to point to
a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file."
    (let (file)
      (while (condition-case ()
                 (progn
                   (setq file
                         (make-temp-name
                          (expand-file-name prefix)))
                   (if dir-flag
                       (make-directory file)
                     (write-region "" nil file nil 'silent nil))
                   nil)
               (file-already-exists t))
        ;; the file was somehow created by someone else between
        ;; `make-temp-name' and `write-region', let's try again.
        nil)
      file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AFAIK easy-menu cannot be used for dynamic menus

(defun dvc-xemacs-dvc-mode-p (buf)
  "Helper function for menu-related functions.

Return t if BUF is a dvc-related buffer."
  (if (bufferp buf)
      (setq buf (format "%s" (symbol-value-in-buffer 'major-mode buf))))
  (string-match "^dvc-" buf))

(defvar dvc-dead-process-buffer-queue nil)

(defun dvc-xemacs-buffers-menu (menu)
  "Create the markers-menu.

MENU is the menu to which items should be added."
  (interactive (list nil))
  (let ((bufs (buffer-list))
        (queue dvc-dead-process-buffer-queue)
        queue-menu
        b)
    ;; the user buffers
    (while bufs
      (setq b (car bufs)
            bufs (cdr bufs))
      (if (dvc-xemacs-dvc-mode-p b)
          (setq menu (cons (vector (buffer-name b)
                                   (list 'switch-to-buffer b) t)
                           menu))))
    (setq menu (sort menu
                     (lambda (m1 m2) (string< (aref m1 0) (aref m2 0)))))
    ;; the queue buffers
    (while queue
      (setq b (car queue)
            queue (cdr queue)
            queue-menu (cons (vector (buffer-name b)
                                     (list 'switch-to-buffer b) t)
                             queue-menu)))
    (setq queue-menu (sort queue-menu
                           (lambda (m1 m2) (string< (aref m1 0) (aref m2 0)))))
    ;; combine menus
    (setq menu (cons (append '("Queue") queue-menu) menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dvc-group-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-grouping-function'.
This groups buffers by major mode.  It only really makes sense if
`buffers-menu-sorting-function' is
'dvc-sort-buffers-menu-by-mode-then-alphabetically'.

 (setq buffers-menu-grouping-function 'dvc-group-buffers-menu-by-mode-then-alphabetically)
BUF1 and BUF2 are successive members of the sorted buffers list after
being passed through `buffers-menu-sort-function'. It should return
non-nil if the second buffer begins a new group.

This is a modified version of
`group-buffers-menu-by-mode-then-alphabetically'
adding an submenu \"DVC\" containing all dvc buffers."
  (cond ((and buf1 buf2
              (not (dvc-xemacs-dvc-mode-p buf1))
              (dvc-xemacs-dvc-mode-p buf2))
         (if (string-match "\\`*" (buffer-name buf1))
             "*Misc*"
           (symbol-value-in-buffer 'mode-name buf1)))
        ((and buf1
              (dvc-xemacs-dvc-mode-p buf1)
              (or (not buf2)
                  (not (dvc-xemacs-dvc-mode-p buf2))))
         "DVC")
        ((string-match "\\`*" (buffer-name buf1))
         (and (null buf2) "*Misc*"))
        ((or (null buf2)
             (string-match "\\`*" (buffer-name buf2))
             (not (eq (symbol-value-in-buffer 'major-mode buf1)
                      (symbol-value-in-buffer 'major-mode buf2))))
         (symbol-value-in-buffer 'mode-name buf1))
        (t nil)))

(defun dvc-sort-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts first by major mode and then alphabetically by name, but puts buffers
beginning with a star at the end of the list.

 (setq buffers-menu-sort-function 'dvc-sort-buffers-menu-by-mode-then-alphabetically)
It will be passed two arguments BUF1 and BUF2 (two buffers to compare)
and will return t if the first is \"less\" than the second.

This is a modified version of `sort-buffers-menu-by-mode-then-alphabetically',
causing all *dvc-* buffers to be treated as having the same major mode."
  (let* ((nam1 (buffer-name buf1))
         (nam2 (buffer-name buf2))
         (inv1p (not (null (string-match "\\` " nam1))))
         (inv2p (not (null (string-match "\\` " nam2))))
         (star1p (not (null (string-match "\\`*" nam1))))
         (star2p (not (null (string-match "\\`*" nam2))))
         (mode1 (symbol-value-in-buffer 'major-mode buf1))
         (mode2 (symbol-value-in-buffer 'major-mode buf2)))
    (if (dvc-xemacs-dvc-mode-p mode1)
        (setq mode1 "dvc"))
    (if (dvc-xemacs-dvc-mode-p mode1)
        (setq mode2 "dvc"))
    (cond ((not (eq inv1p inv2p))
           (not inv1p))
          ((not (eq star1p star2p))
           (not star1p))
          ((and star1p star2p (string-lessp nam1 nam2)))
          ((string-lessp mode1 mode2)
           t)
          ((string-lessp mode2 mode1)
           nil)
          (t
           (string-lessp nam1 nam2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; since the custom.el coming with XEmacs does not know about the :inherit
;; keyword of defface we are dealing with it for our faces ...
(let ((faces (face-list)) face inherit)
  (while faces
    (setq face (car faces)
          faces (cdr faces))
    (when (string-match "^dvc-" (format "%s" face))
      (setq inherit (assoc :inherit (car (custom-face-get-spec face))))
      (if inherit
          (set-face-parent face (cadr inherit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (buffer-substring)))

(unless (functionp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

(unless (functionp 'diff-hunk-next)
  (defalias 'diff-hunk-next 'diff-next-hunk))

(unless (functionp 'diff-hunk-prev)
  (defalias 'diff-hunk-prev 'diff-prev-hunk))

(defalias 'dvc-expand-file-name 'expand-file-name)

;; FIXME: move to dvc-utils?
(defun dvc-xmas-make-temp-dir (prefix)
  "Make a temporary directory using PREFIX.
Return the name of the directory."
  (let ((dir (make-temp-name (expand-file-name prefix (temp-directory)))))
    (make-directory dir)
    dir))

(defalias 'dvc-make-temp-dir 'dvc-xmas-make-temp-dir)

;; From Gnus.
(defun dvc-xmas-move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end buffer))

(defun dvc-xmas-kill-all-overlays ()
  "Delete all extents in the current buffer."
  (map-extents (lambda (extent ignore)
                 (delete-extent extent)
                 nil)))

(defun dvc-xmas-add-text-properties (start end props &optional object)
  (add-text-properties start end props object)
  (put-text-property start end 'start-closed nil object))

(defun dvc-xmas-put-text-property (start end prop value &optional object)
  (put-text-property start end prop value object)
  (put-text-property start end 'start-closed nil object))

(defun dvc-xmas-assq-delete-all (key alist)
  (let ((elem nil))
    (while (setq elem (assq key alist))
      (setq alist (delq elem alist)))
    alist))

(defalias 'dvc-make-overlay 'make-extent)
(defalias 'dvc-delete-overlay 'delete-extent)
(defalias 'dvc-overlay-put 'set-extent-property)
(defalias 'dvc-move-overlay 'dvc-xmas-move-overlay)
(defalias 'dvc-overlay-buffer 'extent-object)
(defalias 'dvc-overlay-start 'extent-start-position)
(defalias 'dvc-overlay-end 'extent-end-position)
(defalias 'dvc-kill-all-overlays 'dvc-xmas-kill-all-overlays)
(defalias 'dvc-extent-detached-p 'extent-detached-p)
(defalias 'dvc-add-text-properties 'dvc-xmas-add-text-properties)
(defalias 'dvc-put-text-property 'dvc-xmas-put-text-property)
(defalias 'dvc-deactivate-mark 'ignore)
(defalias 'dvc-window-edges 'window-pixel-edges)
(defalias 'dvc-assq-delete-all 'dvc-xmas-assq-delete-all)
(defconst dvc-mouse-face-prop 'highlight)
;; end from Gnus

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'dvc-line-number-at-pos (if (functionp 'line-number-at-pos)
                                      'line-number-at-pos
                                    'line-number))


(defvar allow-remote-paths nil)

(if (fboundp 'ewoc-delete)
    (defalias 'dvc-ewoc-delete 'ewoc-delete)
  (defun dvc-ewoc-delete (ewoc &rest nodes)
    "Delete NODES from EWOC."
    (ewoc--set-buffer-bind-dll-let* ewoc
        ((L nil) (R nil) (last (ewoc--last-node ewoc)))
      (dolist (node nodes)
        ;; If we are about to delete the node pointed at by last-node,
        ;; set last-node to nil.
        (when (eq last node)
          (setf last nil (ewoc--last-node ewoc) nil))
        (delete-region (ewoc--node-start-marker node)
                       (ewoc--node-start-marker (ewoc--node-next dll node)))
        (set-marker (ewoc--node-start-marker node) nil)
        (setf L (ewoc--node-left  node)
              R (ewoc--node-right node)
              ;; Link neighbors to each other.
              (ewoc--node-right L) R
              (ewoc--node-left  R) L
              ;; Forget neighbors.
              (ewoc--node-left  node) nil
              (ewoc--node-right node) nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dvc-xemacs)

;; Local Variables:
;; End:

;;; dvc-xemacs.el ends here

;;; dvc-emacs.el --- Compatibility stuff for old versions of GNU Emacs
;;; and for XEmacs.
;;;
;;; This file should be loaded when using Gnu Emacs; load
;;; dvc-xemacs.el when using XEmacs.

;; Copyright (C) 2004, 2007 - 2008 by all contributors

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

;;; Policy:
;;
;; The DVC baseline environment is the current release of Gnu Emacs.
;; However, we also support at least one previous release of Gnu
;; Emacs, and the current release of XEmacs.
;;
;; There is current Gnu Emacs code used in DVC that is not present in
;; XEmacs or previous releases of Gnu Emacs.
;;
;; This file provides versions of that code that work with previous
;; versions of Gnu Emacs. dvc-xemacs.el provides versions of that code
;; that work with XEmacs.
;;
;; There are also functions in Gnu Emacs code used in DVC that have
;; different names in XEmacs. This file and dvc-xemacs.el provide
;; common names for those functions.
;;
;; There may also be functions in Gnu Emacs that have the same name as
;; functions in XEmacs, in which case this file provides a common name
;; to sort things out.
;;
;; In all cases, the code provided here should use names prefixed with
;; `dvc-'. This is to allow for the possibility that other packages
;; also provide the same function, but the code is broken in some way.
;; Our version will work with DVC; theirs will work with their
;; package. DVC code must use the dvc- prefixed name.
;;
;; It might be that some code is truly _not_ broken, but it's much
;; easier to just use the dvc- prefix than to prove that.
;;
;; Some implementations will be duplicated here and in dvc-xemacs.el.
;; That is ok; they may need to diverge if bugs are discovered, and
;; they will most likely be reduced to aliases at different times.

;; DVC developers should normally use Gnu Emacs 22 or XEmacs. In
;; addition, they should occasionally compile with Gnu Emacs 21, or
;; earlier versions of XEmacs, to verify compatibility.
;;
;; As the current release of Gnu Emacs ages, it may be that there are
;; features in the development head of Emacs that would be useful in
;; DVC. Such features can also be provided here.

;; In the future, when we drop support for Gnu Emacs 21, some of the
;; functions provided here can be deleted, and the DVC code that uses
;; it changed to use the Gnu Emacs release name. That will make that
;; code somewhat clearer.

;;; Code:

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

;; These have different names in Gnu Emacs and XEmacs; see dvc-xemacs.el
(defalias 'dvc-make-overlay 'make-overlay)
(defalias 'dvc-delete-overlay 'delete-overlay)
(defalias 'dvc-overlay-put 'overlay-put)
(defalias 'dvc-move-overlay 'move-overlay)
(defalias 'dvc-overlay-buffer 'overlay-buffer)
(defalias 'dvc-overlay-start 'overlay-start)
(defalias 'dvc-overlay-end 'overlay-end)
(defalias 'dvc-extent-detached-p 'ignore)
(defalias 'dvc-extent-start-open 'ignore)
(defalias 'dvc-mail-strip-quoted-names 'mail-strip-quoted-names)
(defalias 'dvc-character-to-event 'identity)
(defalias 'dvc-assq-delete-all 'assq-delete-all)
(defalias 'dvc-add-text-properties 'add-text-properties)
(defalias 'dvc-put-text-property 'put-text-property)
(defconst dvc-mouse-face-prop 'mouse-face)

;; Provide features from Emacs 22 for Emacs 21
;; alphabetical by symbol name

(if (fboundp 'derived-mode-p)
    (defalias 'dvc-derived-mode-p 'derived-mode-p)
  (defun dvc-derived-mode-p (&rest modes)
    "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
    (let ((parent major-mode))
      (while (and (not (memq parent modes))
                  (setq parent (get parent 'derived-mode-parent))))
      parent)))

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

;; In Emacs 22, (expand-file-name "c:/..") returns "c:/". But in Emacs
;; 21, it returns "c:/..". So fix that here. We don't use
;; dvc-expand-file-name everywhere in DVC, to simplify deleting it
;; later. We only use it when this case is likely to be encountered.
(if (and (memq system-type '(ms-dos windows-nt))
         (< emacs-major-version 22))
    (defun dvc-expand-file-name (name &optional default-directory)
      (let ((result (expand-file-name name default-directory)))
        (if (equal (substring result -2 (length result)) "..")
            (setq result (substring result 0 -2)))
        result))
  (defalias 'dvc-expand-file-name 'expand-file-name))

(if (fboundp 'line-number-at-pos)
    (defalias 'dvc-line-number-at-pos 'line-number-at-pos)
  (defun dvc-line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

(if (fboundp 'redisplay)
    (defalias 'dvc-redisplay 'redisplay)
  (defun dvc-redisplay (&optional force)
    (if force
        (let ((redisplay-dont-pause t))
          (sit-for 0))
      (sit-for 0))))

(if (fboundp 'window-body-height)
    (defalias 'dvc-window-body-height 'window-body-height)
  (defalias 'dvc-window-body-height 'window-height))


;; FIXME: move to dvc-utils?
(defun dvc-emacs-make-temp-dir (prefix)
  "Make a temporary directory using PREFIX.
Return the name of the directory."
  (let ((dir (make-temp-name
              (expand-file-name prefix temporary-file-directory))))
    (make-directory dir)
    dir))

(defalias 'dvc-make-temp-dir 'dvc-emacs-make-temp-dir)

(provide 'dvc-emacs)
;;; dvc-emacs.el ends here


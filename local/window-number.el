;;; window-number.el

;; Copyright (C) 2004 Johann "Myrkraverk" Oskarsson
;; <myrkraverk@users.sourceforge.net>

;; Edited by Nelson Elhage <nelhage@mit.edu> to work with CVS emacs
;; and to number windows based on screen position; Also, made indexes
;; 1-base, added window-number-find-file and cleaned up the keymap
;; code

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Introduction
;; ============

;; Window number mode allows you to select windows by numbers.  This
;; edition now works with XEmacs as well as GNU Emacs.  The window
;; numbers do not show up in the mode-line in XEmacs yet, instead a
;; -?- is displayed.  Hopefully this can be fixed soon, but really
;; depends on XEmacs developers.

;; Installation
;; ============

;; Drop this file into your load path.  C-h v load-path RET or F1 v
;; load-path RET will help.  Then place the following lines into your
;; .emacs or ~/.xemacs/init.el and uncomment them.

;; ----------------------------------------------------------------------------

;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables selection of windows according to
;; numbers with the C-x C-j prefix.  Another mode,
;; `window-number-meta-mode' enables the use of the M- prefix."
;;   t)

;; (autoload 'window-number-meta-mode "window-number"
;;   "A global minor mode that enables use of the M- prefix to select
;; windows, use `window-number-mode' to display the window numbers in
;; the mode-line."
;;   t)

;; ----------------------------------------------------------------------------

;; Then you can use M-x window-number-mode RET to turn the mode on, or
;; place (window-number-mode 1) and (window-number-meta-mode 1) into
;; your .emacs or ~/.xemacs/init.el.

;; ----------------------------------------------------------------------------

;; Code starts here.

;; ----------------------------------------------------------------------------


(defun window-number-list ()
  "Returns a list of windows on the current frame, sorted by
positiion"
  (sort (window-list (selected-frame)) 'window-number-compare-windows))

(defun window-number-compare-windows (win-a win-b)
  (let ((pos-a (window-edges win-a))
        (pos-b (window-edges win-b)))
    (let ((a-x (car pos-a))
          (a-y (cadr pos-a))
          (b-x (car pos-b))
          (b-y (cadr pos-b)))
      (if (= a-y b-y)
          (< a-x b-x)
        (< a-y b-y)))))

(defun window-number-select-nth (number)
  "Selects the nth window. If NUMBER is 0, select the minibuffer
if active"
  (interactive "P")
  (cond
   ((zerop number)
    (select-window (minibuffer-window)))
   ((integerp number)
    (let ((window (nth (1- number) (window-number-list))))
      (if (and window
               (or (not (window-minibuffer-p window))
                   (minibuffer-window-active-p window)))
          (select-window window)
        (error "No such window."))))))

(defun window-number-find-file (window)
  (interactive "P")
  (if (or (null window)
          (consp window))
      (let ((fun (if ido-mode 'ido-find-file-other-window 'find-file-other-window)))
        (call-interactively fun))
    (progn
      (window-number-select-nth window)
      (call-interactively 'find-file))))

(defun window-number ()
  "Returns the the number of the current window."
  (length (memq (selected-window)
                (nreverse (window-number-list)))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " -" (number-to-string (window-number)) "-")
   'face
   'window-number-face))

(defvar window-number-mode-map nil
  "Keymap for the window number mode.")

(defvar window-number-prefix "\C-x\C-j"
  "Prefix to type before numbers to jump to windows in
  window-number-mode")

(defun window-number-select ()
  (interactive)
  (let ((n (- (logand last-command-event (1- (expt 2 8)))
              ?0)))
    (if (and (> n 0)
             (<= n 10))
        (window-number-select-nth n))))

(defun window-number-mode-make-map ()
  (let ((keymap (make-sparse-keymap))
        (n 0))
    (while (< n 10)
      (define-key keymap
        (concat window-number-prefix (int-to-string n))
        'window-number-select)
      (setq n (1+ n)))
    keymap))

(unless window-number-mode-map
  (setq window-number-mode-map (window-number-mode-make-map)))

(unless (and (boundp 'window-number-meta-mode-map)
	     'window-number-meta-mode-map)
  (setq window-number-meta-mode-map (make-sparse-keymap))

  (let ((n 0))
    (while (< n 10)
      (define-key window-number-meta-mode-map
        (vector (+ ?\M-0 n))
        'window-number-select)
      (setq n (1+ n)))))

(if (featurep 'xemacs)
    (define-minor-mode window-number-mode
      "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
      :global t
      :init-value nil
      :lighter " -?-")
  
  (define-minor-mode window-number-mode
    "A global minor mode that enables selection of windows
according to numbers with the `window-number-prefix' prefix.
Another mode, `window-number-meta-mode' enables the use of the M-
prefix."
    :global t
    :init-value nil
    :lighter (:eval (window-number-string))))

(define-minor-mode window-number-meta-mode
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  :global t
  :init-value nil)

;;(push (cons 'my-window-number-meta-mode my-window-number-mode-map)
;;       minor-mode-map-alist)


(defface window-number-face
  '((((type tty) (class color))
     (:background "red"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "red")))
  "The face used for the window number in the mode-line.")

(provide 'window-number)

;;; window-number.el ends here.



;;; Incremental search through a mail folder (for Lucid and FSF Emacs 19)
;;; Copyright (C) 1994 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;(provide 'vm-search)

(defun vm-isearch-forward (&optional arg)
  "Incrementally search forward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally.  Specifying a prefix ARG interactively toggles the value of
vm-search-using-regexps for this search."
  (interactive "P")
  (let ((vm-search-using-regexps
	 (if arg (not vm-search-using-regexps) vm-search-using-regexps)))
    (vm-isearch t)))

(defun vm-isearch-backward (&optional arg)
  "Incrementally search backward through the current folder's messages.
Usage is identical to the standard Emacs incremental search.
When the search terminates the message containing point will be selected.

If the variable vm-search-using-regexps is non-nil, regular expressions
are understood; nil means the search will be for the input string taken
literally.  Specifying a prefix ARG interactively toggles the value of
vm-search-using-regexps for this search."
  (interactive "P")
  (let ((vm-search-using-regexps
	 (if arg (not vm-search-using-regexps) vm-search-using-regexps)))
    (vm-isearch nil)))

(defun vm-isearch (forward)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (vm-error-if-virtual-folder)
  (vm-display (current-buffer) t '(vm-isearch-forward vm-isearch-backward)
	      (list this-command 'searching-message))
  (let ((clip-head (point-min))
	(clip-tail (point-max))
	(old-vm-message-pointer vm-message-pointer))
    (unwind-protect
	(progn (select-window (vm-get-visible-buffer-window (current-buffer)))
	       (widen)
	       (add-hook 'pre-command-hook 'vm-isearch-widen)
	       ;; order is significant, we want to narrow after
	       ;; the update
	       (add-hook 'post-command-hook 'vm-isearch-narrow)
	       (add-hook 'post-command-hook 'vm-isearch-update)
	       (isearch-mode forward vm-search-using-regexps nil t)
	       (vm-isearch-update)
	       (if (not (eq vm-message-pointer old-vm-message-pointer))
		   (progn
		     (vm-record-and-change-message-pointer
		      old-vm-message-pointer vm-message-pointer)
		     (vm-update-summary-and-mode-line)
		     ;; vm-show-current-message only adjusts (point-max),
		     ;; it doesn't change (point-min).
		     (widen)
		     (narrow-to-region
		      (if (< (point) (vm-vheaders-of (car vm-message-pointer)))
			  (vm-start-of (car vm-message-pointer))
			(vm-vheaders-of (car vm-message-pointer)))
		      (vm-text-end-of (car vm-message-pointer)))
		     (save-excursion (vm-energize-urls))
		     (vm-display nil nil
				 '(vm-isearch-forward vm-isearch-backward)
				 '(reading-message))
		     ;; turn the unwinds into a noop
		     (setq old-vm-message-pointer vm-message-pointer)
		     (setq clip-head (point-min))
		     (setq clip-tail (point-max)))))
      (remove-hook 'pre-command-hook 'vm-isearch-widen)
      (remove-hook 'post-command-hook 'vm-isearch-update)
      (remove-hook 'post-command-hook 'vm-isearch-narrow)
      (narrow-to-region clip-head clip-tail)
      (setq vm-message-pointer old-vm-message-pointer))))

(defun vm-isearch-widen ()
  (if (eq major-mode 'vm-mode)
      (widen)))

(defun vm-isearch-narrow ()
  (if (eq major-mode 'vm-mode)
      (narrow-to-region
       (if (< (point) (vm-vheaders-of (car vm-message-pointer)))
	   (vm-start-of (car vm-message-pointer))
	 (vm-vheaders-of (car vm-message-pointer)))
       (vm-text-end-of (car vm-message-pointer)))))

(defun vm-isearch-update ()
  (if (eq major-mode 'vm-mode)
      (if (and (>= (point) (vm-start-of (car vm-message-pointer)))
	       (<= (point) (vm-end-of (car vm-message-pointer))))
	  nil
	(let ((mp vm-message-list)
	      (point (point)))
	  (while mp
	    (if (and (>= point (vm-start-of (car mp)))
		     (<= point (vm-end-of (car mp))))
		(setq vm-message-pointer mp mp nil)
	      (setq mp (cdr mp))))
	  (setq vm-need-summary-pointer-update t)
	  (intern (buffer-name) vm-buffers-needing-display-update)
	  (vm-update-summary-and-mode-line)))))

(provide 'vm-search)

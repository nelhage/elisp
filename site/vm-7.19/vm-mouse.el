;;; Mouse related functions and commands
;;; Copyright (C) 1995-1997 Kyle E. Jones
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

;;(provide 'vm-mouse)

(defun vm-mouse-set-mouse-track-highlight (start end &optional overlay)
  (if (null overlay)
	(cond (vm-fsfemacs-p
	       (let ((o (make-overlay start end)))
		 (overlay-put o 'mouse-face 'highlight)
		 o ))
	      (vm-xemacs-p
	       (let ((o (make-extent start end)))
		 (set-extent-property o 'start-open t)
		 (set-extent-property o 'priority 10)
		 (set-extent-property o 'highlight t)
		 o )))
    (cond (vm-fsfemacs-p
	   (move-overlay overlay start end))
	  (vm-xemacs-p
	   (set-extent-endpoints overlay start end)))))

(defun vm-mouse-button-2 (event)
  (interactive "e")
  ;; go to where the event occurred
  (cond ((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event))))
	((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))))
  ;; now dispatch depending on where we are
  (cond ((eq major-mode 'vm-summary-mode)
	 (mouse-set-point event)
	 (beginning-of-line)
	 (if (let ((vm-follow-summary-cursor t))
	       (vm-follow-summary-cursor))
	     nil
	   (setq this-command 'vm-scroll-forward)
	   (call-interactively 'vm-scroll-forward)))
	((eq major-mode 'vm-folders-summary-mode)
	 (mouse-set-point event)
	 (beginning-of-line)
	 (vm-follow-folders-summary-cursor))
	((memq major-mode '(vm-mode vm-virtual-mode vm-presentation-mode))
	 (vm-mouse-popup-or-select event))))

(defun vm-mouse-button-3 (event)
  (interactive "e")
  (if vm-use-menus
      (progn
	;; go to where the event occurred
	(cond ((vm-mouse-xemacs-mouse-p)
	       (set-buffer (window-buffer (event-window event)))
	       (and (event-point event) (goto-char (event-point event))))
	      ((vm-mouse-fsfemacs-mouse-p)
	       (set-buffer (window-buffer (posn-window (event-start event))))
	       (goto-char (posn-point (event-start event)))))
	;; now dispatch depending on where we are
	(cond ((eq major-mode 'vm-summary-mode)
	       (vm-menu-popup-mode-menu event))
	      ((eq major-mode 'vm-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'vm-presentation-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'vm-virtual-mode)
	       (vm-menu-popup-context-menu event))
	      ((eq major-mode 'mail-mode)
	       (vm-menu-popup-context-menu event))))))

(defun vm-mouse-3-help (object)
  nil
  "Use mouse button 3 to see a menu of options.")

(defun vm-mouse-get-mouse-track-string (event)
  (save-excursion
    ;; go to where the event occurred
    (cond ((vm-mouse-xemacs-mouse-p)
	   (set-buffer (window-buffer (event-window event)))
	   (and (event-point event) (goto-char (event-point event))))
	  ((vm-mouse-fsfemacs-mouse-p)
	   (set-buffer (window-buffer (posn-window (event-start event))))
	   (goto-char (posn-point (event-start event)))))
    (cond (vm-fsfemacs-p
	   (let ((o-list (overlays-at (point)))
		 (string nil))
	     (while o-list
	       (if (overlay-get (car o-list) 'mouse-face)
		   (setq string (vm-buffer-substring-no-properties
				 (overlay-start (car o-list))
				 (overlay-end (car o-list)))
			 o-list nil)
		 (setq o-list (cdr o-list))))
	     string ))
	  (vm-xemacs-p
	   (let ((e (extent-at (point) nil 'highlight)))
	     (if e
		 (buffer-substring (extent-start-position e)
				   (extent-end-position e))
	       nil)))
	  (t nil))))

(defun vm-mouse-popup-or-select (event)
  (interactive "e")
  (cond ((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))
	 (let (o-list (found nil))
	   (setq o-list (overlays-at (point)))
	   (while (and o-list (not found))
	     (cond ((overlay-get (car o-list) 'vm-url)
		    (setq found t)
		    (vm-mouse-send-url-at-event event))
		   ((overlay-get (car o-list) 'vm-mime-function)
		    (setq found t)
		    (funcall (overlay-get (car o-list) 'vm-mime-function)
			     (car o-list))))
	     (setq o-list (cdr o-list)))
	   (and (not found) (vm-menu-popup-context-menu event))))
	;; The XEmacs code is not actually used now, since all
	;; selectable objects are handled by an extent keymap
	;; binding that points to a more specific function.  But
	;; this might come in handy later if I want selectable
	;; objects that don't have an extent keymap attached.
	((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event)))
	 (let (e)
	   (cond ((extent-at (point) (current-buffer) 'vm-url)
		  (vm-mouse-send-url-at-event event))
		 ((setq e (extent-at (point) nil 'vm-mime-function))
		  (funcall (extent-property e 'vm-mime-function) e))
		 (t (vm-menu-popup-context-menu event)))))))

(defun vm-mouse-send-url-at-event (event)
  (interactive "e")
  (cond ((vm-mouse-xemacs-mouse-p)
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event)))
	 (vm-mouse-send-url-at-position (event-point event)))
	((vm-mouse-fsfemacs-mouse-p)
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))
	 (vm-mouse-send-url-at-position (posn-point (event-start event))))))

(defun vm-mouse-send-url-at-position (pos &optional browser)
  (save-restriction
    (widen)
    (cond ((vm-mouse-xemacs-mouse-p)
	   (let ((e (extent-at pos (current-buffer) 'vm-url))
		 url)
	     (if (null e)
		 nil
	       (setq url (buffer-substring (extent-start-position e)
					   (extent-end-position e)))
	       (vm-mouse-send-url url browser))))
	  ((vm-mouse-fsfemacs-mouse-p)
	   (let (o-list url o)
	     (setq o-list (overlays-at pos))
	     (while (and o-list (null (overlay-get (car o-list) 'vm-url)))
	       (setq o-list (cdr o-list)))
	     (if (null o-list)
		 nil
	       (setq o (car o-list))
	       (setq url (vm-buffer-substring-no-properties
			  (overlay-start o)
			  (overlay-end o)))
	       (vm-mouse-send-url url browser)))))))

(defun vm-mouse-send-url (url &optional browser switches)
  (if (string-match "^mailto:" url)
      (vm-mail-to-mailto-url url)
    (let ((browser (or browser vm-url-browser))
	  (switches (or switches vm-url-browser-switches)))
      (cond ((symbolp browser)
	     (funcall browser url))
	    ((stringp browser)
	     (message "Sending URL to %s..." browser)
	     (apply 'vm-run-background-command browser
		    (append switches (list url)))
	     (message "Sending URL to %s... done" browser))))))

(defun vm-mouse-send-url-to-netscape (url &optional new-netscape new-window)
  ;; Change commas to %2C to avoid confusing Netscape -remote.
  (while (string-match "," url)
    (setq url (replace-match "%2C" nil t url)))
  (message "Sending URL to Netscape...")
  (if new-netscape
      (apply 'vm-run-background-command vm-netscape-program
	     (append vm-netscape-program-switches (list url)))
    (or (equal 0 (apply 'vm-run-command vm-netscape-program
			(append vm-netscape-program-switches
				(list "-remote"
				      (concat "openURL(" url
					      (if new-window ",new-window" "")
					      ")")))))
	(vm-mouse-send-url-to-netscape url t new-window)))
  (message "Sending URL to Netscape... done"))

(defun vm-mouse-send-url-to-netscape-new-window (url)
  (vm-mouse-send-url-to-netscape url nil t))

(defvar buffer-file-type)

(defun vm-mouse-send-url-to-mosaic (url &optional new-mosaic new-window)
  (vm-mouse-send-url-to-xxxx-mosaic 'mosaic url new-mosaic new-window))

(defun vm-mouse-send-url-to-mmosaic (url &optional new-mosaic new-window)
  (vm-mouse-send-url-to-xxxx-mosaic 'mmosaic url new-mosaic new-window))

(defun vm-mouse-send-url-to-xxxx-mosaic (m-type url &optional
					 new-mosaic new-window)
  (let ((what (cond ((eq m-type 'mmosaic) "mMosaic")
		    (t "Mosaic"))))
    (message "Sending URL to %s..." what)
    (if (null new-mosaic)
	(let ((pid-file (cond ((eq m-type 'mmosaic)
			       "~/.mMosaic/.mosaicpid")
			      (t "~/.mosaicpid")))
	      (work-buffer " *mosaic work*")
	      (coding-system-for-read (vm-line-ending-coding-system))
	      (coding-system-for-write (vm-line-ending-coding-system))
	      pid)
	  (cond ((file-exists-p pid-file)
		 (set-buffer (get-buffer-create work-buffer))
		 (setq selective-display nil)
		 (erase-buffer)
		 (insert-file-contents pid-file)
		 (setq pid (int-to-string (string-to-int (buffer-string))))
		 (erase-buffer)
		 (insert (if new-window "newwin" "goto") ?\n)
		 (insert url ?\n)
		 ;; newline convention used should be the local
		 ;; one, whatever that is.
		 (setq buffer-file-type nil)
		 (if (fboundp 'set-buffer-file-coding-system)
		     (set-buffer-file-coding-system
		      (vm-line-ending-coding-system) nil))
		 (write-region (point-min) (point-max)
			       (concat "/tmp/Mosaic." pid)
			       nil 0)
		 (set-buffer-modified-p nil)
		 (kill-buffer work-buffer)))
	  (cond ((or (null pid)
		     (not (equal 0 (vm-run-command "kill" "-USR1" pid))))
		 (setq new-mosaic t)))))
    (if new-mosaic
	(apply 'vm-run-background-command
	       (cond ((eq m-type 'mmosaic) vm-mmosaic-program)
		     (t vm-mosaic-program))
	       (append (cond ((eq m-type 'mmosaic) vm-mmosaic-program-switches)
			     (t vm-mosaic-program-switches))
		       (list url))))
    (message "Sending URL to %s... done" what)))

(defun vm-mouse-send-url-to-mosaic-new-window (url)
  (vm-mouse-send-url-to-mosaic url nil t))

(defun vm-mouse-send-url-to-konqueror (url &optional new-konqueror)
  (message "Sending URL to Konqueror...")
  (if new-konqueror
      (apply 'vm-run-background-command vm-konqueror-program
	     (append vm-konqueror-program-switches (list url)))
    (or (equal 0 (apply 'vm-run-command vm-konqueror-client-program
			(append vm-konqueror-client-program-switches
				(list "openURL" url))))
	(vm-mouse-send-url-to-konqueror url t)))
  (message "Sending URL to Konqueror... done"))

(defun vm-mouse-send-url-to-konqueror-new-browser (url)
  (vm-mouse-send-url-to-konqueror url t))

(defun vm-mouse-send-url-to-clipboard (url)
  (message "Sending URL to X Clipboard...")
  (cond ((fboundp 'own-selection)
	 (own-selection url 'CLIPBOARD))
	((fboundp 'x-own-clipboard)
	 (x-own-clipboard url))
	((fboundp 'x-own-selection-internal)
	 (x-own-selection-internal 'CLIPBOARD url)))
  (message "Sending URL to X Clipboard... done"))

;; this code courtesy Thomas F. Burdick
(defun vm-mouse-send-url-to-safari (url)
  "Sends URL to Safari, using Apple's Open Scripting Architecture."
  (message "Sending URL to Safari...")
  (with-temp-buffer
    (insert "tell application \"Safari\"\n")
    (insert " activate\n")
    (insert " make new document at the beginning of documents\n")
    (insert (format " set the URL of the front document to \"%s\"\n" url))
    (insert "end tell\n")
    (call-process-region (point-min) (point-max) "/usr/bin/osascript"))
  (message "Sending URL to Safari... done"))

(defun vm-mouse-install-mouse ()
  (cond ((vm-mouse-xemacs-mouse-p)
	 (if (null (lookup-key vm-mode-map 'button2))
	     (define-key vm-mode-map 'button2 'vm-mouse-button-2)))
	((vm-mouse-fsfemacs-mouse-p)
	 (if (null (lookup-key vm-mode-map [mouse-2]))
	     (define-key vm-mode-map [mouse-2] 'vm-mouse-button-2))
	 (if vm-popup-menu-on-mouse-3
	     (progn
	       (define-key vm-mode-map [mouse-3] 'ignore)
	       (define-key vm-mode-map [down-mouse-3] 'vm-mouse-button-3))))))

(defun vm-run-background-command (command &rest arg-list)
  (apply (function call-process) command nil 0 nil arg-list))

(defun vm-run-command (command &rest arg-list)
  (apply (function call-process) command nil nil nil arg-list))

;; return t on zero exit status
;; return (exit-status . stderr-string) on nonzero exit status
(defun vm-run-command-on-region (start end output-buffer command
				       &rest arg-list)
  (let ((tempfile nil)
	;; use binary coding system in FSF Emacs/MULE
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	;; for DOS/Windows command to tell it that its input is
	;; binary.
	(binary-process-input t)
	;; call-process-region calls write-region.
	;; don't let it do CR -> LF translation.
	(selective-display nil)
	status errstring)
    (unwind-protect
	(progn
	  (setq tempfile (vm-make-tempfile-name))
	  (setq status
		(apply 'call-process-region
		       start end command nil
		       (list output-buffer tempfile)
		       nil arg-list))
	  (cond ((equal status 0) t)
		;; even if exit status non-zero, if there was no
		;; diagnostic output the command probably
		;; succeeded.  I have tried to just use exit status
		;; as the failure criterion and users complained.
		((equal (nth 7 (file-attributes tempfile)) 0)
		 (message "%s exited non-zero (code %s)" command status)
		 t)
		(t (save-excursion
		     (message "%s exited non-zero (code %s)" command status)
		     (set-buffer (find-file-noselect tempfile))
		     (setq errstring (buffer-string))
		     (kill-buffer nil)
		     (cons status errstring)))))
      (vm-error-free-call 'delete-file tempfile))))

;; stupid yammering compiler
(defvar vm-mouse-read-file-name-prompt)
(defvar vm-mouse-read-file-name-dir)
(defvar vm-mouse-read-file-name-default)
(defvar vm-mouse-read-file-name-must-match)
(defvar vm-mouse-read-file-name-initial)
(defvar vm-mouse-read-file-name-history)
(defvar vm-mouse-read-file-name-return-value)
(defvar vm-mouse-read-file-name-should-delete-frame)

(defun vm-mouse-read-file-name (prompt &optional dir default
				       must-match initial history)
  "Like read-file-name, except uses a mouse driven interface.
HISTORY argument is ignored."
  (save-excursion
    (or dir (setq dir default-directory))
    (set-buffer (vm-make-work-buffer " *Files*"))
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t
	  default-directory dir)
    (make-local-variable 'vm-mouse-read-file-name-prompt)
    (make-local-variable 'vm-mouse-read-file-name-dir)
    (make-local-variable 'vm-mouse-read-file-name-default)
    (make-local-variable 'vm-mouse-read-file-name-must-match)
    (make-local-variable 'vm-mouse-read-file-name-initial)
    (make-local-variable 'vm-mouse-read-file-name-history)
    (make-local-variable 'vm-mouse-read-file-name-return-value)
    (make-local-variable 'vm-mouse-read-file-name-should-delete-frame)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-dir dir)
    (setq vm-mouse-read-file-name-default default)
    (setq vm-mouse-read-file-name-must-match must-match)
    (setq vm-mouse-read-file-name-initial initial)
    (setq vm-mouse-read-file-name-history history)
    (setq vm-mouse-read-file-name-prompt prompt)
    (setq vm-mouse-read-file-name-return-value nil)
    (setq vm-mouse-read-file-name-should-delete-frame nil)
    (if (and vm-mutable-frames vm-frame-per-completion
	     (vm-multiple-frames-possible-p))
	(save-excursion
	  (setq vm-mouse-read-file-name-should-delete-frame t)
	  (vm-goto-new-frame 'completion)))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-file-name-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-file-name-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-file-name-return-value)
	 (prog1
	     vm-mouse-read-file-name-return-value
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-file-name-event-handler (&optional string)
  (let ((key-doc "Click here for keyboard interface.")
	start list)
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (setq vm-mouse-read-file-name-return-value
			   (save-excursion
			     (vm-keyboard-read-file-name
			      vm-mouse-read-file-name-prompt
			      vm-mouse-read-file-name-dir
			      vm-mouse-read-file-name-default
			      vm-mouse-read-file-name-must-match
			      vm-mouse-read-file-name-initial
			      vm-mouse-read-file-name-history)))
		     (vm-mouse-read-file-name-quit-handler t))
		 (quit (vm-mouse-read-file-name-quit-handler))))
	      ((file-directory-p string)
	       (setq default-directory (expand-file-name string)))
	      (t (setq vm-mouse-read-file-name-return-value
		       (expand-file-name string))
		 (vm-mouse-read-file-name-quit-handler t))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-file-name-prompt)
    (vm-set-region-face start (point) 'bold)
    (cond ((and (not string) vm-mouse-read-file-name-default)
	   (setq start (point))
	   (insert vm-mouse-read-file-name-default)
	   (vm-mouse-set-mouse-track-highlight start (point)))
	  ((not string) nil)
	  (t (insert default-directory)))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n ?\n)
    (setq list (vm-delete-backup-file-names
		(vm-delete-auto-save-file-names
		 (vm-delete-index-file-names
		  (directory-files default-directory)))))
    (vm-show-list list 'vm-mouse-read-file-name-event-handler)
    (setq buffer-read-only t)))

(defun vm-mouse-read-file-name-quit-handler (&optional normal-exit)
  (interactive)
  (if vm-mouse-read-file-name-should-delete-frame
      (vm-maybe-delete-windows-or-frames-on (current-buffer)))
  (if normal-exit
      (throw 'exit nil)
    (throw 'exit t)))

(defvar vm-mouse-read-string-prompt)
(defvar vm-mouse-read-string-completion-list)
(defvar vm-mouse-read-string-multi-word)
(defvar vm-mouse-read-string-return-value)
(defvar vm-mouse-read-string-should-delete-frame)

(defun vm-mouse-read-string (prompt completion-list &optional multi-word)
  (save-excursion
    (set-buffer (vm-make-work-buffer " *Choices*"))
    (use-local-map (make-sparse-keymap))
    (setq buffer-read-only t)
    (make-local-variable 'vm-mouse-read-string-prompt)
    (make-local-variable 'vm-mouse-read-string-completion-list)
    (make-local-variable 'vm-mouse-read-string-multi-word)
    (make-local-variable 'vm-mouse-read-string-return-value)
    (make-local-variable 'vm-mouse-read-string-should-delete-frame)
    (setq vm-mouse-read-string-prompt prompt)
    (setq vm-mouse-read-string-completion-list completion-list)
    (setq vm-mouse-read-string-multi-word multi-word)
    (setq vm-mouse-read-string-return-value nil)
    (setq vm-mouse-read-string-should-delete-frame nil)
    (if (and vm-mutable-frames vm-frame-per-completion
	     (vm-multiple-frames-possible-p))
	(save-excursion
	  (setq vm-mouse-read-string-should-delete-frame t)
	  (vm-goto-new-frame 'completion)))
    (switch-to-buffer (current-buffer))
    (vm-mouse-read-string-event-handler)
    (save-excursion
      (local-set-key "\C-g" 'vm-mouse-read-string-quit-handler)
      (recursive-edit))
    ;; buffer could have been killed
    (and (boundp 'vm-mouse-read-string-return-value)
	 (prog1
	     (if (listp vm-mouse-read-string-return-value)
		 (mapconcat 'identity vm-mouse-read-string-return-value " ")
	       vm-mouse-read-string-return-value)
	   (kill-buffer (current-buffer))))))

(defun vm-mouse-read-string-event-handler (&optional string)
  (let ((key-doc  "Click here for keyboard interface.")
	(bs-doc   "      .... to go back one word.")
	(done-doc "      .... when you're done.")
	start list)
    (if string
	(cond ((equal string key-doc)
	       (condition-case nil
		   (save-excursion
		     (setq vm-mouse-read-string-return-value
			   (vm-keyboard-read-string
			    vm-mouse-read-string-prompt
			    vm-mouse-read-string-completion-list
			    vm-mouse-read-string-multi-word))
		     (vm-mouse-read-string-quit-handler t))
		 (quit (vm-mouse-read-string-quit-handler))))
	      ((equal string bs-doc)
	       (setq vm-mouse-read-string-return-value
		     (nreverse
		      (cdr
		       (nreverse vm-mouse-read-string-return-value)))))
	      ((equal string done-doc)
	       (vm-mouse-read-string-quit-handler t))
	      (t (setq vm-mouse-read-string-return-value
		       (nconc vm-mouse-read-string-return-value
			      (list string)))
		 (if (null vm-mouse-read-string-multi-word)
		     (vm-mouse-read-string-quit-handler t)))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq start (point))
    (insert vm-mouse-read-string-prompt)
    (vm-set-region-face start (point) 'bold)
    (insert (mapconcat 'identity vm-mouse-read-string-return-value " "))
    (insert ?\n ?\n)
    (setq start (point))
    (insert key-doc)
    (vm-mouse-set-mouse-track-highlight start (point))
    (vm-set-region-face start (point) 'italic)
    (insert ?\n)
    (if vm-mouse-read-string-multi-word
	(progn
	  (setq start (point))
	  (insert bs-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)
	  (setq start (point))
	  (insert done-doc)
	  (vm-mouse-set-mouse-track-highlight start (point))
	  (vm-set-region-face start (point) 'italic)
	  (insert ?\n)))
    (insert ?\n)
    (vm-show-list vm-mouse-read-string-completion-list
		  'vm-mouse-read-string-event-handler)
    (setq buffer-read-only t)))

(defun vm-mouse-read-string-quit-handler (&optional normal-exit)
  (interactive)
  (if vm-mouse-read-string-should-delete-frame
      (vm-maybe-delete-windows-or-frames-on (current-buffer)))
  (if normal-exit
      (throw 'exit nil)
    (throw 'exit t)))

(provide 'vm-mouse)

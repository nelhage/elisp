;;; nethack-api.el -- Emacs interface the lisp window-port

;; Copyright (C) 2002,2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske
;; Created: Sat Mar 18 11:24:02 2000
;; Version: $Id: nethack-api.el,v 1.93 2004/11/19 23:05:39 sabetts Exp $
;; Keywords: games

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is the lisp side of the Nethack/C <--> Emacs/Lisp
;; interface.  This is where all the work gets done.
;;
;; Originally a translation of nethack-3.3.0/doc/window.doc
;; from the nethack src package.

;;; Code:

(require 'nethack-compat)
(require 'gamegrid)
(require 'nethack-keys)

;;; Buffer handling
(defvar nh-map-buffer nil)
(defvar nh-status-buffer nil)
(defvar nh-message-buffer nil)
(defvar nh-menu-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs")
(defun nh-menu-buffer (menuid)
  "Return the buffer that corresponds to the MENUID."
  (let ((buffer (cdr (assq menuid nh-menu-buffer-table))))
    (if (buffer-live-p buffer)
	buffer
      'nobuffer)))

(defvar nh-message-highlight-overlay nil
  "Overlay used to highlight new text in the message window.")

(defvar nh-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

(defun nhapi-raw-print (str)
  (save-current-buffer
    (let ((buffer (get-buffer-create nh-raw-print-buffer-name)))
      (pop-to-buffer buffer)
      (delete-other-windows)
      (insert str "\n"))))

(defun nhapi-raw-print-bold (str)
  (nhapi-raw-print
   (nh-propertize str 'face (nh-attr-face 'atr-bold))))

(defun nhapi-curs (x y)
  "Set the cursor in `nh-map-buffer' to X, Y."
  (with-current-buffer nh-map-buffer
   (goto-char (gamegrid-cell-offset (- x 1) y))))


;;; Status/Attribute code:b
(defun nh-propertize-attribute (attribute &optional how)
  (let* ((new-value (car attribute))
	 (old-value (cadr attribute))
	 (age (car (cddr attribute)))
	 (string (format "%s" (or new-value "")))
	 (face (if (<= age nethack-status-highlight-delay)
		   (cond ((numberp new-value)
			  (cond ((eq how nil)
				 (if (> new-value old-value)
				     'nethack-status-good-face
				   'nethack-status-bad-face))
				((eq how 'lower-is-better)
				 (if (> new-value old-value)
				     'nethack-status-bad-face
				   'nethack-status-good-face))))
			 ((null new-value) 
			  nil)
			 (t 'nethack-status-neutral-face)))))
    (if face
	(nh-propertize string 'face face)
      string)))

;; value oldvalue age
(defvar nh-status-attribute-name (list nil nil 0))
(defvar nh-status-attribute-monster (list nil nil 0))
(defvar nh-status-attribute-rank (list nil nil 0))
(defvar nh-status-attribute-St (list 0 0 0))
(defvar nh-status-attribute-Dx (list 0 0 0))
(defvar nh-status-attribute-Co (list 0 0 0))
(defvar nh-status-attribute-In (list 0 0 0))
(defvar nh-status-attribute-Wi (list 0 0 0))
(defvar nh-status-attribute-Ch (list 0 0 0))
(defvar nh-status-attribute-Align (list nil nil 0))
(defvar nh-status-attribute-Dungeon (list nil nil 0))
(defvar nh-status-attribute-Dlvl (list 0 0 0))
(defvar nh-status-attribute-$ (list 0 0 0))
(defvar nh-status-attribute-HP (list 0 0 0))
(defvar nh-status-attribute-HPmax (list 0 0 0))
(defvar nh-status-attribute-PW (list 0 0 0))
(defvar nh-status-attribute-PWmax (list 0 0 0))
(defvar nh-status-attribute-AC (list 0 0 0))
(defvar nh-status-attribute-Level (list 0 0 0))
(defvar nh-status-attribute-XP (list 0 0 0))
(defvar nh-status-attribute-HD (list 0 0 0))
(defvar nh-status-attribute-T (list 0 0 0))
(defvar nh-status-attribute-Score (list 0 0 0))
(defvar nh-status-attribute-confusion (list nil nil 0))
(defvar nh-status-attribute-hunger (list nil nil 0))
(defvar nh-status-attribute-sick (list nil nil 0))
(defvar nh-status-attribute-blind (list nil nil 0))
(defvar nh-status-attribute-stunned (list nil nil 0))
(defvar nh-status-attribute-hallucination (list nil nil 0))
(defvar nh-status-attribute-slimed (list nil nil 0))
(defvar nh-status-attribute-encumbrance (list nil nil 0))

(defun nh-reset-status-variables ()
  (setq nh-status-attribute-name (list nil nil 0)
	nh-status-attribute-monster (list nil nil 0)
	nh-status-attribute-rank (list nil nil 0)
	nh-status-attribute-St (list 0 0 0)
	nh-status-attribute-Dx (list 0 0 0)
	nh-status-attribute-Co (list 0 0 0)
	nh-status-attribute-In (list 0 0 0)
	nh-status-attribute-Wi (list 0 0 0)
	nh-status-attribute-Ch (list 0 0 0)
	nh-status-attribute-Align (list nil nil 0)
	nh-status-attribute-Dungeon (list nil nil 0)
	nh-status-attribute-Dlvl (list 0 0 0)
	nh-status-attribute-$ (list 0 0 0)
	nh-status-attribute-HP (list 0 0 0)
	nh-status-attribute-HPmax (list 0 0 0)
	nh-status-attribute-PW (list 0 0 0)
	nh-status-attribute-PWmax (list 0 0 0)
	nh-status-attribute-AC (list 0 0 0)
	nh-status-attribute-Level (list 0 0 0)
	nh-status-attribute-XP (list 0 0 0)
	nh-status-attribute-HD (list 0 0 0)
	nh-status-attribute-T (list 0 0 0)
	nh-status-attribute-Score (list 0 0 0)
	nh-status-attribute-confusion (list nil nil 0)
	nh-status-attribute-hunger (list nil nil 0)
	nh-status-attribute-sick (list nil nil 0)
	nh-status-attribute-blind (list nil nil 0)
	nh-status-attribute-stunned (list nil nil 0)
	nh-status-attribute-hallucination (list nil nil 0)
	nh-status-attribute-slimed (list nil nil 0)
	nh-status-attribute-encumbrance (list nil nil 0)))

(defun nhapi-update-status (status)
  ;; store the values
  (dolist (i status)
    (let* ((variable (intern (concat "nh-status-attribute-"
				     (car i))))
	   (old-value (car (symbol-value variable)))
	   (new-value (cadr i))
	   (age (car (cddr (symbol-value variable)))))
      (if (equal new-value old-value)
	  (set variable (list new-value 
			      (cadr (symbol-value variable))
			      (+ 1 age)))
	(set variable (list new-value
			    old-value
			    0))
	(when (not (string-equal (car i) "T"))
	  (run-hook-with-args 'nethack-status-attribute-change-functions
			      (car i) new-value old-value))))))

(defun nh-status-string (format)
  (mapconcat
   (lambda (ch)
     (let ((fn (intern-soft (concat "nh-status-" (char-to-string ch)))))
       (if fn (funcall fn) (char-to-string ch))))
   format nil))

(defun nh-print-status ()
;; create a customizable variable to control this:
;;   header line/mode line printing
;;   (with-current-buffer nh-map-buffer
;;     (setq header-line-format 
;; 	  (nh-status-string nethack-status-header-line-format))
;;     (setq mode-line-format
;; 	  (nh-status-string nethack-status-mode-line-format)))

  ;; print the values in the status buffer
  (with-current-buffer nh-status-buffer
    (erase-buffer)
    (insert (nh-status-string nethack-status-buffer-format))))

(defun nh-status-n ()
  (nh-propertize-attribute nh-status-attribute-name))
(defun nh-status-w ()
  (concat "the "
	  (nh-propertize-attribute 
	   (if (car nh-status-attribute-monster)
	       nh-status-attribute-monster
	     nh-status-attribute-rank))))
(defun nh-status-s ()
  (concat "St:" (nh-propertize-attribute nh-status-attribute-St)))
(defun nh-status-d ()
  (concat "Dx:" (nh-propertize-attribute nh-status-attribute-Dx)))
(defun nh-status-c ()
  (concat "Co:" (nh-propertize-attribute nh-status-attribute-Co)))
(defun nh-status-i ()
  (concat "In:" (nh-propertize-attribute nh-status-attribute-In)))
(defun nh-status-W ()
  (concat "Wi:" (nh-propertize-attribute nh-status-attribute-Wi)))
(defun nh-status-C ()
  (concat "Ch:" (nh-propertize-attribute nh-status-attribute-Ch)))
(defun nh-status-A ()
  (nh-propertize-attribute nh-status-attribute-Align))
(defun nh-status-L ()
  (nh-propertize-attribute nh-status-attribute-Dungeon))
(defun nh-status-l ()
  (concat "Dlvl:" (nh-propertize-attribute nh-status-attribute-Dlvl)))
(defun nh-status-g ()
  (concat "$:" (nh-propertize-attribute nh-status-attribute-$)))
(defun nh-status-h ()
  (format "HP:%s(%s)"
	  (nh-propertize-attribute nh-status-attribute-HP)
	  (nh-propertize-attribute nh-status-attribute-HPmax)))
(defun nh-status-p ()
  (format "Pw:%s(%s)"
	  (nh-propertize-attribute nh-status-attribute-PW)
	  (nh-propertize-attribute nh-status-attribute-PWmax)))
(defun nh-status-a ()
  (concat "AC:"
	  (nh-propertize-attribute nh-status-attribute-AC 'lower-is-better)))
(defun nh-status-e ()
  (format "Xp:%s/%s"
	  (nh-propertize-attribute nh-status-attribute-Level)
	  (nh-propertize-attribute nh-status-attribute-XP)))
(defun nh-status-t ()
  (format "T:%d" (car nh-status-attribute-T)))
(defun nh-status-f ()
  (mapconcat 
   (lambda (x) (if (not (string-equal x "")) (concat x " ")))
   (list
    (nh-propertize-attribute nh-status-attribute-confusion)
    (nh-propertize-attribute nh-status-attribute-hunger)
    (nh-propertize-attribute nh-status-attribute-sick)
    (nh-propertize-attribute nh-status-attribute-blind)
    (nh-propertize-attribute nh-status-attribute-stunned)
    (nh-propertize-attribute nh-status-attribute-hallucination)
    (nh-propertize-attribute nh-status-attribute-slimed)
    (nh-propertize-attribute nh-status-attribute-encumbrance))
   ""))


;;; Menu code:
(defun nhapi-menu-putstr (menuid attr str)
  "On buffer associated with MENUID, insert with ATTR the STR."
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (cond (t (goto-char (point-max))
	       (insert (nh-propertize str 
				      'face 
				      (nh-attr-face attr))
		       "\n"))))))

(defun nhapi-message (attr str)
  "Insert STR to `nh-message-buffer' using ATTR face. 
FIXME: doesnt actually use ATTR!"
  (with-current-buffer nh-message-buffer
    (goto-char (point-max))
    (run-hooks 'nethack-before-print-message-hook)
    (insert str "\n")
    ;; cover new text with highlight overlay
    (let ((start (overlay-start nh-message-highlight-overlay)))
      (move-overlay nh-message-highlight-overlay
		    start (point-max)))
    ;; scroll to show maximum output on all windows displaying buffer
    (let ((l (get-buffer-window-list (current-buffer))))
      (save-selected-window
	(mapc (lambda (w)
		(select-window w)
		(set-window-point w (- (point-max) 1))
		(recenter -1))
	      l)))))    

(defun nh-attr-face (attr)
  "Return the face corresponding with ATTR."
  (intern-soft (concat "nethack-" (symbol-name attr) "-face")))

(defconst nh-colors
  [nethack-black-face 		nethack-red-face
   nethack-green-face 		nethack-brown-face
   nethack-blue-face 		nethack-magenta-face
   nethack-cyan-face 		nethack-gray-face
   nethack-dark-gray-face 	nethack-orange-face
   nethack-bright-green-face 	nethack-yellow-face
   nethack-bright-blue-face 	nethack-bright-magenta-face
   nethack-bright-cyan-face 	nethack-white-face]
  "Vector indexed by Nethack's color number.")

(defun nhapi-print-glyph (x y color glyph tile ch &optional special)
  "Insert glyph into `nh-map-buffer'."
  (set-buffer nh-map-buffer)
  (setq x (- x 1))			; FIXME: put this hack in C
  (let ((inhibit-read-only t))
    (if nethack-use-tiles
	(save-excursion 
	  (let ((buffer-read-only nil))
	    (goto-char (gamegrid-cell-offset x y))
	    (delete-char 1)
	    (insert-image (elt nh-tile-vector tile))))
      (gamegrid-set-cell x y ch)
      ;; If the glyph is a pet then color it with the
      ;; nethack-pet-face.
      (let ((color (if (eq special 'pet)
		       'nethack-pet-face
		     (aref nh-colors color))))
	(put-text-property (gamegrid-cell-offset x y)
			   (1+ (gamegrid-cell-offset x y))
			   'face
			   color)))))

(defun nhapi-yn-function (ques choices default)
  ""
  (let ((cursor-in-echo-area t)
	key)

    ;; convert string of choices to a list of ints
    (setq choices (mapcar 'nh-char-to-int
			  (string-to-list choices)))

    (if (/= default 0)
	(setq choices (cons default choices)))

    ;; Add some special keys of our own to the choices
    (setq choices (cons 13 choices))

    (setq key (nh-read-char (concat ques " ")))
    (if (> (length choices) 1)
	(while (not (member key choices))
	  (setq key (nh-read-char (concat 
					(format "(bad %d) " key)
					ques " ")))))
    ;; 13, 27, and 7 are abort keys
    (nh-send (if (or (= 13 key)
		     (= 27 key)
		     (= 7 key))
		 default
	       key))))

(defun nhapi-ask-direction (prompt)
  "Prompt the user for a direction"
  (let* ((cursor-in-echo-area t)
	 (cmd (lookup-key nh-map-mode-map
			  (read-key-sequence-vector (concat prompt " ")))))
    (nh-send
     (cond ((eq cmd 'nethack-command-north) "n")
	   ((eq cmd 'nethack-command-south) "s")
	   ((eq cmd 'nethack-command-west) "w")
	   ((eq cmd 'nethack-command-east) "e")
	   ((eq cmd 'nethack-command-northwest) "nw")
	   ((eq cmd 'nethack-command-northeast) "ne")
	   ((eq cmd 'nethack-command-southwest) "sw")
	   ((eq cmd 'nethack-command-southeast) "se")
	   ((eq cmd 'nethack-command-up) "up")
	   ((eq cmd 'nethack-command-down) "down")
	   ((eq cmd 'nethack-command-rest-one-move) "self")
	   ((eq cmd 'nethack-command-search) "self")
	   (t "nowhere")))))

(defun nhapi-getlin (ques)
  ""
  (nh-send (condition-case nil
	       (read-from-minibuffer (concat ques " "))
	     (quit ""))))

(defun nhapi-player-selection ()
  "Does nothing right now, perhaps simply indicates that the
nhapi-choose-X calls are to follow for actual
role/race/gender/align selection.")

(defun nhapi-choose-attribute (prompt alist abort)
  "Prompts user for an element from the cars of ALIST and returns the
corresponding cdr."
  (nh-send
   (if (> (length alist) 1)
       (let ((completion-ignore-case t))
	 (condition-case nil
	     (cdr (assoc (completing-read prompt alist nil t) alist))
	   (quit abort)))
     (cdar alist))))

(defvar nh-directory nil
  "Location of the nethack directory.

This is set when the process starts by `nhapi-init-nhwindows'.
Do not edit the value of this variable.  Instead, change the value of
`nethack-program'.")

(defun nhapi-display-file (str complain)
  (let ((file (concat nh-directory str)))
    (if (file-exists-p file)
	(view-file file)
      (if complain (message "Cannot find file %s" file)))))

(defvar nh-inventory-need-update nil
  "If non-nil, at the next command prompt, update the menu.")

(defun nhapi-update-inventory ()
;; FIXME: properly(?) implement perm-inven
  (setq nh-inventory-need-update t))

(defun nhapi-doprev-message ()
  ""
  (save-selected-window
    (save-current-buffer		; is this redundant since we
					; already save the selected
					; window? -rcy
      (walk-windows (lambda (w)
		      (select-window w)
		      (set-buffer (window-buffer))
		      (if (eq (current-buffer) nh-message-buffer)
			  (scroll-down)))))))

(defun nhapi-update-positionbar (features)
  ""
  )

(defun nhapi-init-nhwindows (executable &rest args)
  "This is the first function sent by the nethack process.  Does
all of the appropriate setup."
  (setq nh-directory (file-name-directory executable))
  ;; clean up old buffers
  (mapc (lambda (b) (kill-buffer (cdr b))) nh-menu-buffer-table)
  (setq nh-menu-buffer-table nil)
  (if (get-buffer nh-raw-print-buffer-name)
      (kill-buffer nh-raw-print-buffer-name)))

(defun nhapi-exit-nhwindows (str)
  ""
  ;; print the message in STR to the raw print buffer
  (nhapi-raw-print str))

(defun nhapi-create-message-window ()
  "Create the message buffer."
  (with-current-buffer (get-buffer-create "*nethack message*")
    (erase-buffer)
    (setq nh-message-highlight-overlay
	  (make-overlay (point-max) (point-max)))
    (overlay-put nh-message-highlight-overlay 
		 'face 'nethack-message-highlight-face)
    (setq nh-message-buffer (current-buffer))))

(defun nhapi-create-status-window ()
  "Create the status buffer."
  (with-current-buffer (get-buffer-create "*nethack status*")
    (erase-buffer)
    (setq nh-status-buffer (current-buffer))))

(defun nhapi-create-map-window ()
  "Created the map buffer."
  (with-current-buffer (get-buffer-create "*nethack map*")
    (nh-map-mode)
    (setq nh-map-buffer (current-buffer))))

(defun nhapi-create-inventory-window (menuid)
  "Create the inventory window."
  (nhapi-create-menu-window menuid))

(defun nhapi-create-menu-window (menuid)
  "Create a menu window."
  (with-current-buffer (nhapi-create-menu 'menu menuid)
    (setq buffer-read-only t)))

(defun nhapi-create-text-window (menuid)
  "Create a text window."
  ;; text windows are treated as "pick-none" menu windows
  (nhapi-create-menu 'text menuid))

(defun nhapi-create-menu (type menuid)
  "Return a newly created buffer and add it to the menu table.  

The TYPE argument is legacy and serves no real purpose."
  (let* ((name (format "*%s* %d" (symbol-name type) menuid))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables))
    (push (cons menuid buf) nh-menu-buffer-table)
    buf))

(defun nhapi-clear-message ()
  "Move overlay off the last message in `nh-message-buffer'."
  (with-current-buffer nh-message-buffer
    (move-overlay nh-message-highlight-overlay
		  (point-max) (point-max))))

(defconst nh-map-width 79 "Max width of the map.")
(defconst nh-map-height 22 "Max height of the map.")
(defun nhapi-clear-map ()
  "Clear the map."
  (with-current-buffer nh-map-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if nethack-use-tiles
	  (progn ;; FIXME: test to see if emacs is capable of tiles
	    (require 'nethack-tiles)
	    ;; initialize the map with empty tiles
	    (dotimes (i nh-map-height)
	      (dotimes (j nh-map-width)
		(insert-image nh-empty-tile))
	      (insert (propertize "\n" 'face 'nethack-map-tile-face))))
	(setq gamegrid-use-glyphs nil)	; dont try to use gamegrid glyphs
	(let (cursor-type)		; protect from gamegrid-init clobbering
	  (gamegrid-init (make-vector 256 nil)))
	(gamegrid-init-buffer nh-map-width
			      nh-map-height
			      ? )))))
(defun nhapi-block ()
  ;;(nh-read-char "nethack: -- more --")
  (read-from-minibuffer "--more--")
  (nhapi-clear-message)
  (nh-send 'block-dummy))

(defvar nh-active-menu-buffer nil)
(defvar nh-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defun nhapi-display-menu (menuid)
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((window (get-buffer-window nh-message-buffer))
	  (size (count-lines (point-min) (point-max))))
      (if (or (not window)
	      (>= size (window-height window)))
	  (nhapi-select-menu menuid 'pick-none)
	(nhapi-message 'atr-none
			     (buffer-substring (point-min)
					       (- (point-max) 1)))
	(nh-send 'dummy)))))

(defun nhapi-destroy-menu (menuid)
  (save-current-buffer
    (let ((buffer (nh-menu-buffer menuid)))
      (delete-windows-on buffer nil)
      (kill-buffer buffer)
      (setq nh-menu-buffer-table
	    (nh-assq-delete-all menuid nh-menu-buffer-table)))))

(defun nh-menu-mode (how)
  "Major mode for Nethack menus.

\\{nh-menu-mode-map}"
  (setq mode-name (concat "NETHACK MENU "
			  (symbol-name how)))
  (setq major-mode 'nh-menu-mode)
  (use-local-map nh-menu-mode-map)
  (setq nh-menu-how how)
  (run-hooks 'nethack-menu-mode-hook))

(defun nh-menu-toggle-item (&optional count)
  "Toggle the menu item that is associated with the key event that
triggered this function call, if it is a valid option.

Does nothing if this is a pick-none menu.

Automatically submits menu if this is a pick-one menu and an option
was actually toggled."
  (interactive "P")
  (if (not (eq nh-menu-how 'pick-none))
      (let ((case-fold-search nil)
	    (old-point (point)))
	(goto-char (point-min))
	(if (re-search-forward (format "^[%c] \\([-+]\\|[0-9]+\\) .+$" 
				last-command-char)
			       nil t)
	    (let ((value (match-string 1))
		  (start (match-beginning 1))
		  (end (match-end 1))
		  (inhibit-read-only t))
	      (delete-region start end)
	      (goto-char start)
	      (if (and count)
		  (insert (number-to-string (if (consp count)
						(car count)
					      count)))
		(if (string-equal value "-")
		    (insert "+")
		  (insert "-")))
	      (beginning-of-line)
	      (if (eq nh-menu-how 'pick-one)
		  (nh-menu-submit)))
	  (message "No such menu option: %c" last-command-char)
	  (goto-char old-point)))))
	  
(defun nh-menu-toggle-all-items ()
  "Toggle all menu items, only for pick-any menus."
  (interactive)
  (if (eq nh-menu-how 'pick-any)
      (save-excursion
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (while (re-search-forward nh-menu-item-regexp nil t)
	    (let ((value (match-string 2)))
	      (if (string-equal value "-")
		  (replace-match "+" nil nil nil 2)
		(replace-match "-" nil nil nil 2))))))))

(defun nh-menu-goto-next ()
  "Move to the next selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-end-position))
    (goto-char (if (re-search-forward nh-menu-item-regexp nil t)
		   (line-beginning-position)
		 old-point))))

(defun nh-menu-goto-prev ()
  "Move to the previous selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-beginning-position))
    (goto-char (if (re-search-backward nh-menu-item-regexp nil t)
		   (line-beginning-position)
		 old-point))))

(defvar nh-window-configuration nil)
(defun nh-menu-submit ()
  "Submit the selected menu options to the nethack process.

Restores the window configuration what it was before the menu was
displayed."
  (interactive)
  (goto-char (point-min))
  (let ((menu-data nil))
    (while (re-search-forward nh-menu-item-regexp nil t)
      (let ((accelerator (string-to-char (match-string 1)))
	    (value (match-string 2)))
	(cond ((string-equal value "+")
	       (setq value -1))
	      ((string-equal value "-")
	       (setq value 0))
	      (t (setq value (string-to-number value))))
	(if (/= value 0)
	    (setq menu-data (cons (list (nh-char-to-int accelerator) value) menu-data)))))
    (nh-send menu-data)
    (and (window-configuration-p nh-window-configuration)
	 (set-window-configuration nh-window-configuration))
    (setq nh-active-menu-buffer nil)))
	
(defun nh-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    ;; turn off all the options
    (while (re-search-forward nh-menu-item-regexp nil t)
      (replace-match "-" nil nil nil 1)))
  (nh-menu-submit))

(defvar nh-unassigned-accelerator-index 0
  "Index into `nh-accelerator-chars' indicating the next
accelerator that will be used in unassigned menus.")

(defun nhapi-start-menu (menuid)
  ""
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; we don't turn on `nh-menu-mode' yet, since we do not yet know
      ;; "how" this menu is going to work.
      (setq nh-unassigned-accelerator-index 0))))

(defun nh-specify-accelerator ()
  "Return the next accelerator from `nh-accelerator-chars' specified
by `nh-unassigned-accelerator-index'."
  (prog1
      (aref nh-accelerator-chars
	    nh-unassigned-accelerator-index)
    (setq nh-unassigned-accelerator-index
	  (+ 1 nh-unassigned-accelerator-index))))

(defun nhapi-add-menu (menuid glyph tile accelerator groupacc attr str preselected)
  "Create a menu item out of arguments and draw it in the menu
buffer."
  (with-current-buffer (nh-menu-buffer menuid)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
	  (start (point)))
      (if (= accelerator -1)
	  (insert str)
	(insert (format "%c %c %s"
			(if (eq accelerator 0)
			    (nh-specify-accelerator)
			  accelerator)
			(if preselected ?+ ?-)
			str)))
      (put-text-property start (point) 'face (nh-attr-face attr))
      (insert-char ?\n 1 nil)
      (run-hooks 'nethack-add-menu-hook))))

;; FIXME: xemacs propertize bug here
(defun nhapi-end-menu (window prompt)
  ""
  (with-current-buffer (nh-menu-buffer window)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert prompt)
      (newline))))

(defun nhapi-select-menu (menuid how)
  "Display the menu given by MENUID and put the buffer in
`nh-menu-mode'.

Saves the current window configuration so that it can be restored when
the menu is dismissed."
  (let ((buffer (nh-menu-buffer menuid)))
    (if buffer
	(progn
	  (setq nh-window-configuration (current-window-configuration))
	  ;; Use the window displaying the message buffer for the menu
	  ;; buffer, if possible.
	  (let ((message-window (get-buffer-window nh-message-buffer)))
	    (if (not message-window)
		(pop-to-buffer (nh-menu-buffer menuid) nil t)
	      (select-window message-window)
	      (switch-to-buffer (nh-menu-buffer menuid) t)))
	  ;; make window larger, if necessary
	  (let ((bh (nh-window-buffer-height (selected-window)))
		(wh (- (window-height) 1)))
	    (if (> bh wh)
		(enlarge-window (- bh wh))))
	  (nh-menu-mode how)
	  (goto-char (point-min))
	  (message "Displaying menu")
	  (setq nh-active-menu-buffer buffer))
      (error "No such menuid: %d" menuid))))

(defun nhapi-restore-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  (let ((window-min-height (min nethack-status-window-height
				nethack-message-window-height)))
    (delete-other-windows)
    (switch-to-buffer nh-status-buffer)
    (split-window-vertically (- nethack-status-window-height))
    (switch-to-buffer nh-message-buffer)
    (split-window-vertically nethack-message-window-height)
    (switch-to-buffer-other-window nh-map-buffer)
    (if (buffer-live-p nh-active-menu-buffer)
	(pop-to-buffer nh-active-menu-buffer))))

(defun nhapi-bell ()
  "Beep at user."
  (ding))

(defun nhapi-wait-synch ()
  "Does nothing."
  )

(defun nhapi-delay-output ()
  "Sleep for 50ms."
  ;; This is the only way I can get the desired effect of a redisplay
  ;; with a short pause.  Unfortunatly, if a keypress occurs during an
  ;; "animation" we stop getting redisplays.
  (sit-for 0 50)
  ;; Tell process to continue
  (nh-send 'dummy))

(defun nhapi-outrip (window who gold message)
  ""
  (with-current-buffer (nh-menu-buffer window)
    (insert (concat who " -- " message) "\n")))

(defun nhapi-end ()
  (message "Goodbye.")
  (run-hooks 'nethack-end-hook))

;; Options
(defvar nh-options-cbreak nil)
(defvar nh-options-dec-graphics nil)
(defvar nh-options-echo nil)
(defvar nh-options-ibm-graphics nil)
(defvar nh-options-msg-history nil)
(defvar nh-options-num-pad nil)
(defvar nh-options-news nil)
(defvar nh-options-window-inited nil)
(defvar nh-options-vision-inited nil)
(defvar nh-options-menu-tab-sep nil)
(defvar nh-options-menu-requested nil)
(defvar nh-options-num-pad-mode nil)
(defvar nh-options-purge-monsters nil)
(defvar nh-options-bouldersym nil)
(defvar nh-options-travelcc nil)
(defvar nh-options-sanity-check nil)
(defvar nh-options-mon-polycontrol nil)

(defun nhapi-options (cbreak dec-graphics echo ibm-graphics msg-history
			     num-pad news window-inited vision-inited
			     menu-tab-sep menu-requested num-pad-mode
			     purge-monsters bouldersym travelcc
			     sanity-check mon-polycontrol &rest ignore)
  (setq nh-options-cbreak cbreak)
  (setq nh-options-dec-graphics dec-graphics)
  (setq nh-options-echo echo)
  (setq nh-options-ibm-graphics ibm-graphics)
  (setq nh-options-msg-history msg-history)
  (setq nh-options-num-pad num-pad)
  (setq nh-options-news news)
  (setq nh-options-window-inited window-inited)
  (setq nh-options-vision-inited vision-inited)
  (setq nh-options-menu-tab-sep menu-tab-sep)
  (setq nh-options-menu-requested menu-requested)
  (setq nh-options-num-pad-mode num-pad-mode)
  (setq nh-options-purge-monsters purge-monsters)
  (setq nh-options-bouldersym bouldersym)
  (setq nh-options-travelcc travelcc)
  (setq nh-options-sanity-check sanity-check)
  (setq nh-options-mon-polycontrol mon-polycontrol))

(provide 'nethack-api)

;;; nethack-api.el ends here

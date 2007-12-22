;;; nethack.el --- run Nethack as a subprocess

;; Copyright (C) 2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Created: Sat Mar 18 11:31:52 2000
;; Version: $Id: nethack.el,v 1.83 2004/11/19 23:09:09 sabetts Exp $
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
;; Requires: a copy of Nethack 3.4.x with the lisp window port

;;; Code:

(require 'nethack-compat)
(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)

(defgroup nethack nil
  "Emacs lisp frontend to the lisp window port of Nethack 3.4.0."
  :group 'games)

(defcustom nethack-program "nethack"
  "Program to run to start a game of Nethack."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program-args nil
  "Arguments to pass to `nethack-program'."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-highlight-delay 5
  "The number of turns to keep a changed status field highlighted."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-buffer-format 
  "n w s d c i W C A\nL l g h p a e t f"
  "Format string for the status in `nh-status-buffer'."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-mode-line-format
  "s d c i W C g h p a e t"
  "Format string for the status on the mode-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-header-line-format
  "n w <L,l> A   f"
  "Format string for the status on the header-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-purge-buffers t
  "When this variable is non-nil, kill all nethack buffers when nethack quits."
  :type '(boolean)
  :group 'nethack)

;;; Insert variables that control how the status gets displayed here.

(defcustom nethack-use-tiles nil
  "If non-nil, use XPMs to draw tiles."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-map-mode-hook nil
  "Functions to be called after setting up the Nethack map."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-menu-mode-hook nil
  "Functions to be called after setting up a Nethack menu."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-before-print-message-hook nil
  "Hook run before a message is printed."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-end-hook nil
  "Hook run when nethack has ended."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-status-attribute-change-functions nil
  "List of functions to call after a status attribute change.

Three arguments are passed to each function: the name of the
attribute, the new value and the old value."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-load-hook nil
    "Hook run after loading nethack."
    :type '(hook)
    :group 'nethack)

(defcustom nethack-add-menu-hook nil
  "Hook run after a menu option has been added."
  :type '(hook)
  :group 'nethack)


(defgroup nethack-faces nil
  "Customizations for faces used by Enethack."
  :group 'nethack)

(defface nethack-status-good-face
  `((((type tty)
      (class color))
     (:background "green" :foreground "black"))
    (((class color)
      (background light))
     (:background "darkseagreen2"))
    (((class color)
      (background dark))
     (:background "green4")))
  "Face for highlighting good changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-status-bad-face
  `((((type tty)
      (class color))
     (:background "red"))
    (((class color)
      (background light))
     (:background "pink"))
    (((class color)
      (background dark))
     (:background "red"))
    (t 
     (:inverse-video t)))
  "Face for highlighting bad changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-status-neutral-face
  `((((type tty)
      (class color))
     (:foreground "white" :background "blue"))
    (((type tty)
      (class mono))
     (:inverse-video t))
    (((class color)
      (background dark))
     (:background "blue3"))
    (((class color)
      (background light))
     (:background "lightgoldenrod2"))
    (t
     (:background "gray")))
  "Face for highlighting neutral changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-message-highlight-face
  '((t (:foreground "black" :background "green")))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defface nethack-atr-none-face
  `((t ()))
  "Nethack default face."
  :group 'nethack-faces)

(defface nethack-atr-uline-face
  `((t (:underline t)))
  "Nethack underline face.")

(defface nethack-atr-bold-face
  `((t (:bold t)))
  "Nethack bold face."
  :group 'nethack-faces)

(defface nethack-atr-blink-face
  `((t (:inverse-video t)))
  "Nethack blink face."
  :group 'nethack-faces)

(defface nethack-atr-inverse-face
  `((t (:inverse-video t)))
  "Nethack inverse face."
  :group 'nethack-faces)

(defface nethack-black-face
  `((t (:foreground "dark blue")))
  "nethack black face"
  :group 'nethack-faces)

(defface nethack-red-face
  `((((type tty) (class color))
     (:foreground "red"))
    (((class color))
     (:foreground "red"))
    (t (:foreground "gray")))
  "nethack red"
  :group 'nethack-faces)

(defface nethack-green-face
  `((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "lime green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "nethack green"
  :group 'nethack-faces)

(defface nethack-brown-face
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color) (background dark))
     (:foreground "chocolate"))
    (((class color) (background light))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "nethack brown"
  :group 'nethack-faces)

(defface nethack-blue-face
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "dark blue"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (t (:foreground "gray")))
  "nethack blue"
  :group 'nethack-faces)

(defface nethack-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color) (background dark))
     (:foreground "dark magenta"))
    (((class color) (background light))
     (:foreground "dark magenta"))
    (t (:foreground "gray")))
  "nethack magenta"
  :group 'nethack-faces)

(defface nethack-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "dark cyan"))
    (((class color) (background light))
     (:foreground "cyan4"))
    (t (:foreground "gray")))
  "nethack cyan"
  :group 'nethack-faces)

(defface nethack-gray-face
  `((((type tty) (class color))
     (:foreground "white"))
    (((class color) (background dark))
     (:foreground "lightgray"))
    (((class color) (background light))
     (:foreground "darkgray"))
    (t (:foreground "gray")))
  "nethack gray"
  :group 'nethack-faces)

(defface nethack-dark-gray-face
  `((((type tty) (class color))
     (:foreground "black" :bold t))
    (((class color) (background dark))
     (:foreground "darkgray"))
    (((class color) (background light))
     (:foreground "lightgray"))
    (t (:foreground "gray")))
  "nethack dark gray"
  :group 'nethack-faces)

(defface nethack-orange-face
  `((((type tty) (class color))
     (:foreground "red" :bold t))
    (((class color))
     (:foreground "orange"))
    (t (:foreground "gray")))
  "nethack light orange"
  :group 'nethack-faces)

(defface nethack-bright-green-face
  `((((type tty) (class color))
     (:foreground "green" :bold t))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t (:foreground "gray")))
  "nethack bright green"
  :group 'nethack-faces)

(defface nethack-yellow-face
  `((((type tty) (class color))
     (:foreground "yellow" :bold t))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow3"))
    (t (:foreground "gray")))
  "nethack yellow"
  :group 'nethack-faces)

(defface nethack-bright-blue-face
  `((((type tty) (class color))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "nethack bright blue"
  :group 'nethack-faces)

(defface nethack-bright-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta" :bold t))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack bright magenta"
  :group 'nethack-faces)

(defface nethack-bright-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan" :bold t))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan3"))
    (t (:foreground "gray")))
  "nethack bright cyan"
  :group 'nethack-faces)

(defface nethack-white-face
  `((((type tty) (class color))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:foreground "gray")))
  "nethack white"
  :group 'nethack-faces)

(defface nethack-map-tile-face 
  `((((type tty)) 
     nil)
    (t (:font "6x10")))
  "Map face with height less than the tile size (16 pixels)."
  :group 'nethack-faces)

(defface nethack-pet-face
  `((((type tty) (class color))
     (:foreground "black" :background "white" :bold t))
    (((class color) (background dark))
     (:foreground "black" :background "white"))
    (((class color) (background light))
     (:foreground "white" :background "black"))
    (t (:foreground "gray")))
  "nethack white"
  :group 'nethack-faces)


;;; Process
(defvar nh-proc nil)
(defvar nh-proc-buffer-name "*nh-output*")
(defvar nh-proc-kill-buffer-on-quit t
  "When the process ends kill the process buffer if this is t.")
(defvar nh-log-buffer "*nh-log*")

;;;###autoload
(defun nethack ()
  "Start a game of Nethack.

The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (nethack-is-running)
      (progn
	(message "Nethack process already running...")
	(nhapi-restore-window-configuration))
    (progn
      ;; Start the process.
      (if (get-buffer nh-proc-buffer-name)
	  (kill-buffer nh-proc-buffer-name))
      (nethack-start (apply 'start-process "nh" nh-proc-buffer-name
			    nethack-program nethack-program-args)))))

(defun nethack-is-running ()
  "return T if nethack is already running."
  (and (processp nh-proc)
	   (member (process-status nh-proc) '(open run))))

(defun nethack-start (process)
  "Given the process, start nethack. Assumes nethack is not already running."
  (save-excursion
    (setq nh-proc process)
    (nh-reset-status-variables)
    (set-process-filter nh-proc 'nh-filter)
    (set-process-sentinel nh-proc 'nh-sentinel)))

(defun nethack-toggle-tiles ()
  "Toggle the use of tiles on the map."
  (interactive)
  (setq nethack-use-tiles (not nethack-use-tiles))
  (nethack-command-redraw-screen 2))

;;;; Process code to communicate with the Nethack executable
(defconst nh-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

(defun nh-sentinel (proc msg)
  "Nethack background process sentinel.
PROC is the process object and MSG is the exit message."
  (with-current-buffer (process-buffer proc)
    (nh-log (buffer-substring (point-min) (point)))
    (eval-region (point-min) (point-max))
    (insert "Nethack " msg)
;;    (if (not (string-equal msg "Nethack finished"))
;;	(pop-to-buffer (current-buffer)))
    )
  (delete-process proc)
  (if nh-proc-kill-buffer-on-quit
      (kill-buffer (get-buffer nh-proc-buffer-name)))
  (if nethack-purge-buffers
      (nethack-kill-buffers)))

(defvar nh-log-process-text t)
(defun nh-log (string)
  (if nh-log-process-text
      (with-current-buffer (get-buffer-create nh-log-buffer)
	(goto-char (point-max))
	(insert string))))

(defvar nh-at-prompt nil)
(defun nh-filter (proc string)
  "Insert contents of STRING into the buffer associated with PROC.
Evaluate the buffer contents if we are looking at a prompt and then
delete the contents, perhaps logging the text."
  ;; insert output into process buffer
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (forward-line 0)
    (if (looking-at nh-prompt-regexp)
	(let ((prompt (match-string 1)))
	  (nh-log (buffer-substring (point-min) (point)))
	  (eval-region (point-min) (point))
	  (cond ((or (equal prompt "command")
		     (equal prompt "menu"))
		 (nh-print-status)
		 (sit-for 0)
		 (setq nh-at-prompt t)))))))

(defun nh-send (form)
  (let ((command (cond 
		  ((null form) "()") ; the process doesn't handle `nil'
		  ((stringp form) form)
		  (t (prin1-to-string form)))))
    (with-current-buffer (process-buffer nh-proc) (erase-buffer))
    (process-send-string nh-proc (concat command "\n"))
    (nh-log (format ";;; %s\n" command))))
  
(defun nh-send-and-wait (form)
  (nh-send form)
  ;; wait until we get back to a "command" prompt before returning
  (setq nh-at-prompt nil)
  (while (and (member (process-status nh-proc) '(open run))
	      (not nh-at-prompt))
    (accept-process-output nh-proc)))

;;; Buffer code (aka windows in Nethack)
(defvar nh-map-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w   " table)
    (modify-syntax-entry ?\) "w   " table)
    (modify-syntax-entry ?\[ "w   " table)
    (modify-syntax-entry ?\] "w   " table)
    (modify-syntax-entry ?{ "w   " table)
    (modify-syntax-entry ?} "w   " table)
    table)
  "Syntax table used in the Nethack map.")

(defun nh-map-mode ()
  "Major mode for the main Nethack map window.

\\{nh-map-mode-map}"
  (use-local-map nh-map-mode-map)
  (set-syntax-table nh-map-mode-syntax-table)
  (setq mode-name "NETHACK MAP")
  (setq major-mode 'nh-map-mode)
  ;; make scroll-other-window work on the message buffer
  (make-variable-buffer-local 'other-window-scroll-buffer)
  (setq other-window-scroll-buffer nh-message-buffer)
  (run-hooks 'nethack-map-mode-hook))

(defun nethack-kill-buffers ()
  "kill all nethack associated buffers except the nethack process
buffer."
  (when (buffer-live-p nh-map-buffer)
    (kill-buffer nh-map-buffer))
  (when (buffer-live-p nh-status-buffer)
    (kill-buffer nh-status-buffer))
  (when (buffer-live-p nh-message-buffer)
    (kill-buffer nh-message-buffer))
  (mapcar (lambda (x) (when (buffer-live-p (cdr x))
			(kill-buffer (cdr x)))) nh-menu-buffer-table)
  (kill-buffer (get-buffer nh-log-buffer)))
  


(run-hooks 'nethack-load-hook)

(provide 'nethack)


;;; VERSION:
(defun nethack-el-version ()
  (interactive)
  (message (format "nethack-el %s" nethack-el-version)))
(defconst nethack-el-version "0.9.0")

;;; nethack.el ends here

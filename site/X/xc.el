;;; xc.el --- Xcode which handles process/net things and structure things

;; Copyright (C) 1996, 1997, 1998 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: X
;; X-RCS: $Id: xc.el,v 1.7 1998/03/10 23:38:01 zappo Exp $
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;

;;; Commentary:
;;
;; Socket management for a lispy X expression.

(require 'xmath)
(require 'xwin)

;;; Code:
(defvar X nil
  "Set to t in buffers assiciated with X connection logs.")

(defun X-dpy-p (dpy &optional signal)
  "Return t if DPY is a DPY for an X connection.
If SIGNAL, then use signal command to declair that dpy is not really a dpy."
  (let ((v (and (vectorp dpy)
		(aref dpy 0)
		(bufferp (get-buffer (aref dpy 0)))
		(get-buffer-process (aref dpy 0))
		(= (process-exit-status (get-buffer-process (aref dpy 0))) 0)
		(save-excursion (set-buffer (aref dpy 0)) X))))
    (if (and (not v) signal)
	(signal 'wrong-type-argument (list signal 'X-dpy-p dpy))
      v)))
    
(defun X-buffer (name)
  "Make a log buffer for X connection NAME."
  (if (not (stringp name))
      (signal 'wrong-type-argument '(X-buffer stringp name)))
  (set-buffer (get-buffer-create (concat "Xlog-" name)))
  (delete-region (point-min) (point-max))
  (make-local-variable 'X)
  (setq X t)
  (make-local-variable 'X-interpolator-message-buffer)
  (setq X-interpolator-message-buffer nil)
  (make-local-variable 'X-id-counter)
  (setq X-id-counter 1)
  (make-local-variable 'X-window-list)
  (setq X-window-list nil)
  (make-local-variable 'X-atom-list)
  (setq X-atom-list nil)
  (make-local-variable 'X-dpy)
  (setq X-dpy nil)
  (make-local-variable 'X-name)
  (setq X-dpy-name nil)
  (make-local-variable 'X-screen)
  (setq X-dpy-name 0)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    '(lambda () "Kill an X buffer hook"
	       (if (vectorp X-dpy)
		   (aset X-dpy 0 nil))
	       (if (and (get-buffer-process (current-buffer))
			(= (process-exit-status
			    (get-buffer-process (current-buffer))) 0))
		   (X-sentinel)))
	    nil)
  (current-buffer))

(defun X-open-connection (name &optional screen)
  "Start up x connection via lisp to machine NAME.
Optional argument SCREEN specifies the screen number to attach to."
  (interactive)
  (if (not (stringp name))
      (signal 'wrong-type-argument '(X-open-connection stringp name)))
  (if (not screen) (setq screen 0))
  (save-excursion
    (let ((Xp nil)
	  (b (X-buffer name)))
      (X-sentinel)
      (setq X-dpy-name name
	    X-dpy-screen screen
	    Xp (if (not (zerop (length name)))
                   (open-network-stream (concat "X-" name)
                                        b
                                        ;; This string is the standard loopback
                                        ;;"127.0.0.1"
                                        name
                                        (+ 6000 screen))
                 (make-network-process :name "X11-unix"
                                       :family 'local
                                       :service (concat "/tmp/.X11-unix/X" (int-to-string screen))
                                       :buffer b)))
      (set-process-filter Xp 'X-filter)
      (set-process-sentinel Xp 'X-sentinel)
      (message "X: Connection open...")
      Xp)))

(defun X-filter (proc out)
  ;; checkdoc-params: (out)
  "Filter PROC for X network connections."
  (save-excursion
    (set-buffer (process-buffer proc))
    ;; (X-log nil "Receiving:")
    ;; (X-log-verbatum nil out)
    ;; Now stow the data on the interpolator variable, and make it
    ;; bigger as need be.
    (setq X-interpolator-message-buffer
	  (concat X-interpolator-message-buffer out))
    ;; Now guess at what needs to be done to fix this.
    ;; X-dpy is buffer local after XOpenDisplay is run, else nil
    (X-parse-message-guess X-dpy)))

(defun X-send (dpy s)
  "Send the X server DPY the string S."
  (if (not (processp dpy))
      (X-dpy-p dpy 'X-send))
  (if (not (stringp s))
      (signal 'wrong-type-argument '(X-send stringp s)))
  (if (processp dpy)
      (process-send-string dpy s)
    (process-send-string (get-buffer-process (aref dpy 0)) s))
  (save-excursion
    (if (processp dpy)
	(set-buffer (process-buffer dpy))
      (set-buffer (aref dpy 0)))
    ;; (X-log nil "Sending: ")
    ;; (X-log-verbatum nil s)
    ))

(defun X-log (dpy &rest args)
  "Put a message in the in the log buffer specified by DPY.
If DPY is nil, then put into current buffer.  Log additional ARGS as well."
  (save-excursion
    (if dpy (X-dpy-p dpy 'X-log))
    (if dpy (set-buffer (aref dpy 0)))	;use current buffer otherwise
    (goto-char (point-max))
    (insert (eval (cons 'format args)))))

(defun X-log-verbatum (dpy arg)
  "Put a message in the minibuffer and in the log buffer specified by DPY.
If DPY  is nil, then use current buffer.  Log additional ARGs as well."
  (save-excursion
    (if dpy (X-dpy-p dpy 'X-log-verbatum))
    (if dpy (set-buffer (aref dpy 0)))	;use current buffer otherwise
    (goto-char (point-max))
    (insert "[" arg "]\n")))

(defun X-close (dpy)
  "Close all connections based on DPY."
  (X-dpy-p dpy 'X-close)
  (save-excursion
    (set-buffer (aref dpy 0))
    (X-sentinel)))

(defun X-sentinel (&optional process event)
  ;; checkdoc-params: (event)
  "Sentinel attached to X processes.
Also allows you to make sure that the PROCESS id dead."
  (interactive)
  (save-excursion
    (let ((p (if process process (get-buffer-process (current-buffer)))))
      (message "X: Removing process %S" p)
      (sit-for 1)
      (if p (delete-process p)))))

(provide 'xc)
;;; xc ends here

;;; Simple POP (RFC 1939) client for VM
;;; Copyright (C) 1993, 1994, 1997, 1998 Kyle E. Jones
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

;;(provide 'vm-pop)

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-cant-uidl "Can't use UIDL")
      (define-error 'vm-dele-failed "DELE command failed")
      (define-error 'vm-uidl-failed "UIDL command failed"))
  (put 'vm-cant-uidl 'error-conditions '(vm-cant-uidl error))
  (put 'vm-cant-uidl 'error-message "Can't use UIDL")
  (put 'vm-dele-failed 'error-conditions '(vm-dele-failed error))
  (put 'vm-dele-failed 'error-message "DELE command failed")
  (put 'vm-uidl-failed 'error-conditions '(vm-uidl-failed error))
  (put 'vm-uidl-failed 'error-message "UIDL command failed"))

(defsubst vm-folder-pop-maildrop-spec ()
  (aref vm-folder-access-data 0))
(defsubst vm-folder-pop-process ()
  (aref vm-folder-access-data 1))

(defsubst vm-set-folder-pop-maildrop-spec (val)
  (aset vm-folder-access-data 0 val))
(defsubst vm-set-folder-pop-process (val)
  (aset vm-folder-access-data 1 val))

;; Our goal is to drag the mail from the POP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.
(defun vm-pop-move-mail (source destination)
  (let ((process nil)
	(m-per-session vm-pop-messages-per-session)
	(b-per-session vm-pop-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(popdrop (vm-safe-popdrop-string source))
	(statblob nil)
	(can-uidl t)
	(msgid (list nil (vm-popdrop-sans-password source) 'uidl))
	(pop-retrieved-messages vm-pop-retrieved-messages)
	auto-expunge x
	mailbox-count mailbox-size message-size response
	n (retrieved 0) retrieved-bytes process-buffer uidl)
    (setq auto-expunge (cond ((setq x (assoc source vm-pop-auto-expunge-alist))
			      (cdr x))
			     ((setq x (assoc (vm-popdrop-sans-password source)
					     vm-pop-auto-expunge-alist))
			      (cdr x))
			     (t vm-pop-expunge-after-retrieving)))
    (unwind-protect
	(catch 'done
	  (if handler
	      (throw 'done
		     (funcall handler 'vm-pop-move-mail source destination)))
	  (setq process (vm-pop-make-session source))
	  (or process (throw 'done nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    ;; find out how many messages are in the box.
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process)
		  mailbox-count (nth 0 response)
		  mailbox-size (nth 1 response))
	    ;; forget it if the command fails
	    ;; or if there are no messages present.
	    (if (or (null mailbox-count)
		    (< mailbox-count 1))
		(throw 'done nil))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved-bytes 0)
	    (setq statblob (vm-pop-start-status-timer))
	    (vm-set-pop-stat-x-box statblob popdrop)
	    (vm-set-pop-stat-x-maxmsg statblob mailbox-count)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (catch 'skip
		(vm-set-pop-stat-x-currmsg statblob n)
		(if can-uidl
		    (condition-case nil
			(let (list)
			  (vm-pop-send-command process (format "UIDL %d" n))
			  (setq response (vm-pop-read-response process t))
			  (if (null response)
			      (signal 'vm-cant-uidl nil))
			  (setq list (vm-parse response "\\([\041-\176]+\\) *")
				uidl (nth 2 list))
			  (if (null uidl)
			      (signal 'vm-cant-uidl nil))
			  (setcar msgid uidl)
			  (if (member msgid pop-retrieved-messages)
			      (progn
				(if vm-pop-ok-to-ask
				    (message
				     "Skipping message %d (of %d) from %s (retrieved already)..."
				     n mailbox-count popdrop))
				(throw 'skip t))))
		      (vm-cant-uidl
		       ;; something failed, so UIDL must not be working.
		       (if (and (not auto-expunge)
				(or (not vm-pop-ok-to-ask)
				    (not (vm-pop-ask-about-no-uidl popdrop))))
			   (progn
			     (message "Skipping mailbox %s (no UIDL support)"
				      popdrop)
			     (throw 'done (not (equal retrieved 0))))
			 ;; user doesn't care, so go ahead and
			 ;; expunge from the server
			 (setq can-uidl nil
			       msgid nil)))))
		(vm-pop-send-command process (format "LIST %d" n))
		(setq message-size (vm-pop-read-list-response process))
		(vm-set-pop-stat-x-need statblob message-size)
		(if (and (integerp vm-pop-max-message-size)
			 (> message-size vm-pop-max-message-size)
			 (progn
			   (setq response
				 (if vm-pop-ok-to-ask
				     (vm-pop-ask-about-large-message
				      process popdrop message-size n)
				   'skip))
			   (not (eq response 'retrieve))))
		    (progn
		      (if (eq response 'delete)
			  (progn
			    (message "Deleting message %d..." n)
			    (vm-pop-send-command process (format "DELE %d" n))
			    (and (null (vm-pop-read-response process))
				 (throw 'done (not (equal retrieved 0)))))
			(if vm-pop-ok-to-ask
			    (message "Skipping message %d..." n)
			  (message
			   "Skipping message %d in %s, too large (%d > %d)..."
			   n popdrop message-size vm-pop-max-message-size)))
		      (throw 'skip t)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count popdrop)
		(vm-pop-send-command process (format "RETR %d" n))
		(and (null (vm-pop-read-response process))
		     (throw 'done (not (equal retrieved 0))))
		(and (null (vm-pop-retrieve-to-target process destination
						      statblob))
		     (throw 'done (not (equal retrieved 0))))
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(if (and (not auto-expunge) msgid)
		    (setq pop-retrieved-messages
			  (cons (copy-sequence msgid)
				pop-retrieved-messages))
		  ;; Either the user doesn't want the messages
		  ;; kept in the mailbox or there's no UIDL
		  ;; support so there's no way to remember what
		  ;; messages we've retrieved.  Delete the
		  ;; message now.
		  (vm-pop-send-command process (format "DELE %d" n))
		  ;; DELE can't fail but Emacs or this code might
		  ;; blow a gasket and spew filth down the
		  ;; connection, so...
		  (and (null (vm-pop-read-response process))
		       (throw 'done (not (equal retrieved 0))))))
	      (vm-increment n))
	     (not (equal retrieved 0)) ))
      (setq vm-pop-retrieved-messages pop-retrieved-messages)
      (if (and (eq vm-flush-interval t) (not (equal retrieved 0)))
	  (vm-stuff-pop-retrieved))
      (and statblob (vm-pop-stop-status-timer statblob))
      (if process
	  (vm-pop-end-session process)))))

(defun vm-pop-check-mail (source)
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-pop-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	(retrieved vm-pop-retrieved-messages)
	(popdrop (vm-popdrop-sans-password source))
	(count 0)
	x response)
    (unwind-protect
	(save-excursion
	  (catch 'done
	    (if handler
		(throw 'done
		       (funcall handler 'vm-pop-check-mail source)))
	    (setq process (vm-pop-make-session source))
	    (or process (throw 'done nil))
	    (set-buffer (process-buffer process))
	    (vm-pop-send-command process "UIDL")
	    (setq response (vm-pop-read-uidl-long-response process))
	    (if (null response)
		;; server doesn't understand UIDL
		nil
	      (if (null (car response))
		  ;; (nil . nil) is returned if there are no
		  ;; messages in the mailbox.
		  (progn
		    (vm-store-folder-totals source '(0 0 0 0))
		    (throw 'done nil))
		(while response
		  (if (not (and (setq x (assoc (cdr (car response)) retrieved))
				(equal (nth 1 x) popdrop)
				(eq (nth 2 x) 'uidl)))
		      (vm-increment count))
		  (setq response (cdr response))))
	      (vm-store-folder-totals source (list count 0 0 0))
	      (throw 'done (not (eq count 0))))
	    (vm-pop-send-command process "STAT")
	    (setq response (vm-pop-read-stat-response process))
	    (if (null response)
		nil
	      (vm-store-folder-totals source (list (car response) 0 0 0))
	      (not (equal 0 (car response))))))
      (and process (vm-pop-end-session process nil vm-pop-ok-to-ask)))))

(defun vm-expunge-pop-messages ()
  "Deletes all messages from POP mailbox that have already been retrieved
into the current folder.  VM sends POP DELE commands to all the
relevant POP servers to remove the messages."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (if (and (interactive-p) (eq vm-folder-access-method 'pop))
      (error "This command is not meant for POP folders.  Use the normal folder expunge instead."))
  (let ((process nil)
	(source nil)
	(trouble nil)
	(delete-count 0)
	(vm-global-block-new-mail t)
	(vm-pop-ok-to-ask t)
	popdrop uidl-alist data mp match)
    (unwind-protect
	(save-excursion
	  (setq vm-pop-retrieved-messages
		(delq nil vm-pop-retrieved-messages))
	  (setq vm-pop-retrieved-messages
		(sort vm-pop-retrieved-messages
		      (function (lambda (a b)
				  (cond ((string-lessp (nth 1 a) (nth 1 b)) t)
					((string-lessp (nth 1 b)
						       (nth 1 a))
					 nil)
					((string-lessp (car a) (car b)) t)
					(t nil))))))
	  (setq mp vm-pop-retrieved-messages)
	  (while mp
	    (condition-case nil
		(catch 'replay
		  (setq data (car mp))
		  (if (not (equal source (nth 1 data)))
		      (progn
			(if process
			    (progn
			     (vm-pop-end-session process)
			     (setq process nil)))
			(setq source (nth 1 data))
			(setq popdrop (vm-safe-popdrop-string source))
			(condition-case nil
			    (progn
			      (message "Opening POP session to %s..." popdrop)
			      (setq process (vm-pop-make-session source))
			      (if (null process)
				  (signal 'error nil))
			      (message "Expunging messages in %s..." popdrop))
			  (error
			   (message
			    "Couldn't open POP session to %s, skipping..."
			    popdrop)
			   (setq trouble (cons popdrop trouble))
			   (sleep-for 2)
			   (while (equal (nth 1 (car mp)) source)
			     (setq mp (cdr mp)))
			   (throw 'replay t)))
			(set-buffer (process-buffer process))
			(vm-pop-send-command process "UIDL")
			(setq uidl-alist
			      (vm-pop-read-uidl-long-response process))
			(if (null uidl-alist)
			    (signal 'vm-uidl-failed nil))))
		  (if (setq match (rassoc (car data) uidl-alist))
		      (progn
			(vm-pop-send-command process
					     (format "DELE %s" (car match)))
			(if (null (vm-pop-read-response process))
			    (signal 'vm-dele-failed nil))
			(setcar mp nil)
			(vm-increment delete-count)))
		  (setq mp (cdr mp)))
	      (vm-dele-failed
	       (message "DELE %s failed on %s, skipping rest of mailbox..."
			(car match) popdrop)
	       (setq trouble (cons popdrop trouble))
	       (sleep-for 2)
	       (while (equal (nth 1 (car mp)) source)
		 (setq mp (cdr mp)))
	       (throw 'replay t))
	      (vm-uidl-failed
	       (message "UIDL %s failed on %s, skipping this mailbox..."
			(car match) popdrop)
	       (setq trouble (cons popdrop trouble))
	       (sleep-for 2)
	       (while (equal (nth 1 (car mp)) source)
		 (setq mp (cdr mp)))
	       (throw 'replay t))))
	  (if trouble
	      (progn
		(set-buffer (get-buffer-create "*POP Expunge Trouble*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "%s POP message%s expunged.\n\n"
				(if (zerop delete-count) "No" delete-count)
				(if (= delete-count 1) "" "s")))
		(insert "VM had problems expunging messages from:\n")
		(nreverse trouble)
		(setq mp trouble)
		(while mp
		  (insert "   " (car mp) "\n")
		  (setq mp (cdr mp)))
		(setq buffer-read-only t)
		(display-buffer (current-buffer)))
	    (message "%s POP message%s expunged."
		     (if (zerop delete-count) "No" delete-count)
		     (if (= delete-count 1) "" "s"))))
      (and process (vm-pop-end-session process)))
    (setq vm-pop-retrieved-messages
	  (delq nil vm-pop-retrieved-messages))))

(defun vm-pop-make-session (source)
  (let ((process-to-shutdown nil)
	process
	(folder-type vm-folder-type)
	(popdrop (vm-safe-popdrop-string source))
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(use-ssl nil)
	(use-ssh nil)
	(session-name "POP")
	(process-connection-type nil)
	greeting timestamp ssh-process
	host port auth user pass source-list process-buffer source-nopwd)
    (unwind-protect
	(catch 'done
	  ;; parse the maildrop
	  (setq source-list (vm-pop-parse-spec-to-list source))
	  ;; remove pop or pop-ssl from beginning of list if
	  ;; present.
	  (if (= 6 (length source-list))
	      (progn
		(cond ((equal "pop-ssl" (car source-list))
		       (setq use-ssl t
			     session-name "POP over SSL")
		       (if (null vm-stunnel-program)
			   (error "vm-stunnel-program must be non-nil to use POP over SSL.")))
		      ((equal "pop-ssh" (car source-list))
		       (setq use-ssh t
			     session-name "POP over SSH")
		       (if (null vm-ssh-program)
			   (error "vm-ssh-program must be non-nil to use POP over SSH."))))
		(setq source-list (cdr source-list))))
	  (setq host (nth 0 source-list)
		port (nth 1 source-list)
		auth (nth 2 source-list)
		user (nth 3 source-list)
		pass (nth 4 source-list)
		source-nopwd (vm-popdrop-sans-password source))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in POP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in POP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error
	       "No authentication method in POP maildrop specification, \"%s\""
	       source))
	  (if (null user)
	      (error "No user in POP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in POP maildrop specification, \"%s\""
		     source))
	  (if (equal pass "*")
	      (progn
		(setq pass (car (cdr (assoc source-nopwd vm-pop-passwords))))
		(if (null pass)
		    (if (null vm-pop-ok-to-ask)
			(progn (message "Need password for %s" popdrop)
			       (throw 'done nil))
		      (setq pass
			    (vm-read-password
			     (format "POP password for %s: "
				     popdrop)))))))
	  ;; save the password for the sake of
	  ;; vm-expunge-pop-messages, which passes password-less
	  ;; popdrop specifications to vm-make-pop-session.
	  (if (null (assoc source-nopwd vm-pop-passwords))
	      (setq vm-pop-passwords (cons (list source-nopwd pass)
					   vm-pop-passwords)))
	  ;; get the trace buffer
	  (setq process-buffer
		(vm-make-work-buffer (format "trace of %s session to %s"
					     session-name
					     host)))
	  (save-excursion
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    (buffer-disable-undo process-buffer)
	    (make-local-variable 'vm-pop-read-point)
	    ;; clear the trace buffer of old output
	    (erase-buffer)
	    ;; Tell MULE not to mess with the text.
	    (if (fboundp 'set-buffer-file-coding-system)
		(set-buffer-file-coding-system (vm-binary-coding-system) t))
	    (insert "starting " session-name
		    " session " (current-time-string) "\n")
	    (insert (format "connecting to %s:%s\n" host port))
	    ;; open the connection to the server
	    (cond (use-ssl
		   (vm-setup-stunnel-random-data-if-needed)
		   (setq process
			 (apply 'start-process session-name process-buffer
				vm-stunnel-program
				(nconc (vm-stunnel-configuration-args host
								      port)
				       vm-stunnel-program-switches))))
		  (use-ssh
		   (setq process (open-network-stream
				  session-name process-buffer
				  "127.0.0.1"
				  (vm-setup-ssh-tunnel host port))))
		  (t
		   (setq process (open-network-stream session-name
						      process-buffer
						      host port))))
	    (and (null process) (throw 'done nil))
	    (insert-before-markers "connected\n")
	    (setq vm-pop-read-point (point))
	    (process-kill-without-query process)
	    (if (null (setq greeting (vm-pop-read-response process t)))
		(progn (delete-process process)
		       (throw 'done nil)))
	    (setq process-to-shutdown process)
	    ;; authentication
	    (cond ((equal auth "pass")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "PASS %s" pass))
		   (if (null (vm-pop-read-response process))
		       (progn
			 (setq vm-pop-passwords
			       (delete (list source-nopwd pass)
				       vm-pop-passwords))
			 (message "POP password for %s incorrect" popdrop)
			 ;; don't sleep unless we're running synchronously.
			 (if vm-pop-ok-to-ask
			     (sleep-for 2))
			 (throw 'done nil))))
		  ((equal auth "rpop")
		   (vm-pop-send-command process (format "USER %s" user))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil))
		   (vm-pop-send-command process (format "RPOP %s" pass))
		   (and (null (vm-pop-read-response process))
			(throw 'done nil)))
		  ((equal auth "apop")
		   (setq timestamp (vm-parse greeting "[^<]+\\(<[^>]+>\\)")
			 timestamp (car timestamp))
		   (if (null timestamp)
		       (progn
			 (goto-char (point-max))
   (insert-before-markers "<<< ooops, no timestamp found in greeting! >>>\n")
			 (message "Server of %s does not support APOP" popdrop)
			 ;; don't sleep unless we're running synchronously
			 (if vm-pop-ok-to-ask
			     (sleep-for 2))
			 (throw 'done nil)))
		   (vm-pop-send-command
		    process
		    (format "APOP %s %s"
			    user
			    (vm-pop-md5 (concat timestamp pass))))
		   (if (null (vm-pop-read-response process))
		       (progn
			 (setq vm-pop-passwords
			       (delete (list source-nopwd pass)
				       vm-pop-passwords))
			 (message "POP password for %s incorrect" popdrop)
			 (if vm-pop-ok-to-ask
			     (sleep-for 2))
			 (throw 'done nil))))
		  (t (error "Don't know how to authenticate using %s" auth)))
	    (setq process-to-shutdown nil)
	    process ))
      (if process-to-shutdown
	  (vm-pop-end-session process-to-shutdown t))
      (vm-tear-down-stunnel-random-data))))

(defun vm-pop-end-session (process &optional keep-buffer verbose)
  (if (and (memq (process-status process) '(open run))
	   (buffer-live-p (process-buffer process)))
      (save-excursion
	(set-buffer (process-buffer process))
	(vm-pop-send-command process "QUIT")
	;; Previously we did not read the QUIT response because of
	;; TCP shutdown problems (under Windows?) that made it
	;; better if we just closed the connection.  Microsoft
	;; Exchange apparently fails to expunge messages if we shut
	;; down the connection without reading the QUIT response.
	;; So we provide an option and let the user decide what
	;; works best for them.
	(if vm-pop-read-quit-response
	    (progn
	      (and verbose
		   (message "Waiting for response to POP QUIT command..."))
	      (vm-pop-read-response process)
	      (and verbose
		   (message
		    "Waiting for response to POP QUIT command... done"))))))
  (if (not keep-buffer)
      (if (buffer-live-p (process-buffer process))
	  (kill-buffer (process-buffer process)))
    (save-excursion
      (set-buffer (process-buffer process))
      (rename-buffer (concat "saved " (buffer-name)) t)
      (vm-keep-some-buffers (current-buffer) 'vm-kept-pop-buffers
			    vm-pop-keep-failed-trace-buffers)))
  (if (fboundp 'add-async-timeout)
      (add-async-timeout 2 'delete-process process)
    (run-at-time 2 nil 'delete-process process)))

(defun vm-pop-stat-timer (o) (aref o 0))
(defun vm-pop-stat-did-report (o) (aref o 1))
(defun vm-pop-stat-x-box (o) (aref o 2))
(defun vm-pop-stat-x-currmsg (o) (aref o 3))
(defun vm-pop-stat-x-maxmsg (o) (aref o 4))
(defun vm-pop-stat-x-got (o) (aref o 5))
(defun vm-pop-stat-x-need (o) (aref o 6))
(defun vm-pop-stat-y-box (o) (aref o 7))
(defun vm-pop-stat-y-currmsg (o) (aref o 8))
(defun vm-pop-stat-y-maxmsg (o) (aref o 9))
(defun vm-pop-stat-y-got (o) (aref o 10))
(defun vm-pop-stat-y-need (o) (aref o 11))

(defun vm-set-pop-stat-timer (o val) (aset o 0 val))
(defun vm-set-pop-stat-did-report (o val) (aset o 1 val))
(defun vm-set-pop-stat-x-box (o val) (aset o 2 val))
(defun vm-set-pop-stat-x-currmsg (o val) (aset o 3 val))
(defun vm-set-pop-stat-x-maxmsg (o val) (aset o 4 val))
(defun vm-set-pop-stat-x-got (o val) (aset o 5 val))
(defun vm-set-pop-stat-x-need (o val) (aset o 6 val))
(defun vm-set-pop-stat-y-box (o val) (aset o 7 val))
(defun vm-set-pop-stat-y-currmsg (o val) (aset o 8 val))
(defun vm-set-pop-stat-y-maxmsg (o val) (aset o 9 val))
(defun vm-set-pop-stat-y-got (o val) (aset o 10 val))
(defun vm-set-pop-stat-y-need (o val) (aset o 11 val))

(defun vm-pop-start-status-timer ()
  (let ((blob (make-vector 12 nil))
	timer)
    (setq timer (add-timeout 5 'vm-pop-report-retrieval-status blob 5))
    (vm-set-pop-stat-timer blob timer)
    blob ))

(defun vm-pop-stop-status-timer (status-blob)
  (if (vm-pop-stat-did-report status-blob)
      (message ""))
  (if (fboundp 'disable-timeout)
      (disable-timeout (vm-pop-stat-timer status-blob))
    (cancel-timer (vm-pop-stat-timer status-blob))))

(defun vm-pop-report-retrieval-status (o)
  (vm-set-pop-stat-did-report o t)
  (cond ((null (vm-pop-stat-x-got o)) t)
	;; should not be possible, but better safe...
	((not (eq (vm-pop-stat-x-box o) (vm-pop-stat-y-box o))) t)
	((not (eq (vm-pop-stat-x-currmsg o) (vm-pop-stat-y-currmsg o))) t)
	(t (message "Retrieving message %d (of %d) from %s, %s..."
		    (vm-pop-stat-x-currmsg o)
		    (vm-pop-stat-x-maxmsg o)
		    (vm-pop-stat-x-box o)
		    (if (vm-pop-stat-x-need o)
			(format "%d%s of %d%s"
				(vm-pop-stat-x-got o)
				(if (> (vm-pop-stat-x-got o)
				       (vm-pop-stat-x-need o))
				    "!"
				  "")
				(vm-pop-stat-x-need o)
				(if (eq (vm-pop-stat-x-got o)
					(vm-pop-stat-y-got o))
				    " (stalled)"
				  ""))
		      "post processing"))))
  (vm-set-pop-stat-y-box o (vm-pop-stat-x-box o))
  (vm-set-pop-stat-y-currmsg o (vm-pop-stat-x-currmsg o))
  (vm-set-pop-stat-y-maxmsg o (vm-pop-stat-x-maxmsg o))
  (vm-set-pop-stat-y-got o (vm-pop-stat-x-got o))
  (vm-set-pop-stat-y-need o (vm-pop-stat-x-need o)))

(defun vm-pop-check-connection (process)
  (cond ((not (memq (process-status process) '(open run)))
	 (error "POP connection not open: %s" process))
	((not (buffer-live-p (process-buffer process)))
	 (error "POP process %s's buffer has been killed" process))))

(defun vm-pop-send-command (process command)
  (vm-pop-check-connection process)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert-before-markers "PASS <omitted>\r\n")
    (insert-before-markers command "\r\n"))
  (setq vm-pop-read-point (point))
  (process-send-string process (format "%s\r\n" command)))

(defun vm-pop-read-response (process &optional return-response-string)
  (vm-pop-check-connection process)
  (let ((case-fold-search nil)
	 match-end)
    (goto-char vm-pop-read-point)
    (while (not (search-forward "\r\n" nil t))
      (vm-pop-check-connection process)
      (accept-process-output process)
      (goto-char vm-pop-read-point))
    (setq match-end (point))
    (goto-char vm-pop-read-point)
    (if (not (looking-at "+OK"))
	(progn (setq vm-pop-read-point match-end) nil)
      (setq vm-pop-read-point match-end)
      (if return-response-string
	  (buffer-substring (point) match-end)
	t ))))

(defun vm-pop-read-past-dot-sentinel-line (process)
  (vm-pop-check-connection process)
  (let ((case-fold-search nil))
    (goto-char vm-pop-read-point)
    (while (not (re-search-forward "^\\.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let ((opoint (point)))
	(vm-pop-check-connection process)
	(accept-process-output process)
	(goto-char opoint)))
    (setq vm-pop-read-point (point))))

(defun vm-pop-read-stat-response (process)
  (let ((response (vm-pop-read-response process t))
	list)
    (if (null response)
	nil
      (setq list (vm-parse response "\\([^ ]+\\) *"))
      (list (string-to-int (nth 1 list)) (string-to-int (nth 2 list))))))

(defun vm-pop-read-list-response (process)
  (let ((response (vm-pop-read-response process t)))
    (and response
	 (string-to-int (nth 2 (vm-parse response "\\([^ ]+\\) *"))))))

(defun vm-pop-read-uidl-long-response (process)
  (vm-pop-check-connection process)
  (let ((start vm-pop-read-point)
	(list nil)
	n uidl)
    (catch 'done
      (goto-char start)
      (while (not (re-search-forward "^\\.\r\n\\|^-ERR .*$" nil 0))
	(beginning-of-line)
	;; save-excursion doesn't work right
	(let ((opoint (point)))
	  (vm-pop-check-connection process)
	  (accept-process-output process)
	  (goto-char opoint)))
      (setq vm-pop-read-point (point-marker))
      (goto-char start)
      ;; no uidl support, bail.
      (if (not (looking-at "\\+OK"))
	  (throw 'done nil))
      (forward-line 1)
      (while (not (eq (char-after (point)) ?.))
	;; not loking at a number, bail.
	(if (not (looking-at "[0-9]"))
	    (throw 'done nil))
	(setq n (int-to-string (read (current-buffer))))
	(skip-chars-forward " ")
	(setq start (point))
	(skip-chars-forward "\041-\176")
	;; no tag after the message number, bail.
	(if (= start (point))
	    (throw 'done nil))
	(setq uidl (buffer-substring start (point)))
	(setq list (cons (cons n uidl) list))
	(forward-line 1))
      ;; returning nil means the uidl command failed so don't
      ;; return nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list ))))

(defun vm-pop-ask-about-large-message (process popdrop size n)
  (let ((work-buffer nil)
	(pop-buffer (current-buffer))
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (vm-pop-send-command process (format "TOP %d %d" n 0))
	    (if (vm-pop-read-response process)
		(progn
		  (setq start vm-pop-read-point)
		  (vm-pop-read-past-dot-sentinel-line process)
		  (setq end vm-pop-read-point)
		  (setq work-buffer (generate-new-buffer
				     (format "*headers of %s message %d*"
					     popdrop n)))
		  (set-buffer work-buffer)
		  (insert-buffer-substring pop-buffer start end)
		  (forward-line -1)
		  (delete-region (point) (point-max))
		  (vm-pop-cleanup-region (point-min) (point-max))
		  (vm-display-buffer work-buffer)
		  (setq minibuffer-scroll-window (selected-window))
		  (goto-char (point-min))
		  (if (re-search-forward "^Received:" nil t)
		      (progn
			(goto-char (match-beginning 0))
			(vm-reorder-message-headers
			 nil vm-visible-headers
			 vm-invisible-header-regexp)))
		  (set-window-point (selected-window) (point))))
	    (if (y-or-n-p (format "Message %d, size = %d, retrieve? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from popdrop? " n size))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-pop-ask-about-no-uidl (popdrop)
  (let ((work-buffer nil)
	(pop-buffer (current-buffer))
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (setq work-buffer (generate-new-buffer
			       (format "*trouble with %s*" popdrop)))
	    (set-buffer work-buffer)
	    (insert
"You have asked VM to leave messages on the server for the POP mailbox "
popdrop
".  VM cannot do so because the server does not seem to support the POP UIDL command.\n\nYou can either continue to retrieve messages from this mailbox with VM deleting the messages from the server, or you can skip this mailbox, leaving messages on the server and not retrieving any messages.")
	    (fill-individual-paragraphs (point-min) (point-max))
	    (vm-display-buffer work-buffer)
	    (setq minibuffer-scroll-window (selected-window))
	    (yes-or-no-p "Continue retrieving anyway? ")))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-pop-retrieve-to-target (process target statblob)
  (vm-pop-check-connection process)
  (let ((start vm-pop-read-point) end)
    (goto-char start)
    (vm-set-pop-stat-x-got statblob 0)
    (while (not (re-search-forward "^\\.\r\n" nil 0))
      (beginning-of-line)
      ;; save-excursion doesn't work right
      (let* ((opoint (point))
	     (func
	      (function
	       (lambda (beg end len)
		 (if vm-pop-read-point
		     (progn
		       (vm-set-pop-stat-x-got statblob (- end start))
		       (if (zerop (% (random) 10))
			   (vm-pop-report-retrieval-status statblob)))))))
	     (after-change-functions (cons func after-change-functions)))
	(vm-pop-check-connection process)
	(accept-process-output process)
	(goto-char opoint)))
    (vm-set-pop-stat-x-need statblob nil)
    (setq vm-pop-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker))
    (vm-pop-cleanup-region start end)
    (vm-set-pop-stat-x-got statblob nil)
    ;; Some POP servers strip leading and trailing message
    ;; separators, some don't.  Figure out what kind we're
    ;; talking to and do the right thing.
    (if (eq (vm-get-folder-type nil start end) 'unknown)
	(progn
	  (vm-munge-message-separators vm-folder-type start end)
	  (goto-char start)
	  ;; avoid the consing and stat() call for all but babyl
	  ;; files, since this will probably slow things down.
	  ;; only babyl files have the folder header, and we
	  ;; should only insert it if the target folder is empty.
	  (if (and (eq vm-folder-type 'babyl)
		   (cond ((stringp target)
			  (let ((attrs (file-attributes target)))
			    (or (null attrs) (equal 0 (nth 7 attrs)))))
			 ((bufferp target)
			  (save-excursion
			    (set-buffer target)
			    (zerop (buffer-size))))))
	      (let ((opoint (point)))
		(vm-convert-folder-header nil vm-folder-type)
		;; if start is a marker, then it was moved
		;; forward by the insertion.  restore it.
		(setq start opoint)
		(goto-char start)
		(vm-skip-past-folder-header)))
	  (insert (vm-leading-message-separator))
	  (save-restriction
	    (narrow-to-region (point) end)
	    (vm-convert-folder-type-headers 'baremessage vm-folder-type))
	  (goto-char end)
	  (insert-before-markers (vm-trailing-message-separator))))
    (if (stringp target)
	;; Set file type to binary for DOS/Windows.  I don't know if
	;; this is correct to do or not; it depends on whether the
	;; the CRLF or the LF newline convention is used on the inbox
	;; associated with this crashbox.  This setting assumes the LF
	;; newline convention is used.
	(let ((buffer-file-type t)
	      (selective-display nil))
	  (write-region start end target t 0))
      (let ((b (current-buffer)))
	(save-excursion
	  (set-buffer target)
	  (let ((buffer-read-only nil))
	    (insert-buffer-substring b start end)))))
    (delete-region start end)
    t ))

(defun vm-pop-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t))
    (goto-char start)
    ;; chop leading dots
    (while (and (< (point) end) (re-search-forward "^\\."  end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun vm-popdrop-sans-password (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (if (= 6 (length source-list))
	(setq source-list (cdr source-list)))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":"
	    (nth 3 source-list) ":*")))

(defun vm-establish-new-folder-pop-session (&optional interactive)
  (let ((process (vm-folder-pop-process))
	(vm-pop-ok-to-ask interactive))
    (if (processp process)
	(vm-pop-end-session process))
    (setq process (vm-pop-make-session (vm-folder-pop-maildrop-spec)))
    (vm-set-folder-pop-process process)
    process ))

(defun vm-pop-get-uidl-data ()
  (let ((there (make-vector 67 0))
	(process (vm-folder-pop-process)))
    (save-excursion
      (set-buffer (process-buffer process))
      (vm-pop-send-command process "UIDL")
      (let ((start vm-pop-read-point)
	    n uidl)
	(catch 'done
	  (goto-char start)
	  (while (not (re-search-forward "^\\.\r\n\\|^-ERR .*$" nil 0))
	    (beginning-of-line)
	    ;; save-excursion doesn't work right
	    (let ((opoint (point)))
	      (vm-pop-check-connection process)
	      (accept-process-output process)
	      (goto-char opoint)))
	  (setq vm-pop-read-point (point-marker))
	  (goto-char start)
	  ;; no uidl support, bail.
	  (if (not (looking-at "\\+OK"))
	      (throw 'done nil))
	  (forward-line 1)
	  (while (not (eq (char-after (point)) ?.))
	    ;; not loking at a number, bail.
	    (if (not (looking-at "[0-9]"))
		(throw 'done nil))
	    (setq n (int-to-string (read (current-buffer))))
	    (skip-chars-forward " ")
	    (setq start (point))
	    (skip-chars-forward "\041-\176")
	    ;; no tag after the message number, bail.
	    (if (= start (point))
		(throw 'done nil))
	    (setq uidl (buffer-substring start (point)))
	    (set (intern uidl there) n)
	    (forward-line 1))
	  there )))))

(defun vm-pop-get-synchronization-data ()
  (let ((here (make-vector 67 0))
	(there (vm-pop-get-uidl-data))
	(process (vm-folder-pop-process))
	retrieve-list expunge-list
	mp)
    (setq mp vm-message-list)
    (while mp
      (if (null (vm-pop-uidl-of (car mp)))
	  nil
	(set (intern (vm-pop-uidl-of (car mp)) here) (car mp))
	(if (not (boundp (intern (vm-pop-uidl-of (car mp)) there)))
	    (setq expunge-list (cons (car mp) expunge-list))))
      (setq mp (cdr mp)))
    (mapatoms (function
	       (lambda (sym)
		 (if (and (not (boundp (intern (symbol-name sym) here)))
			  (not (assoc (symbol-name sym)
				      vm-pop-retrieved-messages)))
		     (setq retrieve-list (cons
					  (cons (symbol-name sym)
						(symbol-value sym))
					  retrieve-list)))))
	      there)
    (list retrieve-list expunge-list)))

(defun vm-pop-synchronize-folder (&optional interactive
					    do-remote-expunges
					    do-local-expunges
					    do-retrieves)
  (if (and do-retrieves vm-block-new-mail)
      (error "Can't get new mail until you save this folder."))
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-pop-session interactive)))
      nil
    (if do-retrieves
	(vm-assimilate-new-messages))
    (let* ((sync-data (vm-pop-get-synchronization-data))
	   (retrieve-list (car sync-data))
	   (local-expunge-list (nth 1 sync-data))
	   (process (vm-folder-pop-process))
	   (n 1)
	   (statblob nil)
	   (popdrop (vm-folder-pop-maildrop-spec))
	   (safe-popdrop (vm-safe-popdrop-string popdrop))
	   r-list mp got-some message-size
	   (folder-buffer (current-buffer)))
      (if (and do-retrieves retrieve-list)
	  (save-excursion
	    (vm-save-restriction
	     (widen)
	     (goto-char (point-max))
	     (condition-case error-data
		 (save-excursion
		   (set-buffer (process-buffer process))
		   (setq statblob (vm-pop-start-status-timer))
		   (vm-set-pop-stat-x-box statblob safe-popdrop)
		   (vm-set-pop-stat-x-maxmsg statblob
					     (length retrieve-list))
		   (setq r-list retrieve-list)
		   (while r-list
		     (vm-set-pop-stat-x-currmsg statblob n)
		     (vm-pop-send-command process (format "LIST %s"
							  (cdr (car r-list))))
		     (setq message-size (vm-pop-read-list-response process))
		     (vm-set-pop-stat-x-need statblob message-size)
		     (vm-pop-send-command process
					  (format "RETR %s"
						  (cdr (car r-list))))
		     (and (null (vm-pop-read-response process))
			  (error "server didn't say +OK to RETR %s command"
				 (cdr (car r-list))))
		     (vm-pop-retrieve-to-target process folder-buffer
						statblob)
		     (setq r-list (cdr r-list)
			   n (1+ n))))
	       (error
		(message "Retrieval from %s signaled: %s" safe-popdrop
			 error-data))
	       (quit
		(message "Quit received during retrieval from %s"
			 safe-popdrop)))
	     (and statblob (vm-pop-stop-status-timer statblob))
	     ;; to make the "Mail" indicator go away
	     (setq vm-spooled-mail-waiting nil)
	     (intern (buffer-name) vm-buffers-needing-display-update)
	     (vm-increment vm-modification-counter)
	     (vm-update-summary-and-mode-line)
	     (setq mp (vm-assimilate-new-messages t))
	     (setq got-some mp)
	     (setq r-list retrieve-list)
	     (while mp
	       (vm-set-pop-uidl-of (car mp) (car (car r-list)))
	       (vm-set-stuff-flag-of (car mp) t)
	       (setq mp (cdr mp)
		     r-list (cdr r-list))))))
      (if do-local-expunges
	  (vm-expunge-folder t t local-expunge-list))
      (if (and do-remote-expunges
	       vm-pop-messages-to-expunge)
	  (let ((process (vm-folder-pop-process)))
	    ;; POP servers usually allow only one remote accessor
	    ;; at a time vm-expunge-pop-messages will set up its
	    ;; own connection so we get out of its way by closing
	    ;; our connection.
	    (if (and (processp process)
		     (memq (process-status process) '(open run)))
		(vm-pop-end-session process))
	    (setq vm-pop-retrieved-messages
		  (mapcar (function (lambda (x) (list x popdrop 'uidl)))
			  vm-pop-messages-to-expunge))
	    (vm-expunge-pop-messages)
	    (setq vm-pop-messages-to-expunge
		  (mapcar (function (lambda (x) (car x)))
			  vm-pop-retrieved-messages))))
      got-some)))

(defun vm-pop-folder-check-for-mail (&optional interactive)
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-pop-session interactive)))
      nil
    (let ((result (car (vm-pop-get-synchronization-data))))
      (vm-pop-end-session (vm-folder-pop-process))
      result )))

(defun vm-pop-find-spec-for-name (name)
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (equal name (nth 1 (car list)))
	  (setq done t)
	(setq list (cdr list))))
    (and list (car (car list)))))

(defun vm-pop-find-name-for-spec (spec)
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (equal spec (car (car list)))
	  (setq done t)
	(setq list (cdr list))))
    (and list (nth 1 (car list)))))

(defun vm-pop-find-name-for-buffer (buffer)
  (let ((list vm-pop-folder-alist)
	(done nil))
    (while (and (not done) list)
      (if (eq buffer (vm-get-file-buffer (vm-pop-make-filename-for-spec
					  (car (car list)))))
	  (setq done t)
	(setq list (cdr list))))
    (and list (nth 1 (car list)))))

(defun vm-pop-make-filename-for-spec (spec &optional scrub-password scrub-spec)
  (let (md5 list)
    (if (and (null scrub-password) (null scrub-spec))
	nil
      (setq list (vm-pop-parse-spec-to-list spec))
      (setcar (vm-last list) "*")
      (if scrub-spec
	  (progn
	    (cond ((= (length list) 6)
		   (setcar list "pop")
		   (setcar (nthcdr 2 list) "*")
		   (setcar (nthcdr 3 list) "*"))
		  (t
		   (setq list (cons "pop" list))
		   (setcar (nthcdr 2 list) "*")
		   (setcar (nthcdr 3 list) "*")))))
      (setq spec (mapconcat (function identity) list ":")))
    (setq md5 (vm-md5-string spec))
    (expand-file-name (concat "pop-cache-" md5)
		      (or vm-pop-folder-cache-directory
			  vm-folder-directory
			  (getenv "HOME")))))

(defun vm-pop-parse-spec-to-list (spec)
  (if (string-match "\\(pop\\|pop-ssh\\|pop-ssl\\)" spec)
      (vm-parse spec "\\([^:]+\\):?" 1 5)
    (vm-parse spec "\\([^:]+\\):?" 1 4)))

(provide 'vm-pop)

;;; Simple IMAP4 (RFC 2060) client for VM
;;; Copyright (C) 1998, 2001, 2003 Kyle E. Jones
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

;;(provide 'vm-imap)

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-imap-protocol-error "IMAP protocol error"))
  (put 'vm-imap-protocol-error 'error-conditions
       '(vm-imap-protocol-error error))
  (put 'vm-imap-protocol-error 'error-message "IMAP protocol error"))

(defun vm-imap-capability (cap &optional process)
  (if process
      (save-excursion
	(set-buffer (process-buffer process))
	(memq cap vm-imap-capabilities))
    (memq cap vm-imap-capabilities)))

(defun vm-imap-auth-method (auth)
  (memq auth vm-imap-auth-methods))

(defsubst vm-folder-imap-maildrop-spec ()
  (aref vm-folder-access-data 0))
(defsubst vm-folder-imap-process ()
  (aref vm-folder-access-data 1))
(defsubst vm-folder-imap-uid-validity ()
  (aref vm-folder-access-data 2))
(defsubst vm-folder-imap-uid-list ()
  (aref vm-folder-access-data 3))
(defsubst vm-folder-imap-mailbox-count ()
  (aref vm-folder-access-data 4))
(defsubst vm-folder-imap-read-write ()
  (aref vm-folder-access-data 5))
(defsubst vm-folder-imap-can-delete ()
  (aref vm-folder-access-data 6))
(defsubst vm-folder-imap-body-peek ()
  (aref vm-folder-access-data 7))
(defsubst vm-folder-imap-permanent-flags ()
  (aref vm-folder-access-data 8))

(defsubst vm-set-folder-imap-maildrop-spec (val)
  (aset vm-folder-access-data 0 val))
(defsubst vm-set-folder-imap-process (val)
  (aset vm-folder-access-data 1 val))
(defsubst vm-set-folder-imap-uid-validity (val)
  (aset vm-folder-access-data 2 val))
(defsubst vm-set-folder-imap-uid-list (val)
  (aset vm-folder-access-data 3 val))
(defsubst vm-set-folder-imap-mailbox-count (val)
  (aset vm-folder-access-data 4 val))
(defsubst vm-set-folder-imap-read-write (val)
  (aset vm-folder-access-data 5 val))
(defsubst vm-set-folder-imap-can-delete (val)
  (aset vm-folder-access-data 6 val))
(defsubst vm-set-folder-imap-body-peek (val)
  (aset vm-folder-access-data 7 val))
(defsubst vm-set-folder-imap-permanent-flags (val)
  (aset vm-folder-access-data 8 val))

;; Our goal is to drag the mail from the IMAP maildrop to the crash box.
;; just as if we were using movemail on a spool file.
;; We remember which messages we have retrieved so that we can
;; leave the message in the mailbox, and yet not retrieve the
;; same messages again and again.
(defun vm-imap-move-mail (source destination)
  (let ((process nil)
	(m-per-session vm-imap-messages-per-session)
	(b-per-session vm-imap-bytes-per-session)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-move-mail)
			(wrong-number-of-arguments
			  (find-file-name-handler source)))))
	(imapdrop (vm-safe-imapdrop-string source))
	(statblob nil)
	(msgid (list nil nil (vm-imapdrop-sans-password source) 'uid))
	(imap-retrieved-messages vm-imap-retrieved-messages)
	(did-delete nil)
	(source-nopwd (vm-imapdrop-sans-password source))
	use-body-peek auto-expunge x select source-list uid
	can-delete read-write uid-validity
	mailbox mailbox-count message-size response
	n (retrieved 0) retrieved-bytes process-buffer)
    (setq auto-expunge (cond ((setq x (assoc source
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     ((setq x (assoc (vm-imapdrop-sans-password source)
					     vm-imap-auto-expunge-alist))
			      (cdr x))
			     (t vm-imap-expunge-after-retrieving)))
    (unwind-protect
	(catch 'end-of-session
	  (if handler
	      (throw 'end-of-session
		     (funcall handler 'vm-imap-move-mail source destination)))
	  (setq process (vm-imap-make-session source))
	  (or process (throw 'end-of-session nil))
	  (setq process-buffer (process-buffer process))
	  (save-excursion
	    (set-buffer process-buffer)
	    ;; find out how many messages are in the box.
	    (setq source-list (vm-parse source "\\([^:]+\\):?")
		  mailbox (nth 3 source-list))
	    (setq select (vm-imap-select-mailbox process mailbox))
	    (setq mailbox-count (nth 0 select)
		  uid-validity (nth 1 select)
		  read-write (nth 2 select)
		  can-delete (nth 3 select)
		  use-body-peek (vm-imap-capability 'IMAP4REV1))
	    ;; sweep through the retrieval list, removing entries
	    ;; that have been invalidated by the new UIDVALIDITY
	    ;; value.
	    (setq imap-retrieved-messages
		  (vm-imap-clear-invalid-retrieval-entries
		   source-nopwd
		   imap-retrieved-messages
		   uid-validity))
	    ;; loop through the maildrop retrieving and deleting
	    ;; messages as we go.
	    (setq n 1 retrieved-bytes 0)
	    (setq statblob (vm-imap-start-status-timer))
	    (vm-set-imap-stat-x-box statblob imapdrop)
	    (vm-set-imap-stat-x-maxmsg statblob mailbox-count)
	    (while (and (<= n mailbox-count)
			(or (not (natnump m-per-session))
			    (< retrieved m-per-session))
			(or (not (natnump b-per-session))
			    (< retrieved-bytes b-per-session)))
	      (catch 'skip
		(vm-set-imap-stat-x-currmsg statblob n)
		(let (list)
		  (setq list (vm-imap-get-uid-list process n n))
		  (setq uid (cdr (car list)))
		  (setcar msgid uid)
		  (setcar (cdr msgid) uid-validity)
		  (if (member msgid imap-retrieved-messages)
		      (progn
			(if vm-imap-ok-to-ask
			    (message
			     "Skipping message %d (of %d) from %s (retrieved already)..."
			     n mailbox-count imapdrop))
			(throw 'skip t))))
		(setq message-size (vm-imap-get-message-size process n))
		(vm-set-imap-stat-x-need statblob message-size)
		(if (and (integerp vm-imap-max-message-size)
			 (> message-size vm-imap-max-message-size)
			 (progn
			   (setq response
				 (if vm-imap-ok-to-ask
				     (vm-imap-ask-about-large-message
				      process message-size n)
				   'skip))
			   (not (eq response 'retrieve))))
		    (progn
		      (if (and read-write can-delete (eq response 'delete))
			  (progn
			    (message "Deleting message %d..." n)
			    (vm-imap-delete-message process n)
			    (setq did-delete t))
			(if vm-imap-ok-to-ask
			    (message "Skipping message %d..." n)
			  (message
			   "Skipping message %d in %s, too large (%d > %d)..."
			   n imapdrop message-size vm-imap-max-message-size)))
		      (throw 'skip t)))
		(message "Retrieving message %d (of %d) from %s..."
			 n mailbox-count imapdrop)
		(if use-body-peek
		    (progn
		      (vm-imap-send-command process
					    (format "FETCH %d (BODY.PEEK[])"
						    n))
		      (vm-imap-retrieve-to-target process destination
						  statblob t))
		  (progn
		       (vm-imap-send-command process
					     (format
					      "FETCH %d (RFC822.PEEK)" n))
		       (vm-imap-retrieve-to-target process destination
						   statblob nil)))
		(vm-increment retrieved)
		(and b-per-session
		     (setq retrieved-bytes (+ retrieved-bytes message-size)))
		(if (not auto-expunge)
		    (setq imap-retrieved-messages
			  (cons (copy-sequence msgid)
				imap-retrieved-messages))
		  ;; The user doesn't want the messages
		  ;; kept in the mailbox.
		  ;; Delete the message now.
		  (if (and read-write can-delete)
		      (progn
			(vm-imap-delete-message process n)
			(setq did-delete t)))))
	      (vm-increment n))
	    (if did-delete
		(progn
		  ;; CLOSE forces an expunge and avoids the EXPUNGE
		  ;; responses.
		  (vm-imap-send-command process "CLOSE")
		  (vm-imap-read-ok-response process)))
	    (not (equal retrieved 0)) ))
      (setq vm-imap-retrieved-messages imap-retrieved-messages)
      (if (and (eq vm-flush-interval t) (not (equal retrieved 0)))
	  (vm-stuff-imap-retrieved))
      (and statblob (vm-imap-stop-status-timer statblob))
      (if process
	  (vm-imap-end-session process)))))

(defun vm-imap-check-mail (source)
  (let ((process nil)
	(handler (and (fboundp 'find-file-name-handler)
		      (condition-case ()
			  (find-file-name-handler source 'vm-imap-check-mail)
			(wrong-number-of-arguments
			 (find-file-name-handler source)))))
	(retrieved vm-imap-retrieved-messages)
	(imapdrop (vm-imapdrop-sans-password source))
	(count 0)
	msg-count uid-validity x response select mailbox source-list)
    (unwind-protect
	(prog1
	    (save-excursion
	      (catch 'end-of-session
		(if handler
		    (throw 'end-of-session
			   (funcall handler 'vm-imap-check-mail source)))
		(setq process (vm-imap-make-session source))
		(or process (throw 'end-of-session nil))
		(set-buffer (process-buffer process))
		(setq source-list (vm-parse source "\\([^:]+\\):?")
		      mailbox (nth 3 source-list))
		(setq select (vm-imap-select-mailbox process mailbox)
		      msg-count (car select)
		      uid-validity (nth 1 select))
		(if (zerop msg-count)
		    (progn
		      (vm-store-folder-totals source '(0 0 0 0))
		      (throw 'end-of-session nil)))
		;; sweep through the retrieval list, removing entries
		;; that have been invalidated by the new UIDVALIDITY
		;; value.
		(setq retrieved
		  (vm-imap-clear-invalid-retrieval-entries imapdrop
							   retrieved
							   uid-validity))
		(setq response (vm-imap-get-uid-list process 1 msg-count))
		(if (null response)
		    nil
		  (if (null (car response))
		      ;; (nil . nil) is returned if there are no
		      ;; messages in the mailbox.
		      (progn
			(vm-store-folder-totals source '(0 0 0 0))
			(throw 'end-of-session nil))
		    (while response
		      (if (not (and (setq x (assoc (cdr (car response))
						   retrieved))
				    (equal (nth 1 x) imapdrop)
				    (eq (nth 2 x) 'uid)))
			  (vm-increment count))
		      (setq response (cdr response))))
		  (vm-store-folder-totals source (list count 0 0 0))
		  (throw 'end-of-session (not (eq count 0))))
		(not (equal 0 (car select)))))
	  (setq vm-imap-retrieved-messages retrieved))
      (and process (vm-imap-end-session process)))))

(defun vm-expunge-imap-messages ()
  "Deletes all messages from IMAP mailbox that have already been retrieved
into the current folder.  VM sets the \\Deleted flag on all such messages
on all the relevant IMAP servers and then immediately expunges."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-virtual-folder)
  (let ((process nil)
	(source nil)
	(trouble nil)
	(delete-count 0)
	(vm-global-block-new-mail t)
	(vm-imap-ok-to-ask t)
	(did-delete nil)
	msg-count can-delete read-write uid-validity
	select-response source-list imapdrop uid-alist mailbox data mp match)
    (unwind-protect
	(save-excursion
	  (setq vm-imap-retrieved-messages
		(sort vm-imap-retrieved-messages
		      (function (lambda (a b)
				  (cond ((string-lessp (nth 2 a) (nth 2 b)) t)
					((string-lessp (nth 2 b)
						       (nth 2 a))
					 nil)
					((string-lessp (nth 1 a) (nth 1 b)) t)
					((string-lessp (nth 1 b) (nth 1 a))
					 nil)
					((string-lessp (nth 0 a) (nth 0 b)) t)
					(t nil))))))
	  (setq mp vm-imap-retrieved-messages)
	  (while mp
	    (catch 'replay
	      (condition-case error-data
		  (progn
		    (setq data (car mp))
		    (if (not (equal source (nth 2 data)))
			(progn
			  (if process
			      (progn
				(if did-delete
				    (progn
				      (vm-imap-send-command process "CLOSE")
				      (vm-imap-read-ok-response process)))
				(vm-imap-end-session process)
				(setq process nil
				      did-delete nil)))
			  (setq source (nth 2 data))
			  (setq imapdrop (vm-safe-imapdrop-string source))
			  (condition-case error-data
			      (progn
				(message "Opening IMAP session to %s..."
					 imapdrop)
				(setq process (vm-imap-make-session source))
				(if (null process)
				    (signal 'error nil))
				(set-buffer (process-buffer process))
				(setq source-list (vm-parse source
							    "\\([^:]+\\):?")
				      mailbox (nth 3 source-list)
				      select-response (vm-imap-select-mailbox
						       process mailbox)
				      msg-count (car select-response)
				      uid-validity (nth 1 select-response)
				      read-write (nth 2 select-response)
				      can-delete (nth 3 select-response))
				(setq mp
				      (vm-imap-clear-invalid-retrieval-entries
				       source
				       mp
				       uid-validity))
				(if (not (eq data (car mp)))
				    ;; this entry must have been
				    ;; discarded as invalid, so
				    ;; skip it and process the
				    ;; entry that is now at the
				    ;; head of the list.
				    (throw 'replay t))
				(if (not can-delete)
				    (error "Can't delete messages in mailbox %s, skipping..." mailbox))
				(if (not read-write)
				    (error "Mailbox %s is read-only, skipping..." mailbox))
				(message "Expunging messages in %s..." imapdrop))
			    (error
			     (if (cdr error-data)
				 (apply 'message (cdr error-data))
			       (message
				"Couldn't open IMAP session to %s, skipping..."
				imapdrop))
			     (setq trouble (cons imapdrop trouble))
			     (sleep-for 2)
			     (while (equal (nth 1 (car mp)) source)
			       (setq mp (cdr mp)))
			     (throw 'replay t)))
			  (if (zerop msg-count)
			      (progn
				(while (equal (nth 1 (car mp)) source)
				  (setq mp (cdr mp)))
				(throw 'replay t)))
			  (setq uid-alist
				(vm-imap-get-uid-list
				 process 1 msg-count))))
		    (if (setq match (rassoc (car data) uid-alist))
			(progn
			  (vm-imap-delete-message process (car match))
			  (setq did-delete t)
			  (vm-increment delete-count))))
		(error
		 (setq trouble (cons imapdrop trouble))
		 (message "Something signaled: %s"
			  (prin1-to-string error-data))
		 (sleep-for 2)
		 (message "Skipping rest of mailbox %s..." imapdrop)
		 (sleep-for 2)
		 (while (equal (nth 2 (car mp)) source)
		   (setq mp (cdr mp)))
		 (throw 'replay t)))
	      (setq mp (cdr mp))))
	  (if did-delete
	      (progn
		(vm-imap-send-command process "CLOSE")
		(vm-imap-read-ok-response process)))
	  (if trouble
	      (progn
		(set-buffer (get-buffer-create "*IMAP Expunge Trouble*"))
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "%s IMAP message%s expunged.\n\n"
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
	    (message "%s IMAP message%s expunged."
		     (if (zerop delete-count) "No" delete-count)
		     (if (= delete-count 1) "" "s"))))
      (and process (vm-imap-end-session process)))
    (or trouble (setq vm-imap-retrieved-messages nil))))

(defun vm-imap-make-session (source)
  (let ((process-to-shutdown nil)
	(folder-type vm-folder-type)
	process ooo
	(imapdrop (vm-safe-imapdrop-string source))
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(use-ssl nil)
	(use-ssh nil)
	(session-name "IMAP")
	(process-connection-type nil)
	greeting timestamp
	host port mailbox auth user pass source-list process-buffer
	source-nopwd-nombox)
    (unwind-protect
	(catch 'end-of-session
	  ;; parse the maildrop
	  (setq source-list (vm-parse source "\\([^:]+\\):?")
		host (nth 1 source-list)
		port (nth 2 source-list)
;;		mailbox (nth 3 source-list)
		auth (nth 4 source-list)
		user (nth 5 source-list)
		pass (nth 6 source-list)
		source-nopwd-nombox
		(vm-imapdrop-sans-password-and-mailbox source))
	  (cond ((equal auth "preauth") t)
		((equal "imap-ssl" (car source-list))
		 (setq use-ssl t
		       session-name "IMAP over SSL")
		 (if (null vm-stunnel-program)
		     (error "vm-stunnel-program must be non-nil to use IMAP over SSL.")))
		((equal "imap-ssh" (car source-list))
		 (setq use-ssh t
		       session-name "IMAP over SSH")
		 (if (null vm-ssh-program)
		     (error "vm-ssh-program must be non-nil to use IMAP over SSH."))))
	  ;; carp if parts are missing
	  (if (null host)
	      (error "No host in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null port)
	      (error "No port in IMAP maildrop specification, \"%s\""
		     source))
	  (if (string-match "^[0-9]+$" port)
	      (setq port (string-to-int port)))
	  (if (null auth)
	      (error "No authentication method in IMAP maildrop specification, \"%s\"" source))
	  (if (null user)
	      (error "No user in IMAP maildrop specification, \"%s\""
		     source))
	  (if (null pass)
	      (error "No password in IMAP maildrop specification, \"%s\""
		     source))
	  (if (and (equal pass "*")
		   (not (equal auth "preauth")))
	      (progn
		(setq pass (car (cdr (assoc source-nopwd-nombox
					    vm-imap-passwords))))
		(if (null pass)
		    (if (null vm-imap-ok-to-ask)
			(progn (message "Need password for %s" imapdrop)
			       (throw 'end-of-session nil))
		      (setq pass
			    (vm-read-password
			     (format "IMAP password for %s: "
				     imapdrop)))))))
	  ;; save the password for the sake of
	  ;; vm-expunge-imap-messages, which passes password-less
	  ;; imapdrop specifications to vm-make-imap-session.
	  (if (null (assoc source-nopwd-nombox vm-imap-passwords))
	      (setq vm-imap-passwords (cons (list source-nopwd-nombox pass)
					    vm-imap-passwords)))
	  ;; get the trace buffer
	  (setq process-buffer
		(vm-make-work-buffer (format "trace of %s session to %s"
					     session-name
					     host)))
	  (save-excursion
	    (set-buffer process-buffer)
	    (setq vm-folder-type (or folder-type vm-default-folder-type))
	    (buffer-disable-undo process-buffer)
	    (make-local-variable 'vm-imap-read-point)
	    ;; clear the trace buffer of old output
	    (erase-buffer)
	    ;; Tell MULE not to mess with the text.
	    (if (fboundp 'set-buffer-file-coding-system)
		(set-buffer-file-coding-system (vm-binary-coding-system) t))
	    (if (equal auth "preauth")
		(setq process
		      (run-hook-with-args-until-success 'vm-imap-session-preauth-hook
							host port mailbox
							user pass)))
	    (if (processp process)
		(set-process-buffer process (current-buffer))
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
	      (and (null process) (throw 'end-of-session nil))
	      (insert-before-markers "connected\n"))
	    (setq vm-imap-read-point (point))
	    (process-kill-without-query process)
	    (if (null (setq greeting (vm-imap-read-greeting process)))
		(progn (delete-process process)
		       (throw 'end-of-session nil)))
	    (setq process-to-shutdown process)
	    (set (make-local-variable 'vm-imap-session-done) nil)
	    ;; record server capabilities
	    (vm-imap-send-command process "CAPABILITY")
	    (if (null (setq ooo (vm-imap-read-capability-response process)))
		(throw 'end-of-session nil))
	    (set (make-local-variable 'vm-imap-capabilities) (car ooo))
	    (set (make-local-variable 'vm-imap-auth-methods) (nth 1 ooo))
	    ;; authentication
	    (cond ((equal auth "login")
		   ;; LOGIN must be supported by all imap servers,
		   ;; no need to check for it in CAPABILITIES.
		   (vm-imap-send-command process
					 (format "LOGIN %s %s"
						 (vm-imap-quote-string user)
						 (vm-imap-quote-string pass)))
		   (and (null (vm-imap-read-ok-response process))
			(progn
			  (setq vm-imap-passwords
				(delete (list source-nopwd-nombox pass)
					vm-imap-passwords))
			  (message "IMAP password for %s incorrect" imapdrop)
			  ;; don't sleep unless we're running synchronously.
			  (if vm-imap-ok-to-ask
			      (sleep-for 2))
			  (throw 'end-of-session nil))))
		  ((equal auth "cram-md5")
		   (if (not (vm-imap-auth-method 'CRAM-MD5))
		       (error "CRAM-MD5 authentication unsupported by this server"))
		   (let ((ipad (make-string 64 54))
			 (opad (make-string 64 92))
			 (command "AUTHENTICATE CRAM-MD5")
			 (secret (concat
				  pass
				  (make-string (max 0 (- 64 (length pass)))
					       0)))
			 response p challenge answer)
		     (vm-imap-send-command process command)
		     (setq response (vm-imap-read-response process))
		     (if (vm-imap-response-matches response 'VM 'NO)
			 (error "server said NO to %s" command))
		     (if (vm-imap-response-matches response 'VM 'BAD)
			 (vm-imap-protocol-error "server said BAD to %s"
						 command))
		     (cond ((vm-imap-response-matches response '+ 'atom)
			    (setq p (cdr (nth 1 response))
				  challenge (buffer-substring
					     (nth 0 p)
					     (nth 1 p))
				  challenge (vm-mime-base64-decode-string
					     challenge)))
			   (t
			    (error "Don't understand AUTHENTICATE response")))
		     (setq answer
			   (concat
			    user " "
			    (vm-md5-string
			     (concat
			      (vm-xor-string secret opad)
			      (vm-md5-raw-string 
			       (concat
				(vm-xor-string secret ipad) challenge)))))
			   answer (vm-mime-base64-encode-string answer))
		     (vm-imap-send-command process answer nil t)
		     (and (null (vm-imap-read-ok-response process))
			  (progn
			    (setq vm-imap-passwords
				  (delete (list source-nopwd-nombox pass)
					  vm-imap-passwords))
			    (message "IMAP password for %s incorrect" imapdrop)
			    ;; don't sleep unless we're running synchronously.
			    (if vm-imap-ok-to-ask
				(sleep-for 2))
			    (throw 'end-of-session nil)))))
		  ((equal auth "preauth")
		   (if (not (eq greeting 'preauth))
		       (progn
			 (message "IMAP session was not pre-authenticated")
			 ;; don't sleep unless we're running synchronously.
			 (if vm-imap-ok-to-ask
			     (sleep-for 2))
			 (throw 'end-of-session nil))))
		  (t (error "Don't know how to authenticate using %s" auth)))
	    (setq process-to-shutdown nil)
	    process ))
      (if process-to-shutdown
	  (vm-imap-end-session process-to-shutdown t))
      (vm-tear-down-stunnel-random-data))))

(defun vm-imap-end-session (process &optional keep-buffer)
  (if (and (memq (process-status process) '(open run))
	   (buffer-live-p (process-buffer process)))
      (save-excursion
	(set-buffer (process-buffer process))
	;; vm-imap-end-session might have already been called on
	;; this process, so don't logout and schedule the killing
	;; the process again if it's already been done.
	(if vm-imap-session-done
	    nil
	  (vm-imap-send-command process "LOGOUT")
	  (setq vm-imap-session-done t)
	  ;; we don't care about the response.
	  ;; try reading it anyway and see who complains.
	  (vm-imap-read-ok-response process)
	  (if (and (not vm-imap-keep-trace-buffer) (not keep-buffer))
	      (kill-buffer (process-buffer process))
	    (save-excursion
	      (set-buffer (process-buffer process))
	      (rename-buffer (concat "saved " (buffer-name)) t)
	      (vm-keep-some-buffers (current-buffer) 'vm-kept-imap-buffers
				    vm-imap-keep-failed-trace-buffers)))
	  (if (fboundp 'add-async-timeout)
	      (add-async-timeout 2 'delete-process process)
	    (run-at-time 2 nil 'delete-process process))))))

(defun vm-imap-stat-timer (o) (aref o 0))
(defun vm-imap-stat-did-report (o) (aref o 1))
(defun vm-imap-stat-x-box (o) (aref o 2))
(defun vm-imap-stat-x-currmsg (o) (aref o 3))
(defun vm-imap-stat-x-maxmsg (o) (aref o 4))
(defun vm-imap-stat-x-got (o) (aref o 5))
(defun vm-imap-stat-x-need (o) (aref o 6))
(defun vm-imap-stat-y-box (o) (aref o 7))
(defun vm-imap-stat-y-currmsg (o) (aref o 8))
(defun vm-imap-stat-y-maxmsg (o) (aref o 9))
(defun vm-imap-stat-y-got (o) (aref o 10))
(defun vm-imap-stat-y-need (o) (aref o 11))

(defun vm-set-imap-stat-timer (o val) (aset o 0 val))
(defun vm-set-imap-stat-did-report (o val) (aset o 1 val))
(defun vm-set-imap-stat-x-box (o val) (aset o 2 val))
(defun vm-set-imap-stat-x-currmsg (o val) (aset o 3 val))
(defun vm-set-imap-stat-x-maxmsg (o val) (aset o 4 val))
(defun vm-set-imap-stat-x-got (o val) (aset o 5 val))
(defun vm-set-imap-stat-x-need (o val) (aset o 6 val))
(defun vm-set-imap-stat-y-box (o val) (aset o 7 val))
(defun vm-set-imap-stat-y-currmsg (o val) (aset o 8 val))
(defun vm-set-imap-stat-y-maxmsg (o val) (aset o 9 val))
(defun vm-set-imap-stat-y-got (o val) (aset o 10 val))
(defun vm-set-imap-stat-y-need (o val) (aset o 11 val))

(defun vm-imap-start-status-timer ()
  (let ((blob (make-vector 12 nil))
	timer)
    (setq timer (add-timeout 5 'vm-imap-report-retrieval-status blob 5))
    (vm-set-imap-stat-timer blob timer)
    blob ))

(defun vm-imap-stop-status-timer (status-blob)
  (if (vm-imap-stat-did-report status-blob)
      (message ""))
  (if (fboundp 'disable-timeout)
      (disable-timeout (vm-imap-stat-timer status-blob))
    (cancel-timer (vm-imap-stat-timer status-blob))))

(defun vm-imap-report-retrieval-status (o)
  (vm-set-imap-stat-did-report o t)
  (cond ((null (vm-imap-stat-x-got o)) t)
	;; should not be possible, but better safe...
	((not (eq (vm-imap-stat-x-box o) (vm-imap-stat-y-box o))) t)
	((not (eq (vm-imap-stat-x-currmsg o) (vm-imap-stat-y-currmsg o))) t)
	(t (message "Retrieving message %d (of %d) from %s, %s..."
		    (vm-imap-stat-x-currmsg o)
		    (vm-imap-stat-x-maxmsg o)
		    (vm-imap-stat-x-box o)
		    (if (vm-imap-stat-x-need o)
			(format "%d%s of %d%s"
				(vm-imap-stat-x-got o)
				(if (> (vm-imap-stat-x-got o)
				       (vm-imap-stat-x-need o))
				    "!"
				  "")
				(vm-imap-stat-x-need o)
				(if (eq (vm-imap-stat-x-got o)
					(vm-imap-stat-y-got o))
				    " (stalled)"
				  ""))
		      "post processing"))))
  (vm-set-imap-stat-y-box o (vm-imap-stat-x-box o))
  (vm-set-imap-stat-y-currmsg o (vm-imap-stat-x-currmsg o))
  (vm-set-imap-stat-y-maxmsg o (vm-imap-stat-x-maxmsg o))
  (vm-set-imap-stat-y-got o (vm-imap-stat-x-got o))
  (vm-set-imap-stat-y-need o (vm-imap-stat-x-need o)))

(defun vm-imap-check-connection (process)
  (cond ((not (memq (process-status process) '(open run)))
	 (error "IMAP connection not open: %s" process))
	((not (buffer-live-p (process-buffer process)))
	 (error "IMAP process %s's buffer has been killed" process))))

(defun vm-imap-send-command (process command &optional tag no-tag)
  (vm-imap-check-connection process)
  (goto-char (point-max))
  (or no-tag (insert-before-markers (or tag "VM") " "))
  (let ((case-fold-search t))
    (if (string-match "^LOGIN" command)
	(insert-before-markers "LOGIN <parameters omitted>\r\n")
      (insert-before-markers command "\r\n")))
  (setq vm-imap-read-point (point))
  ;; previously we had a process-send-string call for each string
  ;; to avoid extra consing but that caused a lot of packet overhead.
  (if no-tag
      (process-send-string process (format "%s\r\n" command))
    (process-send-string process (format "%s %s\r\n" (or tag "VM") command))))

(defun vm-imap-select-mailbox (process mailbox &optional just-examine)
  (let ((imap-buffer (current-buffer))
	(command (if just-examine "EXAMINE" "SELECT"))
	tok response p
	(flags nil)
	(permanent-flags nil)
	(msg-count nil)
	(uid-validity nil)
	(read-write (not just-examine))
	(can-delete t)
	(need-ok t))
    (vm-imap-send-command process (format "%s %s" command mailbox))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to %s" command))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to %s" command))
      (cond ((vm-imap-response-matches response '* 'OK 'vector)
	     (setq p (cdr (nth 2 response)))
	     (cond ((vm-imap-response-matches p 'UIDVALIDITY 'atom)
		    (setq tok (nth 1 p))
		    (setq uid-validity (buffer-substring (nth 1 tok)
							 (nth 2 tok))))
		   ((vm-imap-response-matches p 'PERMANENTFLAGS 'list)
		    (setq permanent-flags (nth 1 p)))))
	    ((vm-imap-response-matches response '* 'FLAGS 'list)
	     (setq flags (nth 2 response)))
	    ((vm-imap-response-matches response '* 'atom 'EXISTS)
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-count (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-WRITE))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK '(vector READ-ONLY))
	     (setq need-ok nil read-write t))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    (if (null flags)
	(vm-imap-protocol-error "FLAGS missing from SELECT responses"))
    (if (null msg-count)
	(vm-imap-protocol-error "EXISTS missing from SELECT responses"))
    (if (null uid-validity)
	(vm-imap-protocol-error "UIDVALIDITY missing from SELECT responses"))
    (setq can-delete (vm-imap-scan-list-for-flag flags "\\Deleted"))
    (list msg-count uid-validity read-write can-delete permanent-flags) ))

(defun vm-imap-get-uid-list (process first last)
  (let ((list nil)
	(imap-buffer (current-buffer))
	tok msg-num uid response p
	(need-ok t))
    (vm-imap-send-command process (format "FETCH %s:%s (UID)" first last))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to UID FETCH"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to UID FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'UID 'atom))
		 (vm-imap-protocol-error
		  "expected (UID number) in FETCH response"))
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     (setq tok (nth 1 p))
	     (setq uid (buffer-substring (nth 1 tok) (nth 2 tok))
		   list (cons (cons msg-num uid) list)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
      ;; returning nil means the uid fetch failed so return
      ;; something other than nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list )))

(defun vm-imap-get-flags-list (process first last)
  (let ((list nil)
	(imap-buffer (current-buffer))
	tok msg-num flag flags response p
	(need-ok t))
    (vm-imap-send-command process (format "FETCH %s:%s (FLAGS)" first last))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to FLAGS FETCH"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to FLAGS FETCH"))
      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'FLAGS 'list))
		 (vm-imap-protocol-error
		  "expected (FLAGS list) in FETCH response"))
	     (setq tok (nth 1 response))
	     (goto-char (nth 1 tok))
	     (setq msg-num (read imap-buffer))
	     (setq p (cdr (nth 1 p))
		   flags nil)
	     (while p
	       (setq tok (car p))
	       (if (not (vm-imap-response-matches tok 'atom))
		   (vm-imap-protocol-error
		    "expected atom in FLAGS list in FETCH response"))
	       (setq flag (buffer-substring (nth 1 tok) (nth 2 tok))
		     flags (cons flag flags)
		     p (cdr p)))
	     (setq list (cons (cons msg-num flags) list)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
      ;; returning nil means the fetch failed so return
      ;; something other than nil if there aren't any messages.
      (if (null list)
	  (cons nil nil)
	list )))

(defun vm-imap-ask-about-large-message (process size n)
  (let ((work-buffer nil)
	(imap-buffer (current-buffer))
	(need-ok t)
	(need-header t)
	response fetch-response
	list p
	start end)
    (unwind-protect
	(save-excursion
	  (save-window-excursion
	    (vm-imap-send-command process
				  (format "FETCH %d (RFC822.HEADER)" n))
	    (while need-ok
	      (setq response (vm-imap-read-response process))
	      (if (vm-imap-response-matches response 'VM 'NO)
		  (error "server said NO to header FETCH"))
	      (if (vm-imap-response-matches response 'VM 'BAD)
		  (vm-imap-protocol-error "server said BAD to header FETCH"))
	      (cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
		     (setq fetch-response response
			   need-header nil))
		    ((vm-imap-response-matches response 'VM 'OK)
		     (setq need-ok nil))))
	    (if need-header
		(vm-imap-protocol-error "FETCH OK sent before FETCH response"))
	    (setq vm-imap-read-point (point-marker))
	    (setq list (cdr (nth 3 fetch-response)))
	    (if (not (vm-imap-response-matches list 'RFC822\.HEADER 'string))
		(vm-imap-protocol-error
		 "expected (RFC822.HEADER string) in FETCH response"))
	    (setq p (nth 1 list)
		  start (nth 1 p)
		  end (nth 2 p))
	    (setq work-buffer (generate-new-buffer "*imap-glop*"))
	    (set-buffer work-buffer)
	    (insert-buffer-substring imap-buffer start end)
	    (vm-imap-cleanup-region (point-min) (point-max))
	    (vm-display-buffer work-buffer)
	    (setq minibuffer-scroll-window (selected-window))
	    (goto-char (point-min))
	    (if (re-search-forward "^Received:" nil t)
		(progn
		  (goto-char (match-beginning 0))
		  (vm-reorder-message-headers
		   nil vm-visible-headers
		   vm-invisible-header-regexp)))
	    (set-window-point (selected-window) (point))
	    (if (y-or-n-p (format "Message %d, size = %d, retrieve? " n size))
		'retrieve
	      (if (y-or-n-p (format "Delete message %d from maildrop? " n size))
		  'delete
		'skip))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-imap-retrieve-to-target (process target statblob bodypeek)
  (let ((start vm-imap-read-point)
	(need-msg t)
	end fetch-response list p)
    (goto-char start)
    (vm-set-imap-stat-x-got statblob 0)
    (let* ((func
	    (function
	     (lambda (beg end len)
	       (if vm-imap-read-point
		   (progn
		     (vm-set-imap-stat-x-got statblob (- end start))
		     (if (zerop (% (random) 10))
			 (vm-imap-report-retrieval-status statblob)))))))
	   (after-change-functions (cons func after-change-functions))
	   (need-ok t)
	   response)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to message FETCH"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to message FETCH"))
	(cond ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	       (setq fetch-response response
		     need-msg nil))
	      ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil)))))
    (if need-msg
	(vm-imap-protocol-error "FETCH OK sent before FETCH response"))
    ;; must make the read point a marker so that it stays fixed
    ;; relative to the text when we modify things below.
    (setq vm-imap-read-point (point-marker))
    (setq list (cdr (nth 3 fetch-response)))
    (cond
     (bodypeek
      (if (not (vm-imap-response-matches list 'BODY '(vector) 'string))
	  (vm-imap-protocol-error
	   "expected (BODY[] string) in FETCH response"))
      (setq p (nth 2 list)
	    start (nth 1 p)))
     (t
      (if (not (vm-imap-response-matches list 'RFC822 'string))
	  (vm-imap-protocol-error
	   "expected (RFC822 string) in FETCH response"))
      (setq p (nth 1 list)
	    start (nth 1 p))))
    (goto-char (nth 2 p))
    (setq end (point-marker))
    (vm-set-imap-stat-x-need statblob nil)
    (vm-imap-cleanup-region start end)
    (vm-munge-message-separators vm-folder-type start end)
    (goto-char start)
    (vm-set-imap-stat-x-got statblob nil)
    ;; avoid the consing and stat() call for all but babyl
    ;; files, since this will probably slow things down.
    ;; only babyl files have the folder header, and we
    ;; should only insert it if the crash box is empty.
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
    (insert-before-markers (vm-trailing-message-separator))
    ;; Some IMAP servers don't understand Sun's stupid
    ;; From_-with-Content-Length style folder and assume the last
    ;; newline in the message is a separator.  And so the server
    ;; strips it, leaving us with a message that does not end
    ;; with a newline.  Add the newline if needed.
    ;;
    ;; HP Openmail seems to have this problem.
    (if (and (not (eq ?\n (char-after (1- (point)))))
	     (memq vm-folder-type '(From_-with-Content-Length BellFrom_)))
	(insert-before-markers "\n"))
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

(defun vm-imap-delete-message (process n)
  (vm-imap-send-command process (format "STORE %d +FLAGS.SILENT (\\Deleted)"
					n))
  (if (null (vm-imap-read-ok-response process))
      (vm-imap-protocol-error "STORE ... +FLAGS.SILENT (\\Deleted) failed")))

(defun vm-imap-get-message-size (process n)
  (let ((list nil)
	(imap-buffer (current-buffer))
	tok size response p
	(need-size t)
	(need-ok t))
    (vm-imap-send-command process (format "FETCH %d:%d (RFC822.SIZE)" n n))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to size FETCH"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to size FETCH"))
      (cond ((and need-size
		  (vm-imap-response-matches response '* 'atom 'FETCH 'list))
	     (setq need-size nil)
	     (setq p (cdr (nth 3 response)))
	     (if (not (vm-imap-response-matches p 'RFC822\.SIZE 'atom))
		 (vm-imap-protocol-error
		  "expected (RFC822.SIZE number) in FETCH response"))
	     (setq tok (nth 1 p))
	     (goto-char (nth 1 tok))
	     (setq size (read imap-buffer)))
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil))))
    size ))

(defun vm-imap-read-capability-response (process)
  (let (response r cap-list auth-list (need-ok t))
    (while need-ok
      (setq response (vm-imap-read-response process))
      (if (vm-imap-response-matches response 'VM 'NO)
	  (error "server said NO to CAPABILITY"))
      (if (vm-imap-response-matches response 'VM 'BAD)
	  (vm-imap-protocol-error "server said BAD to CAPABILITY"))
      (if (vm-imap-response-matches response 'VM 'OK)
	  (setq need-ok nil)
	(if (not (vm-imap-response-matches response '* 'CAPABILITY))
	    nil
	  ;; skip * CAPABILITY
	  (setq response (cdr (cdr response)))
	  (while response
	    (setq r (car response))
	    (if (not (eq (car r) 'atom))
		nil
	      (if (save-excursion
		    (goto-char (nth 1 r))
		    (let ((case-fold-search t))
		      (eq (re-search-forward "AUTH=." (nth 2 r) t)
			  (+ 6 (nth 1 r)))))
		  (progn
		    (setq auth-list (cons (intern
					   (upcase (buffer-substring
						    (+ 5 (nth 1 r))
						    (nth 2 r))))
					  auth-list)))
		(setq r (car response))
		(if (not (eq (car r) 'atom))
		    nil
		  (setq cap-list (cons (intern
					(upcase (buffer-substring
						 (nth 1 r) (nth 2 r))))
				       cap-list)))))
	    (setq response (cdr response))))))
    (if (or cap-list auth-list)
	(list (nreverse cap-list) (nreverse auth-list))
      nil)))

(defun vm-imap-read-greeting (process)
  (let (response)
    (setq response (vm-imap-read-response process))
    (cond ((vm-imap-response-matches response '* 'OK)
	   t )
	  ((vm-imap-response-matches response '* 'PREAUTH)
	   'preauth )
	  (t nil))))

(defun vm-imap-read-ok-response (process)
  (let (response retval (done nil))
    (while (not done)
      (setq response (vm-imap-read-response process))
      (cond ((vm-imap-response-matches response '*)
	     nil )
	    ((vm-imap-response-matches response 'VM 'OK)
	     (setq retval t done t))
	    (t (setq retval nil done t))))
    retval ))

(defun vm-imap-cleanup-region (start end)
  (setq end (vm-marker end))
  (save-excursion
    (goto-char start)
    ;; CRLF -> LF
    (while (and (< (point) end) (search-forward "\r\n"  end t))
      (replace-match "\n" t t)))
  (set-marker end nil))

(defun vm-imapdrop-sans-password (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":"
	    (nth 3 source-list) ":"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":*")))

(defun vm-imapdrop-sans-password-and-mailbox (source)
  (let (source-list)
    (setq source-list (vm-parse source "\\([^:]+\\):?"))
    (concat (nth 0 source-list) ":"
	    (nth 1 source-list) ":"
	    (nth 2 source-list) ":*:"
	    (nth 4 source-list) ":"
	    (nth 5 source-list) ":*")))

(defun vm-imap-read-response (process)
  (let ((list nil) tail obj)
    (goto-char vm-imap-read-point)
    (while (not (eq (car (setq obj (vm-imap-read-object process)))
		    'end-of-line))
      (if (null list)
	  (setq list (cons obj nil)
		tail list)
	(setcdr tail (cons obj nil))
	(setq tail (cdr tail))))
    list ))

(defun vm-imap-read-object (process &optional skip-eol)
  (let ((done nil)
	opoint
	(token nil))
    (while (not done)
      (skip-chars-forward " \t")
      (cond ((< (- (point-max) (point)) 2)
	     (setq opoint (point))
	     (vm-imap-check-connection process)
	     (accept-process-output process)
	     (goto-char opoint))
	    ((looking-at "\r\n")
	     (forward-char 2)
	     (setq token '(end-of-line) done (not skip-eol)))
	    ((looking-at "\\[")
	     (forward-char 1)
	     (let* ((list (list 'vector))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-bracket))
		 (if (eq (car obj) 'close-paren)
		     (vm-imap-protocol-error "unexpected )"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at "\\]")
	     (forward-char 1)
	     (setq token '(close-bracket) done t))
	    ((looking-at "(")
	     (forward-char 1)
	     (let* ((list (list 'list))
		    (tail list)
		    obj)
	       (while (not (eq (car (setq obj (vm-imap-read-object process t)))
			       'close-paren))
		 (if (eq (car obj) 'close-bracket)
		     (vm-imap-protocol-error "unexpected ]"))
		 (setcdr tail (cons obj nil))
		 (setq tail (cdr tail)))
	       (setq token list done t)))
	    ((looking-at ")")
	     (forward-char 1)
	     (setq token '(close-paren) done t))
	    ((looking-at "{")
	     (forward-char 1)
	     (let (start obj n-octets)
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'atom))
		   (vm-imap-protocol-error "number expected after {"))
	       (setq n-octets (string-to-int
			       (buffer-substring (nth 1 obj)
						 (nth 2 obj))))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'close-brace))
		   (vm-imap-protocol-error "} expected"))
	       (setq obj (vm-imap-read-object process))
	       (if (not (eq (car obj) 'end-of-line))
		   (vm-imap-protocol-error "CRLF expected"))
	       (setq start (point))
	       (while (< (- (point-max) start) n-octets)
		 (vm-imap-check-connection process)
		 (accept-process-output process))
	       (goto-char (+ start n-octets))
	       (setq token (list 'string start (point))
		     done t)))
	    ((looking-at "}")
	     (forward-char 1)
	     (setq token '(close-brace) done t))
	    ((looking-at "\042") ;; double quote
	     (forward-char 1)
	     (let ((start (point))
		   (curpoint (point)))
	       (while (not done)
		 (skip-chars-forward "^\042")
		 (setq curpoint (point))
		 (if (looking-at "\042")
		     (progn
		       (setq done t)
		       (forward-char 1))
		   (vm-imap-check-connection process)
		   (accept-process-output process)
		   (goto-char curpoint))
	       (setq token (list 'string start curpoint)))))
	    ;; should be (looking-at "[\000-\040\177-\377]")
	    ;; but Microsoft Exchange emits 8-bit chars.
	    ((looking-at "[\000-\040\177]")
	     (vm-imap-protocol-error "unexpected char (%d)"
				     (char-after (point))))
	    (t
	     (let ((start (point))
		   (curpoint (point))
		   ;; We should be considering 8-bit chars as
		   ;; non-word chars also but Microsoft Exchange
		   ;; uses them, despite the RFC 2060 prohibition.
		   ;; If we ever resume disallowing 8-bit chars,
		   ;; remember to write the range as \177-\376 ...
		   ;; \376 instead of \377 because Emacs 19.34 has
		   ;; a bug in the fastmap initialization code
		   ;; that causes it to infloop.
		   (not-word-chars "^\000-\040\177()[]{}")
		   (not-word-regexp "[][\000-\040\177(){}]"))
	       (while (not done)
		 (skip-chars-forward not-word-chars)
		 (setq curpoint (point))
		 (if (looking-at not-word-regexp)
		     (setq done t)
		   (vm-imap-check-connection process)
		   (accept-process-output process)
		   (goto-char curpoint))
		 (setq token (list 'atom start curpoint)))))))
    (setq vm-imap-read-point (point))
    token ))

(defun vm-imap-response-matches (response &rest expr)
  (let ((case-fold-search t) e r)
    (catch 'done
      (while (and expr response)
	(setq e (car expr)
	      r (car response))
	(cond ((stringp e)
	       (if (or (not (eq (car r) 'string))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward e (nth 2 r) t) (nth 2 r)))))
		   (throw 'done nil)))
	      ((numberp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (int-to-string e)
						  (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil)))
	      ((consp e)
	       (if (not (eq (car e) (car r)))
		   (throw 'done nil))
	       (apply 'vm-imap-response-matches (cdr r) (cdr e)))
	      ((eq e 'atom)
	       (if (not (eq (car r) 'atom))
		   (throw 'done nil)))
	      ((eq e 'vector)
	       (if (not (eq (car r) 'vector))
		   (throw 'done nil)))
	      ((eq e 'list)
	       (if (not (eq (car r) 'list))
		   (throw 'done nil)))
	      ((eq e 'string)
	       (if (not (eq (car r) 'string))
		   (throw 'done nil)))
	      ;; this must to come after all the comparisons for
	      ;; specific symbols.
	      ((symbolp e)
	       (if (or (not (eq (car r) 'atom))
		       (save-excursion
			 (goto-char (nth 1 r))
			 (not (eq (search-forward (symbol-name e) (nth 2 r) t)
				  (nth 2 r)))))
		   (throw 'done nil))))
	(setq response (cdr response)
	      expr (cdr expr)))
      t )))

(defun vm-imap-bail-if-server-says-farewell (response)
  (if (vm-imap-response-matches response '* 'BYE)
      (throw 'end-of-session t)))

(defun vm-imap-protocol-error (&rest args)
  (set (make-local-variable 'vm-imap-keep-trace-buffer) t)
  (signal 'vm-imap-protocol-error (list (apply 'format args))))

(defun vm-imap-scan-list-for-flag (list flag)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward flag (nth 2 e) t) (nth 2 e))
	      (throw 'done t)))
	(setq list (cdr list)))
      nil )))

;; like Lisp get but for IMAP property lists like those returned by FETCH.
(defun vm-imap-plist-get (list name)
  (setq list (cdr list))
  (let ((case-fold-search t) e)
    (catch 'done
      (while list
	(setq e (car list))
	(if (not (eq (car e) 'atom))
	    nil
	  (goto-char (nth 1 e))
	  (if (eq (search-forward name (nth 2 e) t) (nth 2 e))
	      (throw 'done (car (cdr list)))))
	(setq list (cdr (cdr list))))
      nil )))

(defun vm-imap-clear-invalid-retrieval-entries (source-nopwd retrieved
						uid-validity)
  (let ((x retrieved)
	(prev nil))
    (while x
      (if (and (equal source-nopwd (nth 2 (car x)))
	       (not (equal (nth 1 (car x)) uid-validity)))
	  (if prev
	      (setcdr prev (cdr x))
	    (setq retrieved (cdr retrieved))))
      (setq x (cdr x)))
    retrieved ))

(defun vm-imap-quote-string (string)
  (vm-with-string-as-temp-buffer string 'vm-imap-quote-buffer))

(defun vm-imap-quote-buffer ()
  (goto-char (point-min))
  (insert "\"")
  (while (re-search-forward "[\"\\]" nil t)
    (forward-char -1)
    (insert "\\")
    (forward-char 1))
  (goto-char (point-max))
  (insert "\""))

(defun vm-establish-new-folder-imap-session (&optional interactive)
  (let ((process (vm-folder-imap-process))
	mailbox select mailbox-count uid-validity permanent-flags
	read-write can-delete body-peek
	(vm-imap-ok-to-ask interactive))
    (if (processp process)
	(vm-imap-end-session process))
    (setq process (vm-imap-make-session (vm-folder-imap-maildrop-spec)))
    (vm-set-folder-imap-process process)
    (setq mailbox (vm-imap-parse-spec-to-list (vm-folder-imap-maildrop-spec))
	  mailbox (nth 3 mailbox))
    (save-excursion
      (set-buffer (process-buffer process))
      (setq select (vm-imap-select-mailbox process mailbox))
      (setq mailbox-count (nth 0 select)
	    uid-validity (nth 1 select)
	    read-write (nth 2 select)
	    can-delete (nth 3 select)
	    permanent-flags (nth 4 select)
	    body-peek (vm-imap-capability 'IMAP4REV1)))
    (vm-set-folder-imap-uid-validity uid-validity)
    (vm-set-folder-imap-mailbox-count mailbox-count)
    (vm-set-folder-imap-read-write read-write)
    (vm-set-folder-imap-can-delete can-delete)
    (vm-set-folder-imap-body-peek body-peek)
    (vm-set-folder-imap-permanent-flags permanent-flags)
    process ))

(defun vm-imap-get-uid-data ()
  (if (eq 0 (vm-folder-imap-mailbox-count))
      (make-vector 67 0)
    (let ((there (make-vector 67 0))
	  (process (vm-folder-imap-process))
	  (mailbox-count (vm-folder-imap-mailbox-count))
	  list)
      (save-excursion
	(set-buffer (process-buffer process))
	(setq list (vm-imap-get-uid-list process 1 mailbox-count))
	(while list
	  (set (intern (cdr (car list)) there) (car (car list)))
	  (setq list (cdr list)))
	there ))))

(defun vm-imap-get-message-flags (process m &optional norecord)
  (let (need-ok p r flag response saw-seen)
    (save-excursion
      (set-buffer (process-buffer process))
      (vm-imap-send-command process
			    (format "UID FETCH %s (FLAGS)"
				    (vm-imap-uid-of m)))
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to UID FETCH (FLAGS)"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to UID FETCH (FLAGS)"))
	(if (vm-imap-response-matches response '* 'BYE)
	    (vm-imap-protocol-error "server said BYE to UID FETCH (FLAGS)"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'atom 'FETCH 'list)
	       (setq r (nthcdr 3 response)
		     r (car r)
		     r (vm-imap-plist-get r "FLAGS")
		     r (cdr r))
	       (while r
		 (setq p (car r))
		 (if (not (eq (car p) 'atom))
		     nil
		   (setq flag (downcase (buffer-substring (nth 1 p) (nth 2 p))))
		   (cond ((string= flag "\\answered")
			  (vm-set-replied-flag m t norecord))
			 ((string= flag "\\deleted")
			  (vm-set-deleted-flag m t norecord))
			 ((string= flag "\\seen")
			  (vm-set-unread-flag m nil norecord)
			  (vm-set-new-flag m nil norecord)
			  (setq saw-seen t))
			 ((string= flag "\\recent")
			  (vm-set-new-flag m t norecord))))
		 (setq r (cdr r)))
	       (if (not saw-seen)
		   (vm-set-unread-flag m t norecord))))))))

(defun vm-imap-store-message-flags (process m perm-flags)
  (let (need-ok flags response)
    (save-excursion
      (set-buffer (process-buffer process))
      (if (and (vm-replied-flag m)
	       (vm-imap-scan-list-for-flag perm-flags "\\Answered"))
	  (setq flags (cons (intern "\\Answered") flags)))
      (if (and (not (vm-unread-flag m))
	       (vm-imap-scan-list-for-flag perm-flags "\\Seen"))
	  (setq flags (cons (intern "\\Seen") flags)))
      (if (and (vm-deleted-flag m)
	       (vm-imap-scan-list-for-flag perm-flags "\\Deleted"))
	  (setq flags (cons (intern "\\Deleted") flags)))
      (vm-imap-send-command process
			    (format "UID STORE %s FLAGS %s"
				    (vm-imap-uid-of m)
				    (if flags flags "()")))
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to UID FETCH (FLAGS)"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to UID FETCH (FLAGS)"))
	(if (vm-imap-response-matches response '* 'BYE)
	    (vm-imap-protocol-error "server said BYE to UID FETCH (FLAGS)"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))))
      (vm-set-attribute-modflag-of m nil))))

(defun vm-imap-save-message (process m mailbox)
  (let (need-ok need-plus flags response string)
    ;; save the message's flag along with it.
    ;; don't save the deleted flag.
    (if (vm-replied-flag m)
	(setq flags (cons (intern "\\Answered") flags)))
    (if (not (vm-unread-flag m))
	(setq flags (cons (intern "\\Seen") flags)))
    (save-excursion
      (set-buffer (vm-buffer-of (vm-real-message-of m)))
      (save-restriction
	(widen)
	(setq string (buffer-substring (vm-headers-of m) (vm-text-end-of m)))))
    (save-excursion
      (set-buffer (process-buffer process))
      (condition-case nil
	  (vm-imap-create-mailbox process mailbox)
	(error nil))
      (vm-imap-send-command process
			    (format "APPEND %s %s {%d}"
				    (vm-imap-quote-string mailbox)
				    (if flags flags "()")
				    (length string)))
      (setq need-plus t)
      (while need-plus
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to APPEND command"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to APPEND command"))
	(if (vm-imap-response-matches response '* 'BYE)
	    (vm-imap-protocol-error "server said BYE to APPEND command"))
	(cond ((vm-imap-response-matches response '+)
	       (setq need-plus nil))))
      (vm-imap-send-command process string nil t)
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to APPEND data"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to APPEND data"))
	(if (vm-imap-response-matches response '* 'BYE)
	    (vm-imap-protocol-error "server said BYE to APPEND data"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil)))))))

(defun vm-imap-get-synchronization-data ()
  (let ((here (make-vector 67 0))
	(there (vm-imap-get-uid-data))
	(process (vm-folder-imap-process))
	(uid-validity (vm-folder-imap-uid-validity))
	retrieve-list expunge-list
	mp)
    (setq mp vm-message-list)
    (while mp
      (if (or (null (vm-imap-uid-of (car mp)))
	      (not (equal (vm-imap-uid-validity-of (car mp)) uid-validity)))
	  nil
	(set (intern (vm-imap-uid-of (car mp)) here) (car mp))
	(if (not (boundp (intern (vm-imap-uid-of (car mp)) there)))
	    (setq expunge-list (cons (car mp) expunge-list))))
      (setq mp (cdr mp)))
    (mapatoms (function
	       (lambda (sym)
		 (if (and (not (boundp (intern (symbol-name sym) here)))
			  (not (assoc (symbol-name sym)
				      vm-imap-retrieved-messages)))
		     (setq retrieve-list (cons
					  (cons (symbol-name sym)
						(symbol-value sym))
					  retrieve-list)))))
	      there)
    (list retrieve-list expunge-list)))

(defun vm-imap-synchronize-folder (&optional interactive
					     do-remote-expunges
					     do-local-expunges
					     do-retrieves
					     do-attributes)
  (if (and do-retrieves vm-block-new-mail)
      (error "Can't get new mail until you save this folder."))
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-imap-session interactive)))
      nil
    (if do-retrieves
	(vm-assimilate-new-messages))
    (let* ((sync-data (vm-imap-get-synchronization-data))
	   (retrieve-list (car sync-data))
	   (local-expunge-list (nth 1 sync-data))
	   (process (vm-folder-imap-process))
	   (n 1)
	   (statblob nil)
	   (imapdrop (vm-folder-imap-maildrop-spec))
	   (uid-validity (vm-folder-imap-uid-validity))
	   (safe-imapdrop (vm-safe-imapdrop-string imapdrop))
	   (use-body-peek (vm-folder-imap-body-peek))
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
		   (setq statblob (vm-imap-start-status-timer))
		   (vm-set-imap-stat-x-box statblob safe-imapdrop)
		   (vm-set-imap-stat-x-maxmsg statblob
					      (length retrieve-list))
		   (setq r-list retrieve-list)
		   (while r-list
		     (vm-set-imap-stat-x-currmsg statblob n)
		     (setq message-size (vm-imap-get-message-size
					 process (cdr (car r-list))))
		     (vm-set-imap-stat-x-need statblob message-size)
		     (if use-body-peek
			 (progn
			   (vm-imap-send-command process
						 (format
						  "FETCH %s (BODY.PEEK[])"
						  (cdr (car r-list))))
			   (vm-imap-retrieve-to-target process folder-buffer
						       statblob t))
		       (progn
			 (vm-imap-send-command process
					       (format
						"FETCH %s (RFC822.PEEK)"
						(cdr (car r-list))))
			 (vm-imap-retrieve-to-target process folder-buffer
						     statblob nil)))
		     (setq r-list (cdr r-list)
			   n (1+ n))))
	       (error
		(message "Retrieval from %s signaled: %s" safe-imapdrop
			 error-data))
	       (quit
		(message "Quit received during retrieval from %s"
			 safe-imapdrop)))
	     (and statblob (vm-imap-stop-status-timer statblob))
	     ;; to make the "Mail" indicator go away
	     (setq vm-spooled-mail-waiting nil)
	     (intern (buffer-name) vm-buffers-needing-display-update)
	     (vm-increment vm-modification-counter)
	     (vm-update-summary-and-mode-line)
	     (setq mp (vm-assimilate-new-messages t))
	     (setq got-some mp)
	     (setq r-list retrieve-list)
	     (while mp
	       (vm-set-imap-uid-of (car mp) (car (car r-list)))
	       (vm-set-imap-uid-validity-of (car mp) uid-validity)
	       (condition-case nil
		   (vm-imap-get-message-flags process (car mp) t)
		 (error nil))
	       (vm-set-stuff-flag-of (car mp) t)
	       (setq mp (cdr mp)
		     r-list (cdr r-list))))))
      (if do-attributes
	  (let ((mp vm-message-list)
		(perm-flags (vm-folder-imap-permanent-flags)))
	    (while mp
	      (if (not (vm-attribute-modflag-of (car mp)))
		  nil
		(condition-case nil
		    (vm-imap-store-message-flags process (car mp) perm-flags)
		  (error nil)))
	      (setq mp (cdr mp)))))
      (if do-local-expunges
	  (vm-expunge-folder t t local-expunge-list))
      (if (and do-remote-expunges
	       vm-imap-messages-to-expunge)
	  (let ((process (vm-folder-imap-process)))
	    (if (and (processp process)
		     (memq (process-status process) '(open run)))
		(vm-imap-end-session process))
	    (setq vm-imap-retrieved-messages
		  (mapcar (function (lambda (x) (list (car x) (cdr x)
						      imapdrop 'uid)))
			  vm-imap-messages-to-expunge))
	    (vm-expunge-imap-messages)
	    (setq vm-imap-messages-to-expunge
		  (mapcar (function (lambda (x) (cons (car x) (car (cdr x)))))
			  vm-imap-retrieved-messages))))
      got-some)))

(defun vm-imap-folder-check-for-mail (&optional interactive)
  (if (or vm-global-block-new-mail
	  (null (vm-establish-new-folder-imap-session interactive)))
      nil
    (let ((result (car (vm-imap-get-synchronization-data))))
      (vm-imap-end-session (vm-folder-imap-process))
      result )))

(defun vm-imap-find-spec-for-buffer (buffer)
  (let ((list vm-imap-server-list)
	(done nil))
    (while (and (not done) list)
      (if (eq buffer (vm-get-file-buffer (vm-imap-make-filename-for-spec
					  (car list))))
	  (setq done t)
	(setq list (cdr list))))
    (and list (car list))))

(defun vm-imap-make-filename-for-spec (spec)
  (let (md5 list)
    (setq spec (vm-imap-normalize-spec spec))
    (setq md5 (vm-md5-string spec))
    (expand-file-name (concat "imap-cache-" md5)
		      (or vm-imap-folder-cache-directory
			  vm-folder-directory
			  (getenv "HOME")))))

(defun vm-imap-normalize-spec (spec)
  (let (list)
    (setq list (vm-imap-parse-spec-to-list spec))
    (setcar (vm-last list) "*")
    (setcar list "imap")
    (setcar (nthcdr 2 list) "*")
    (setcar (nthcdr 4 list) "*")
    (setq spec (mapconcat (function identity) list ":"))
    spec ))

(defun vm-imap-parse-spec-to-list (spec)
  (vm-parse spec "\\([^:]+\\):?" 1 6))

(defun vm-imap-spec-list-to-host-alist (spec-list)
  (let (host-alist)
    (while spec-list
      (setq host-alist (cons
			(cons
			 (nth 1 (vm-imap-parse-spec-to-list (car spec-list)))
			 (car spec-list))
			host-alist)
	    spec-list (cdr spec-list)))
    host-alist ))

(defun vm-read-imap-folder-name (prompt spec-list selectable-only)
  "Read an IMAP server and mailbox, return an IMAP mailbox spec."
  (let (host c-list spec process mailbox list
	(vm-imap-ok-to-ask t)
	(host-alist (vm-imap-spec-list-to-host-alist spec-list)))
    (if (null host-alist)
	(error "No known IMAP servers.  Please set vm-imap-server-list."))
    (setq host (if (cdr host-alist)
		   (completing-read "IMAP server: " host-alist nil t)
		 (car (car host-alist)))
	  spec (cdr (assoc host host-alist))
	  process (vm-imap-make-session spec)
	  c-list (and process (vm-imap-mailbox-list process selectable-only)))
    (vm-imap-end-session process)
    ;; evade the XEmacs dialog box.
    (let ((use-dialog-box nil))
      (setq mailbox (vm-read-string prompt c-list)))
    (setq list (vm-imap-parse-spec-to-list spec))
    (setcar (nthcdr 3 list) mailbox)
    (mapconcat 'identity list ":")))

(defun vm-imap-directory-separator (process ref)
  (let ((c-list nil)
	sep p r response need-ok)
    (vm-imap-check-connection process)
    (save-excursion
      (set-buffer (process-buffer process))
      (vm-imap-send-command process (format "LIST %s \"\""
					    (vm-imap-quote-string ref)))
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to LIST"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to LIST"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'LIST 'list 'string)
	       (setq r (nthcdr 3 response)
		     p (car r)
		     sep (buffer-substring (nth 1 p) (nth 2 p))))
	      ((vm-imap-response-matches response '* 'LIST 'list)
	       (vm-imap-protocol-error "unexpedcted LIST response"))))
      sep )))

(defun vm-imap-mailbox-list (process selectable-only)
  (let ((c-list nil)
	p r response need-ok)
    (vm-imap-check-connection process)
    (save-excursion
      (set-buffer (process-buffer process))
      (vm-imap-send-command process "LIST \"\" \"*\"")
      (setq need-ok t)
      (while need-ok
	(setq response (vm-imap-read-response process))
	(if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to LIST"))
	(if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to LIST"))
	(if (vm-imap-response-matches response '* 'BYE)
	    (vm-imap-protocol-error "server said BYE to LIST"))
	(cond ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))
	      ((vm-imap-response-matches response '* 'LIST 'list)
	       (setq r (nthcdr 2 response)
		     p (car r))
	       (if (and selectable-only
			(vm-imap-scan-list-for-flag p "\\Noselect"))
		   nil
		 (setq r (nthcdr 4 response)
		       p (car r))
		 (if (memq (car p) '(atom string))
		     (setq c-list (cons (buffer-substring (nth 1 p) (nth 2 p))
					c-list)))))))
      c-list )))

(defun vm-imap-read-boolean-response (process)
  (let ((need-ok t) retval response)
    (while need-ok
      (vm-imap-check-connection process)
      (setq response (vm-imap-read-response process))
      (cond ((vm-imap-response-matches response 'VM 'OK)
	     (setq need-ok nil retval t))
	    ((vm-imap-response-matches response 'VM 'NO)
	     (setq need-ok nil retval nil))
	    ((vm-imap-response-matches response '* 'BYE)
	     (vm-imap-protocol-error "server said BYE"))
	    ((vm-imap-response-matches response 'VM 'BAD)
	     (vm-imap-protocol-error "server said BAD"))))
    retval ))

(defun vm-imap-create-mailbox (process mailbox
			       &optional dont-create-parent-directories)
  (if (not dont-create-parent-directories)
      (let (dir sep sep-regexp i)
	(setq sep (vm-imap-directory-separator process "")
	      sep-regexp (regexp-quote sep)
	      i 0)
	(while (string-match sep-regexp mailbox i)
	  (setq dir (substring mailbox i (match-end 0)))
	  (vm-imap-create-mailbox process dir t)
	  ;; ignore command result since creating a directory will
	  ;; routinely fail with "File exists".  We'll generate a
	  ;; real error if the final mailbox creation fails.
	  (vm-imap-read-boolean-response process)
	  (setq i (match-end 0)))))
  (vm-imap-send-command process (format "CREATE %s"
					(vm-imap-quote-string mailbox)))
  (if (null (vm-imap-read-boolean-response process))
      (error "IMAP CREATE of %s failed" mailbox)))

(defun vm-imap-delete-mailbox (process mailbox)
  (vm-imap-send-command process (format "DELETE %s"
					(vm-imap-quote-string mailbox)))
  (if (null (vm-imap-read-boolean-response process))
      (error "IMAP DELETE of %s failed" mailbox)))

(defun vm-imap-rename-mailbox (process source dest)
  (vm-imap-send-command process (format "RENAME %s %s"
					(vm-imap-quote-string source)
					(vm-imap-quote-string dest)))
  (if (null (vm-imap-read-boolean-response process))
      (error "IMAP RENAME of %s to %s failed" source dest)))

(defun vm-create-imap-folder (folder)
  "Create a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name "Create IMAP folder: "
				       vm-imap-server-list nil)))))
  (let ((vm-imap-ok-to-ask t)
	process mailbox)
    (save-excursion
      (setq process (vm-imap-make-session folder))
      (if (null process)
	  (error "Couldn't open IMAP session for %s"
		 (vm-safe-imapdrop-string folder)))
      (set-buffer (process-buffer process))
      (setq mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
      (vm-imap-create-mailbox process mailbox)
      (message "Folder %s created" (vm-safe-imapdrop-string folder)))))

(defun vm-delete-imap-folder (folder)
  "Delete a folder on an IMAP server.
First argument FOLDER is read from the minibuffer if called
interactively.  Non-interactive callers must provide an IMAP
maildrop specification for the folder as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name "Delete IMAP folder: "
				       vm-imap-server-list nil)))))
  (let ((vm-imap-ok-to-ask t)
	process mailbox)
    (setq process (vm-imap-make-session folder))
    (if (null process)
	(error "Couldn't open IMAP session for %s"
	       (vm-safe-imapdrop-string folder)))
    (save-excursion
      (set-buffer (process-buffer process))
      (setq mailbox (nth 3 (vm-imap-parse-spec-to-list folder)))
      (vm-imap-delete-mailbox process mailbox)
      (message "Folder %s deleted" (vm-safe-imapdrop-string folder)))))

(defun vm-rename-imap-folder (source dest)
  "Rename a folder on an IMAP server.
Argument SOURCE and DEST are read from the minibuffer if called
interactively.  Non-interactive callers must provide full IMAP
maildrop specifications for SOURCE and DEST as described in the
documentation for `vm-spool-files'."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command)
	   source dest)
       (setq source (vm-read-imap-folder-name "Rename IMAP folder: "
					      vm-imap-server-list t))
       (setq dest (vm-read-imap-folder-name
		   (format "Rename %s to: " (vm-safe-imapdrop-string source))
		   (list source) nil))
       (list source dest))))
  (let ((vm-imap-ok-to-ask t)
	process mailbox-source mailbox-dest)
    (setq process (vm-imap-make-session source))
    (if (null process)
	(error "Couldn't open IMAP session for %s"
	       (vm-safe-imapdrop-string source)))
    (save-excursion
      (set-buffer (process-buffer process))
      (setq mailbox-source (nth 3 (vm-imap-parse-spec-to-list source)))
      (setq mailbox-dest (nth 3 (vm-imap-parse-spec-to-list dest)))
      (vm-imap-rename-mailbox process mailbox-source mailbox-dest)
      (message "Folder %s renamed to %s" (vm-safe-imapdrop-string source)
	       (vm-safe-imapdrop-string dest)))))

(provide 'vm-imap)

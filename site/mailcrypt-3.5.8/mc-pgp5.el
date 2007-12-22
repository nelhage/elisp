; mc-pgp.el, PGP support for Mailcrypt
;; Copyright (C) 1998  Len Budney <lbudney@pobox.com>

;;{{{ Licensing
;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;}}}
(require 'mailcrypt)
(require 'expect)

(defvar mc-pgp50-user-id (user-login-name)
  "*PGP ID of your default identity.")
(defvar mc-pgp50-pgpe-path "pgpe" "*The PGP 5.0 'pgpe' executable.")
(defvar mc-pgp50-pgps-path "pgps" "*The PGP 5.0 'pgps' executable.")
(defvar mc-pgp50-pgpv-path "pgpv" "*The PGP 5.0 'pgpv' executable.")
(defvar mc-pgp50-pgpk-path "pgpk" "*The PGP 5.0 'pgpk' executable.")
(defvar mc-pgp50-display-snarf-output nil
  "*If t, pop up the PGP output window when snarfing keys.")
(defvar mc-pgp50-always-fetch nil
  "*If t, always fetch missing keys. If 'never, never fetch. If nil,
ask the user.")
(defvar mc-pgp50-alternate-keyring nil
  "*Public keyring to use instead of default.")
(defvar mc-pgp50-comment
  (format "Processed by Mailcrypt %s, an Emacs/PGP interface" mc-version)
  "*Comment field to appear in ASCII armor output.  If nil, let PGP
use its default.")

(defconst mc-pgp50-msg-begin-line "^-----BEGIN PGP MESSAGE-----\r?$"
  "Text for start of PGP message delimiter.")
(defconst mc-pgp50-msg-end-line "^-----END PGP MESSAGE-----\r?$"
  "Text for end of PGP message delimiter.")
(defconst mc-pgp50-signed-begin-line "^-----BEGIN PGP SIGNED MESSAGE-----\r?$"
  "Text for start of PGP signed messages.")
(defconst mc-pgp50-signed-end-line "^-----END PGP SIGNATURE-----\r?$"
  "Text for end of PGP signed messages.")
(defconst mc-pgp50-key-begin-line "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for start of PGP public key.")
(defconst mc-pgp50-key-end-line "^-----END PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for end of PGP public key.")
(defconst mc-pgp50-error-re "^\\(ERROR:\\|WARNING:\\).*"
  "Regular expression matching an error from PGP")
(defconst mc-pgp50-sigok-re "^.*Good signature.*"
  "Regular expression matching a PGP signature validation message")
(defconst mc-pgp50-badsig-re "^BAD signature.*"
  "Regular expression matching a PGP signature validation message")
(defconst mc-pgp50-newkey-re "^\\([0-9]+\\) +matching keys? found"
  "Regular expression matching a PGP key snarf message")
(defconst mc-pgp50-nokey-re
  "No encryption keys found for: \\(.+\\)$"
  "Regular expression matching a PGP missing-key messsage")
(defconst mc-pgp50-key-expected-re
  "Signature by unknown keyid: \\(.+\\)$")

(defvar mc-pgp50-key-cache nil
  "Association list mapping PGP IDs to canonical \"keys\".  A \"key\"
is a pair (USER-ID . KEY-ID) which identifies the canonical IDs of the
PGP ID.")

;; Thanks to Brian Warner.
(defun mc-pgp50-lookup-key (str)
  ;; Look up the string STR in the user's key ring, expecting to find
  ;; a private key.  Returns a pair of strings (USER-ID . KEY-ID)
  ;; which uniquely identifies the matching key, or nil if no key
  ;; matches.  Example:
  ;; warner@foo -> ("Brian Warner <warner@foo.lothar.com>" "0x12345678")
  (if (equal str "***** CONVENTIONAL *****") nil
    (let ((result (cdr-safe (assoc str mc-pgp50-key-cache)))
	  (keyid-re "^sec\\S *\\s +\\w+\\s +\\(\\w+\\)\\s +")
	  (userid-re "^uid\\s +\\(.*\\)$")
	  (revoke-re "REVOKED")
	  (obuf (current-buffer))
	  buffer keyid key-start key-end)
      (if (null result)
	  (unwind-protect
	      (progn
		(setq buffer (generate-new-buffer " *mailcrypt temp"))
		(call-process mc-pgp50-pgpk-path nil buffer nil
			      "+language=en" "-l" str)
		(set-buffer buffer)
		(goto-char (point-min))

		(while 
		    (and (null result)
			 ;; first find a keyid line for a secret key
			 (re-search-forward keyid-re nil t))
		  ;; groovy. Remember the keyid and then search
		  ;; forward for the userid
		  (progn
		    (setq keyid (buffer-substring-no-properties
				 (match-beginning 1) (match-end 1)))
		    (setq key-start (match-beginning 1))
		    (if (re-search-forward userid-re nil t)
			(progn
			  (save-restriction
			    (setq result 
				  (cons (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))
					keyid))
			    (setq key-end (match-end 1))
			    (narrow-to-region key-start key-end)
			    (goto-char (point-min))
			    (if (re-search-forward revoke-re nil t)
				(setq result nil)
				(setq mc-pgp50-key-cache 
				      (cons (cons str result)
					    mc-pgp50-key-cache)))
			    ))))))
	(if buffer (kill-buffer buffer))
	(set-buffer obuf)))
      (if (null result) nil )             ; We don't mind a missing "secring"
      result)))

;; Thanks to Brian Warner
(defun mc-pgp50-generic-parser (proc oldbuf start end newbuf passwd)
  (let (result)
    (process-send-region proc start end)
    (set-buffer newbuf)
    (process-send-eof proc)
    (while (eq 'run (process-status proc))
      (accept-process-output proc 5))
    (goto-char (point-min))
    (setq result (process-exit-status proc))
    (cond ((not (eq result 0))
	   (prog1
	       nil
	     (if (mc-message 
		  "^Need a pass phrase to decrypt private key:$" 
		  (current-buffer))
		 (mc-deactivate-passwd t)
	       (mc-message mc-pgp50-error-re (current-buffer)
			   (format "PGP exited with status %d" result)))))
	  ((re-search-forward mc-pgp50-nokey-re nil t)
	   nil)
	  (t
	   (and
	    (goto-char (point-min))
	    (re-search-forward "-----BEGIN PGP.*-----$" nil t)
	    (setq start (match-beginning 0))
	    (goto-char (point-max))
	    (re-search-backward "^-----END PGP.*-----\n" nil t)
	    (setq end (match-end 0))
	    (set-buffer oldbuf)
	    (list t (cons start end)))))))

(defun mc-pgp50-process-region 
  (beg end passwd program args parser &optional buffer)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name "/bin/sh")
	mybuf result rgn proc results)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process-shell-command "*PGP*" mybuf program 
		       "2>&1" args))

	  ;; Now hand the process to the parser, which returns the exit
	  ;; status of the dead process and the limits of the region
	  ;; containing the PGP results.
	  (setq results (funcall parser proc obuf beg end mybuf passwd))
	  (setq result  (car results))
	  (setq rgn     (cadr results))

	  ;; Hack to force a status_notify() in Emacs 19.29
	  (set-buffer mybuf)

	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (mc-message result))

	    ;; If the parser found something, migrate it to the old
	    ;; buffer.  In particular, the parser's job is to return
	    ;; a cons of the form ( beg . end ) delimited the result
	    ;; of PGP in the new buffer.
	  (if (consp rgn)
	      (progn
		(set-buffer obuf)
		(delete-region beg end)
		(goto-char beg)
		(insert-buffer-substring mybuf (car rgn) (cdr rgn))
		(set-buffer mybuf)
		(delete-region (car rgn) (cdr rgn))))

	  ;; Return nil on failure and exit code on success
	  (if rgn result nil))

      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf))
      rgn)))

;;; }

(defun mc-pgp50-encrypt-parser (proc oldbuf start end newbuf passwd)
  (let (results result rgn)
    (set-buffer newbuf)
    (goto-char (point-max))
    (setq results nil)
    (progn
      (unwind-protect
	  (with-expect proc
	    (message "Encrypting message...")
	    (set-buffer oldbuf)
	    (if passwd
		(progn
		  (process-send-string proc (concat passwd "\n"))
		  (or mc-passwd-timeout (mc-deactivate-passwd t))))
	    (process-send-region proc start end)
	    (set-buffer newbuf)
	    (process-send-eof proc)
	    
	    ;; Test output of the program, looking for
	    ;; errors.
	    (expect-cond
	     
	     ;; OPTION 1:  Great!  The data is now encrypted!
	     ("-----END PGP MESSAGE-----"
	      
	      ;; Catch the exit status.
	      (setq result (process-exit-status proc))
	      (delete-process proc)
	      (message "Encryption complete.")
	      
	      ;; Delete everything preceding the signed data.
	      (goto-char (point-max))
	      (re-search-backward 
	       "-----BEGIN PGP MESSAGE-----" nil t)
	      (delete-region (point-min) (match-beginning 0))
	      (setq rgn (point-min))
	      
	      ;; Convert out CR/NL -> NL
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t)
		(replace-match "\n"))
	      
	      ;; Delete everything after the signature.
	      (goto-char (point-min))
	      (re-search-forward
	       "-----END PGP MESSAGE-----\n" nil t)
	      (delete-region (match-end 0) (point-max))
	      
	      ;; Return the exit status, with the region
	      ;; limits!
	      (setq rgn (cons rgn (point-max)))
	      (setq results (list result rgn)))
	     
	     ;; OPTION 2:  The passphrase is no good.
	     ("Enter pass phrase:" 
	      (interrupt-process proc)
	      (delete-process proc)
	      (mc-deactivate-passwd t)
	      (setq results '("Incorrect passphrase." nil)))
	     
	     ;; OPTION 3:  There are keys missing.  Just bug out 
	     ;; of the whole thing, for now.
	     ("\nNo encryption keys found for:"
	      (interrupt-process proc)
	      (delete-process proc)
	      (setq results '("One or more public keys are missing" nil)))
	     
	     ;; OPTION 3a:  There are bad keys in the keyring.  This is
	     ;; an odd variant of 3, for example when using a key from
	     ;; the "Cyber-Knights Termplar," which generates 8K keys.
	     ("\nNo valid keys found"
	      (interrupt-process proc)
	      (delete-process proc)
	      (setq results '("One or more public keys are missing" nil)))
	     
	     ;; OPTION 4:  No recipients specified.
	     ("You must specify at least one recipient"
	      (interrupt-process proc)
	      (delete-process proc)
	      (mc-deactivate-passwd t)
	      (setq results '("No recipients specified" nil)))
	     
	     ;; OPTION 5:  The program exits.
	     (exit
	      (setq results (list 
			     (process-exit-status proc) nil)))))
	(set-buffer oldbuf))
      results)))

(defun mc-pgp50-encrypt-region (recipients start end &optional id sign)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	;; Crock.  Rewrite someday.
	(mc-pgp-always-sign mc-pgp-always-sign)
	(obuf (current-buffer))
	action msg args key passwd result pgp-id)
    (setq args (list "+NoBatchInvalidKeys=off" "-fat" "+batchmode=1"))
    (setq action (if recipients "Encrypting" "Armoring"))
    (setq msg (format "%s..." action))  ; May get overridden below
    (if mc-pgp50-comment
	(setq args (cons (format "+comment=\"%s\"" mc-pgp50-comment) args)))
    (if mc-pgp50-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp50-alternate-keyring)))))
    (if 
	(and (not (eq mc-pgp-always-sign 'never))
	     (or 
	      mc-pgp-always-sign 
	      sign 
	      (y-or-n-p "Sign the message? ")))
	     
	(progn
	  (setq mc-pgp-always-sign t)
	  (setq key (mc-pgp50-lookup-key (or id mc-pgp50-user-id)))
	  (if (not key) (error "No key available for signing."))
	  (setq passwd
		(mc-activate-passwd
		 (cdr key)
		 (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
	  (setq args
		(nconc args (list "-s" "-u" (cdr key))))
	  (setenv "PGPPASSFD" "0")
	  (setq msg (format "%s+signing as %s ..." action (car key))))
      (setq mc-pgp-always-sign 'never))

    (or key
	(setq key (mc-pgp50-lookup-key mc-pgp50-user-id)))

    (if (and recipients mc-encrypt-for-me key)
	(setq recipients (cons (cdr key) recipients)))

    (if recipients
	(setq args (append args
			   (apply 'append (mapcar 
					   '(lambda (x) (list 
							 "-r" 
							 (concat "\"" x "\"")))
					   recipients)))))

    (message "pgpe %s" (mapconcat 'identity args " "))
    (message "%s" msg)
    (setq result (mc-pgp50-process-region 
		  start end passwd mc-pgp50-pgpe-path args
		  'mc-pgp50-encrypt-parser buffer))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (re-search-forward mc-pgp50-nokey-re nil t)
	  (progn
	    (if result (error "This should never happen."))
	    (setq pgp-id (buffer-substring-no-properties
			  (match-beginning 1) (match-end 1)))
	    (if (and (not (eq mc-pgp50-always-fetch 'never))
		     (or mc-pgp50-always-fetch
			 (y-or-n-p
			  (format "Key for '%s' not found; try to fetch? "
				  pgp-id))))
		(progn
		  (mc-pgp50-fetch-key (cons pgp-id nil))
		  (set-buffer obuf)
		  (mc-pgp50-encrypt-region recipients start end id))
	      (mc-message mc-pgp50-nokey-re buffer)
	      nil))
	(if (not result)
	    nil
	  (message "%s Done." msg)
	  t)))))

(defun mc-pgp50-decrypt-parser (proc oldbuf start end newbuf passwd)
 (let (rgn result results)
  (setenv "PGPPASSFD" "0")
  (set-buffer newbuf)
  (goto-char (point-max))
  (progn
    (unwind-protect
	(with-expect proc
	  (message "Sending passphrase...")
	  (expect-send (concat passwd "\n"))
	  (or mc-passwd-timeout (mc-deactivate-passwd t))
	  (expect "No files specified.  Using stdin."
	    (message "Passphrase sent.  Decrypting...")
	    (set-buffer oldbuf)
	    (process-send-region proc start end)
	    (set-buffer newbuf)
	    (process-send-eof proc)

	    ;; Test output of the program, looking for
	    ;; errors.
	    (expect-cond

	     ;; OPTION 1:  Here comes the message!
	     ("Opening file \"stdout\" type .*\.\n"

	      ;; Record this point!
	      (setq rgn (point))
	      (message "Decryption complete.")

	      ;; Now wait out the process.
	      (while (eq 'run (process-status proc))
		(accept-process-output proc 5))
	      (setq result (process-exit-status proc))
	      (delete-process proc)

	      ;; Check whether there was a good signature made.
	      (goto-char (point-max))
	      (if
		  (re-search-backward 
		   "\\(Good signature made.*by\\)\\( key:\n\\(\n\\|.\\)*\\)"
		   nil t)
		  (progn
		    (setq rgn (cons rgn (match-beginning 1)))
		    (setq result
			  (concat 
			   (buffer-substring-no-properties
			    (match-beginning 1)
			    (match-end 1))
			   " "
			   (buffer-substring-no-properties
			    (match-beginning 2)
			    (match-end 2)))))

		;; Note that we don't check for a BAD signature.  The
		;; effect is that the "BAD signature" message is
		;; embedded in the message itself.  Is it impolite to
		;; stick stuff into people's messages?  Well, forgery
		;; is impolite, too!
		(setq rgn (cons rgn (point-max))))

	      ;; Return the exit status, with the region
	      ;; limits!
	      (setq results (list result rgn)))
			
	     ;; OPTION 2:  Awww...bad passphrase!
	     ("Enter pass phrase:" 
	      (mc-deactivate-passwd t)
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results 
		    '("Incorrect passphrase or wrong key" nil)))

	     ;; OPTION 3:  Not encrypted to us!
	     ("Cannot decrypt message.  It can only be decrypted by:"
	      (interrupt-process proc)
	      (delete-process proc)

	      ;; Return the bad news.
	      (setq results '("Message corrupt or not encrypted to you" nil)))

	     ;; OPTION 4:  The program exits.
	     (exit
	      (setq results (list 
			     (process-exit-status proc) nil)))))))
    results)))

(defun mc-pgp50-decrypt-region (start end &optional id)
  ;; returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if
  ;; the decryption succeeded and verified is t if there was a valid signature
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	args key new-key passwd result pgp-id)
    (undo-boundary)
    (setq key (mc-pgp50-lookup-key (or id mc-pgp50-user-id)))
    (if (not key) (error "No key available for decrypting."))
    (setq passwd
     (if key
	 (mc-activate-passwd (cdr key)
			     (format "PGP passphrase for %s (%s): "
				     (car key) (cdr key)))
       (mc-activate-passwd id "PGP passphrase for conventional decryption: ")))
    (if passwd
	(setenv "PGPPASSFD" "0"))
    (setq args '("+verbose=1" "+batchmode" "+language=en" "-f"))
    (if mc-pgp50-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp50-alternate-keyring)))))

    (message "pgpv %s" (mapconcat 'identity args " "))
    (message "Decrypting...")
    (setq result
	  (mc-pgp50-process-region
	   start end passwd mc-pgp50-pgpv-path
	   args 'mc-pgp50-decrypt-parser buffer))
    (cond
     (result
      ;; If verification failed due to missing key, offer to fetch it.
      ;; This is unchanged from mailcrypt 3.4, and should still work.
      ;; Execution never reaches this point when using PGP 5.0, however.
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(if (re-search-forward mc-pgp50-key-expected-re nil t)
	    (setq pgp-id (concat "0x" (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 1))))))
      (if (and pgp-id
	       (not (eq mc-pgp50-always-fetch 'never))
	       (or mc-pgp50-always-fetch
		   (y-or-n-p
		    (format "Key %s not found; attempt to fetch? " pgp-id)))
	       (mc-pgp50-fetch-key (cons nil pgp-id)))
	  (progn
	    (undo-start)
	    (undo-more 1)
	    (mc-pgp50-decrypt-region start end id))
	(mc-message mc-pgp50-key-expected-re buffer)
	(cons t (eq result 0))))
     ;; Decryption failed; maybe we need to use a different user-id
     ((save-excursion
	(and
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (re-search-forward
	  "^Key for user ID:.*\n.*Key ID \\([0-9A-F]+\\)" nil t)
	 (setq new-key
	       (mc-pgp50-lookup-key
		(concat "0x" (buffer-substring-no-properties
			      (match-beginning 1)
			      (match-end 1)))))
	 (not (and id (equal key new-key)))))
      (mc-pgp50-decrypt-region start end (cdr new-key)))
     ;; Or maybe it is conventionally encrypted
     ((save-excursion
	(and
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (re-search-forward "^File is conventionally encrypted" nil t)))
      (if (null key) (mc-deactivate-passwd t))
      (mc-pgp50-decrypt-region start end "***** CONVENTIONAL *****"))
     (t
      (mc-display-buffer buffer)
      (if (mc-message "^\aError: +Bad pass phrase\\.$" buffer)
	  (mc-deactivate-passwd t)
	(mc-message mc-pgp50-error-re buffer "Error decrypting buffer"))
      (cons nil nil)))))

(defun mc-pgp50-verify-parser (proc oldbuf start end newbuf passwd)
  (let (results)
    (setenv "PGPPASSFD")		; Delete this to exit batchmode!
    (set-buffer newbuf)
    (goto-char (point-max))
    (progn
      (unwind-protect
	  (with-expect proc
	    (expect "No files specified.  Using stdin."
	      (set-buffer oldbuf)
	      (process-send-region proc start end)
	      (set-buffer newbuf)
	      (process-send-eof proc)

	      ;; Test output of the program, looking for
	      ;; errors.
	      (expect-cond

	       ;; OPTION 1:  Great!  The signature is approved!
	       ("Good signature made"

		;; Let the program finish
		(process-send-eof proc)
		(while (eq 'run (process-status proc))
		  (accept-process-output proc 5))

		;; Read the success message
		(goto-char (point-max))
		(re-search-backward
		 "\\(Good signature made.*by\\)\\( key:\n\\(\n\\|.\\)*\\)"
		 nil t)

		;; Return the good news!
		(setq results
		      (list
		       (concat 
			(buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			" "
			(buffer-substring-no-properties
			 (match-beginning 2)
			 (match-end 2))) t)))

	       ;; OPTION 2:  Shucks!  The signature is invalid!
	       ("BAD signature made.*by key:\n"

		;; Let the program finish
		(process-send-eof proc)
		(while (eq 'run (process-status proc))
		  (accept-process-output proc 5))

		;; Read the warning message
		(goto-char (point-max))
		(re-search-backward
		 "\\(BAD signature made.*by\\).*\n.*\n\[ \t\]*\\(.*\\)\n" 
		 nil t)

		;; Return the bad news
		(setq results
		      (list
		       (concat 
			(buffer-substring-no-properties
			 (match-beginning 1)
			 (match-end 1))
			" "
			(buffer-substring-no-properties
			 (match-beginning 2)
			 (match-end 2))) t)))

	       ;; OPTION 2a:  We don't have the right public key!
	       ("\n.*Signature by unknown keyid: 0x.*\n"

		;; Catch the exit status.
		(delete-process proc)

		;; Return the good news!
		(setq results 
		      '("You don't have this person's public key!" nil)))

	       ;; OPTION 2b:  The message is not signed w/PGP 5.0
	       ("\n.*Opening file ./dev/null. type .*\n"

		;; Catch the exit status.
		(delete-process proc)

		;; Return the good news!
		(setq results 
		      '("Not a PGP 5.0 signed message!" nil)))

	       ;; OPTION 3:  Awww...This isn't clearsigned, it's encrypted!
	       ("Enter pass phrase:" 
		(interrupt-process proc)
		(delete-process proc)

		;; Return the bad news.
		(setq results 
		      '("Decrypt the message; that will verify it" nil)))

	       ;; OPTION 4:  The program exits.
	       (exit
		(setq results (list 
			       (process-exit-status proc) nil)))))))
      results)))

(defun mc-pgp50-sign-parser (proc oldbuf start end newbuf passwd)
  (let (result results rgn)
    (setenv "PGPPASSFD" "0")
    (set-buffer newbuf)
    (goto-char (point-max))
    (progn
      (unwind-protect
	  (with-expect proc
	    (message "Sending passphrase...")
	    (expect-send (concat passwd "\n"))
	    (or mc-passwd-timeout (mc-deactivate-passwd t))
	    (expect "No files specified.  Using stdin."
	      (message "Passphrase sent.  Signing...")
	      (set-buffer oldbuf)
	      (process-send-region proc start end)
	      (set-buffer newbuf)
	      (process-send-eof proc)

	      ;; Test output of the program, looking for
	      ;; errors.
	      (expect-cond

	       ;; OPTION 1:  Great!  The data is now signed!
	       ("-----END PGP SIGNATURE-----"

		;; Catch the exit status.
		(setq result (process-exit-status proc))
		(delete-process proc)
		(message "Signing complete.")

		;; Delete everything preceding the signed data.
		(goto-char (point-max))
		(re-search-backward 
		 "-----BEGIN PGP SIGNED MESSAGE-----" nil t)
		(delete-region (point-min) (match-beginning 0))
		(setq rgn (point-min))

		;; Convert out CR/NL -> NL
		(goto-char (point-min))
		(while (search-forward "\r\n" nil t)
		  (replace-match "\n"))

		;; Delete everything after the signature.
		(goto-char (point-min))
		(re-search-forward
		 "-----END PGP SIGNATURE-----\n" nil t)
		(delete-region (match-end 0) (point-max))
			 
		;; Return the exit status, with the region
		;; limits!
		(setq rgn (cons rgn (point-max)))
		(setq results (list result rgn)))
			

	       ;; OPTION 1.a:  The data is now signed, but is 8bit data.
	       ("-----END PGP MESSAGE-----"

		;; Catch the exit status.
		(setq result (process-exit-status proc))
		(delete-process proc)
		(message "Signing complete.")

		;; Delete everything preceding the signed data.
		(goto-char (point-max))
		(re-search-backward 
		 "-----BEGIN PGP MESSAGE-----" nil t)
		(delete-region (point-min) (match-beginning 0))
		(setq rgn (point-min))

		;; Convert out CR/NL -> NL
		(goto-char (point-min))
		(while (search-forward "\r\n" nil t)
		  (replace-match "\n"))

		;; Delete everything after the signature.
		(goto-char (point-min))
		(re-search-forward
		 "-----END PGP MESSAGE-----\n" nil t)
		(delete-region (match-end 0) (point-max))
			 
		;; Return the exit status, with the region
		;; limits!
		(setq rgn (cons rgn (point-max)))
		(setq results (list result rgn)))
			

	       ;; OPTION 2:  Awww...bad passphrase!
	       ("Enter pass phrase:" 
		(mc-deactivate-passwd t)
		(interrupt-process proc)
		(delete-process proc)

		;; Return the bad news.
		(setq results '("Incorrect passphrase" nil)))

	       ;; OPTION 3:  The program exits.
	       (exit
		(setq results (list 
			       (process-exit-status proc) nil)))))))
      results)))

(defun mc-pgp50-sign-region (start end &optional id unclear)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key)
    (setq key (mc-pgp50-lookup-key (or id mc-pgp50-user-id)))
    (if (not key) (error "No key available for signing."))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (list
	   "-fat" "+verbose=1" "+language=en"
	    (format "+clearsig=%s" (if unclear "off" "on"))
	    "+batchmode" "-u" (cdr key)))
    (if mc-pgp50-comment
	(setq args (cons (format "+comment=\"%s\"" mc-pgp50-comment) args)))

    (message "pgps %s" (mapconcat 'identity args " "))
    (message "Signing as %s ..." (car key))
    (if (mc-pgp50-process-region start end passwd mc-pgp50-pgps-path args
			   'mc-pgp50-sign-parser)
	(progn
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mc-pgp50-verify-region (start end &optional no-fetch)
  (let ((buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args pgp-id)
    (setq args '("+verbose=1" "+batchmode" "+language=en" "-f"))
    (if mc-pgp50-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp50-alternate-keyring)))))

    (message "pgpv %s" (mapconcat 'identity args " "))
    (message "Verifying...")
    (if (mc-pgp50-process-region
	 start end nil mc-pgp50-pgpv-path args 'mc-pgp50-verify-parser buffer)
	t
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(if (and
	     (not no-fetch)
	     (re-search-forward mc-pgp50-key-expected-re nil t)
	     (setq pgp-id
		   (concat "" (buffer-substring-no-properties
				 (match-beginning 1)
				 (match-end 1))))
	     (not (eq mc-pgp50-always-fetch 'never))
	     (or mc-pgp50-always-fetch
		 (y-or-n-p
		  (format "Key %s not found; attempt to fetch? " pgp-id)))
	     (mc-pgp50-fetch-key (cons nil pgp-id))
	     (set-buffer obuf))
	    (mc-pgp50-verify-region start end t)
	  (mc-message mc-pgp50-error-re buffer)
	  nil)))))

(defun mc-pgp50-insert-public-key (&optional id)
  (let ((buffer (get-buffer-create mc-buffer-name))
	args)
    (setq id (or id mc-pgp50-user-id))
    (setq args (list "+verbose=1" "+batchmode" "+language=en" "-x" id))
    (if mc-pgp50-comment
	(setq args (cons (format "+comment=\"%s\"" mc-pgp50-comment) args)))

    (if (mc-pgp50-process-region (point) (point) nil mc-pgp50-pgpk-path
			   args 'mc-pgp50-generic-parser buffer)
	(progn
	  (mc-message "Key for user ID: .*" buffer)
	  t))))

(defun mc-pgp50-snarf-parser (proc oldbuf start end newbuf passwd)
  (let (result)
    ;; "pgpk +batchmode=1 -a file" is confused. Although it doesn't actually
    ;; read the key from stdin, sometimes it will just stare at you for several
    ;; seconds unless you type *something* into stdin. I've observed that it
    ;; absorbs about 680 bits of entropy from /dev/random too, so it might be
    ;; (pointlessly) generating some random data and thinks it should wait
    ;; for some keyboard input to mix into the random pool, but gives up after
    ;; a little while and gets on with the non-random business of importing a
    ;; key. Send an EOF to make it hurry up. Sometimes that helps.
    (process-send-eof proc)
    (while (eq 'run (process-status proc))
      (accept-process-output proc 5))
    (setq result (process-exit-status proc))
    (list (eq result 0) t)))

(defun mc-pgp50-snarf-keys (start end)
  ;; Returns number of keys found.
  (let ((buffer (get-buffer-create mc-buffer-name)) tmpstr args tempfile)
    ;; a known bug in pgp5.0: "pgpk -a" doesn't read from stdin like it should.
    ;; workaround: write key to a tempfile, then feed tempfile to pgpk -a
    (setq tempfile 
	  (make-temp-name (expand-file-name "mailcrypt-pgp50-snarffile-"
					    mc-temp-directory)))
    (write-region start end tempfile)
    (setq args (list "+verbose=1" "+batchmode=1" "+language=en" "-a" tempfile))
    (if mc-pgp50-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp50-alternate-keyring)))))
    (message "Snarfing...")
    (if (mc-pgp50-process-region start end nil mc-pgp50-pgpk-path args
			   'mc-pgp50-snarf-parser buffer)
	(save-excursion
	  (delete-file tempfile)
	  ;; look for "3 matching keys found" in the stderr. We don't actually
	  ;; find out how many are new versus how many are old. Just pretend
	  ;; they're all new.
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (if (re-search-forward mc-pgp50-newkey-re nil t)
	      (progn
		(if mc-pgp50-display-snarf-output (mc-display-buffer buffer))
		(setq tmpstr (buffer-substring-no-properties
			      (match-beginning 1) 
			      (match-end 1)))
		(if (equal tmpstr "No")
		    0
		  (car (read-from-string tmpstr))))))
      (delete-file tempfile)
      (mc-display-buffer buffer)
      (mc-message mc-pgp50-error-re buffer "Error snarfing PGP keys")
      0)))

(defun mc-scheme-pgp50 ()
  (list
   (cons 'encryption-func 		'mc-pgp50-encrypt-region)
   (cons 'decryption-func		'mc-pgp50-decrypt-region)
   (cons 'signing-func			'mc-pgp50-sign-region)
   (cons 'verification-func 		'mc-pgp50-verify-region)
   (cons 'key-insertion-func 		'mc-pgp50-insert-public-key)
   (cons 'snarf-func			'mc-pgp50-snarf-keys)
   (cons 'msg-begin-line 		mc-pgp50-msg-begin-line)
   (cons 'msg-end-line 			mc-pgp50-msg-end-line)
   (cons 'signed-begin-line 		mc-pgp50-signed-begin-line)
   (cons 'signed-end-line 		mc-pgp50-signed-end-line)
   (cons 'key-begin-line 		mc-pgp50-key-begin-line)
   (cons 'key-end-line 			mc-pgp50-key-end-line)
   (cons 'user-id			mc-pgp50-user-id)))

;;{{{ Key fetching

(defvar mc-pgp50-always-fetch nil
  "*If t, always attempt to fetch missing keys, or never fetch if
'never.")

(defvar mc-pgp50-keyserver-url-template
  "/htbin/pks-extract-key.pl?op=get&search=%s"
  "The URL to pass to the keyserver.")

(defvar mc-pgp50-keyserver-address "pgp.ai.mit.edu"
  "Host name of keyserver.")

(defvar mc-pgp50-keyserver-port 80
  "Port on which the keyserver's HTTP daemon lives.")

(defvar mc-pgp50-hkpserver-req-template
  "/pks/lookup?op=get&exact=on&search=%s HTTP/1.0"
  "The request to pass to the HKP keyserver.")

(defvar mc-pgp50-hkpserver-address "horowitz.surfnet.nl"
  "Host name of HKP keyserver.")

(defvar mc-pgp50-hkpserver-port 11371
  "Port on which the HKP keyserver's Horowitz Key Protocol daemon lives.")

(defvar mc-pgp50-fetch-timeout 20
  "*Timeout, in seconds, for any particular key fetch operation.")

(defvar mc-pgp50-fetch-keyring-list nil
  "*List of strings which are filenames of public keyrings to search
when fetching keys.")

(defsubst mc-pgp50-buffer-get-key (buf)
  "Return the first key block in BUF as a string, or nil if none found."
  (save-excursion
    (let (start)
      (set-buffer buf)
      (goto-char (point-min))
      (and (re-search-forward mc-pgp50-key-begin-line nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward mc-pgp50-key-end-line nil t)
	   (buffer-substring-no-properties start (match-end 0))))))

(defun mc-pgp50-fetch-from-keyrings (id)
  (let ((keyring-list mc-pgp50-fetch-keyring-list)
	buf proc key)
    (unwind-protect
	(progn
	  (message "Fetching %s from keyrings..." (or (cdr id) (car id)))
	  (while (and (not key) keyring-list)
	    (setq buf (generate-new-buffer " *mailcrypt temp*"))
	    (setq proc
		  (start-process "*PGP*" buf mc-pgp50-pgpk-path "-kxaf"
				 "+verbose=0" "+batchmode"
				 (format "+pubring=%s" (car keyring-list))
				 (or (cdr id) (car id))))
	    ;; Because PGPPASSFD might be set
	    (process-send-string proc "\r\n")
	    (while (eq 'run (process-status proc))
	      (accept-process-output proc 5))
	    (setq key (mc-pgp50-buffer-get-key buf))
	    (setq keyring-list (cdr keyring-list)))
	  key)
      (if buf (kill-buffer buf))
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc)))))

(defun mc-pgp50-fetch-from-http (id)
  (let (buf connection)
    (unwind-protect
	(progn
	  (message "Fetching %s via HTTP to %s..."
		   (or (cdr id) (car id)) mc-pgp50-keyserver-address)
	  (setq buf (generate-new-buffer " *mailcrypt temp*"))
	  (setq connection
		(open-network-stream 
		 "*key fetch*" 
		 buf 
		 mc-pgp50-keyserver-address
		 mc-pgp50-keyserver-port))
	  (process-send-string
	   connection
	   (concat "GET " (format mc-pgp50-keyserver-url-template
				  (or (cdr id) (car id))) "\r\n"))
	  (while (and (eq 'open (process-status connection))
		      (accept-process-output 
		       connection 
		       mc-pgp50-fetch-timeout)))
	  (mc-pgp50-buffer-get-key buf))
      (if buf (kill-buffer buf))
      (if connection (delete-process connection)))))

(defun mc-pgp50-fetch-from-hkp (id)
  (let (buf connection)
    (unwind-protect
	(progn
	  (message "Fetching %s via Horowitz Key Protocol to %s..."
		   (or (cdr id) (car id)) mc-pgp50-hkpserver-address)
	  (setq buf (generate-new-buffer " *mailcrypt temp*"))
	  (setq connection
		(open-network-stream 
		 "*key fetch*" 
		 buf 
		 mc-pgp50-hkpserver-address
		 mc-pgp50-hkpserver-port))
	  (process-send-string
	   connection
	   (concat "GET " (format mc-pgp50-hkpserver-req-template
				  (or (cdr id) (car id))) "\r\n\r\n"))
	  (while (and (eq 'open (process-status connection))
		      (accept-process-output 
		       connection 
		       mc-pgp50-fetch-timeout)))
	  (mc-pgp50-buffer-get-key buf))
      (if buf (kill-buffer buf))
      (if connection (delete-process connection)))))

(defun mc-pgp50-fetch-from-finger (id)
  (let (buf connection user host)
    (unwind-protect
	(and (car id)
	     (string-match "^\\(.+\\)@\\([^@]+\\)$" (car id))
	     (progn
	       (message "Trying finger %s..." (car id))
	       (setq user (substring (car id)
				     (match-beginning 1) (match-end 1)))
	       (setq host (substring (car id)
				     (match-beginning 2) (match-end 2)))
	       (setq buf (generate-new-buffer " *mailcrypt temp*"))
	       (condition-case nil
		   (progn
		     (setq connection
			   (open-network-stream "*key fetch*" buf host 79))
		     (process-send-string connection
					  (concat "/W " user "\r\n"))
		     (while
			 (and (eq 'open (process-status connection))
			      (accept-process-output connection
						     mc-pgp50-fetch-timeout)))
		     (mc-pgp50-buffer-get-key buf))
		 (error nil))))
      (if buf (kill-buffer buf))
      (if connection (delete-process connection)))))

(defvar mc-pgp50-fetch-methods '(mc-pgp50-fetch-from-keyrings
			       mc-pgp50-fetch-from-hkp
			       mc-pgp50-fetch-from-finger
			       mc-pgp50-fetch-from-http)
  "List of methods to try when attempting to fetch a key.  Each
element is a function to call with an ID as argument.  See the
documentation for the function mc-pgp50-fetch-key for a description of
the ID.")

(defun mc-pgp50-fetch-key (&optional id)
  "Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key."
  (interactive)
  (let ((methods mc-pgp50-fetch-methods)
	(process-connection-type nil) key proc buf args)

    (if (null id)
	(setq id (cons (read-string "Fetch key for: ") nil)))
    (while (and (not key) methods)
      (setq key (funcall (car methods) id))
      (setq methods (cdr methods)))
    (if (not (stringp key))
	(progn
	  (message "Key not found.")
	  nil)
      ;; Maybe I'll do this right someday.
      (unwind-protect
	  (save-window-excursion
	    (setq buf (generate-new-buffer " *PGP Key Info*"))
	    (pop-to-buffer buf)
	    (if (< (window-height) (/ (frame-height) 2))
		(enlarge-window (- (/ (frame-height) 2)
				   (window-height))))
	    (setq args '("-f" "--verbose=0" "--batchmode=1" "--pubring=junk"))
	    (setq proc (apply 'start-process "*PGP*" buf
			      mc-pgp50-pgpv-path args))
	    ;; Because PGPPASSFD might be set
	    (process-send-string proc "\r\n")
	    (process-send-string proc key)
	    (process-send-string proc "\r\n")
	    (process-send-string proc "n\r\n")
	    (process-send-eof proc)
	    (set-buffer buf)
	    (while (eq 'run (process-status proc))
	      (accept-process-output proc 5)
	      (goto-char (point-min)))
	    (if (y-or-n-p "Add this key to keyring? ")
		(progn
		  (setq args '("-f" "--verbose=0" "--batchmode=1"))
		  (if mc-pgp50-alternate-keyring
		      (setq args
			    (append args (list (format 
						"--pubring=%s"
						mc-pgp50-alternate-keyring)))))
		  (setq proc
			(apply 'start-process "*PGP*" buf
			       mc-pgp50-pgpv-path args))
		  ;; Because PGPPASSFD might be set
		  (process-send-string proc "\r\n")
		  (process-send-string proc key)
		  (process-send-string proc "\r\n")
		  (process-send-eof proc)
		  (while (eq 'run (process-status proc))
		    (accept-process-output proc 5))
		  t)))
	(if buf (kill-buffer buf))))))

;;}}}

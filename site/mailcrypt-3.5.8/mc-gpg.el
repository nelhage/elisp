;; mc-gpg.el, GPG support for Mailcrypt
;; Copyright (C) 1995  Jin Choi <jin@atype.com>
;;                     Patrick LoPresti <patl@lcs.mit.edu>
;;               1998  Brian Warner <warner@lothar.com>

;; $Id: mc-gpg.el,v 1.21 2002/09/25 00:12:55 warner Exp $

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

; pieces to do:

; #key lookup?
; #mc-gpg-encrypt-region
;  need to deal with untrusted keys, missing keys (offer to fetch), --throw
; #mc-gpg-decrypt-region [anything not clearsigned] (a,as,ae,ase)
;  need to implement signature-key fetch, ponder --throw-keyid case
;  keys without passphrases, sigs with bad algorithms (ignore sig? warn?)
; #mc-gpg-sign-region (clearsign/notclearsign)
; #mc-gpg-verify-region [clearsigned only] (ok/badsig/missingkey/corruptmsg)
; #mc-gpg-insert-public-key (comment, altkeyring)
; #mc-gpg-snarf-keys (one, multiple, old, corrupt)
; key fetching (is there a GPG key server yet?)
; clean up use of buffers, #kill off old tmp buffers
; in verify-region, print date of signature too
;  ~maybe have bad-signature message print keyid/date? (no, sig is invalid,
;  ~ anything other than its invalidity is misleading)
; make messages shorter (get it all to fit in echo area)

; enhancements I'd like to add
;  trustdb status reporting during encryption/decryption: show the best trust
;   path to the recipient/signer?
;  completion on local id when signing (--list-secret-keys should know them)
;  algorithm preferences, possibly by destination user
;   (this is embedded in gpg)
;  extra options, possibly by destination user. Maybe for pgp5.0/pgp2.6 compat?
;  rfc2015 operation (MIME: application/pgp-signature, etc)
;  signature dates are currently reported with just the date. Find a time
;   formatting function and use the longtime in the VALIDSIG message.

; mc-gpg-alternate-keyring seems dubious.. have two options, public/private?

; using a shell introduces concerns about quoting and such. If the name of a
; key used as a recipient or as a mc-gpg-user-id (a key to sign with) has a
; double quote or ! or weird stuff, things could break.

; encrypting to a nontrusted key is problematic: when not in --batch mode,
; gpg warns the user and asks if they want to use the key anyway. In --batch
; mode, it fails, even if we give --yes. Worse yet, if we encrypt to multiple
; recipients, the untrusted ones get dropped withou flagging an error (stderr
; does get a message, but it doesn't indicate which keys had a problem)

(defvar mc-gpg-user-id (user-login-name)
  "*GPG ID of your default identity.")
(defvar mc-gpg-path "gpg" "*The GPG executable.")
(defvar mc-gpg-display-snarf-output nil
  "*If t, pop up the GPG output window when snarfing keys.")
(defvar mc-gpg-always-fetch 'never
  "*If t, always fetch missing keys. If 'never, never fetch. If nil,
ask the user.")
(defvar mc-gpg-alternate-keyring nil
  "*Public keyring to use instead of default.")
(defvar mc-gpg-comment
   (format "Processed by Mailcrypt %s <http://mailcrypt.sourceforge.net/>"
	   mc-version)
  "*Comment field to appear in ASCII armor output.  If nil, let GPG use its 
default.")
(defconst mc-gpg-msg-begin-line "^-----BEGIN PGP MESSAGE-----\r?$"
  "Text for start of GPG message delimiter.")
(defconst mc-gpg-msg-end-line "^-----END PGP MESSAGE-----\r?$"
  "Text for end of GPG message delimiter.")
(defconst mc-gpg-signed-begin-line "^-----BEGIN PGP SIGNED MESSAGE-----\r?$"
  "Text for start of GPG signed messages.")
(defconst mc-gpg-signed-end-line "^-----END PGP SIGNATURE-----\r?$"
  "Text for end of GPG signed messages.")
(defconst mc-gpg-key-begin-line "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for start of GPG public key.")
(defconst mc-gpg-key-end-line "^-----END PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for end of GPG public key.")
(defvar mc-gpg-extra-args nil
  "Extra arguments to pass to all invocations of gpg. Used during debugging to
set --homedir, to use special test keys instead of the developer's normal
keyring.")
(defvar mc-gpg-debug-buffer nil
  "A buffer for debugging messages. If nil, no debugging messages are logged.
BEWARE! Sensitive data (including your passphrase) is put here. Set this with:
 (setq mc-gpg-debug-buffer (get-buffer-create \"mc debug\"))")

;; we use with-current-buffer for clarity. emacs19 doesn't have it. This
;; code is cribbed from lazy-lock.el which does the same thing
(eval-when-compile
  ;; We use this for clarity and speed.  Borrowed from a future Emacs.
  (or (fboundp 'with-current-buffer)
      (defmacro with-current-buffer (buffer &rest body)
	"Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY."
	(` (save-excursion (set-buffer (, buffer)) (,@ body)))))
  )

(defun mc-gpg-debug-print (string)
  (if (and (boundp 'mc-gpg-debug-buffer) mc-gpg-debug-buffer)
      (print string mc-gpg-debug-buffer)))

;; the insert parser will return '(t) and insert the whole of stdout if 
;; rc == 0, and will return '(nil rc stderr) if rc != 0
(defun mc-gpg-insert-parser (stdoutbuf stderrbuf statusbuf rc parserdata)
  (mc-gpg-debug-print 
   (format "(mc-gpg-generic-parser stdoutbuf=%s stderrbuf=%s rc=%s"
	   stdoutbuf stderrbuf rc))
  (if (= rc 0)
      '(t (t))
    (list nil nil rc (with-current-buffer stderrbuf (buffer-string))))
)

;; the null parser returns rc and never inserts anything
(defun mc-gpg-null-parser (stdoutbuf stderrbuf statusbuf rc parserdata)
  (list nil rc))

; utility function (variant of mc-process-region):
; take region in current buffer, send as stdin to a process
; maybe send in a passphrase first
; three buffers of output are collected: stdout, stderr, and --status-fd
;
; parser is called with stdoutbuf as the current buffer as
;  (parser stdoutbuf stderrbuf statusbuf rc parserdata)
; and is expected to return a list:
;  '(REPLACEP RESULT)
;
; if REPLACEP is true, the original buffer's [beg..end] will be replaced by
; the stdout data buffer's contents (all of it). Otherwise the original buffer
; is left alone. RESULT (specifically (cdr parser-return-value)) is returned
; by mc-gpg-process-region.

(defun mc-gpg-process-region (beg end passwd program args parser bufferdummy
				  &optional parserdata)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name "/bin/sh") ;; ??? force? need sh (not tcsh) for "2>"
	; other local vars
	mybuf 
	stderr-tempfilename stderr-buf
	status-tempfilename status-buf
	proc rc status parser-result
	)
    (mc-gpg-debug-print (format 
       "(mc-gpg-process-region beg=%s end=%s passwd=%s program=%s args=%s parser=%s bufferdummy=%s)"
       beg end passwd program args parser bufferdummy))
    (setq stderr-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-stderr-"
					    mc-temp-directory)))
    (setq status-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-status-"
					    mc-temp-directory)))
    (unwind-protect
	(progn
	  ;; get output places ready
	  (setq mybuf (get-buffer-create " *mailcrypt stdout temp"))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)

	  (if passwd
	      (setq args (append '("--passphrase-fd" "0") args)))
	  (setq args (append (list (concat "2>" stderr-tempfilename)) args))
	  (setq args (append (list (concat "3>" status-tempfilename)) args))
	  (setq args (append '("--status-fd" "3") args))

	  (if mc-gpg-extra-args
	      (setq args (append mc-gpg-extra-args args)))

	  (mc-gpg-debug-print (format "prog is %s, args are %s" 
				      program 
				      (mapconcat '(lambda (x) 
						    (format "'%s'" x)) 
						 args " ")))

	  (setq proc
		(apply 'start-process-shell-command "*GPG*" mybuf 
		       program args))
	  ;; send in passwd if necessary
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  ;; send in the region
	  (process-send-region proc beg end)
	  ;; finish it off
	  (process-send-eof proc)
	  ;; wait for it to finish
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  ;; remember result codes
	  (setq status (process-status proc))
	  (setq rc (process-exit-status proc))
	  (mc-gpg-debug-print (format "prog finished, rc=%s" rc))

	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)

	  ;; remove the annoying "yes your process has finished" message
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*GPG.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))

	  ;; ponder process death: signal, not just rc!=0
	  (if (or (eq 'stop status) (eq 'signal status))
	      ;; process died
	      (error "%s exited abnormally: '%s'" program rc) ;;is rc a string?
	    )

	  (if (= 127 rc)
	      (error "%s could not be found" program) ;; at least on my system
	    )

	  ;; fill stderr buf
	  (setq stderr-buf (get-buffer-create " *mailcrypt stderr temp"))
	  (buffer-disable-undo stderr-buf)
	  (set-buffer stderr-buf)
	  (erase-buffer)
	  (insert-file-contents stderr-tempfilename)

	  ;; fill status buf
	  (setq status-buf (get-buffer-create " *mailcrypt status temp"))
	  (buffer-disable-undo status-buf)
	  (set-buffer status-buf)
	  (erase-buffer)
	  (insert-file-contents status-tempfilename)

	  ;; feed the parser
	  (set-buffer mybuf)
	  (setq parser-result (funcall parser 
				       mybuf stderr-buf status-buf 
				       rc parserdata))
	  (mc-gpg-debug-print (format " parser returned %s" parser-result))

	  ;; what did the parser tell us?
	  (if (car parser-result)
	      ;; yes, replace region
	      (progn
		(set-buffer obuf)
		(delete-region beg end)
		(goto-char beg)
		(insert-buffer-substring mybuf)
		))

	  ;; return result
	  (cdr parser-result)
	  )
      ;; cleanup forms
      (if (and proc (eq 'run (process-status proc)))
	  ;; it is still running. kill it.
	  (interrupt-process proc))
      (set-buffer obuf)
      (delete-file stderr-tempfilename)
      (delete-file status-tempfilename)
      ;; kill off temporary buffers unless we're debugging
      (if (or (not (boundp 'mc-gpg-debug-buffer))
	      (not mc-gpg-debug-buffer))
	  (progn
	    (if (get-buffer " *mailcrypt stdout temp")
		(kill-buffer " *mailcrypt stdout temp"))
	    (if (get-buffer " *mailcrypt stderr temp")
		(kill-buffer " *mailcrypt stderr temp"))
	    (if (get-buffer " *mailcrypt status temp")
		(kill-buffer " *mailcrypt status temp"))
	    ))
)))


; this lookup is used to turn key identifiers into names suitable for
; presentation to the user. When decrypting, the hex keyid to which the
; incoming message is encrypted is looked up to ask the user for a passphrase
; by name. When encrypting, the user's id (mc-gpg-user-id) is looked up to
; ask for a passphrase, and if mc-gpg-encrypt-to-me is true, the user's id
; is looked up to provide a full name to gpg. gpg is always given full names,
; because the hex keyids it provides might not work for both signing and
; encryption (split keys in gpg/pgp5)
;
;31:warner@zs2-pc4% gpg --list-secret-keys --with-colons --no-greeting
;/home/warner/.gnupg/secring.gpg
;-------------------------------
;sec::1024:17:1FE9CBFDC63B6750:1998-08-04:0:::Brian Warner (temporary GPG key) <warner@lothar.com>:
;ssb::1024:20:C68E8DE9F759FBDE:1998-08-04:0:::
;sec::768:17:16BD446D567E33CF:1998-08-04:0:::signature (sample signature key) <key@key>:
;sec::768:16:D514CB72B37D9AF4:1998-08-04:0:::crypt (crypt) <crypt@crypt>:
;sec::1024:17:4DBDD3258230A3E0:1998-08-04:0:::dummyy <d@d>:
;ssb::1024:20:549B0E6CBBBB43D1:1998-08-04:0:::
;
; we use the whole user id string (Brian..lothar.com>) as USER-ID, and the
; long keyid 1FE9CBFDC63B6750 for KEY-ID

(defvar mc-gpg-key-cache nil
  "Association list mapping GPG IDs to canonical \"keys\".  A \"key\"
is a pair (USER-ID . KEY-ID) which identifies the canonical IDs of the
GPG ID.")

(defun mc-gpg-lookup-key (str &optional type)
  ;; Look up the string STR in the user's secret key ring.  Return a
  ;; pair of strings (USER-ID . KEY-ID) which uniquely identifies the
  ;; matching key, or nil if no key matches.
  (let (args)
    (if (string= str "***** CONVENTIONAL *****") nil
      (let ((result (cdr-safe (assoc str mc-gpg-key-cache)))
	    (key-regexp
	     "^\\(sec\\|pub\\):[^:]*:[^:]*:[^:]*:\\([^:]*\\):[^:]*:[^:]*:[^:]*:[^:]*:\\([^:]*\\):"
	     )
	    (obuf (current-buffer))
	    buffer)
	(if (null result)
	    (unwind-protect
		(progn
		  (setq buffer (generate-new-buffer " *mailcrypt temp"))
		  (setq args (list 
			      "--with-colons" 
			      "--no-greeting" "--batch" 
			      "--list-secret-keys" str 
			      ))
		  (if mc-gpg-alternate-keyring
		      (setq args (append (list "--keyring" 
					       mc-gpg-alternate-keyring) 
					 args)))
		  (if mc-gpg-extra-args
		      (setq args (append mc-gpg-extra-args args)))
		  (mc-gpg-debug-print 
		   (format "lookup: args are %s" args))
		  (let ((coding-system-for-read 
			 (if (and (fboundp 'coding-system-p)
				  (coding-system-p 'utf-8))
			     'utf-8 nil)))
		    (apply 'call-process mc-gpg-path nil buffer nil args))
		  (set-buffer buffer)
		  (goto-char (point-min))
		  (if (re-search-forward key-regexp nil t)
		      (progn
			(setq result
			      (cons (buffer-substring-no-properties
				     (match-beginning 3) (match-end 3))
				    (concat
				     "0x"
				     (buffer-substring-no-properties
				      (match-beginning 2) (match-end 2)))))
			(setq mc-gpg-key-cache (cons (cons str result)
						     mc-gpg-key-cache)))))
					;(if buffer (kill-buffer buffer))
	      (set-buffer obuf)))
	(if (null result)
	    (error "No GPG secret key for %s" str))
	result))))

;gpg: no info to calculate a trust probability
;gpg: no valid addressees
;gpg: [stdin]: encryption failed: No such user id

(defun mc-gpg-encrypt-region (recipients start end &optional id sign)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	action msg args key passwd result gpg-id)
    (mc-gpg-debug-print (format 
       "(mc-gpg-encrypt-region recipients=%s start=%s end=%s id=%s sign=%s)"
       recipients start end id sign))
    
    (setq args (list 
		"--batch" "--armor" "--textmode" "--always-trust"
		(if recipients "--encrypt" "--store")
		))
    (setq action (if recipients "Encrypting" "Armoring"))
    (setq msg (format "%s..." action))  ; May get overridden below
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-alternate-keyring
	(setq args (append (list "--keyring" mc-gpg-alternate-keyring) args)))

    (if (and (not (eq mc-pgp-always-sign 'never))
	     (or mc-pgp-always-sign sign (y-or-n-p "Sign the message? ")))
	(progn
	  (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id) 'encrypt))
	  (setq passwd
		(mc-activate-passwd
		 (cdr key)
		 (format "GPG passphrase for %s (%s): " (car key) (cdr key))))
	  (setq args
		(append (list "--local-user" (cdr key)
			      "--sign" 
			      )
			args))
	  (setq msg (format "%s+signing as %s ..." action (car key)))
	  (if (not recipients)
	      ;; the --store is last in args. remove it. remove --textmode too
	      (setq args (nreverse (cddr (nreverse args)))))
	  )
      )

    ; if we're supposed to encrypt for the user too, we need their key
    ;; FIXME: we only need their public key, not the secret one. Some users
    ;; (the author included) keep their secret keys offline unless needed
    ;; (but the public ones are still available).. the --list-secret-keys
    ;; done by mc-gpg-lookup-key will fail in this situation. Change
    ;; mc-gpg-lookup-key to have a way to look for public keys too.
    (if (and recipients mc-encrypt-for-me)
	(setq recipients (cons (cdr (or key
					(setq key (mc-gpg-lookup-key 
						   mc-gpg-user-id 'encrypt)))
				    ) recipients)))

    ; push(@args, map {qq<-r "$_">} @recipients) if @recipients; # roughly
    (if recipients
	(setq args (append (apply 'append 
				  (mapcar '(lambda (x) 
					     (list "--recipient" 
						   (concat "\"" x "\""))) 
					  recipients))
			   args)))

    (message "%s" msg)
    (setq result (mc-gpg-process-region start end passwd mc-gpg-path args
					'mc-gpg-insert-parser buffer))
    (if (not (car result))
	(error "%s failed: %s" msg (nth 2 result)))

    t
))


; GPG DECRYPT BEHAVIOR:  gnupg-0.9.9 only
;  (all status messages are prefixed by "[GNUPG:] "

; signed (not encrypted) by a known key [S.s1v]:
;  rc == 0, stdout has message
;  status:
;   SIG_ID <sigid> <date> <longtime>
;   GOODSIG <longkeyid> <username>
;   VALIDSIG <keyfingerprint> <date> <longtime>
;   TRUST_foo

; signed (not encrypted) by unknown key [S.s4]:
;  rc == 2, stdout has message
;  status:
;   ERRSIG <longkeyid> <pubkeyalgo> <hashalgo> <sigclass> <longtime> <rc>
;   NO_PUBKEY <longkeyid>

; encrypted to a private key we don't have [E.e3]:
;  rc == 2,
;  stderr: gpg: decryption failed: secret key not available
;  status:
;   ENC_TO <longkeyid> <keytype> <keylength==0>
;   NO_SECKEY <longkeyid>
;   DECRYPTION_FAILED

; encrypted to us, our key has no passphrase
;  rc == 0?
;  stderr: gpg: NOTE: secret key foo is NOT protected
;  status:
;   ENC_TO <longkeyid> <keytype> <keylen==0>
;   GOOD_PASSPHRASE
;   DECRYPTION_OKAY

; encrypted to us, but we didn't give a passphrase [E.e1r, no pw]:
;  rc == 2
;  stderr: gpg: fatal: Can't query password in batchmode
;  status:
;    ENC_TO <longkeyid> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid> <otherlongkeyid> <keytype> <keylen==0>
;    MISSSING_PASSPHRASE
;    BAD_PASSPHRASE <longkeyid>
;    DECRYPTION_FAILED
; (N.B.: gpg cannot tell tell the difference between no passphrase and an
;  empty passphrase.)

; encrypted to us *and someone else*, no passphrase [E.e3re1r, no pw]:
;  rc == 2?
;  stderr: gpg: fatal: Can't query password in batchmode
;  status:
;    ENC_TO <longkeyid1> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid1> <otherlongkeyid> <keytype> <keylen==0>
;    MISSSING_PASSPHRASE
;    BAD_PASSPHRASE <longkeyid1>
;    ENC_TO <longkeyid2> .. ..
;    NO_SECKEY <longkeyid2>
;    DECRYPTION_FAILED

; encrypted to us, but we used the wrong passphrase [E.e1r, bad pw]:
;  rc == 2
;  stderr: gpg: public key decryption failed: [Bb]ad passphrase
;  status:
;    ENC_TO <longkeyid> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid> <otherlongkeyid> <keytype> <keylen==0>
;    BAD_PASSPHRASE <longkeyid>
;    DECRYPTION_FAILED

; encrypted to us, good passphrase [E.e1r, good pw]:
;  rc == 0, stdout has message
;  status:
;    ENC_TO <longkeyid> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid> <otherlongkeyid> <keytype> <keylen==0>
;    GOOD_PASSPHRASE
;    DECRYPTION_OKAY

; encrypted to us, good passphrase, signed by trusted/untrusted party
;                                        [ES.e1r.s1v, good ps]:
;  rc == 0, stdout has message
;  stderr: gpg: Signature made <date> using DSA key ID <pubkeyid>
;  stderr: gpg: Good signature from "<keyname>"
;  status:
;    ENC_TO <longkeyid> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid> <otherlongkeyid> <keytype> <keylen==0>
;    GOOD_PASSPHRASE
;    SIG_ID <sigid> <date> <longtime>
;    GOODSIG <longkeyid> <username>
;    VALIDSIG <keyfingerprint> <date> <longtime>
;    TRUST_(UNDEFINED|NEVER|MARGINAL|FULLY|ULTIMATE)
;    DECRYPTION_OKAY

; encrypted to us, good passphrase, signed by unknown party [ES.e1r.s4]:
;  rc == 2, stdout has message
;  stderr: gpg: Signature made <date> using DSA key ID <pubkeyid>
;  stderr: gpg: Can't check signature: [Pp]ublic key not found
;  status:
;    ENC_TO <longkeyid> <keytype> <keylength==0>
;    NEED_PASSPHRASE <longkeyid> <otherlongkeyid> <keytype> <keylen==0>
;    GOOD_PASSPHRASE
;    ERRSIG <longkeyid> <pubkeyalgo> <hashalgo> <sigclass> <longtime> <rc>
;     rc: 4 is unknown algorithm, 9 is missing public key
;    NO_PUBKEY <longkeyid>
;    DECRYPTION_OKAY

; symmetrically encrypted, we didn't give a passphrase
;  rc == 2, stderr: gpg: fatal: Can't query password in batchmode
;  status:
;    NEED_PASSPHRASE_SYM <cipheralgo> <s2kmode> <s2khash>
;    MISSING_PASSPHRASE
;    DECRYPTION_FAILED

; symmetrically encrypted, we gave the wrong passphrase
;  rc == 2, stderr: gpg: decryption failed: [Bb]ad key
;  status:
;    NEED_PASSPHRASE_SYM <cipheralgo> <s2kmode> <s2khash>
;    DECRYPTION_FAILED

; symmetrically encrypted, good passphrase
;  rc == 0, stdout: message
;  status:
;    NEED_PASSPHRASE_SYM <cipheralgo> <s2kmode> <s2khash>
;    DECRYPTION_OKAY

; armored [A]:
;  rc == 0, stdout: message
;  no status

; corrupted armor
;  rc == 2, stderr: gpg: CRC error; stuff - stuff

; ( to test: multiple recipients, keys without passphrases)


;; this parser's return convention:
;;   '( (
;;       replacep ; consumed by process-region: decrypt was successful
;;0      have-secret-key ; t: we are a recipient (TODO: stealth), 
;;                         'symmetric : need passphrase
;;                         'signed : signed not encrypted
;;                         nil: not a recipient
;;1      passphrase-ok ; t was good, nil was bad, keyid: need pw for keyid
;;2      signature: 
;;        nil: no sig
;;        keyid-hex : don't have signature key
;;        '(keyid-string t trust date) : good signature on date with trust
;;        '(keyid-string nil trust date) : bad signature on date with trust
;;       )
;;      )
; todo: stealth ("--throw-keyid")?
;       when there is a signature that we can't check because of a bad algo
;       then we pretend there wasn't a signature. extend the return convention
;       to signal this case.
;       when there is a signature that we can't check because we don't
;       currently have a key, and if we successfully fetch that key in
;       mc-gpg-decrypt-region, how do we restart the operation?

;; cases:
;;  *not addressed to us (nil nil nil)
;;  *just armored (same as good symmetric) ('symmetric t nil)
;;  conventionally encrypted
;;   *didn't give passphrase ('symmetric "***** CONVENTIONAL *****" nil)
;;   did give passphrase
;;    *bad passphrase ('symmetric nil nil)
;;    *good passphrase ('symmetric t nil)
;;  signed (not clearsigned), not encrypted
;;    *don't have key ('signed t keyid)
;;    do have key
;;     *good sig ('signed t (t keyid-string trust date))
;;     *bad sig ('signed t (nil keyid-string trust date))
;;  encrypted to us:
;;   *didn't give passphrase (t keyid nil)
;;   gave passphrase:
;;    *bad passphrase (t nil nil)
;;    good passphrase
;;     decrypted ok
;;      *no signature (t t nil)
;;      yes signature
;;       *don't have key (offer to fetch) (t t keyid)
;;       do have key
;;        *good sig (t t (t keyid-string date trust))
;;        *bad sig (t t (nil keyid-string date trust))

;; a subfunction to extract the signature info. Used in both decrypt-parser
;; and verify-parser. Call with statusbuf. Returns
;;  '(sigtype sigid sigdate sigtrust)

(defun mc-gpg-sigstatus-parser ()
  (let (sigtype sigid sigdate sigtrust)

    ;; sigtype: GOOD, BAD, ERR
    ;; sigid: who made the signature? (a name if possible, else hex keyid)
    ;; sigdate: date string of when the sig was made
    (goto-char (point-min))
    (if (re-search-forward "^\\[GNUPG:\\] +\\(GOOD\\|BAD\\|ERR\\)SIG\\b" 
			   nil t)
	(progn
	  (setq sigtype (match-string 1))
	  (goto-char (point-min))
	  (if (and (or (string= sigtype "GOOD") (string= sigtype "BAD"))
		   (re-search-forward
		    "^\\[GNUPG:\\] +\\(GOOD\\|BAD\\)SIG +\\(\\S +\\) +\\(.*\\)$" nil t))
	      ;; match-string 2 is the hex keyid of the signator. 
	      ;; #3 is the name
	      (setq sigid (match-string 3)))

	  ;; for ERRSIG:
	  ;;   match-string #1 is the hex keyid, #2 is the algorithm ID
	  ;;       (17: DSA, 1,3: RSA, 20: Elgamal)
	  ;;  #3: hashalgo, #4: sigclass, #5: longtime, #6: rc
	  ;;   (rc==4 for unknown algo, 9 for missing public key)
	  ;; we only set sigtype if:
	  ;;   (#1 is present), and 
	  ;;   ((#6 is missing) or (#6 == 9))
	  ;; the idea being to not fetch a key if we aren't going to be able
	  ;; to use the algorithm it wants
	  (goto-char (point-min))
	  (if (and (string= sigtype "ERR")
		   (re-search-forward
		    "^\\[GNUPG:\\] +ERRSIG +\\(\\S +\\)" nil t))
	      (let (errsig-rc (sigid-temp (match-string 1)))
		(goto-char (point-min))
		(if (re-search-forward
		     "^\\[GNUPG:\\] +ERRSIG +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\)" nil t)
		    (setq errsig-rc (match-string 6)))
		(if (or
		     (not errsig-rc)
		     (string= errsig-rc "9"))
		    (setq sigid sigid-temp))
		))
	  
	  ;; for GOODSIG:
	  ;;  VALIDSIG should be present, with <keyfingerprint> <date> <time>
	  (goto-char (point-min))
	  (if (and (string= sigtype "GOOD")
		   (re-search-forward
		    "^\\[GNUPG:\\] +SIG_ID +\\(\\S +\\) +\\(\\S +\\)\\b" 
		    nil t))
	      (setq sigdate (match-string 2))
	    ;; in gpg >= 0.9.7, a third field is a longtime value (seconds
	    ;; since epoch)
	    )
	  
	  ;; sigtrust: how trusted is the signing key?
	  (goto-char (point-min))
	  (if (re-search-forward "^\\[GNUPG:\\] +\\(TRUST_\\S +\\)$" nil t)
	      (setq sigtrust (match-string 1)))
	  ))
        
    (list sigtype sigid sigdate sigtrust))
  )

    
; this parser's job is to find the decrypted data if any is available. The
; code in -decrypt-region will worry about reporting other status information
; like signatures. PARSERDATA is non-nil if a passphrase was given to GPG.

(defun mc-gpg-decrypt-parser (stdoutbuf stderrbuf statusbuf rc parserdata)
  (let 
      (
       decryptstatus ; DECRYPTION_(OKAY|FAILED)
       no-seckey ; NO_SECKEY
       keyid ; NEED_PASSPHRASE <keyid>
       missing-passphrase ; MISSING_PASSPHRASE
       symmetric ; NEED_PASSPHRASE_SYM
       badpass ; BAD_PASSPHRASE
       sigtype ; GOODSIG, BADSIG, ERRSIG
       sigid ;; GOODSIG <keyid>  (note: not SIG_ID!), 
             ;;; or ERRSIG <keyid> if ERRSIG-rc is 9 for missing pubkey
       sigdate ; VALIDSIG .. <date>
       sigtrust ; TRUST_(UNDEFINED|NEVER|MARGINAL|FULLY|ULTIMATE)
       )
    ;; this code is split into two pieces. The first scans statusbuf
    ;; (and stderr if absolutely necessary) for keywords, setting the
    ;; local variables to describe what happened during our decryption attempt.
    ;; We don't try too hard to interpret the results yet.

    ;; the second part (the big cond statement below) interprets those vars
    ;; to decide what to report to the caller

    (set-buffer statusbuf)

    ;; decryptstatus: no decryption took place, one was ok, or one failed
    (goto-char (point-min))
    (if (re-search-forward
	 "^\\[GNUPG:\\] +DECRYPTION_\\(OKAY\\|FAILED\\)\\b"
	 nil t)
	(setq decryptstatus (match-string 1)))

    ;; no-seckey: set if we saw a NO_SECKEY message.
    (goto-char (point-min))
    (if (re-search-forward
	 "^\\[GNUPG:\\] +NO_SECKEY\\b"
	 nil t)
	(setq no-seckey t))
    
    ;; keyid: the message is encrypted to one of our private keys and we
    ;; need a passphrase from the user. which one?
    (goto-char (point-min))
    (if (re-search-forward "^\\[GNUPG:\\] +NEED_PASSPHRASE +\\(\\S +\\)" 
			   nil t)
	(setq keyid (concat "0x" (match-string 1))))

    ;; missing-passphrase: set if we saw MISSING_PASSPHRASE
    (goto-char (point-min))
    (if (re-search-forward "^\\[GNUPG:\\] +MISSING_PASSPHRASE\\b"
			   nil t)
	(setq missing-passphrase t))

    ;; symmetric: Set if the message is symmetrically encrypted. 
    (goto-char (point-min))
    (if (re-search-forward
	 "^\\[GNUPG:\\] +NEED_PASSPHRASE_SYM\\b"
	 nil t)
	(setq symmetric t))

    ;; badpass: GPG did not get a good passphrase. Either we didn't give one
    ;;  or we gave the wrong one.
    (goto-char (point-min))
    (if (re-search-forward "^\\[GNUPG:\\] +BAD_PASSPHRASE\\b" 
			   nil t)
	(setq badpass t))

    (let ((sigstuff (mc-gpg-sigstatus-parser)))
      (setq sigtype (nth 0 sigstuff))
      (setq sigid (nth 1 sigstuff))
      (setq sigdate (nth 2 sigstuff))
      (setq sigtrust (nth 3 sigstuff))
      )

    ;; begin second piece: stare at those variables and decide what happened.
    ;; refer to the "cases:" comment above for what we look for.

    (mc-gpg-debug-print 
     (format
      "decrypt-parser: decryptstatus=%s no-seckey=%s keyid=%s missing-passphrase=%s symmetric=%s badpass=%s sigtype=%s sigid=%s sigdate=%s sigtrust=%s rc=%s"
      decryptstatus no-seckey keyid missing-passphrase symmetric badpass sigtype sigid sigdate sigtrust rc))

    (cond

     ((and (not decryptstatus) (not (or keyid symmetric)))
      ;; either corrupt, armored-only, signed-only
      ;;  or we're using an old gpg and no passphrase was requested:
      ;;   either corrupt, armored-only, signed-only, or not for us.
      (cond
       (sigtype
	;; signed-only. extract info
	(cond
	 ((string= sigtype "GOOD")	  ;; good signature
	  (list t 'signed t (list t sigid sigtrust sigdate)))
	 ((string= sigtype "BAD")   ;; bad signature
	  (list t 'signed t (list nil sigid sigtrust sigdate)))
	 ((string= sigtype "ERR")   ;; couldn't check: why?
	  (if sigid
	      ;; didn't have the key, we can fetch it
	      (list t 'signed t sigid)
	    ;; can't use it. pretend it wasn't signed.
	    (list t t t nil)))
	 (t  ;; sigtype is bogus
	  (error "sigtype was bogus. Shouldn't happen."))
	 ))
       ((not (= rc 0))  ;; corrupt
	(error "The message was corrupt."))
       (t  ;; armored-only
	(list t 'symmetric t nil))
       ))

     ((or 
       (string= decryptstatus "FAILED")
       ;; couldn't decrypt: not to us, need pw, bad pw
       (and (not decryptstatus) 
	    (or keyid symmetric)
	    (not (= rc 0)) 
	    (not (string= sigtype "ERR")))
       ;; or old gpg and we could have decrypted it (a passphrase was
       ;; requested), but the decrypt went bad (rc!=0 but not due to ERRSIG)
       )
      (cond
       ((and (not symmetric) (not keyid))
	;; didn't ask for a passphrase, ergo it isn't for us
	(list nil nil nil nil))
       ((or missing-passphrase (not parserdata))
	;; we didn't give a passphrase, need pubkey or symmetric
	(if symmetric
	    (list nil 'symmetric "***** CONVENTIONAL *****" nil)
	  (list nil t keyid nil)))
       (symmetric ;; symmetric fails without a BAD_PASSPHRASE
	(list nil 'symmetric nil nil))
       ((or badpass parserdata)
	;; probably pubkey, we gave the wrong passphrase
	(list nil t nil nil))
       (t  ;; shouldn't happen, error out
	(error "decryption failed, but I don't know why. Shouldn't happen."))
       ))

     ((or
       (string= decryptstatus "OKAY")
       ;; decrypted okay, check for signature
       (and (not decryptstatus)
	    keyid
	    (not (= rc 0))
	    (string= sigtype "ERR"))
       ;; or old gpg and sigcheck went bad (rc!=0 due to ERRSIG)
       (and (not decryptstatus)
	    keyid
	    (= rc 0))
       ;; or old gpg, passphrase was requested, no errors reported
       )
      (cond
       (sigtype   ;; there was a signature, extract the info (never sym here)
	(cond
	 ((string= sigtype "GOOD")  ;; good signature
	  (list t t t (list t sigid sigtrust sigdate)))
	 ((string= sigtype "BAD")   ;; bad signature
	  (list t t t (list nil sigid sigtrust sigdate)))
	 ((string= sigtype "ERR")   ;; couldn't check: why?
	  (if sigid
	      ;; didn't have the key. we can fetch it.
	      (list t t t sigid)
	    ;; no keyid, or we can't use it. pretend there wasn't a sig.
	    (list t t t nil)))
	 (t  ;; sigtype is bogus
	  (error "sigtype was bogus. Shouldn't happen."))
	 ))
       (t         ;; there wasn't a signature
	(if symmetric
	    (list t 'symmetric t nil)
	  (list t t t nil)))
       ))

     (t  ;; decryptstatus was bogus. error out.
      (error "decryptstatus was bogus '%s'. Shouldn't happen." decryptstatus))

     )
    ))




;; message about who made the signature. This is a bit wide.. the date can
;; easily run off the echo area. Consider replacing 'Good signature' with
;; 'good sig', but keep it consistent with everything else. This function is
;; used by both the decrypt section and the verify section.
;; todo: should the keyid be put in here? If the user reads the trustvalue,
;;  and if they have a trust path, then they can trust the name.
(defun mc-gpg-format-sigline (goodp sigid sigtrust sigdate)
  (if goodp
      (format "Good signature from '%s' %s made %s"
	      sigid sigtrust sigdate)
    (format "BAD SIGNATURE claiming to be from '%s'" sigid)
    ))

;; decrypt-region is first called without ID. This means we'll try to decrypt
;; without a passphrase, almost guaranteed to fail, but it will tell us which
;; key is necessary. We then call decrypt-region again, this time with ID
;; set. This second time will lookup ID and ask the user for the passphrase.

(defun mc-gpg-decrypt-region (start end &optional id)
  ;; returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if
  ;; the decryption succeeded and verified is t if there was a valid signature
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args key new-key passwd result gpg-id)
    (mc-gpg-debug-print (format "(mc-gpg-decrypt-region start=%s end=%s id=%s)"
				start end id))
    (undo-boundary)
    (if id
	;; second time through, now we know who the message is for.
	;; id is either a hex keyid of the (first?) secret key that is in
	;; the message's recipient list, or "**..CONVENTIONAL.."
	(progn
	  (setq key (mc-gpg-lookup-key id 'encrypt))
	  ;; key is nil if CONVENTIONAL, (string . hexid) otherwise
	  (setq passwd
		(if key
		    (mc-activate-passwd (cdr key)
					(format 
					 "GPG passphrase for %s (%s): "
					 (car key) (cdr key)))
		  (mc-activate-passwd 
		   id "GPG passphrase for conventional decryption: ")))
	  (if (string= passwd "")
	      (progn
		(mc-deactivate-passwd t)
		(error "Empty passphrases are bad, mmkay?")))
	  ;; in particular, they cause an infinite loop. If the key doesn't
	  ;; have a passphrase, the decryption should have worked the first
	  ;; time around.
	  ))
    (setq args '("--batch"))
    (if mc-gpg-alternate-keyring
	(setq args (append args (list "--keyring" mc-gpg-alternate-keyring))))
    (setq args (append args '("--decrypt"))) ; this wants to be last
    (message "Decrypting...")
    ;; pass ID as the parserdata. This will be non-nil if a passphrase was
    ;; given (i.e. 2nd pass), which affects decrypt status parsing
    (setq result
	  (mc-gpg-process-region
	   start end passwd mc-gpg-path args 'mc-gpg-decrypt-parser buffer id))
    ;(message "Decrypting... Done.")
    ;; result: '(HAVE-SECRET-KEY PASSPHRASE-OK SIG)
    ;;  SIG: nil, sigkeyid, or '(KEYID GOODP TRUSTLEVEL DATESTRING)
    (cond
     ((not (nth 0 result)) ;; we were not a recipient
      (error "This message is not addressed to you"))
     ((not (nth 1 result)) ;; passphrase-ok is nil: bad passphrase
      (mc-deactivate-passwd t)
      (error "That passphrase was wrong"))
     ((not (equal (nth 1 result) t)) ;; passphrase-ok is keyid: need passphrase
      ;; get passphrase for (nth 1 result), try again
      (mc-gpg-decrypt-region start end (nth 1 result))
      )
     ;; passphrase was ok, were able to decrypt
     ((nth 2 result) ;; there was a signature
      (let ((sig (nth 2 result)))
	(cond
	 ((atom sig) ;; don't have the signature key
	  (progn
	    ;; offer to fetch the key, then what? run again? must we undo 1st?
	    (mc-message-sigstatus
             (format "cannot check signature from keyid %s" sig))
	    (if (and (not (eq mc-gpg-always-fetch 'never))
		     (or mc-gpg-always-fetch
			 (y-or-n-p
			  (format "Key %s not found; attempt to fetch? " sig)))
		     (mc-gpg-fetch-key (cons nil sig)))
		(progn
		  (undo-start)
		  (undo-more 1)
		  (mc-gpg-decrypt-region start end id))
	      '(t . nil))
	    ))
	 ((nth 0 sig) ;; good signature
	  (progn
	    (mc-message-sigstatus (mc-gpg-format-sigline 
				   t (nth 1 sig) (nth 2 sig) (nth 3 sig)))
	    '(t . t)
	    ))
	 (t ;; bad signature
	  (progn
	    (mc-message-sigstatus (mc-gpg-format-sigline 
				   nil (nth 1 sig) (nth 2 sig) (nth 3 sig))
				  t ; get their attention
				  )
	    '(t . nil)
	    ))
       )))
     (t ;; no signature
      (message "Decrypting... Done.")
      '(t . nil)
      ))
    ))

(defun mc-gpg-sign-region (start end &optional id unclear)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key result)
    (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id) 'sign))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "GPG passphrase for %s (%s): " (car key) (cdr key))))
    (setq args
	  (list
	   "--batch" "--armor"
	   "--local-user" (cdr key)
	   (if unclear "--sign" "--clearsign")
	   ))
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-extra-args
	(setq args (append mc-gpg-extra-args args)))
    (message "Signing as %s ..." (car key))
    (setq result (mc-gpg-process-region start end passwd mc-gpg-path args
					'mc-gpg-insert-parser buffer))
    (if (car result)
	(message "Signing as %s ... Done." (car key))
      (progn
	(mc-deactivate-passwd t)
	(error "Signature failed: %s" (nth 2 result))
	))
    (car result)
))


; GPG VERIFY BEHAVIOR: gnupg-0.9.9 only
;  (all status messages are prefixed by "[GNUPG:] "
;  (filenames in [] are my parts of my testsuite)

; corrupted sig (armor is corrupt) [CS.s1bad]:
;  rc == 2
;  stderr: gpg: CRC error; stuff - stuff
;          gpg: packet(1) with unknown version

; GOOD sig from a known key [CS.s1v,CS.s2v,CS.s3v]
;  rc == 0
;  status:
;    SIG_ID <sigid> <date> <longtime>
;    GOODSIG <longkeyid> <username>
;    VALIDSIG <keyfingerprint> <date> <longtime>
;    TRUST_(UNDEFINED|NEVER|MARGINAL|FULLY|ULTIMATE)

; BAD sig from a known key [CS.s1f]:
;  rc == 1
;  status: BADSIG <longkeyid> <username>

; unknown key [CS.s4]:
;  rc == 2
;  status: 
;   ERRSIG <longkeyid> <pubkeyalgo> <hashalgo> <sigclass> <longtime> <rc==9>
;   NO_PUBKEY <longkeyid>

;; so no status messages mean armor corruption

;; return convention for mc-gpg-verify-parser:
;;  (same as sig section of decrypt parser)
;;   sigid : signed by an unknown key, need this key to verify
;;   '(t sigid sigtrust sigdate): good sig from sigid
;;   '(nil sigid sigtrust sigdate): forged sig "from" sigid
;; (actual return includes a leading nil because the verify-parser should
;;  never replace the region with stdout)

(defun mc-gpg-verify-parser (stdoutbuf stderrbuf statusbuf rc parserdata)
  (let (sigtype sigid sigdate sigtrust)
    ;; parse FOOSIG with the same code as decrypt-parser
    (set-buffer statusbuf)

    (let ((sigstuff (mc-gpg-sigstatus-parser)))
      (setq sigtype (nth 0 sigstuff))
      (setq sigid (nth 1 sigstuff))
      (setq sigdate (nth 2 sigstuff))
      (setq sigtrust (nth 3 sigstuff))
      )

    (mc-gpg-debug-print 
     (format
      "decrypt-parser: sigtype=%s sigid=%s sigdate=%s sigtrust=%s"
      sigtype sigid sigdate sigtrust))

    (if (and (not (= rc 0)) 
	     (not sigtype))
	(error "The message was corrupt."))

    (cond
     ((string= sigtype "ERR")
      (list nil sigid))
     ((string= sigtype "GOOD")
      (list nil (list t sigid sigtrust sigdate))) ;; good sig
     (t
      (list nil (list nil sigid sigtrust sigdate))))
    ))


; check a signature, print a message about its validity. Returns t if the
; sig was valid, nil otherwise

(defun mc-gpg-verify-region (start end &optional no-fetch)
  (let ((buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args result)
    (setq args '("--batch" "--verify"))
    (if mc-gpg-alternate-keyring
	(setq args (append "--keyring" mc-gpg-alternate-keyring args)))
    (message "Verifying...")
    (setq result (mc-gpg-process-region
		  start end nil mc-gpg-path args 'mc-gpg-verify-parser buffer))
    (mc-gpg-debug-print (format "process-region returned %s" result))
    (setq result (car result))

    (cond 

     ((atom result) 
      ;; need key
      (if (and
	   (not no-fetch)
	   (not (eq mc-gpg-always-fetch 'never))
	   (or mc-gpg-always-fetch
	       (y-or-n-p
		(format "Key %s not found; attempt to fetch? " result)))
	   (mc-gpg-fetch-key (cons nil result))
	   (set-buffer obuf))
	  (mc-gpg-verify-region start end t)
	(error "Can't check signature: Public key %s not found" result)))

     ((nth 0 result)
      ;; good sig
      (progn
	(message (mc-gpg-format-sigline
		  t (nth 1 result) (nth 2 result) (nth 3 result)))
	t))

     (t
      ;; bad sig
      (progn
	(ding)
	(message (mc-gpg-format-sigline
		  nil (nth 1 result) (nth 2 result) (nth 3 result)))
	nil))
    )
))

(defun mc-gpg-insert-public-key (&optional id)
  (let ((buffer (get-buffer-create mc-buffer-name))
	args result)
    (setq id (or id mc-gpg-user-id))
    (setq args (list "--export" "--armor" "--batch" (concat "\"" id "\"")))
    (if mc-gpg-comment
	(setq args (append (list "--comment" (format "'%s'" mc-gpg-comment))
			   args)))
    (if mc-gpg-alternate-keyring
	(setq args (append (list "--keyring" mc-gpg-alternate-keyring) args)))

    (setq result (mc-gpg-process-region (point) (point) nil mc-gpg-path
					args 'mc-gpg-insert-parser buffer))
    (if (car result)
	(message (format "Key for user ID: %s" id))
      (message "failed: %s" (nth 2 result)))
    (car result)
))

;; GPG IMPORT BEHAVIOR: gnupg-0.9.9 only

;; status:
;;  IMPORT_RES (12 fields)
;;   1 <count> : number of keys seen
;;   2 <no_user_id> : the number of keys without valid userids, including
;;                    keys that weren't self-signed
;;   3 <imported> : new public keys
;;   4 <imported_rsa> : new RSA public keys (included in <imported>)
;;   5 <unchanged> : old public keys
;;   6 <n_uids>
;;   7 <n_subk>
;;   8 <n_sigs>
;;   9 <n_revoc>
;;   10 <sec_read> : number of secret keys seen
;;   11 <sec_imported> : new secret keys
;;   12 <sec_dups> : old secret keys

;;   the first three are for public keys, the last three are for secret keys.
;;   add them together, I guess. It's unlikely that anyone will be importing
;;   armored secret keys via email, but if they do it will be reported as if
;;   it were a public key.

;; return convention: 
;;  error with stderr if rc != 0
;;  '(count bad new old changed secretp)

(defun mc-gpg-snarf-parser (stdoutbuf stderrbuf statusbuf rc parserdata)
  (if (eq rc 0)
      (let (count bad new old changed secretp)
	(set-buffer statusbuf)
	(goto-char (point-min))
	(if (re-search-forward
	     "^\\[GNUPG:\\] +IMPORT_RES +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\) +\\(\\S +\\)"
	     nil t)
	    (progn
	      (setq count (string-to-number (match-string 1)))
	      (setq bad (string-to-number (match-string 2)))
	      (setq new (+ (string-to-number (match-string 3)) 
			   (string-to-number (match-string 11))))
	      (setq old (+ (string-to-number (match-string 5)) 
			   (string-to-number (match-string 12))))
	      (setq changed (- count bad new old))
	      (setq secretp (not (string= (match-string 10) "0")))
	      (list nil count bad new old changed secretp))
	  (error "No key import status: your GnuPG is too old."))
	)
    (error (with-current-buffer stderrbuf (buffer-string))))
)

(defun mc-gpg-snarf-keys (start end)
  ;; Returns number of keys found.
  (let ((buffer (get-buffer-create mc-buffer-name))
	results args msg)
    (setq args '("--import" "--batch"))
    (if mc-gpg-alternate-keyring
	(setq args (append args (list "--keyring" mc-gpg-alternate-keyring))))
    (message "Snarfing...")
    (setq results (mc-gpg-process-region start end nil mc-gpg-path args
					 'mc-gpg-snarf-parser buffer))
    ;; don't have to update trustdb: gpg does it automatically (although it
    ;; might take a few seconds if a lot of keys or signatures have been
    ;; added).

    ;; Is there any point to displaying this message? mc-snarf-keys will
    ;; display a simple "%d new keys found" message right after we return.
    ;; Well, print it anyway, if the user looks in the *Messages* buffer
    ;; they'll see more.
    (setq msg (format "%d keys seen" (nth 0 results)))
    (if (not (zerop (nth 1 results)))
	(setq msg (concat msg (format ", %d bad" (nth 1 results)))))
    (if (not (zerop (nth 2 results)))
	(setq msg (concat msg (format ", %d new" (nth 2 results)))))
    (if (not (zerop (nth 3 results)))
	(setq msg (concat msg (format ", %d old" (nth 3 results)))))
    (if (not (zerop (nth 4 results)))
	(setq msg (concat msg (format ", %d changed" (nth 4 results)))))
    (if (nth 5 results)
	(setq msg (concat msg ", SECRET KEYS IMPORTED")))

    (message msg)
    (nth 2 results)
    ))

(defun mc-scheme-gpg ()
  (list
   (cons 'encryption-func 		'mc-gpg-encrypt-region)
   (cons 'decryption-func		'mc-gpg-decrypt-region)
   (cons 'signing-func			'mc-gpg-sign-region)
   (cons 'verification-func 		'mc-gpg-verify-region)
   (cons 'key-insertion-func 		'mc-gpg-insert-public-key)
   (cons 'snarf-func			'mc-gpg-snarf-keys)
   (cons 'msg-begin-line 		mc-gpg-msg-begin-line)
   (cons 'msg-end-line 			mc-gpg-msg-end-line)
   (cons 'signed-begin-line 		mc-gpg-signed-begin-line)
   (cons 'signed-end-line 		mc-gpg-signed-end-line)
   (cons 'key-begin-line 		mc-gpg-key-begin-line)
   (cons 'key-end-line 			mc-gpg-key-end-line)
   (cons 'user-id			mc-gpg-user-id)))

;;{{{ Key fetching

(defvar mc-gpg-always-fetch 'never
  "*If t, always attempt to fetch missing keys, or never fetch if
'never.")

(defun mc-gpg-fetch-key (&optional id)
  "Attempt to fetch a key for addition to GPG keyring.  Interactively,
prompt for string matching key to fetch.

This function is not yet implemented. The GPG documentation suggests a simple
keyserver protocol, but as far as I know it has not yet been implemented
anywhere."

  (error "Key fetching not yet implemented"))

;;}}}

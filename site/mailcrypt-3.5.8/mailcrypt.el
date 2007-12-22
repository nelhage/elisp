;; mailcrypt.el v3.5.6, mail encryption with PGP
;; Copyright (C) 1995  Jin Choi <jin@atype.com>
;;                     Patrick LoPresti <patl@lcs.mit.edu>
;;           (C) 1998  Len Budney <lbudney@pobox.com>
;;           (C) 2001  Brian Warner <warner@lothar.com>
;; Any comments or suggestions welcome.
;; Inspired by pgp.el, by Gray Watson <gray@antaire.com>.

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

;;{{{ Load some required packages

(eval-when-compile
  ;; Quiet warnings
  (autoload 'start-itimer "itimer")
  (autoload 'cancel-itimer "itimer")
  (autoload 'delete-itimer "itimer"))

(require 'easymenu)
(require 'comint)
(require 'rfc822)

(eval-and-compile
  (condition-case nil (require 'itimer) (error nil))
  (if (not (featurep 'itimer))
      (condition-case nil (require 'timer) (error nil)))

  (if (not (fboundp 'buffer-substring-no-properties))
      (fset 'buffer-substring-no-properties 'buffer-substring)))

(defconst mc-xemacs-p (string-match "XEmacs" emacs-version))

(autoload 'mc-decrypt "mc-toplev" nil t)
(autoload 'mc-verify "mc-toplev" nil t)
(autoload 'mc-snarf "mc-toplev" nil t)
(autoload 'mc-pgp-fetch-key "mc-pgp" nil t)
(autoload 'mc-encrypt "mc-toplev" nil t)
(autoload 'mc-sign "mc-toplev" nil t)
(autoload 'mc-insert-public-key "mc-toplev" nil t)
(autoload 'mc-remailer-encrypt-for-chain "mc-remail" nil t)
(autoload 'mc-remailer-insert-response-block "mc-remail" nil t)
(autoload 'mc-remailer-insert-pseudonym "mc-remail" nil t)
(autoload 'mc-setversion "mc-setversion" nil t)

;;}}}

;;{{{ Minor mode variables and functions

(defvar mc-pgp-always-sign nil 
  "*If t, always sign encrypted PGP messages, or never sign if 'never.")

(defvar mc-read-mode nil
  "Non-nil means Mailcrypt read mode key bindings are available.")

(defvar mc-write-mode nil
  "Non-nil means Mailcrypt write mode key bindings are available.")

(make-variable-buffer-local 'mc-read-mode)
(make-variable-buffer-local 'mc-write-mode)

(defvar mc-read-mode-string " MC-r"
  "*String to put in mode line when Mailcrypt read mode is active.")

(defvar mc-write-mode-string " MC-w"
  "*String to put in mode line when Mailcrypt write mode is active.")

(defvar mc-read-mode-map nil
  "Keymap for Mailcrypt read mode bindings.")

(defvar mc-write-mode-map nil
  "Keymap for Mailcrypt write mode bindings.")

(or mc-read-mode-map
    (progn
      (setq mc-read-mode-map (make-sparse-keymap))
      (define-key mc-read-mode-map "\C-c/f" 'mc-deactivate-passwd)
      (define-key mc-read-mode-map "\C-c/d" 'mc-decrypt)
      (define-key mc-read-mode-map "\C-c/v" 'mc-verify)
      (define-key mc-read-mode-map "\C-c/a" 'mc-snarf)
      (define-key mc-read-mode-map "\C-c/k" 'mc-pgp-fetch-key)))

(or mc-write-mode-map
    (progn
      (setq mc-write-mode-map (make-sparse-keymap))
      (define-key mc-write-mode-map "\C-c/f" 'mc-deactivate-passwd)
      (define-key mc-write-mode-map "\C-c/e" 'mc-encrypt)
      (define-key mc-write-mode-map "\C-c/s" 'mc-sign)
      (define-key mc-write-mode-map "\C-c/x" 'mc-insert-public-key)
      (define-key mc-write-mode-map "\C-c/k" 'mc-pgp-fetch-key)
      (define-key mc-write-mode-map "\C-c/r"
	'mc-remailer-encrypt-for-chain)
      (define-key mc-write-mode-map "\C-c/b"
	'mc-remailer-insert-response-block)
      (define-key mc-write-mode-map "\C-c/p"
	'mc-remailer-insert-pseudonym)))

(easy-menu-define
 mc-read-mode-menu (if mc-xemacs-p nil (list mc-read-mode-map))
 "Mailcrypt read mode menu."
 '("Mailcrypt"
   ["Decrypt Message" mc-decrypt t]
   ["Verify Signature" mc-verify t]
   ["Snarf Keys" mc-snarf t]
   ["Fetch Key" mc-pgp-fetch-key t]
   ["Forget Passphrase(s)" mc-deactivate-passwd t]))

(easy-menu-define
 mc-write-mode-menu (if mc-xemacs-p nil (list mc-write-mode-map))
 "Mailcrypt write mode menu."
 '("Mailcrypt"
   ["Encrypt Message" mc-encrypt t]
   ["Sign Message" mc-sign t]
   ["Insert Public Key" mc-insert-public-key t]
   ["Fetch Key" mc-pgp-fetch-key t]
   ["Encrypt for Remailer(s)" mc-remailer-encrypt-for-chain t]
   ["Insert Pseudonym" mc-remailer-insert-pseudonym t]
   ["Insert Response Block" mc-remailer-insert-response-block t]
   ["Forget Passphrase(s)" mc-deactivate-passwd t]))

(or (assq 'mc-read-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'mc-read-mode mc-read-mode-map)
		minor-mode-map-alist)))

(or (assq 'mc-write-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'mc-write-mode mc-write-mode-map)
		minor-mode-map-alist)))

(or (assq 'mc-read-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(mc-read-mode mc-read-mode-string) minor-mode-alist)))

(or (assq 'mc-write-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(mc-write-mode mc-write-mode-string) minor-mode-alist)))

(defun mc-read-mode (&optional arg)
  "\nMinor mode for interfacing with cryptographic functions.
\\<mc-read-mode-map>
\\[mc-decrypt]\t\tDecrypt an encrypted message
\\[mc-verify]\t\tVerify signature on a clearsigned message
\\[mc-snarf]\t\tAdd public key(s) to keyring
\\[mc-pgp-fetch-key]\t\tFetch a PGP key via finger or HTTP
\\[mc-deactivate-passwd]\t\tForget passphrase(s)\n"
  (interactive)
  (setq mc-read-mode
	(if (null arg) (not mc-read-mode)
	  (> (prefix-numeric-value arg) 0)))
  (and mc-read-mode mc-write-mode (mc-write-mode nil))
  (if mc-read-mode
      (easy-menu-add mc-read-mode-menu)
    (easy-menu-remove mc-read-mode-menu)))
	
(defun mc-write-mode (&optional arg)
  "\nMinor mode for interfacing with cryptographic functions.
\\<mc-write-mode-map>
\\[mc-encrypt]\t\tEncrypt (and optionally sign) message
\\[mc-sign]\t\tClearsign message
\\[mc-insert-public-key]\t\tExtract public key from keyring and insert into message
\\[mc-pgp-fetch-key]\t\tFetch a PGP key via finger or HTTP
\\[mc-remailer-encrypt-for-chain]\t\tEncrypt message for remailing
\\[mc-remailer-insert-pseudonym]\t\tInsert a pseudonym (for remailing)
\\[mc-remailer-insert-response-block]\t\tInsert a response block (for remailing)
\\[mc-deactivate-passwd]\t\tForget passphrase(s)\n"
  (interactive)
  (setq mc-write-mode
	(if (null arg) (not mc-write-mode)
	  (> (prefix-numeric-value arg) 0)))
  (and mc-write-mode mc-read-mode (mc-read-mode nil))
  (if mc-write-mode
      (easy-menu-add mc-write-mode-menu)
    (easy-menu-remove mc-write-mode-menu)))

(defun mc-install-read-mode ()
  (interactive)
  (mc-read-mode 1))

(defun mc-install-write-mode ()
  (interactive)
  (mc-write-mode 1))

;;}}}

;;{{{ Note:
;; The funny triple braces you see are used by `folding-mode', a minor
;; mode by Jamie Lokier, available from the elisp archive.
;;}}}

;;{{{ User variables.
(defconst mc-version "3.5.8")
(defvar mc-temp-directory 
  (cond ((fboundp 'temp-directory) (temp-directory))
	((boundp 'temporary-file-directory) temporary-file-directory)
	("/tmp/"))
  "*Default temp directory to be used by Mailcrypt.")
(defvar mc-default-scheme 'mc-scheme-pgp "*Default encryption scheme to use.")
(defvar mc-passwd-timeout 60
  "*Time to deactivate password in seconds after a use.
nil or 0 means deactivate immediately.  If the only timer package available
is the 'timer' package, then this can be a string in timer format.")

(defvar mc-ripem-user-id (or (getenv "RIPEM_USER_NAME")
			     (user-full-name) "*Your RIPEM user ID."))

(defvar mc-always-replace nil
  "*If t, decrypt mail messages in place without prompting.

If 'never, always use a viewer instead of replacing.")

(defvar mc-use-default-recipients nil "*Assume that the message should
  be encoded for everyone listed in the To, Cc, and Bcc fields.")

(defvar mc-encrypt-for-me nil "*Encrypt all outgoing messages with
  user's public key.")

(defvar mc-pre-signature-hook nil
  "*List of hook functions to run immediately before signing.")
(defvar mc-post-signature-hook nil
  "*List of hook functions to run immediately after signing.")
(defvar mc-pre-encryption-hook nil 
  "*List of hook functions to run immediately before encrypting.")
(defvar mc-post-encryption-hook nil 
  "*List of hook functions to run after encrypting.")
(defvar mc-pre-decryption-hook nil 
  "*List of hook functions to run immediately before decrypting.")
(defvar mc-post-decryption-hook nil 
  "*List of hook functions to run after decrypting.")

(defconst mc-buffer-name "*MailCrypt*"
  "Name of temporary buffer for mailcrypt")

(defvar mc-modes-alist
  '((rmail-mode (decrypt . mc-rmail-decrypt-message)
		(verify . mc-rmail-verify-signature))
    (rmail-summary-mode (decrypt . mc-rmail-summary-decrypt-message)
			(verify . mc-rmail-summary-verify-signature)
			(snarf . mc-rmail-summary-snarf-keys))
    (mew-draft-mode (encrypt . mc-encrypt-message)
                    (sign . mc-sign-message))
    (mew-message-mode (decrypt . mc-mew-decrypt-message))
    (mew-summary-mode (decrypt . mc-mew-summary-decrypt-message)
                      (verify . mc-mew-summary-verify-signature)
                      (snarf . mc-mew-summary-snarf-keys))
    (vm-mode (decrypt . mc-vm-decrypt-message)
	     (verify . mc-vm-verify-signature)
	     (snarf . mc-vm-snarf-keys))
    (vm-virtual-mode (decrypt . mc-vm-decrypt-message)
		     (verify . mc-vm-verify-signature)
		     (snarf . mc-vm-snarf-keys))
    (vm-summary-mode (decrypt . mc-vm-decrypt-message)
		     (verify . mc-vm-verify-signature)
		     (snarf . mc-vm-snarf-keys))
    (mh-folder-mode (decrypt . mc-mh-decrypt-message)
		    (verify . mc-mh-verify-signature)
		    (snarf . mc-mh-snarf-keys))
    (message-mode (encrypt . mc-encrypt-message)
                  (sign . mc-sign-message))
    (gnus-summary-mode (decrypt . mc-gnus-decrypt-message)
		       (verify . mc-gnus-verify-signature)
		       (snarf . mc-gnus-snarf-keys))
    (gnus-article-mode (decrypt . mc-gnus-decrypt-message)
		       (verify . mc-gnus-verify-signature)
		       (snarf . mc-gnus-snarf-keys))
    (mail-mode (encrypt . mc-encrypt-message)
	       (sign . mc-sign-message))
    (vm-mail-mode (encrypt . mc-encrypt-message)
		  (sign . mc-sign-message))
    (mh-letter-mode (encrypt . mc-encrypt-message)
		    (sign . mc-sign-message))
    (news-reply-mode (encrypt . mc-encrypt-message)
		     (sign . mc-sign-message)))

  "Association list (indexed by major mode) of association lists
(indexed by operation) of functions to call for each major mode.")

;;}}}
;;{{{ Program variables and constants.

(defvar mc-timer nil "Timer object for password deactivation.")

(defvar mc-passwd-cache nil "Cache for passphrases.")

(defvar mc-schemes '(("pgp50" . mc-scheme-pgp50)
		     ("pgp" . mc-scheme-pgp)
		     ("gpg" . mc-scheme-gpg)
		     ))

;;}}}

;;{{{ Utility functions.

(defun mc-message-delimiter-positions (start-re end-re &optional begin)
  ;; Returns pair of integers (START . END) that delimit message marked off
  ;; by the regular expressions start-re and end-re. Optional argument BEGIN
  ;; determines where we should start looking from.
  (setq begin (or begin (point-min)))
  (let (start)
    (save-excursion
      (goto-char begin)
      (and (re-search-forward start-re nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward end-re nil t)
	   (cons start (point))))))


(defun mc-split (regexp str)
  "Splits STR into a list of elements which were separated by REGEXP,
stripping initial and trailing whitespace."
  (let ((data (match-data))
	(retval '())
	beg end)
    (unwind-protect
	(progn
	  (string-match "[ \t\n]*" str)	; Will always match at 0
	  (setq beg (match-end 0))
	  (setq end (string-match "[ \t\n]*\\'" str))
	  (while (string-match regexp str beg)
	    (setq retval
		  (cons (substring str beg (match-beginning 0)) 
			retval))
	    (setq beg (match-end 0)))
	  (if (not (= (length str) beg)) ; Not end
	      (setq retval (cons (substring str beg end) retval)))
	  (nreverse retval))
      (store-match-data data))))

;;; FIXME - Function never called?
;(defun mc-temp-display (beg end &optional name)
;  (let (tmp)
;    (if (not name)
;	(setq name mc-buffer-name))
;    (if (string-match name "*ERROR*")
;	(progn
;	  (message "mailcrypt: An error occured!  See *ERROR* buffer.")
;	  (beep)))
;    (setq tmp (buffer-substring beg end))
;    (delete-region beg end)
;    (save-excursion
;      (save-window-excursion
;	(with-output-to-temp-buffer name
;	  (princ tmp))))))

;; In case I ever decide to do this right.
;; LRB - Thanks Pat! This helped a lot in updating mixmaster support.
;; mc-field-name-regexp now catches precisely those email headers 
;; which are RFC-822 compliant.
(defconst mc-field-name-regexp 
  (concat 
   "^\\([" 
   (char-to-string 33) "-" (char-to-string 57)
   (char-to-string 59) "-" (char-to-string 126)
   "]*\\)"))
(defconst mc-field-body-regexp "\\(.*\\(\n[ \t].*\\)*\n\\)")

(defun mc-get-fields (&optional matching bounds nuke)
  "Get all header fields within BOUNDS.  Return as an
alist ((FIELD-NAME . FIELD-BODY) (FIELD-NAME . FIELD-BODY) ...).

Argument MATCHING, if present, is a regexp which each FIELD-NAME
must match exactly.  Matching is case-insensitive.

Optional arg NUKE, if non-nil, means eliminate all fields returned."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t)
	    (header-field-regexp
	     (concat mc-field-name-regexp ":" mc-field-body-regexp))
	    ret name body field-start field-end)
	;; Ensure exact match
	(if matching
	    (setq matching (concat "^\\(" matching "\\)$")))

	(if bounds
	    (narrow-to-region (car bounds) (cdr bounds)))

	(goto-char (point-max))
 
	(while (re-search-backward header-field-regexp nil 'move)
	  (setq field-start (match-beginning 0))
	  (setq field-end (match-end 0))
	  (setq name (buffer-substring-no-properties
		      (match-beginning 1) (match-end 1)))
	  (setq body (buffer-substring-no-properties
		      (match-beginning 2) (match-end 2)))
	  (if (or (null matching) (string-match matching name))
	      (progn
		(setq ret (cons (cons name body) ret))
		(if nuke
		    (delete-region field-start field-end)))))
	ret))))

(defsubst mc-strip-address (addr)
  "Strip everything from ADDR except the basic Email address."
  (car (rfc822-addresses addr)))

(defun mc-strip-addresses (addr-list)
  "Strip everything from the addresses in ADDR-LIST except the basic
Email address.  ADDR-LIST may be a single string or a list of strings."
  (if (not (listp addr-list)) (setq addr-list (list addr-list)))
  (setq addr-list
	(mapcar
	 (function (lambda (s) (rfc822-addresses s)))
	 addr-list))
  (setq addr-list (apply 'append addr-list))
  (mapconcat 'mc-strip-address addr-list ", "))

(defun mc-display-buffer (buffer)
  "Like display-buffer, but always display top of the buffer."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (display-buffer buffer)))

(defun mc-message (msg &optional buffer default)
  ;; returns t if we used msg, nil if we used default
  (let ((retval t))
    (if buffer
	(setq msg
	      (save-excursion
		(set-buffer buffer)
		(goto-char (point-min))
		(if (re-search-forward msg nil t)
		    (buffer-substring-no-properties
		     (match-beginning 0) (match-end 0))
		  (setq retval nil)
		  default))))
    (if msg (message-or-box "%s" msg))
    retval))

(defun mc-message-sigstatus (msg &optional attention)
  "Emit a signature status line. If ATTENTION is non-nil, be noisy about it."
  ;; this function exists to be overridden by the unit tests
  (if attention (ding))
  (message-or-box "%s" msg)
)

(defun mc-process-region (beg end passwd program args parser &optional buffer)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process "*PGP*" mybuf program args))
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  (process-send-region proc beg end)
	  (process-send-eof proc)
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  (setq result (process-exit-status proc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*PGP.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited abnormally: '%s'" program result)
	    (setq rgn (funcall parser result))
	    ;; If the parser found something, migrate it
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (delete-region beg end)
		  (goto-char beg)
		  (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn)))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf)))))

;;}}}

;;{{{ Passphrase management
(defun mc-activate-passwd (id &optional prompt)
  "Activate the passphrase matching ID, using PROMPT for a prompt.
Return the passphrase.  If PROMPT is nil, only return value if cached."
  (cond ((featurep 'itimer)
	 (if mc-timer (delete-itimer mc-timer))
	 (setq mc-timer (if mc-passwd-timeout
			    (start-itimer "mc-itimer"
					  'mc-deactivate-passwd
					  mc-passwd-timeout)
			  nil)))
	((featurep 'timer)
	 (let ((string-time (if (integerp mc-passwd-timeout)
				(format "%d sec" mc-passwd-timeout)
			      mc-passwd-timeout)))
	   (if mc-timer (cancel-timer mc-timer))
	   (setq mc-timer (if string-time
			      (run-at-time string-time 
					   nil 'mc-deactivate-passwd)
			    nil)))))
  (let ((cell (assoc id mc-passwd-cache))
	passwd)
    (setq passwd (cdr-safe cell))
    (if (and (not passwd) prompt)
	(setq passwd (comint-read-noecho prompt)))
    (if cell
	(setcdr cell passwd)
      (setq mc-passwd-cache (cons (cons id passwd) mc-passwd-cache)))
    passwd))

(defun mc-deactivate-passwd (&optional inhibit-message)
  "*Deactivate the passphrase cache."
  (interactive)
  (if mc-timer
      (cond ((featurep 'itimer) (delete-itimer mc-timer))
	    ((featurep 'timer) (cancel-timer mc-timer))))
  (mapcar
   (function
    (lambda (cell)
      (if (stringp (cdr-safe cell)) (fillarray (cdr cell) 0))
      (setcdr cell nil)))
   mc-passwd-cache)
  (or inhibit-message
      (not (interactive-p))
      (message "Passphrase%s deactivated"
	       (if (> (length mc-passwd-cache) 1) "s" ""))))

;;}}}

;;{{{ Define several aliases so that an apropos on `mailcrypt' will
;; return something.
(defalias 'mailcrypt-encrypt 'mc-encrypt)
(defalias 'mailcrypt-decrypt 'mc-decrypt)
(defalias 'mailcrypt-sign 'mc-sign)
(defalias 'mailcrypt-verify 'mc-verify)
(defalias 'mailcrypt-insert-public-key 'mc-insert-public-key)
(defalias 'mailcrypt-snarf 'mc-snarf)
;;}}}

(provide 'mailcrypt)

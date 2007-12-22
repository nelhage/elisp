;; mc-toplev.el, entry point functions for Mailcrypt
;; Copyright (C) 1995  Jin Choi <jsc@mit.edu>
;;                     Patrick LoPresti <patl@lcs.mit.edu>

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
(require 'mailcrypt)
(require 'mail-utils)
(require 'rfc822)

(eval-when-compile
  ;; RMAIL
  (condition-case nil (require 'rmail) (error nil))
  (autoload 'rmail-abort-edit "rmailedit")
  (autoload 'rmail-cease-edit "rmailedit")
  ;; Is this a good idea?
  (defvar rmail-buffer nil)

  ;; VM
  (condition-case nil (require 'vm) (error nil))

  ;; GNUS
  (condition-case nil (require 'gnus) (error nil))
  (autoload 'gnus-summary-select-article "gnus-sum")
  (autoload 'gnus-summary-edit-article "gnus-sum")
  (autoload 'gnus-summary-edit-article-postpone "gnus-sum")
  (autoload 'gnus-summary-edit-article-done "gnus-sum")

  ;; MH-E
  (condition-case nil (require 'mh-e) (error nil))

  ;; Mew
  (condition-case nil (require 'mew) (error nil))
  (autoload 'mew-summary-display "mew-summary")
  (autoload 'mew-buffer-message "mew")

)

(eval-and-compile
  (condition-case nil (require 'mailalias) (error nil)))

(autoload 'mc-scheme-pgp   "mc-pgp"  nil t)
(autoload 'mc-scheme-pgp50 "mc-pgp5" nil t)
(autoload 'mc-scheme-gpg   "mc-gpg"  nil t)

;;}}}

;;{{{ Encryption

(defun mc-cleanup-recipient-headers (str)
  ;; Takes a comma separated string of recipients to encrypt for and,
  ;; assuming they were possibly extracted from the headers of a reply,
  ;; returns a list of the address components.
  (mapcar 'mc-strip-address
	  (rfc822-addresses str)))

(defun mc-find-headers-end ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (if (looking-at "^::\n")
	(re-search-forward "^\n" nil t))
    (if (looking-at "^##\n")
	(re-search-forward "^\n" nil t))
    (point-marker)))

(defun mc-encrypt (arg)
  "*Encrypt the current buffer.

Exact behavior depends on current major mode.

With \\[universal-argument], prompt for User ID to sign as.

With \\[universal-argument] \\[universal-argument], prompt for encryption scheme to use."
  (interactive "p")
  (mc-encrypt-region arg nil nil))

(defun mc-encrypt-region (arg start end)
  "*Encrypt the current region."
  (interactive "p\nr")
  (let* ((mode-alist (cdr-safe (assq major-mode mc-modes-alist)))
	 (func (or (cdr-safe (assq 'encrypt mode-alist))
		   'mc-encrypt-generic))
	 sign scheme from)
    (if (>= arg 4)
	(setq from (read-string "User ID: ")
	      sign t))
    (if (>= arg 16)
	(setq scheme
	      (cdr (assoc
		    (completing-read "Encryption Scheme: " mc-schemes)
		    mc-schemes))))
    (funcall func nil scheme start end from sign)))

(defun mc-encrypt-generic (&optional recipients scheme start end from sign)
  "*Generic function to encrypt a region of data."
  (save-excursion
    (or start (setq start (point-min-marker)))
    (or (markerp start) (setq start (copy-marker start)))
    (or end (setq end (point-max-marker)))
    (or (markerp end) (setq end (copy-marker end)))
    (run-hooks 'mc-pre-encryption-hook)
    (cond ((stringp recipients)
	   (setq recipients
		 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)))
	  ((null recipients)
	   (setq recipients
		 (mc-cleanup-recipient-headers (read-string "Recipients: "))))
	  (t (error "mc-encrypt-generic: recipients not string or nil")))
    (or scheme (setq scheme mc-default-scheme))
    (if (funcall (cdr (assoc 'encryption-func (funcall scheme)))
		 recipients start end from sign)
	(progn
	  (run-hooks 'mc-post-encryption-hook)
	  t))))

(defun mc-encrypt-message (&optional recipients scheme start end from sign)
  "*Encrypt a message for RECIPIENTS using the given encryption SCHEME.
RECIPIENTS is a comma separated string. If SCHEME is nil, use the value
of `mc-default-scheme'.  Returns t on success, nil otherwise."
  (save-excursion
    (let ((headers-end (mc-find-headers-end))
	  default-recipients)

      (setq default-recipients
	    (save-restriction
	      (goto-char (point-min))
	      (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$"))
	      (narrow-to-region (point-min) (point))
	      (and (featurep 'mailalias)
		   (not (featurep 'mail-abbrevs))
		   mail-aliases
		   (expand-mail-aliases (point-min) (point-max)))
	      (mc-strip-addresses
	       (mapcar 'cdr
		       (mc-get-fields "to\\|cc\\|bcc")))))

      (if (not from)
	  (save-restriction
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "\n"))
	    (narrow-to-region (point) headers-end)
	    (setq from (mail-fetch-field "From"))))
      
      (if (not recipients)
	  (setq recipients
		(if mc-use-default-recipients
		    default-recipients
		  (read-from-minibuffer "Recipients: " default-recipients))))
     
      (or start (setq start headers-end))
      (or end (setq end (point-max-marker)))

      (mc-encrypt-generic recipients scheme start end from sign))))
      

;;}}}
;;{{{ Decryption

(defun mc-decrypt ()
  "*Decrypt a message in the current buffer.

Exact behavior depends on current major mode."
  (interactive)
  (let* ((mode-alist (cdr-safe (assq major-mode mc-modes-alist)))
	 (func (or (cdr-safe (assq 'decrypt mode-alist))
		   'mc-decrypt-message)))
    (funcall func)))

(defun mc-decrypt-message ()
  "Decrypt whatever message is in the current buffer.
Returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if the encryption
succeeded and VERIFIED is t if it had a valid signature."
  (save-excursion
    (let ((schemes mc-schemes)
	  limits 
	  (scheme mc-default-scheme))

      ; Attempt to find a message signed according to the default
      ; scheme.
      (if mc-default-scheme
	  (setq
	   limits
	   (mc-message-delimiter-positions
	    (cdr (assoc 'msg-begin-line (funcall mc-default-scheme)))
	    (cdr (assoc 'msg-end-line (funcall mc-default-scheme))))))

      ; We can't find a message signed in the default scheme.
      ; Step through all the schemes we know, trying to identify
      ; the applicable one by examining headers.
      (while (and (null limits)
		  schemes
		  (setq scheme (cdr (car schemes)))
		  (not (setq
			limits
			(mc-message-delimiter-positions
			 (cdr (assoc 'msg-begin-line (funcall scheme)))
			 (cdr (assoc 'msg-end-line (funcall scheme)))))))
	(setq schemes (cdr schemes)))
      
      (if (null limits)
	  (error "Found no encrypted message in this buffer.")
	(run-hooks 'mc-pre-decryption-hook)
	(let ((resultval (funcall (cdr (assoc 'decryption-func
					      (funcall scheme))) 
				  (car limits) (cdr limits))))
	  (goto-char (point-min))
	  (if (car resultval) ; decryption succeeded
	      (run-hooks 'mc-post-decryption-hook))
	  resultval)))))
;;}}}  
;;{{{ Signing
(defun mc-sign (arg)
  "*Sign a message in the current buffer.

Exact behavior depends on current major mode.

With one prefix arg, prompts for private key to use, with two prefix args,
also prompts for encryption scheme to use.  With negative prefix arg,
inhibits clearsigning (pgp)."
  (interactive "p")
  (mc-sign-region arg nil nil))

(defun mc-sign-region (arg start end)
  "*Sign the current region."
  (interactive "p\nr")
  (let* ((mode-alist (cdr-safe (assq major-mode mc-modes-alist)))
	 (func (or (cdr-safe (assq 'sign mode-alist))
		   'mc-sign-generic))
	 from scheme)
    (if (>= arg 16)
	(setq scheme
	      (cdr (assoc
		    (completing-read "Encryption Scheme: " mc-schemes)
		    mc-schemes))))
    (if (>= arg 4)
	(setq from (read-string "User ID: ")))

    (funcall func from scheme start end (< arg 0))))

(defun mc-sign-generic (withkey scheme start end unclearsig)
  (or scheme (setq scheme mc-default-scheme))
  (or start (setq start (point-min-marker)))
  (or (markerp start) (setq start (copy-marker start)))
  (or end (setq end (point-max-marker)))
  (or (markerp end) (setq end (copy-marker end)))
  (run-hooks 'mc-pre-signature-hook)
  (if (funcall (cdr (assoc 'signing-func (funcall scheme)))
	       start end withkey unclearsig)
      (progn
	(run-hooks 'mc-post-signature-hook)
	t)))

(defun mc-sign-message (&optional withkey scheme start end unclearsig)
  "Clear sign the message."
  (save-excursion
    (let ((headers-end (mc-find-headers-end)))
      (or withkey
	  (progn
	    (goto-char (point-min))
	    (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator) "\n"))
	    (save-restriction
	      (narrow-to-region (point) headers-end)
	      (setq withkey (mail-fetch-field "From")))))
      (or start (setq start headers-end))
      (or end (setq end (point-max-marker)))
      (mc-sign-generic withkey scheme start end unclearsig))))

;;}}}
;;{{{ Signature verification

(defun mc-verify ()
  "*Verify a message in the current buffer.

Exact behavior depends on current major mode."
  (interactive)
  (let* ((mode-alist (cdr-safe (assq major-mode mc-modes-alist)))
	 (func (or (cdr-safe (assq 'verify mode-alist))
		   'mc-verify-signature)))
    (funcall func)))

(defun mc-verify-signature ()
  "*Verify the signature of the signed message in the current buffer.
Show the result as a message in the minibuffer. Returns t if the signature
is verified."
  (save-excursion
    (let ((schemes mc-schemes)
	  limits 
	  (scheme mc-default-scheme))

      ; Attempt to find a message signed according to the default
      ; scheme.
      (if mc-default-scheme
	  (setq
	   limits
	   (mc-message-delimiter-positions
	    (cdr (assoc 'signed-begin-line (funcall mc-default-scheme)))
	    (cdr (assoc 'signed-end-line (funcall mc-default-scheme))))))

      ; We can't find a message signed in the default scheme.
      ; Step through all the schemes we know, trying to identify
      ; the applicable one by examining headers.
      (while (and (null limits)
		  schemes
		  (setq scheme (cdr (car schemes)))
		  (not
		   (setq
		    limits
		    (mc-message-delimiter-positions
		     (cdr (assoc 'signed-begin-line (funcall scheme)))
		     (cdr (assoc 'signed-end-line (funcall scheme)))))))
	(setq schemes (cdr schemes)))

      (if (null limits)
	  (error "Found no signed message in this buffer.")
	(funcall (cdr (assoc 'verification-func (funcall scheme)))
		 (car limits) (cdr limits))))))


;;}}}
;;{{{ Key management

;;{{{ mc-insert-public-key

(defun mc-insert-public-key (&optional userid scheme)
  "*Insert your public key at point.
With one prefix arg, prompts for user id to use. With two prefix
args, prompts for encryption scheme."
  (interactive
   (let (arglist)
     (if (not (and (listp current-prefix-arg)
		   (numberp (car current-prefix-arg))))
	 nil
       (if (>= (car current-prefix-arg) 16)
	   (setq arglist
		 (cons (cdr (assoc (completing-read "Encryption Scheme: "
						    mc-schemes)
				   mc-schemes))
		       arglist)))
       (if (>= (car current-prefix-arg) 4)
	   (setq arglist (cons (read-string "User ID: ") arglist))))
     arglist))

;  (if (< (point) (mc-find-headers-end))
;      (error "Can't insert key inside message header"))
  (or scheme (setq scheme mc-default-scheme))
  (or userid (setq userid (cdr (assoc 'user-id (funcall scheme)))))
    
  ;; (goto-char (point-max))
  (if (not (bolp))
      (insert "\n"))
  (funcall (cdr (assoc 'key-insertion-func (funcall scheme))) userid))

;;}}}
;;{{{ mc-snarf-keys

(defun mc-snarf ()
  "*Add all public keys in the buffer to your keyring.

Exact behavior depends on current major mode."
  (interactive)
  (let* ((mode-alist (cdr-safe (assq major-mode mc-modes-alist)))
	 (func (or (cdr-safe (assq 'snarf mode-alist))
		   'mc-snarf-keys)))
    (funcall func)))

(defun mc-snarf-keys ()
  "*Add all public keys in the buffer to your keyring."
  (interactive)
  (let ((schemes mc-schemes)
	(start (point-min))
	(found 0)
	limits 
	(scheme mc-default-scheme))
    (save-excursion
      (catch 'done
	(while t

	  ; Attempt to find a message signed according to the default
	  ; scheme.
	  (if mc-default-scheme
	      (setq
	       limits
	       (mc-message-delimiter-positions
		(cdr (assoc 'key-begin-line (funcall mc-default-scheme)))
		(cdr (assoc 'key-end-line (funcall mc-default-scheme)))
		start)))
	  ; We can't find a message signed in the default scheme.
	  ; Step through all the schemes we know, trying to identify
	  ; the applicable one by examining headers.
	  (while (and (null limits)
		      schemes
		      (setq scheme (cdr (car schemes)))
		      (not
		       (setq
			limits
			(mc-message-delimiter-positions
			 (cdr (assoc 'key-begin-line (funcall scheme)))
			 (cdr (assoc 'key-end-line (funcall scheme)))
			 start))))
	    (setq schemes (cdr schemes)))
	  (if (null limits)
	      (throw 'done found)
	    (setq start (cdr limits))
	    (setq found (+ found (funcall (cdr (assoc 'snarf-func
						      (funcall scheme))) 
					  (car limits) (cdr limits)))))))
      (message (format "%d new key%s found" found
		       (if (eq 1 found) "" "s"))))))
;;}}}
;;}}}
;;{{{ Mode specific functions

;;{{{ RMAIL
(defun mc-rmail-summary-verify-signature ()
  "*Verify the signature in the current message."
  (interactive)
  (if (not (eq major-mode 'rmail-summary-mode))
      (error
       "mc-rmail-summary-verify-signature called in inappropriate buffer"))
  (save-excursion
    (set-buffer rmail-buffer)
    (mc-verify)))

(defun mc-rmail-summary-decrypt-message ()
  "*Decrypt the contents of this message"
  (interactive)
  (if (not (eq major-mode 'rmail-summary-mode))
      (error
       "mc-rmail-summary-decrypt-message called in inappropriate buffer"))
  (save-excursion
    (set-buffer rmail-buffer)
    (mc-decrypt)))

(defun mc-rmail-summary-snarf-keys ()
  "*Adds keys from current message to public key ring"
  (interactive)
  (if (not (eq major-mode 'rmail-summary-mode))
      (error
       "mc-rmail-summary-snarf-keys called in inappropriate buffer"))
  (save-excursion
    (set-buffer rmail-buffer)
    (mc-snarf)))

(defun mc-rmail-verify-signature ()
  "*Verify the signature in the current message."
  (interactive)
  (if (not (equal mode-name "RMAIL"))
      (error "mc-rmail-verify-signature called in a non-RMAIL buffer"))
  ;; Hack to load rmailkwd before verifying sig
  (rmail-add-label "verified")
  (rmail-kill-label "verified")
  (if (mc-verify-signature)
      (rmail-add-label "verified")))

(defun mc-rmail-decrypt-message ()
  "*Decrypt the contents of this message"
  (interactive)
  (let (decryption-result)
    (if (not (equal mode-name "RMAIL"))
	(error "mc-rmail-decrypt-message called in a non-RMAIL buffer"))
    (unwind-protect
	(progn
	  (rmail-edit-current-message)
	  (setq decryption-result (mc-decrypt-message))
	  (cond ((not (car decryption-result))
		 (rmail-abort-edit))
		((and (not (eq mc-always-replace 'never))
		      (or mc-always-replace
			  (y-or-n-p
			   "Replace encrypted message with decrypted? ")))
		 (rmail-cease-edit)
		 (rmail-kill-label "edited")
		 (rmail-add-label "decrypted")
		 (if (cdr decryption-result)
		     (rmail-add-label "verified")))
		(t
		 (let ((tmp (generate-new-buffer "*Mailcrypt Viewing*")))
		   (copy-to-buffer tmp (point-min) (point-max))
		   (rmail-abort-edit)
		   (switch-to-buffer tmp t)
		   (goto-char (point-min))
		   (insert "From Mailcrypt-" mc-version " "
			   (current-time-string) "\n")
		   (rmail-convert-file)
		   (rmail-mode)
		   (use-local-map (copy-keymap (current-local-map)))
		   (local-set-key "q" 'mc-rmail-view-quit)
		   (set-buffer-modified-p nil)
		   (rmail-add-label "decrypted")
		   (if (cdr decryption-result)
		       (rmail-add-label "verified"))))))
      (if (eq major-mode 'rmail-edit-mode)
	  (rmail-abort-edit)))))

(defun mc-rmail-view-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (set-buffer-modified-p nil)
    (rmail-quit)
    (kill-buffer buf)))

;;}}}
;;{{{ VM
(defun mc-vm-verify-signature ()
  "*Verify the signature in the current VM message"
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    (vm-widen-page)
    (mc-verify-signature)))

(defun mc-vm-decrypt-message ()
  "*Decrypt the contents of the current VM message"
  (interactive)
  (let ((vm-frame-per-edit nil)
	from-line)
    (if (interactive-p)
	(vm-follow-summary-cursor))
;   (vm-select-folder-buffer) ;; TNX Eric C. Newton for commenting out.
    (vm-check-for-killed-summary)
    (vm-error-if-folder-read-only)
    (vm-error-if-folder-empty)

    ;; store away a valid "From " line for possible later use.
    (setq from-line (vm-leading-message-separator))
    (vm-edit-message)
    (cond ((not (condition-case condition-data
		    (car (mc-decrypt-message))
		  (error
		   (vm-edit-message-abort)
		   (error (message "Decryption failed: %s" 
				   (car (cdr condition-data)))))))
           (vm-edit-message-abort)
	   (error "Decryption failed."))
	  ((and (not (eq mc-always-replace 'never))
		(or mc-always-replace
		    (y-or-n-p "Replace encrypted message with decrypted? ")))
	   (let ((this-command 'vm-edit-message-end))
	     (vm-edit-message-end)))
          (t
           (let ((tmp (generate-new-buffer "*Mailcrypt Viewing*")))
             (copy-to-buffer tmp (point-min) (point-max))
             (vm-edit-message-abort)
             (switch-to-buffer tmp t)
	     (goto-char (point-min))
	     (insert from-line)	     
	     (set-buffer-modified-p nil)
	     (vm-mode t))))))

(defun mc-vm-snarf-keys ()
  "*Snarf public key from the contents of the current VM message"
  (interactive)
  (if (interactive-p)
      (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (save-restriction
    (vm-widen-page)
    (mc-snarf-keys)))

;;}}}
;;{{{ GNUS

(defun mc-gnus-verify-signature ()
  (interactive)
  (gnus-summary-select-article t)
  (save-excursion
    (set-buffer gnus-original-article-buffer)
    (save-restriction (widen) (mc-verify-signature))))

(defun mc-gnus-snarf-keys ()
  (interactive)
  (gnus-summary-select-article t)
  (gnus-eval-in-buffer-window gnus-original-article-buffer
    (save-restriction (widen) (mc-snarf-keys))))

(defun mc-gnus-decrypt-message ()
  (interactive)
  (gnus-summary-select-article t)
  ;; Gnus 5 has the string "Gnus" instead of "GNUS" in gnus-version.
  (if (not (let ((case-fold-search nil))
	     (string-match "Gnus" gnus-version)))
      (gnus-eval-in-buffer-window
       gnus-article-buffer
       (save-restriction (widen) (mc-decrypt-message)))
    ;; Gnus 5 allows editing of articles.  (Actually, it makes a great
    ;; mail reader.)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (gnus-summary-edit-article t)
      (save-restriction
	(widen)
	(cond ((not (car (mc-decrypt-message)))
	       (gnus-summary-edit-article-postpone))
	      ((and (not (gnus-group-read-only-p))
		    (not (eq mc-always-replace 'never))
		    (or mc-always-replace
			(y-or-n-p
			 "Replace encrypted message on disk? ")))
	       (gnus-summary-edit-article-done))
	      (t
	       (gnus-summary-edit-article-postpone)))))))

;;}}}		
;;{{{ MH
(defvar mc-mh-backup-msg 3
  "If 0, never back up MH messages.  If 3, always back up messages.")

(defun mc-mh-decrypt-message ()
  "Decrypt the contents of the current MH message in the show buffer."
  (interactive "P")
  (let* ((msg (mh-get-msg-num t))
	 (msg-filename (mh-msg-filename msg))
	 (show-buffer (get-buffer mh-show-buffer))
	 decrypt-okay decrypt-on-disk)
    (setq
     decrypt-on-disk
     (and (not (eq mc-always-replace 'never))
	  (or mc-always-replace
	      (y-or-n-p "Replace encrypted message on disk? "))))
    (if decrypt-on-disk
	(progn
	  (save-excursion
	    (set-buffer (create-file-buffer msg-filename))
	    (insert-file-contents msg-filename t)
	    (if (setq decrypt-okay (car (mc-decrypt-message)))
		(save-buffer mc-mh-backup-msg)
	      (message "Decryption failed.")
	      (set-buffer-modified-p nil))
	    (kill-buffer nil))
	  (if decrypt-okay
	      (if (and show-buffer
		       (equal msg-filename (buffer-file-name show-buffer)))
		  (save-excursion
		    (save-window-excursion
		      (mh-invalidate-show-buffer)))))
	  (mh-show msg))
      (mh-show msg)
      (save-excursion
        (set-buffer mh-show-buffer)
        (let ((read-only buffer-read-only))
          (unwind-protect
              (progn
                (setq buffer-read-only nil)
                (if (setq decrypt-okay (car (mc-decrypt-message)))
                    (progn
                      (goto-char (point-min))
                      (set-buffer-modified-p nil))
                  (message "Decryption failed.")))
            (setq buffer-read-only read-only)
            )))
      (if (not decrypt-okay)
	  (progn
	    (mh-invalidate-show-buffer)
	    (mh-show msg))))))

(defun mc-mh-verify-signature ()
  "*Verify the signature in the current MH message."
  (interactive)
  (mh-show)
  (mh-in-show-buffer (mh-show-buffer)
    (mc-verify-signature)))
    

(defun mc-mh-snarf-keys ()
  (interactive)
  (mh-show)
  (mh-in-show-buffer (mh-show-buffer)
    (mc-snarf-keys)))

;;}}}

;;{{{ mew
;; decrypt, verify, snarf, encrypt, sign
;; in summary mode, decrypt/verify/snarf must switch to correct buffer first
;; in correct buffer, generic functions are probably sufficient.
;;  mew doesn't have any labels to be added (though I really wish it did)
;;  could have a "replace encrypted with decrypted" hook for decrypt

;; autoloads:
;;  mew-message-mode
;;   (add-hook 'mew-message-mode-hook 'mc-install-read-mode)
;;  mew-summary-mode
;;   (add-hook 'mew-summary-mode-hook 'mc-install-read-mode)
;;  mew-draft-mode
;;   (add-hook 'mew-draft-mode-hook 'mc-install-write-mode)

(defun mc-mew-summary-decrypt-message()
  "*Decrypt the current message"
  (interactive)
  (if (not (eq major-mode 'mew-summary-mode))
      (error
       "mc-mew-summary-decrypt-message called in inappropriate buffer"))
  (save-excursion
    (mew-summary-display t)
    (set-buffer (mew-buffer-message))
    (mc-decrypt)
))

(defun mc-mew-summary-verify-signature()
  "*Verify the signature in the current message."
  (interactive)
  (if (not (eq major-mode 'mew-summary-mode))
      (error
       "mc-mew-summary-verify-signature called in inappropriate buffer"))
  (save-excursion
    (mew-summary-display t)
    (set-buffer (mew-buffer-message))
    (mc-verify)
))

(defun mc-mew-summary-snarf-keys()
  "*Add keys from the current message to the public keyring."
  (interactive)
  (if (not (eq major-mode 'mew-summary-mode))
      (error
       "mc-mew-summary-snarf-keys called in inappropriate buffer"))
  (save-excursion
    (mew-summary-display t)
    (set-buffer (mew-buffer-message))
    (mc-snarf)
))

(defun mc-mew-decrypt-message ()
  "*Decrypt the contents of this message."
  ;; This is a hack to deal with the fact that mew-message buffers are
  ;; generally read-only. For now, there is no option to replace the
  ;; encrypted message in-place; it simply disappears when you move to a
  ;; different message.
  (interactive)
  (let ((read-only buffer-read-only))
    (unwind-protect
        (save-excursion
          (setq buffer-read-only nil)
          (mc-decrypt-message)
          )
      (setq buffer-read-only read-only)
      )
))

;;}}}

;;}}}

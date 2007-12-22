
(setq load-path (append '("..") load-path))
(load-library "mailcrypt")
(load-library "mc-toplev")
(load-library "mc-gpg")
(setq mc-gpg-extra-args '("--homedir" "gpg-keys/exported"))
(setq mc-test-testcasedir "gpg-testcases")
(setq mc-gpg-always-fetch 'never)

(defvar mc-test-verbose nil)

(defun mc-test-generate-plaintext (long)
  (if long
      "This is a test message."
    "This is a long test message."
    ))

(defun mc-test-encrypt ()
  (let ((b (get-buffer-create "mc plaintext"))
	(recipients '("owner1"))
	(mc-default-scheme 'mc-scheme-gpg)
	(mc-pgp-always-sign 'never)
	)
    (set-buffer b)
    (erase-buffer)
    (insert (mc-test-generate-plaintext nil))
    (mc-gpg-encrypt-region recipients (point-min-marker) (point-max-marker))
    )
  )

;; this one replaces the normal version when testing. mc-activate-passwd-count
;; must be set to 0 by the enclosing function. mc-activate-passwd-alist must
;; be set to an alist of user ids and passphrases.
(defun mc-activate-passwd (id &optional prompt)
  (let (pw)
    (setq mc-activate-passwd-count (+ mc-activate-passwd-count 1))
    (if (> mc-activate-passwd-count 5)
	(error "mc-activate-passwd looping forever, id '%s'" id))
    (if mc-test-verbose
        (message "activate-passwd id'%s' prompt'%s'" id prompt))
    (setq pw (assoc id mc-test-passwd-alist))
    (if pw
	(cdr pw)
      (message "don't know passphrase, using 'unknown'. alist is %s"
	       mc-test-passwd-alist)
      "unknown"
      )
    ))
  
;; this one replaces the normal verion. mc-message-sigstatus-text should
;; be defined in the enclosing function.
(defun mc-message-sigstatus (id &optional attention)
  (setq mc-message-sigstatus-text id))

;; test cases are perl-generated elisp alists. The keys are:
;;  name: the name of this test case
;;  crypttext: a string with the multi-line encrypted message
;;
;;  encryption_id: a string with the user-id that mailcrypt ought to ask for
;;                 This must be the exact text.
;;  passphrase: the passphrase for that key, nil if harness should use 'bogus'
;;
;;  error: a string with the error mailcrypt is supposed to emit
;;  plaintext: the string it is supposed to decrypt to, or 'nil' for error
;;  signature_status: mailcrypt should emit this status message

(defun mc-test-load-testcase (file)
  (let (testcase)
    (setq testcase
	  (with-temp-buffer
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (read (current-buffer))
	    ))
    (if mc-test-verbose
        (message "testcase name is %s" (cdr (assoc 'name testcase))))
    testcase)
  )


(defun mc-test-error (fmt &rest args)
  (apply 'message fmt args))

(defun mc-test-decrypt-test (file)
  (let (
	(mc-activate-passwd-func 'mc-test-activate-passwd)
	(mc-activate-passwd-count 0)
	(mc-test-passwd-queries '())
	(mc-message-func 'mc-test-message)
	(mc-test-messages '())
	(mc-message-sigstatus-text nil)
        (testcase-file (expand-file-name file mc-test-testcasedir))
	b testcase mc-test-passwd-alist errortext sigstatus rc
	expected-error expected-plaintext expected-sigstatus)

    (setq b (get-buffer-create "mc crypttext"))
    (message "Testing %s ..." file)
    (if mc-test-verbose
        (message "testing with case %s" testcase-file))
    (setq testcase (mc-test-load-testcase testcase-file))


    (setq mc-test-passwd-alist 
	  (list 
	   (cons (cdr (assoc 'encryption_id testcase))
		 (cdr (assoc 'passphrase testcase))
		 )))
	 
    (set-buffer b)
    (erase-buffer)
    (insert (cdr (assoc 'crypttext testcase))) ; insert crypttext

    ;; attempt decryption. If a passphrase is requested, it will use the one
    ;; from the testcase file.

    (condition-case err
	;; protected form
	(setq rc
	      (mc-gpg-decrypt-region (point-min-marker) (point-max-marker)))
      ;; error handler
      (error
       (setq errortext (error-message-string err))
       )
      )

    ; check assorted status stuff
    (if mc-test-verbose
        (message "errortext was '%s'" errortext))
    (if mc-test-verbose
        (message "rc was '%s'" rc))
    (if mc-message-sigstatus-text
	(setq sigstatus mc-message-sigstatus-text))

    ;; did we expect an error?
    (setq expected-error (cdr (assoc 'error testcase)))
    (if expected-error
	(if (not (equal errortext expected-error))
	    (error "errortext did not match: expected '%s', got '%s'"
		   expected-error errortext)
	  )
      (if errortext
	  (error "got unexpected error '%s'" errortext))
      )


    ; was the decryption supposed to be successful?
    (setq expected-plaintext (cdr (assoc 'plaintext testcase)))
    (if expected-plaintext
	(if (not (equal expected-plaintext (buffer-string)))
	    (error "plaintext did not match: expected '%s', got '%s'"
		   expected-plaintext (buffer-string))
	  )
      )

    (setq expected-sigstatus (cdr (assoc 'signature_status testcase)))
    (if expected-sigstatus
	(if (not (equal expected-sigstatus sigstatus))
	    (error "sigstatus did not match: expected '%s', got '%s'"
		   expected-sigstatus sigstatus)
	  )
      (if sigstatus
	  (error "unexpected signature status '%s'" sigstatus))
      )

    (message " test %s passed" file)
))

; error works liks this:
;(defun error (&rest args)
;    (signal 'error (list (apply 'format args)))))

(defun run-one-test ()
  ; it would be nice to take the test name from argv. see (command-line-args)
  (setq mc-test-verbose t)
  (mc-test-decrypt-test "SE")
)

(defun run-all-tests ()
  (let (cases)
        
    (setq cases (append cases '("E.e1r" "E.e2r" "E.e3" "E.e4")))
    (setq cases (append cases '("ES.e1r.s1v" "ES.e1r.s2v" "ES.e1r.s3v"
                                "ES.e1r.s4" "ES.e3.s1v" "ES.e4.s1v")
                       ))
    (setq cases (append cases '("S.s1v" "S.s2v" "S.s3v" "S.s4")))
    (setq cases (append cases '("CS.s1v" "CS.s2v" "CS.s3v" "CS.s4")))
    (setq cases (append cases '("SE")))
    (dolist (onecase cases)
      (mc-test-decrypt-test onecase))
))

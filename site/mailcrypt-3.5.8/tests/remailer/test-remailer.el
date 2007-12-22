
(setq load-path (append '("../..") load-path))
(load-library "mailcrypt")
(load-library "mc-toplev")
(load-library "mc-gpg")
(load-library "mc-remail")

(defvar mc-test-verbose nil)

(defun mc-test-generate-plaintext (long)
  (if long
      "This is a long test message."
    "This is a test message."
    ))

;; to run an automated test of the remailer, we need to do the following:
;;  generate a plaintext message, to a given recipient, in a Message buffer
;;  pick a remailer chain
;;  encrypt the message to that chain
;;  hand the plaintext, recipient, chain, and crypttext to ./unwind.py
;;  verify that unwind.py exited successfully
;; The remailer chain can pick from three fake remailers: rem[123]@test.test

(defun mc-test-remailer-unwind (chain)
  ;; call this in the encrypted message buffer
  (let ((chainstring "") start end errbuf)
    ;; find message body
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (setq start (point))
    (setq end (point-max))

    (setq errbuf (get-buffer-create "mc-errors"))

    ;; build chainstring
    (dolist (hop chain)
      (let* ((addr (mc-remailer-address hop))
             (stripped (substring addr 1 -1))
             )
        (setq chainstring (concat chainstring stripped ","))
        ))
    (setq chainstring (substring chainstring 0 -1))
    ;; "rem1@test.test,rem2@test.test,rem3@test.test"

    ;; spawn unwind.py, pipe crypttext to stdin
    ;; note that we call-process with "python" "unwind.py". Removing the
    ;; explicit invocation of python requires an absolute path to unwind.py,
    ;; and that would make it annoying to run this on other systems.
    ;; I tried cmd=./unwind.py but it didn't work. Passing it through a shell
    ;; caused other problems.
    (call-process-region start end
                         "python"
                         nil  ; DELETE: don't delete text as it is sent
                         errbuf ;; DESTINATION log stdout/stderr
                         nil  ; DISPLAY: don't update display
                         ;; args
                         "unwind.py"
                         "user@test.test" ; recipient
                         chainstring
                         "test subject"
                         )
    ))



(defun mc-test-encrypt-remailer (chain-name)
  (let* ((b (get-buffer-create "mc plaintext"))
         (recipients '("user@test.test"))
         (mc-default-scheme 'mc-scheme-gpg)
         (mc-pgp-always-sign 'never)
         (mc-gpg-extra-args '("--homedir" "remailer-keys"))
         (mc-levien-file-name "rlist.txt")
         (mc-remailer-user-chains '( ("123" ["rem1" "rem2" "rem3"])))
         chains chain rc
	)

    (message "testing chain %s" chain-name)

    (set-buffer b)
    (erase-buffer)
    (insert "To: user@test.test\n")
    (insert "Subject: test subject\n")
    (insert "--text follows this line--\n")
    (insert "test message\n")
    (mail-mode)

    (mc-reread-levien-file)
    (setq chains (mc-remailer-make-chains-alist))
    (setq chain (mc-remailer-canonicalize-chain
                 (cdr (assoc chain-name chains)) chains))

    ;(mc-remailer-encrypt-for-chain)
    (mc-rewrite-for-chain chain)
    ;; crypttext is now in "mc plaintext" buffer, after --text follows etc--
    ;; chain is '("<rem1@test.test>" "<rem2@test.test>" "<rem3@test.test>").
    ;; recip is user@test.test.

    (setq rc (mc-test-remailer-unwind chain))
    (if (not (= rc 0))
        (error "got unexpected error, rc %d" rc)
      )
    (message " test with chain %s passed" chain-name)
    )
  )

(defun run-one-test ()
  ; it would be nice to take the test name from argv. see (command-line-args)
  (setq mc-test-verbose t)
  (mc-test-encrypt-remailer "123")
)

(defun run-all-tests ()
  (let (cases)
        
    (setq cases (append '("123") cases))
    (dolist (onecase cases)
      (mc-test-encrypt-remailer onecase))
))

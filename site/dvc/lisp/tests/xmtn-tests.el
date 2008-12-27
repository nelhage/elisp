;;; xmtn-tests.el --- Automated regression tests for xmtn

;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; Automated regression tests for xmtn.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

;; These tests require elunit.el from
;; http://dev.technomancy.us/phil/wiki/ElUnit .

(eval-and-compile
  (require 'cl)
  (require 'elunit)
  (require 'elp) ;; elp-elapsed-time is a 'defsubst', so we require elp at load time, not run time.
  (require 'xmtn-match)
  (require 'xmtn-dvc)
  (require 'dvc-tests-utils "tests/dvc-tests-utils.el"))

(defun xmtn-tests--keypair-string ()
  "[keypair xmtn-test]
MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDFE8/sRvdvN5+F5aFVpXeJpz0eKAzhYdWB
uW3L0C1tWnLk+HzYV13ewKMtFzwkoTeITTX5q372zH2XSIcUR2jBCArQf8Ru40886nLwG7zU
G1cI3B86akQknDUn3t9C1jEHXlBJiPLwaWrcmMFhoA+PnE49gopudw4q6Yhg1YCOqwIDAQAB#
MIICyTBDBgkqhkiG9w0BBQ0wNjAeBgkqhkiG9w0BBQwwEQQIccoCNMR2fIYCAggAAgEYMBQG
CCqGSIb3DQMHBAgjnJz0whELeQSCAoDEzuBbQf7hf43ULUZR7gFBrXilg+KBgItlA0Mz6jmI
0+LzoHhJiU3rnyR1MsXkf7uCBFje5Uqj53vUrnrBbxgGBFHwOw1Kic+lbDtvAKlNLPPPl9h8
W9QrQYhEg9VsmYBUvxZnyw5Kmafpmh1wC/fRSchDmWyhUeJHtkZhnUgcG9OFi6z8JT64/VGw
ZhB46Q2dGLrygjHRArA8FIOX5dlGzyRNfa0w5dVWZED7IcQVCoBLwLiEb9woK+fyEuK12fM+
23U8/sAO74MMOoyvs+OoloPtgniHuRdc/1RV9CS9k64mnzJdOnhR/GxQIL36LZcNrHvnM9Nn
xrK2yDkuk39JcLDJlFPZok7vluEn1GCKKGce3Z2LP6VPTJAqBHgt1fTMBAT5bc7rbVQxzVEU
56anNOMR1T9MRnbX5u5Hpj5mNIqbWX+g3YCIgKIJXbtD57GixPP4s/mP2EcAAeZvWiGeTF6Z
GyNq8USmlEjXpMrIWqLk+f6OzDyvk05sTQByRlKwOGzgbyNnWsetKC97wFfsBExNKhKeFFTV
6HOehUEPHIrikNaLed52czpqaKcQ67uVfdWXs3drwS7V0RRtTdcAzy0u95bERPrRpCY3tq/a
CGp3K4RF00eJQLBa94D9LYIEMBk4evfKCijcId0b4kzIQS1SI1sytnt+P1zPQaV5yAetOOD/
fuHfnYU27Mqis5V23xo1ibjDS1fa3/E6XK2P+Y3rHuyjQ/QbFlcBwj0vjv8yqwRWOe5y6Msd
f6S7jhNd76i/o3K/DmnpnI1N8RODAd77uejpe8K0xthzk2q02VtrBXA7jpY7oSaIaKJPov6v
YPFoLxe1V5oOyoe3ap0H
[end]")

(defun xmtn-tests--default-rc-file ()
  ;; Monotone versions up to and including 0.33 don't allow empty
  ;; passphrases.
  "function get_passphrase(keypair_id) return \"a\" end")

;;; This is preferable over seperate set-up and tear-down functions
;;; since it allows us to make use of `unwind-protect' and dynamic
;;; bindings.

(defun xmtn-tests--call-with-test-environment (xmtn--body)
  (lexical-let ((body xmtn--body))
    (lexical-let ((temp-dir nil))
      (unwind-protect
          (progn
            (setq temp-dir (file-name-as-directory
                            (xmtn--make-temp-file "xmtn-tests-" t)))
            (lexical-let ((key-dir (concat temp-dir "keys/"))
                          (rc-file (concat temp-dir "rc")))
              (let* ((default-directory temp-dir)
                     (dvc-test-mode t)
                     (xmtn-additional-arguments
                      `("--db" ,(concat temp-dir "a.mtn")
                        "--keydir" ,key-dir
                        "--norc"
                        "--rcfile" ,rc-file)))
                (make-directory key-dir)
                (with-temp-file (concat key-dir "xmtn-tests")
                  (insert (xmtn-tests--keypair-string) ?\n))
                (with-temp-file rc-file
                  (insert (xmtn-tests--default-rc-file) ?\n))
                (xmtn--run-command-sync nil '("db" "init"))
                (xmtn--run-command-sync nil '("setup"
                                              "--branch" "invalid.xmtn-tests"
                                              "workspace"))
                (let ((default-directory (concat temp-dir "workspace/")))
                  (funcall body
                           :root default-directory)))))
        (when temp-dir
          (dired-delete-file temp-dir 'always))))))

(defun xmtn-tests--call-with-test-history (xmtn--body)
  (lexical-let ((body xmtn--body))
    (xmtn-tests--call-with-test-environment
     (function*
      (lambda (&key ((:root xmtn--root)))
        (lexical-let ((root xmtn--root)
                      (file-name "file-1")
                      revision-1
                      revision-2)
          (with-temp-file file-name (insert "a\n"))
          (xmtn--add-files root (list file-name))
          (xmtn--run-command-sync root `("commit" "--message=commit 1"))
          (setq revision-1 (xmtn--get-base-revision-hash-id root))
          (with-temp-file file-name (insert "b\n"))
          (xmtn--run-command-sync root `("commit" "--message=commit 2"))
          (setq revision-2 (xmtn--get-base-revision-hash-id root))
          (funcall body
                   :root root
                   :file-name file-name
                   :revision-1 revision-1
                   :revision-2 revision-2)))))))

(defmacro* xmtn-tests--with-test-environment ((&rest keys) &body body)
  (declare (indent 1) (debug (sexp body)))
  `(xmtn-tests--call-with-test-environment (function* (lambda (,@keys) ,@body))))

(defmacro* xmtn-tests--with-test-history ((&rest keys) &body body)
  (declare (indent 1) (debug (sexp body)))
  `(xmtn-tests--call-with-test-history (function* (lambda (,@keys) ,@body))))


(defsuite xmtn
  (xmtn--match
   (progn
     (assert (xmtn-match--match-variable-p '$x ?$))
     (assert (xmtn-match--match-variable-p '@x ?@))
     (assert (not (xmtn-match--match-variable-p "$x" ?$)))
     (assert (not (xmtn-match--match-variable-p 'x ?$)))
     (assert (xmtn-match--contains-match-variable-p '$x ?$))
     (assert (xmtn-match--contains-match-variable-p '(a b $x c) ?$))
     (assert (xmtn-match--contains-match-variable-p '[a $y $z c] ?$))
     (assert (xmtn-match--contains-match-variable-p '(nil . $y) ?$))
     (assert (xmtn-match--contains-match-variable-p '((() $a)) ?$))
     (assert (not (xmtn-match--contains-match-variable-p 'x ?$)))
     (assert (not (xmtn-match--contains-match-variable-p '(a . b) ?$)))
     (assert (not (xmtn-match--contains-match-variable-p nil ?$)))
     (assert (not (xmtn-match--contains-match-variable-p '((() ())) ?$)))
     (assert (not (xmtn-match--contains-match-variable-p nil ?$)))
     (assert (equal (xmtn-match '(a b)
                      (($y $y) nil)
                      ($z z))
                    '(a b)))
     (assert (equal (xmtn-match '(a a)
                      (($y $y) y))
                    'a))
     (assert (equal (xmtn-match '(a b)
                      ($z z)
                      ($z nil))
                    '(a b)))
     (assert (xmtn-match nil ([t $y] y) ($z t)))
     (assert (xmtn-match [foo bar] ([foo $y] y)))
     (assert (xmtn-match [foo bar] ((a . b) nil) ([foo bar] t)))
     (assert (xmtn-match nil (nil t)))))
  (xmtn--version-case
   (flet ((xmtn--latest-mtn-release ()  ;flet has dynamic scope in Emacs Lisp
                                    '(2 5 "y")))
     (let* ((xmtn-executable 'xmtn-dummy)
            (xmtn--*command-version-cached-for-executable* xmtn-executable))
       (let ((xmtn--*cached-command-version* '(2 5 "x")))
         (assert
          (xmtn--version-case
           ((and (= 2 5) (>= 2 5) (or (= 2 4) (<= 3 0))
                 (<= 2 6) (/= 1 5) (not (/= 2 5))
                 (not (>= 2 6))
                 (not (<= 2 4))
                 (not (< 2 5))
                 (not (< 2 4))) t)
           (t nil)))
         (assert
          (not (ignore-errors
                 (xmtn--version-case
                  (nil t)))))
         (assert (xmtn--version-case ((mainline> 2 4) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 2 5) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 2 6) nil) (t t))))
       (let ((xmtn--*cached-command-version* '(2 5 "y")))
         (assert (xmtn--version-case ((mainline> 2 4) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 2 5) nil) (t t)))
         (assert (xmtn--version-case ((mainline> 2 6) nil) (t t))))
       (let ((xmtn--*cached-command-version* '(1 5 "w")))
         (assert (xmtn--version-case ((mainline> 2 4) nil) (t t)))
         (assert (xmtn--version-case ((mainline> 2 5) nil) (t t)))
         (assert (xmtn--version-case ((mainline> 1 4) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 1 5) nil) (t t))))
       (let ((xmtn--*cached-command-version* '(2 6 "z")))
         (assert (xmtn--version-case ((mainline> 2 4) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 2 5) t) (t nil)))
         (assert (xmtn--version-case ((mainline> 2 6) nil) (t t)))))))
  (log
   (save-window-excursion
     (xmtn-tests--with-test-history (&key &allow-other-keys)
       ;; The test is simply that this doesn't crash.
       (dvc-log)
       (dvc-revlist-show-item))))
  (file-diff
   ;; The test is simply that this doesn't crash.
   (save-window-excursion
     (xmtn-tests--with-test-history (&key file-name &allow-other-keys)
       (find-file file-name)
       (unwind-protect
           (progn
             (insert "x")
             (save-excursion
               (call-interactively #'dvc-file-diff)))
         (revert-buffer t t)))))
  (diff
   ;; The test is simply that this doesn't crash.
   (save-window-excursion
     (xmtn-tests--with-test-history (&key file-name &allow-other-keys)
       (find-file file-name)
       (let ((buffer (current-buffer)))
         (unwind-protect
             (progn
               (insert "x")
               (write-region (point-min) (point-max)
                             file-name nil 'no-message nil nil)
               (set-buffer-modified-p nil)
               (call-interactively #'dvc-diff))
           (dvc-tests-wait-async)
           (with-current-buffer buffer
             (set-buffer-modified-p nil)
             (kill-buffer buffer)))))))
  (automate-buffer-numbering
   (xmtn-tests--with-test-history (&key root &allow-other-keys)
     (xmtn-automate-with-session (session root)
       (xmtn-automate-with-command (handle-1 session '("graph") :may-kill-p t)
         (sleep-for 0.5)
         (xmtn-automate-terminate-processes-in-root root)
         (xmtn-automate-with-command (handle-2 session '("graph")
                                               :may-kill-p nil)
           (assert (not (equal (xmtn-automate-command-buffer handle-1)
                               (xmtn-automate-command-buffer handle-2))))
           (xmtn-automate-command-wait-until-finished handle-2))))))
  (automate-several-commands
   (xmtn-tests--with-test-history (&key root &allow-other-keys)
     ;; The test is simply that this doesn't crash.
     (xmtn-automate-with-session (session root)
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p nil))
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p nil))
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p nil))
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p nil))
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p t))
       (xmtn-automate-with-command (cmd session '("graph") :may-kill-p t)
         ;;(xmtn-automate-command-wait-until-finished cmd)
         ))
     ;; Try to delay deletion of our temp workspace until process has
     ;; terminated.
     (sleep-for 1)))
  (non-ascii-file-name
   (let ((umlaut (string (make-char 'latin-iso8859-1 #xe4)))) ; umlaut a
     (xmtn-tests--with-test-environment (&key root)
       (let ((file-name umlaut))
         (let ((file-name-coding-system 'utf-8)) ; not sure about this...
           (with-temp-file file-name             ; create empty file
             (progn)))
         (xmtn--add-files root (list file-name))
         (let ((manifest (xmtn--get-manifest root `(local-tree ,root))))
           (xmtn-match manifest
             (((dir "") (file $file-name-here $hash-id $attributes))
              (assert (equal file-name-here file-name) t)
              (assert (endp attributes)))))
         ;; Check whether xmtn-automate encodes the file name
         ;; correctly when passing it to monotone.  The actual command
         ;; doesn't matter as much as the fact that monotone receives
         ;; it correctly.
         (xmtn--with-automate-command-output-basic-io-parser
             (next-stanza root (xmtn--version-case
                                ((mainline> 0 35) `("get_attributes" ,file-name))
                                (t `("attributes" ,file-name))))
           (xmtn-match (funcall next-stanza)
             ((("format_version" (string "1")))))
           (assert (null (funcall next-stanza)) t))))))
  (non-ascii-file-contents
   (let ((umlaut (string (make-char 'latin-iso8859-1 #xe4)))) ; umlaut a
     (xmtn-tests--with-test-environment (&key root)
       (let ((file-name "foo")
             (contents (concat umlaut "\n"))
             (coding-system 'iso-8859-1-unix))
         (with-temp-file file-name
           (setq buffer-file-coding-system coding-system)
           (insert contents))
         (xmtn--add-files root (list file-name))
         (xmtn--run-command-sync root (list "commit" "--message=commit foo"))
         (let ((content-id "77785e6fd883a5e27a62bc6f26365e1b37e1900f"))
           (assert (equal (xmtn--file-contents-as-string root content-id)
                          (encode-coding-string contents coding-system))
                   t))))))
  (non-ascii-cert-value
   (let ((umlaut (string (make-char 'latin-iso8859-1 #xe4)))) ; umlaut a
     (xmtn-tests--with-test-history (&key root revision-2 &allow-other-keys)
       (let ((cert-name "test-cert")
             (cert-value umlaut))
         (xmtn--run-command-sync root `("cert" "--"
                                        ,revision-2
                                        ,cert-name ,cert-value))
         (let ((certs (xmtn--list-parsed-certs root revision-2)))
           (let ((matching-certs (remove* cert-name certs
                                          :key #'third
                                          :test-not #'equal)))
             (xmtn-match matching-certs
               ((($email ok $cert-name-here $cert-value-here t))
                (assert (equal cert-name-here cert-name) t)
                (assert (equal cert-value-here cert-value) t)))))))))
  (dvc-file-diff-with-non-ascii-contents
   (save-window-excursion
     (let ((umlaut (string (make-char 'latin-iso8859-1 #xe4)))) ; umlaut a
       (xmtn-tests--with-test-environment (&key root)
         (let ((file-name "foo")
               (contents (concat umlaut "\n"))
               (coding-system 'utf-8-unix))
           (with-temp-file file-name
             (setq buffer-file-coding-system coding-system)
             (insert contents))
           (xmtn--add-files root (list file-name))
           (xmtn--run-command-sync root (list "commit" "--message=commit foo"))
           (with-temp-buffer
             (let ((coding-system-for-read coding-system))
               (insert-file-contents file-name t))
             (dvc-file-diff file-name)
             (assert (eql (point-min) (point-max)))))))))
  (buffer-file-coding-system-in-dvc-dvc-file-diff
   (save-window-excursion
     (let ((umlaut (string (make-char 'latin-iso8859-1 #xe4)))) ; umlaut a
       (xmtn-tests--with-test-environment (&key root)
         (let ((file-name "foo")
               (contents (concat umlaut "\n"))
               (coding-system-1 'utf-8-unix)
               (coding-system-2 'iso-8859-1-unix))
           (with-temp-file file-name
             (setq buffer-file-coding-system coding-system-1)
             (insert contents))
           (xmtn--add-files root (list file-name))
           (xmtn--run-command-sync root (list "commit" "--message=commit foo"))
           (with-temp-buffer
             (insert-file-contents file-name t)
             (setq buffer-file-coding-system coding-system-2)
             (let ((coding-system-for-read coding-system-1))
               (dvc-file-diff file-name))
             (assert (not (eql (point-min) (point-max))))))))))
  (file-diff-after-rename
   (xmtn-tests--with-test-history (&key root ((:file-name file-name-1))
                                        revision-2
                                        &allow-other-keys)
     (let ((file-name-2 "bar"))
       (xmtn--run-command-sync root
                               (xmtn--version-case
                                ((>= 0 34)
                                 `("mv" "--" ,file-name-1 ,file-name-2))
                                (t
                                 `("mv" "-e" "--" ,file-name-1 ,file-name-2))))
       (with-temp-buffer
         (xmtn--revision-get-file-helper file-name-2 revision-2)
         (assert (equal (buffer-substring (point-min) (point-max))
                        "b\n")
                 t)))))
  (diff-from-revlog
   (save-window-excursion
     (xmtn-tests--with-test-history (&key &allow-other-keys)
       (unwind-protect
           (progn
             (dvc-changelog)
             (dvc-revision-next)
             (dvc-revlist-diff))
         (dvc-tests-wait-async)))))
  (stdio-command-options
   (xmtn--version-case
    ((>= 0 31)
     (xmtn-tests--with-test-history (&key root file-name
                                          revision-1 revision-2
                                          &allow-other-keys)
       (let ((root default-directory))
         (assert
          (equal
           (xmtn-automate-simple-command-output-lines
            root `(("revision" ,revision-1
                    "revision" ,revision-2)
                   "content_diff" ,file-name))
           '("============================================================"
             "--- file-1	3f786850e387550fdab836ed7e6dc881de23001b"
             "+++ file-1	89e6c98d92887913cadf06b2adb97f26cde4849b"
             "@@ -1 +1 @@"
             "-a"
             "+b"))
          t))))
    (t
     (xmtn-tests--with-test-history (&key root file-name
                                          revision-1 revision-2)
       (assert (not (ignore-errors
                      (message "%S" (xmtn-automate-simple-command-output-lines
                                     root `(("revision" ,revision-1
                                             "revision" ,revision-2)
                                            "content_diff" ,file-name)))
                      t)))))))
  (xmtn-dvc-command-version
   ;; Should not error.
   (xmtn-dvc-command-version))
  (dvc-file-diff-write-file-hooks
   (save-window-excursion
     (xmtn-tests--with-test-history (&key file-name &allow-other-keys)
       (find-file file-name)
       (unwind-protect
           (progn
             (let ((write-file-hooks (list (lambda ()
                                             (assert nil)))))
               (insert "x")
               (save-excursion
                 (call-interactively #'dvc-file-diff))))
         (revert-buffer t t)))))
  (get-content-changed-closure
   (save-window-excursion
     (xmtn-tests--with-test-history (&key root file-name revision-1 revision-2
                                          &allow-other-keys)
       (let ((other-file-name (concat file-name "2"))
             (renamed-file-name (concat file-name "x"))
             revision-3 revision-4 revision-5)
         (progn
           (with-temp-file other-file-name (insert "a\n"))
           (xmtn--add-files root (list other-file-name))
           (xmtn--run-command-sync root `("commit" "--message=commit"))
           (setq revision-3 (xmtn--get-base-revision-hash-id root)))
         (progn
           (xmtn--run-command-sync root
                                   (xmtn--version-case
                                    ((>= 0 34)
                                     `("mv" "--" ,file-name ,renamed-file-name))
                                    (t
                                     `("mv" "-e" "--" ,file-name
                                       ,renamed-file-name))))
           (xmtn--run-command-sync root `("commit" "--message=commit"))
           (setq revision-4 (xmtn--get-base-revision-hash-id root)))
         (progn
           (with-temp-file renamed-file-name (insert "c\n"))
           (xmtn--run-command-sync root `("commit" "--message=commit"))
           (setq revision-5 (xmtn--get-base-revision-hash-id root)))
         (flet ((check (file start-rev expected-results)
                  (let ((actual (xmtn--get-content-changed-closure
                                 root `(revision ,start-rev) file)))
                    (unless (null (set-exclusive-or expected-results
                                                    actual
                                                    :test #'equal))
                      (error "file=%S start-rev=%s expected=%S actual=%S; revisions=%S"
                             file start-rev expected-results actual
                             (list revision-1 revision-2 revision-3 revision-4
                                   revision-5))))))
           (check file-name revision-1 `((,revision-1 ,file-name)))
           ;; Some of these checks fail with mtn 0.30; not
           ;; investigated further.
           ;;
           ;; 0.30 reports ((1 file))
           (check file-name revision-2 `((,revision-1 ,file-name)
                                         (,revision-2 ,file-name)))

           ;; 0.30 reports ((1 file))
           (check file-name revision-3 `((,revision-1 ,file-name)
                                         (,revision-2 ,file-name)))
           ;; 0.30 reports ((1 file) (4 renamed))
           (check renamed-file-name revision-4 `((,revision-1 ,file-name)
                                                 (,revision-2 ,file-name)))
           ;; 0.30 reports ((1 file) (4 renamed))
           (check renamed-file-name revision-5 `((,revision-1 ,file-name)
                                                 (,revision-2 ,file-name)
                                                 (,revision-5
                                                  ,renamed-file-name)))
           (check other-file-name revision-3 `((,revision-3 ,other-file-name)))
           (check other-file-name revision-4 `((,revision-3 ,other-file-name)))
           (check other-file-name revision-5 `((,revision-3 ,other-file-name)))
           )))))
  (locale
   ;; The test is simply that this doesn't crash.
   (let ((process-environment (list* "LC_MESSAGES=de_DE" process-environment))
         (xmtn--*cached-command-version* nil))
     ;; Unfortunately, in my configuration, I don't seem to be able to
     ;; get monotone to print non-English messages at all.  So, for
     ;; me, this doesn't actually fail even without the appropriate
     ;; changes to `xmtn--call-with-environment-for-subprocess'.
     (xmtn-check-command-version)))

  (xmtn--file-registered-p
   (xmtn-tests--with-test-history (&key root file-name &allow-other-keys)
     (assert (xmtn--file-registered-p root file-name))
     (assert (not (xmtn--file-registered-p root "nonexistent-file")))))

  (dvc-status-add
   (save-window-excursion
     (xmtn-tests--with-test-environment
         (&key &allow-other-keys)
       ;; add and commit an unknown file, using dvc-status keystrokes
       (with-temp-file "unknown" (insert "unknown - to be added\n"))
       (with-temp-file "unknown-marked" (insert "unknown, marked\n"))
       (dvc-status)
       (dvc-tests-wait-async)
       (assert (looking-at "   unknown       unknown"))
       (execute-kbd-macro (vector dvc-key-add))
       (dvc-tests-wait-async)
       (assert (looking-at "   added         unknown"))
       (forward-line)
       (assert (looking-at "   unknown       unknown-marked"))
       (execute-kbd-macro (vector dvc-key-mark dvc-key-add))
       ;; FIXME: checking for the mark doesn't work; something about the fontification of the line.
       (dvc-tests-wait-async)
       (execute-kbd-macro (vector dvc-key-unmark))
       (assert (looking-at "   added         unknown-marked"))
       ;; FIXME: commit hangs when run from this test, in xmtn--insert-log-edit-hints, which runs stuff asynchronously
       ;;  (execute-kbd-macro (vector dvc-key-commit))
       ;;  (dvc-tests-wait-async)
       ;;  (debug)
       ;;  (execute-kbd-macro (vector "C-c" "C-c"))
       ;; this works
       (dvc-log-edit)
       (dvc-tests-wait-async)
       (dvc-log-edit-done)
       (dvc-tests-wait-async)

       ;; currently need dvc-status-refresh to see results of the
       ;; commit; eventually dvc-status will edit the ewoc directly
       (dvc-status-refresh)
       (dvc-tests-wait-async)
       (assert (looking-at "$"))
       )))
  )

(defvar xmtn-tests--profile-history (list))

(defun xmtn-tests--profile ()
  (interactive)
  (unless (not xmtn--*enable-assertions*)
    (unless (y-or-n-p "Assertions appear to be enabled.  Continue anyway? ")
      (error "Aborted")))
  (let ((command
         (read-from-minibuffer "Profile xmtn command: "
                               nil read-expression-map t
                               'xmtn-tests--profile-history))
        (reps 20))
    (elp-instrument-package "xmtn-")
    (elp-instrument-package "dvc-")
    (elp-instrument-package "process-")
    (elp-instrument-package "ewoc-")
    (elp-instrument-function 'accept-process-output)
    (elp-instrument-function 'buffer-substring-no-properties)
    (elp-reset-all)
    (setq elp-reset-after-results nil)
    ;; FIXME: Maybe use benchmark.el.
    (let ((gc-cons-threshold (max gc-cons-threshold 100000000))
          (run-time 0)
          (gc-time 0))
      (assert (garbage-collect))
      (loop for rep from 1
            repeat reps
            do
            (with-temp-message (format "Profiling, repetition %s of %s..."
                                       rep reps)
              (save-excursion
                (save-window-excursion
                  (let ((start-time (current-time)))
                    (eval command)
                    (let ((end-time (current-time)))
                      (incf run-time (elp-elapsed-time start-time
                                                       end-time))))))
              (assert (let ((start-time (current-time)))
                        (prog1
                            (garbage-collect)
                          (let ((end-time (current-time)))
                            (incf gc-time (elp-elapsed-time start-time
                                                            end-time))))))))
      (elp-results)
      (setq truncate-lines t)
      (goto-char (point-min))
      (insert (format "Command: %S\n" command))
      (insert (format "Repetitions: %s\n" reps))
      (insert "\n")
      (insert (format "Wall time (excluding gc): %s\n" run-time))
      (insert (format "GC time (bogus):          %s\n" gc-time))
      (insert "\n"))
    (elp-restore-all))
  (message "Profiling finished"))

(defun xmtn-tests--time ()
  (interactive)
  (unless (not xmtn--*enable-assertions*)
    (unless (y-or-n-p "Assertions appear to be enabled.  Continue anyway? ")
      (error "Aborted")))
  (let ((command
         (read-from-minibuffer "Time xmtn command: "
                               nil read-expression-map t
                               'xmtn-tests--profile-history))
        (reps 10)) ;; FIXME: dies on rep 30 on Windows MinGW
    ;; Run command once before starting timing to get everything in cache
    (eval command)
    (let ((run-time 0))
      (assert (garbage-collect))
      (loop for rep from 1
            repeat reps
            do
            (with-temp-message (format "Timing, repetition %s of %s..."
                                       rep reps)
              (save-excursion
                (save-window-excursion
                  (let ((start-time (current-time)))
                    (eval command)
                    (let ((end-time (current-time)))
                      (incf run-time (elp-elapsed-time start-time
                                                       end-time))))))))
      (switch-to-buffer-other-window (get-buffer-create
                                      "*xmtn timing results*"))
      (erase-buffer)
      (setq truncate-lines t)
      (goto-char (point-min))
      (insert (format "Command: %S\n" command))
      (insert (format "Repetitions: %s\n" reps))
      (insert "\n")
      (insert (format "Wall time (including gc): %s\n" run-time))
      (insert "\n")))
  (message "Timing finished"))

(defun xmtn-tests--parse-basic-io-inventory-benchmark (mtn-executable tree)
  (let ((default-directory tree)
        (xmtn-executable mtn-executable)
        (xmtn--*cached-command-version* nil))
    (xmtn-automate-with-session (session (dvc-tree-root))
      (xmtn-automate-with-command (handle session '("inventory"))
        (xmtn-automate-command-wait-until-finished handle)
        (xmtn-automate-command-check-for-and-report-error handle)
        (xmtn-basic-io-with-stanza-parser (parser (xmtn-automate-command-buffer
                                                   handle))
          (let ((changed 0)
                (total 0)
                (unknown 0)
                (ignored 0))
            (loop for stanza = (funcall parser)
                  while stanza
                  do (incf total)
                  do (let ((status (second (assoc "status" stanza))))
                       (xmtn-match status
                         ((string "known"))
                         ((string "missing"))
                         ((string "unknown") (incf unknown))
                         ((string "ignored") (incf ignored)))
                       (let ((changes (second (assoc "changes" stanza))))
                         (unless (null changes)
                           (incf changed)))))
            (message "total=%s changed=%s ignored=%s unknown=%s"
                     total changed ignored unknown)))))))

(provide 'xmtn-tests)
;;; xmtn-tests.el ends here

;;; Encryption and related functions for VM
;;; Copyright (C) 2001 Kyle E. Jones
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

;;(provide 'vm-crypto)

;; compatibility
(fset 'vm-pop-md5 'vm-md5-string)

(defun vm-md5-region (start end)
  (if (fboundp 'md5)
      (md5 (current-buffer) start end)
    (let ((buffer nil)
	  (retval nil)
	  (curbuf (current-buffer)))
      (unwind-protect
	  (save-excursion
	    (setq buffer (vm-make-work-buffer))
	    (set-buffer buffer)
	    (insert-buffer-substring curbuf start end)
	    ;; call-process-region calls write-region.
	    ;; don't let it do CR -> LF translation.
	    (setq selective-display nil)
	    (setq retval
		  (call-process-region (point-min) (point-max)
				       vm-pop-md5-program
				       t buffer nil))
	    (if (not (equal retval 0))
		(progn
		  (error "%s failed: exited with code %s"
			 vm-pop-md5-program retval)))
	    ;; md5sum generates extra output even when summing stdin.
	    (goto-char (point-min))
 	    (if (re-search-forward " [ *]?-\n" nil t)
		(replace-match ""))

	    (goto-char (point-min))
	    (if (or (re-search-forward "[^0-9a-f\n]" nil t)
		    (< (point-max) 32))
		(error "%s produced bogus MD5 digest '%s'"
		       vm-pop-md5-program 
		       (vm-buffer-substring-no-properties (point-min) 
							  (point-max))))
	    ;; MD5 digest is 32 chars long
	    ;; mddriver adds a newline to make neaten output for tty
	    ;; viewing, make sure we leave it behind.
	    (vm-buffer-substring-no-properties (point-min) (+ (point-min) 32)))
	(and buffer (kill-buffer buffer))))))

;; output is in hex
(defun vm-md5-string (string)
  (if (fboundp 'md5)
      (md5 string)
    (vm-with-string-as-temp-buffer
     string (function
	     (lambda ()
	       (goto-char (point-min))
	       (insert (vm-md5-region (point-min) (point-max)))
	       (delete-region (point) (point-max)))))))

;; output is the raw digest bits, not hex
(defun vm-md5-raw-string (s)
  (setq s (vm-md5-string s))
  (let ((raw (make-string 16 0))
	(i 0) n
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)
			   ;; some mailer uses lower-case hex
			   ;; digits despite this being forbidden
			   ;; by the MIME spec.
			   (?a . 10)  (?b . 11)  (?c . 12)  (?d . 13)
			   (?e . 14)  (?f . 15))))
    (while (< i 32)
      (setq n (+ (* (cdr (assoc (aref s i) hex-digit-alist)) 16)
		 (cdr (assoc (aref s (1+ i)) hex-digit-alist))))
      (aset raw (/ i 2) n)
      (setq i (+ i 2)))
    raw ))

(defun vm-xor-string (s1 s2)
  (let ((len (length s1))
	result (i 0))
    (if (/= len (length s2))
	(error "strings not of equal length"))
    (setq result (make-string len 0))
    (while (< i len)
      (aset result i (logxor (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    result ))

(defun vm-setup-ssh-tunnel (host port)
  (let (local-port process done)
    (while (not done)
      (setq local-port (+ 1025 (random (- 65536 1025)))
	    process nil)
      (condition-case nil
	  (progn
	    (setq process
		  (open-network-stream "TEST-CONNECTION" nil
				       "127.0.0.1" local-port))
	    (process-kill-without-query process))
	(error nil))
      (cond ((null process)
	     (setq process
		   (apply 'start-process
			  (format "SSH tunnel to %s:%s" host port)
			  (vm-make-work-buffer)
			  vm-ssh-program
			  (nconc
			   (list "-L"
				 (format "%d:%s:%s" local-port host port))
			   (copy-sequence vm-ssh-program-switches)
			   (list host vm-ssh-remote-command)))
		   done t)
	     (process-kill-without-query process)
	     (set-process-sentinel process 'vm-process-sentinel-kill-buffer))
	    (t
	     (delete-process process))))

    ;; wait for some output from vm-ssh-remote-command.  this
    ;; ensures that when we return the ssh connection is ready to
    ;; do port-forwarding.
    (accept-process-output process)

    local-port ))

(defun vm-generate-random-data-file (n-octets)
  (let ((file (vm-make-tempfile))
	work-buffer (i n-octets))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (while (> i 0)
	    (insert-char (random 256) 1)
	    (setq i (1- i)))
	  (write-region (point-min) (point-max) file nil 0))
      (and work-buffer (kill-buffer work-buffer)))
    file ))

(defun vm-setup-stunnel-random-data-if-needed ()
  (cond ((null vm-stunnel-random-data-method) nil)
	((eq vm-stunnel-random-data-method 'generate)
	 (if (and (stringp vm-stunnel-random-data-file)
		  (file-readable-p vm-stunnel-random-data-file))
	     nil
	   (setq vm-stunnel-random-data-file
		 (vm-generate-random-data-file (* 4 1024)))))))

(defun vm-tear-down-stunnel-random-data ()
  (if (stringp vm-stunnel-random-data-file)
      (vm-error-free-call 'delete-file vm-stunnel-random-data-file))
  (setq vm-stunnel-random-data-file nil))

(defun vm-stunnel-random-data-args ()
  (cond ((null vm-stunnel-random-data-method) nil)
	((eq vm-stunnel-random-data-method 'generate)
	 (list "-R" vm-stunnel-random-data-file))
	(t nil)))

(defun vm-stunnel-configuration-args (host port)
  (if (eq vm-stunnel-wants-configuration-file 'unknown)
      (setq vm-stunnel-wants-configuration-file
	    (not (eq (call-process vm-stunnel-program nil nil nil "-h") 0))))
  (if (not vm-stunnel-wants-configuration-file)
      (nconc (vm-stunnel-random-data-args)
	     (list "-W" "-c" "-r"
		   (format "%s:%s" host port)))
    (let ((work-buffer nil)
	  (workfile (vm-stunnel-configuration-file)))
      (unwind-protect
	  (save-excursion
	    (setq work-buffer (vm-make-work-buffer))
	    (set-buffer work-buffer)
	    (if (and vm-stunnel-program-additional-configuration-file
		     (stringp vm-stunnel-program-additional-configuration-file)
		     (file-readable-p
		      vm-stunnel-program-additional-configuration-file))
		(insert-file-contents
		 vm-stunnel-program-additional-configuration-file))
	    (insert "client = yes\n")
	    (insert "RNDfile = " vm-stunnel-random-data-file "\n")
	    (insert "RNDoverwrite = no\n")
	    (insert "connect = " (format "%s:%s" host port) "\n")
	    (write-region (point-min) (point-max) workfile nil 0))
	(and work-buffer (kill-buffer work-buffer)))
      (list workfile) )))

(defun vm-stunnel-configuration-file ()
  (if vm-stunnel-configuration-file
      vm-stunnel-configuration-file
    (setq vm-stunnel-configuration-file (vm-make-tempfile))
    (vm-register-global-garbage-files (list vm-stunnel-configuration-file))
    vm-stunnel-configuration-file))

(provide 'vm-crypto)

;;; MIME support functions
;;; Copyright (C) 1997-1998, 2000, 2001, 2003 Kyle E. Jones
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

;;(provide 'vm-mime)

(defvar enable-multibyte-characters)
(defvar default-enable-multibyte-characters)

(defun vm-mime-error (&rest args)
  (signal 'vm-mime-error (list (apply 'format args)))
  (error "can't return from vm-mime-error"))

(if (fboundp 'define-error)
    (progn
      (define-error 'vm-image-too-small "Image too small")
      (define-error 'vm-mime-error "MIME error"))
  (put 'vm-image-too-small 'error-conditions '(vm-image-too-small error))
  (put 'vm-image-too-small 'error-message "Image too small")
  (put 'vm-mime-error 'error-conditions '(vm-mime-error error))
  (put 'vm-mime-error 'error-message "MIME error"))

(defun vm-make-layout (&rest plist)
  (vector
   (plist-get plist 'type)
   (plist-get plist 'qtype)
   (plist-get plist 'encoding)
   (plist-get plist 'id)
   (plist-get plist 'description)
   (plist-get plist 'disposition)
   (plist-get plist 'qdisposition)
   (plist-get plist 'header-start)
   (plist-get plist 'header-end)
   (plist-get plist 'body-start)
   (plist-get plist 'body-end)
   (plist-get plist 'parts)
   (plist-get plist 'cache)
   (plist-get plist 'message-symbol)
   (plist-get plist 'display-error)
   (plist-get plist 'layout-is-converted)
   (plist-get plist 'unconverted-layout)))

(defun vm-mm-layout-type (e) (aref e 0))
(defun vm-mm-layout-qtype (e) (aref e 1))
(defun vm-mm-layout-encoding (e) (aref e 2))
(defun vm-mm-layout-id (e) (aref e 3))
(defun vm-mm-layout-description (e) (aref e 4))
(defun vm-mm-layout-disposition (e) (aref e 5))
(defun vm-mm-layout-qdisposition (e) (aref e 6))
(defun vm-mm-layout-header-start (e) (aref e 7))
(defun vm-mm-layout-header-end (e) (aref e 8))
(defun vm-mm-layout-body-start (e) (aref e 9))
(defun vm-mm-layout-body-end (e) (aref e 10))
(defun vm-mm-layout-parts (e) (aref e 11))
(defun vm-mm-layout-cache (e) (aref e 12))
(defun vm-mm-layout-message-symbol (e) (aref e 13))
(defun vm-mm-layout-message (e)
  (symbol-value (vm-mm-layout-message-symbol e)))
;; if display of MIME part fails, error string will be here.
(defun vm-mm-layout-display-error (e) (aref e 14))
(defun vm-mm-layout-is-converted (e) (aref e 15))
(defun vm-mm-layout-unconverted-layout (e) (aref e 16))

(defun vm-set-mm-layout-type (e type) (aset e 0 type))
(defun vm-set-mm-layout-qtype (e type) (aset e 1 type))
(defun vm-set-mm-layout-encoding (e encoding) (aset e 2 encoding))
(defun vm-set-mm-layout-id (e id) (aset e 3 id))
(defun vm-set-mm-layout-description (e des) (aset e 4 des))
(defun vm-set-mm-layout-disposition (e d) (aset e 5 d))
(defun vm-set-mm-layout-qdisposition (e d) (aset e 6 d))
(defun vm-set-mm-layout-header-start (e start) (aset e 7 start))
(defun vm-set-mm-layout-header-end (e start) (aset e 8 start))
(defun vm-set-mm-layout-body-start (e start) (aset e 9 start))
(defun vm-set-mm-layout-body-end (e end) (aset e 10 end))
(defun vm-set-mm-layout-parts (e parts) (aset e 11 parts))
(defun vm-set-mm-layout-cache (e c) (aset e 12 c))
(defun vm-set-mm-layout-display-error (e c) (aset e 14 c))
(defun vm-set-mm-layout-is-converted (e c) (asef e 15 c))
(defun vm-set-mm-layout-unconverted-layout (e layout) (aset e 16 layout))

(defun vm-mime-make-message-symbol (m)
  (let ((s (make-symbol "<<m>>")))
    (set s m)
    s ))

(defun vm-mime-make-cache-symbol ()
  (let ((s (make-symbol "<<c>>")))
    (set s s)
    s ))

(defun vm-mm-layout (m)
  (or (vm-mime-layout-of m)
      (progn (vm-set-mime-layout-of m (vm-mime-parse-entity-safe m))
	     (vm-mime-layout-of m))))

(defun vm-mm-encoded-header (m)
  (or (vm-mime-encoded-header-flag-of m)
      (progn (setq m (vm-real-message-of m))
	     (vm-set-mime-encoded-header-flag-of
	      m
	      (save-excursion
		(set-buffer (vm-buffer-of m))
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (vm-headers-of m))
		    (let ((case-fold-search t))
		      (or (re-search-forward vm-mime-encoded-word-regexp
					     (vm-text-of m) t)
			  'none))))))
	     (vm-mime-encoded-header-flag-of m))))

(defun vm-mime-Q-decode-region (start end)
  (let ((buffer-read-only nil))
    (subst-char-in-region start end ?_ (string-to-char " ") t)
    (vm-mime-qp-decode-region start end)))

(fset 'vm-mime-B-decode-region 'vm-mime-base64-decode-region)

(defun vm-mime-Q-encode-region (start end)
  (let ((buffer-read-only nil)
	(val))
    (setq val (vm-mime-qp-encode-region start end t))
    (subst-char-in-region start end (string-to-char " ") ?_ t)
    val ))

(defun vm-mime-B-encode-region (start end)
  (vm-mime-base64-encode-region start end nil t))

(defun vm-mime-base64-decode-string (string)
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-decode-region (point-min) (point-max))))))

(defun vm-mime-base64-encode-string (string)
  (vm-with-string-as-temp-buffer
   string
   (function
    (lambda () (vm-mime-base64-encode-region (point-min) (point-max)
					     nil t)))))

(defun vm-mime-crlf-to-lf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\r\n" nil t)
	  (delete-char -2)
	  (insert "\n"))))))
      
(defun vm-mime-lf-to-crlf-region (start end)
  (let ((buffer-read-only nil))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (search-forward "\n" nil t)
	  (delete-char -1)
	  (insert "\r\n"))))))
      
(defun vm-encode-coding-region (b-start b-end coding-system &rest foo)
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq oldsize (buffer-size))
	  (setq retval (apply 'encode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (setq start (point-min) end (point-max))
	  (setq retval (buffer-size))
	  (save-excursion
	    (set-buffer b)
	    (goto-char b-start)
	    (insert-buffer-substring work-buffer start end)
	    (delete-region (point) (+ (point) oldsize))
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after encode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-decode-coding-region (b-start b-end coding-system &rest foo)
  (let ((work-buffer nil)
	start end
	oldsize
	retval
	(b (current-buffer)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  (setq retval (apply 'decode-coding-region (point-min) (point-max)
			      coding-system foo))
	  (and vm-fsfemacs-p (set-buffer-multibyte t))
	  (setq start (point-min) end (point-max))
	  (save-excursion
	    (set-buffer b)
	    (goto-char b-start)
	    (delete-region (point) (+ (point) oldsize))
	    (insert-buffer-substring work-buffer start end)
	    ;; Fixup the end point.  I have found no other way to
	    ;; let the calling function know where the region ends
	    ;; after decode-coding-region has scrambled the markers.
	    (and (markerp b-end)
		 (set-marker b-end (point)))
	    retval ))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-charset-decode-region (charset start end)
  (or (markerp end) (setq end (vm-marker end)))
  (cond ((or vm-xemacs-mule-p vm-fsfemacs-mule-p)
	 (if (or (and vm-xemacs-p (memq (device-type) '(x gtk mswindows)))
		 vm-fsfemacs-p
		 nil)
	     (let ((buffer-read-only nil)
		   (cell (cdr (vm-string-assoc
			       charset
			       vm-mime-mule-charset-to-coding-alist)))
		   (opoint (point)))
	       (if cell
		   (progn
		     ;; decode 8-bit indeterminate char to correct
		     ;; char in correct charset.
		     (vm-decode-coding-region start end (car cell))
		     (put-text-property start end 'vm-string t)
		     (put-text-property start end 'vm-charset charset)
		     (put-text-property start end 'vm-coding (car cell))))
	       ;; In XEmacs 20.0 beta93 decode-coding-region moves point.
	       (goto-char opoint))))
	((not (vm-multiple-fonts-possible-p)) nil)
	((vm-mime-default-face-charset-p charset) nil)
	(t
	 (let ((font (cdr (vm-string-assoc
			   charset
			   vm-mime-charset-font-alist)))
	       (face (make-face (make-symbol "temp-face")))
	       (e (vm-make-extent start end)))
	   (put-text-property start end 'vm-string t)
	   (put-text-property start end 'vm-charset charset)
	   (if font
	       (condition-case data
		   (progn (set-face-font face font)
			  (if vm-fsfemacs-p
			      (put-text-property start end 'face face)
			    (vm-set-extent-property e 'duplicable t)
			    (vm-set-extent-property e 'face face)))
		 (error nil)))))))

(defun vm-mime-transfer-decode-region (layout start end)
  (let ((case-fold-search t) (crlf nil))
    (if (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
	    (vm-mime-types-match "message" (car (vm-mm-layout-type layout))))
	(setq crlf t))
    (cond ((string-match "^base64$" (vm-mm-layout-encoding layout))
	   (vm-mime-base64-decode-region start end crlf))
	  ((string-match "^quoted-printable$"
			 (vm-mm-layout-encoding layout))
	   (vm-mime-qp-decode-region start end))
	  ((string-match "^x-uue$\\|^x-uuencode$"
			 (vm-mm-layout-encoding layout))
	   (vm-mime-uuencode-decode-region start end crlf)))))

(defun vm-mime-base64-decode-region (start end &optional crlf)
  (or (markerp end) (setq end (vm-marker end)))
  (and (> (- end start) 200)
       (message "Decoding base64..."))
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(bits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" vm-mime-base64-alphabet)))
    (unwind-protect
	(save-excursion
	  (cond
	   ((and (featurep 'base64)
		 (fboundp 'base64-decode-region)
		 ;; W3 reportedly has a Lisp version of this, and
		 ;; there's no point running it.
		 (subrp (symbol-function 'base64-decode-region))
		 ;; The FSF Emacs version of this is unforgiving
		 ;; of errors, which is not in the spirit of the
		 ;; MIME spec, so avoid using it.
		 (not vm-fsfemacs-p))
	    (condition-case data
		(base64-decode-region start end)
	      (error (vm-mime-error "%S" data)))
	    (and crlf (vm-mime-crlf-to-lf-region start end)))
	   (t
	    (setq work-buffer (vm-make-work-buffer))
	    (if vm-mime-base64-decoder-program
		(let* ((binary-process-output t) ; any text already has CRLFs
		       ;; use binary coding system in FSF Emacs/MULE
		       (coding-system-for-read (vm-binary-coding-system))
		       (coding-system-for-write (vm-binary-coding-system))
		       (status (apply 'vm-run-command-on-region
				      start end work-buffer
				      vm-mime-base64-decoder-program
				      vm-mime-base64-decoder-switches)))
		  (if (not (eq status t))
		      (vm-mime-error "%s" (cdr status))))
	      (goto-char start)
	      (skip-chars-forward non-data-chars end)
	      (while (not done)
		(setq inputpos (point))
		(cond
		 ((> (skip-chars-forward vm-mime-base64-alphabet end) 0)
		  (setq lim (point))
		  (while (< inputpos lim)
		    (setq bits (+ bits 
				  (aref vm-mime-base64-alphabet-decoding-vector
					(char-after inputpos))))
		    (vm-increment counter)
		    (vm-increment inputpos)
		    (cond ((= counter 4)
			   (vm-insert-char (lsh bits -16) 1 nil work-buffer)
			   (vm-insert-char (logand (lsh bits -8) 255) 1 nil
					   work-buffer)
			   (vm-insert-char (logand bits 255) 1 nil work-buffer)
			   (setq bits 0 counter 0))
			  (t (setq bits (lsh bits 6)))))))
		(cond
		 ((= (point) end)
		  (if (not (zerop counter))
		      (vm-mime-error "at least %d bits missing at end of base64 encoding"
				     (* (- 4 counter) 6)))
		  (setq done t))
		 ((= (char-after (point)) 61) ; 61 is ASCII equals
		  (setq done t)
		  (cond ((= counter 1)
			 (vm-mime-error "at least 2 bits missing at end of base64 encoding"))
			((= counter 2)
			 (vm-insert-char (lsh bits -10) 1 nil work-buffer))
			((= counter 3)
			 (vm-insert-char (lsh bits -16) 1 nil work-buffer)
			 (vm-insert-char (logand (lsh bits -8) 255)
					 1 nil work-buffer))
			((= counter 0) t)))
		 (t (skip-chars-forward non-data-chars end)))))
	    (and crlf
		 (save-excursion
		   (set-buffer work-buffer)
		   (vm-mime-crlf-to-lf-region (point-min) (point-max))))
	    (goto-char start)
	    (insert-buffer-substring work-buffer)
	    (delete-region (point) end))))
      (and work-buffer (kill-buffer work-buffer))))
  (and (> (- end start) 200)
       (message "Decoding base64... done")))

(defun vm-mime-base64-encode-region (start end &optional crlf B-encoding)
  (or (markerp end) (setq end (vm-marker end)))
  (and (> (- end start) 200)
       (message "Encoding base64..."))
  (let ((work-buffer nil)
	(counter 0)
	(cols 0)
	(bits 0)
	(alphabet vm-mime-base64-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (and crlf (vm-mime-lf-to-crlf-region start end))
	  (cond
	   ((and (featurep 'base64)
		 (fboundp 'base64-encode-region)
		 ;; W3 reportedly has a Lisp version of this, and
		 ;; there's no point running it.
		 (subrp (symbol-function 'base64-encode-region)))
	    (condition-case data
		(base64-encode-region start end B-encoding)
	      (wrong-number-of-arguments
	       ;; call with two args and then strip out the
	       ;; newlines if we're doing B encoding.
	       (condition-case data
		   (base64-encode-region start end)
		 (error (vm-mime-error "%S" data)))
	       (if B-encoding
		   (save-excursion
		     (goto-char start)
		     (while (search-forward "\n" end t)
		       (delete-char -1)))))
	      (error (vm-mime-error "%S" data))))
	   (t
	    (setq work-buffer (vm-make-work-buffer))
	    (if vm-mime-base64-encoder-program
		(let ((status (apply 'vm-run-command-on-region
				     start end work-buffer
				     vm-mime-base64-encoder-program
				     vm-mime-base64-encoder-switches)))
		  (if (not (eq status t))
		      (vm-mime-error "%s" (cdr status)))
		  (if B-encoding
		      (save-excursion
			(set-buffer work-buffer)
			;; if we're B encoding, strip out the line breaks
			(goto-char (point-min))
			(while (search-forward "\n" nil t)
			  (delete-char -1)))))
	      (setq inputpos start)
	      (while (< inputpos end)
		(setq bits (+ bits (char-after inputpos)))
		(vm-increment counter)
		(cond ((= counter 3)
		       (vm-insert-char (aref alphabet (lsh bits -18)) 1 nil
				       work-buffer)
		       (vm-insert-char (aref alphabet (logand (lsh bits -12) 63))
				       1 nil work-buffer)
		       (vm-insert-char (aref alphabet (logand (lsh bits -6) 63))
				       1 nil work-buffer)
		       (vm-insert-char (aref alphabet (logand bits 63)) 1 nil
				       work-buffer)
		       (setq cols (+ cols 4))
		       (cond ((= cols 72)
			      (setq cols 0)
			      (if (not B-encoding)
				  (vm-insert-char ?\n 1 nil work-buffer))))
		       (setq bits 0 counter 0))
		      (t (setq bits (lsh bits 8))))
		(vm-increment inputpos))
	      ;; write out any remaining bits with appropriate padding
	      (if (= counter 0)
		  nil
		(setq bits (lsh bits (- 16 (* 8 counter))))
		(vm-insert-char (aref alphabet (lsh bits -18)) 1 nil
				work-buffer)
		(vm-insert-char (aref alphabet (logand (lsh bits -12) 63))
				1 nil work-buffer)
		(if (= counter 1)
		    (vm-insert-char ?= 2 nil work-buffer)
		  (vm-insert-char (aref alphabet (logand (lsh bits -6) 63))
				  1 nil work-buffer)
		  (vm-insert-char ?= 1 nil work-buffer)))
	      (if (> cols 0)
		  (vm-insert-char ?\n 1 nil work-buffer)))
	    (or (markerp end) (setq end (vm-marker end)))
	    (goto-char start)
	    (insert-buffer-substring work-buffer)
	    (delete-region (point) end)))
	  (and (> (- end start) 200)
	       (message "Encoding base64... done"))
	  (- end start))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-qp-decode-region (start end)
  (and (> (- end start) 200)
       (message "Decoding quoted-printable..."))
  (let ((work-buffer nil)
	(buf (current-buffer))
	(case-fold-search nil)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)
			   ;; some mailer uses lower-case hex
			   ;; digits despite this being forbidden
			   ;; by the MIME spec.
			   (?a . 10)  (?b . 11)  (?c . 12)  (?d . 13)
			   (?e . 14)  (?f . 15)))
	inputpos stop-point copy-point)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (if vm-mime-qp-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    start end work-buffer
				    vm-mime-qp-decoder-program
				    vm-mime-qp-decoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status))))
	    (goto-char start)
	    (setq inputpos start)
	    (while (< inputpos end)
	      (skip-chars-forward "^=\n" end)
	      (setq stop-point (point))
	      (cond ((looking-at "\n")
		     ;; spaces or tabs before a hard line break must be ignored
		     (skip-chars-backward " \t")
		     (setq copy-point (point))
		     (goto-char stop-point))
		    (t (setq copy-point stop-point)))
	      (save-excursion
		(set-buffer work-buffer)
		(insert-buffer-substring buf inputpos copy-point))
	      (cond ((= (point) end) t)
		    ((looking-at "\n")
		     (vm-insert-char ?\n 1 nil work-buffer)
		     (forward-char))
		    (t;; looking at =
		     (forward-char)
		     ;; a-f because some mailers use lower case hex
		     ;; digits despite them being forbidden by the
		     ;; MIME spec.
		     (cond ((looking-at "[0-9A-Fa-f][0-9A-Fa-f]")
			    (vm-insert-char (+ (* (cdr (assq (char-after (point))
							     hex-digit-alist))
						  16)
					       (cdr (assq (char-after
							   (1+ (point)))
							  hex-digit-alist)))
					    1 nil work-buffer)
			    (forward-char 2))
			   ((looking-at "\n") ; soft line break
			    (forward-char))
			   ((looking-at "\r")
			    ;; assume the user's goatloving
			    ;; delivery software didn't convert
			    ;; from Internet's CRLF newline
			    ;; convention to the local LF
			    ;; convention.
			    (forward-char))
			   ((looking-at "[ \t]")
			    ;; garbage added in transit
			    (skip-chars-forward " \t" end))
			   (t (vm-mime-error "something other than line break or hex digits after = in quoted-printable encoding")))))
	      (setq inputpos (point))))
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (and (> (- end start) 200)
       (message "Decoding quoted-printable... done")))

(defun vm-mime-qp-encode-region (start end &optional Q-encoding quote-from)
  (and (> (- end start) 200)
       (message "Encoding quoted-printable..."))
  (let ((work-buffer nil)
	(buf (current-buffer))
	(cols 0)
	(hex-digit-alist '((?0 .  0)  (?1 .  1)  (?2 .  2)  (?3 .  3)
			   (?4 .  4)  (?5 .  5)  (?6 .  6)  (?7 .  7)
			   (?8 .  8)  (?9 .  9)  (?A . 10)  (?B . 11)
			   (?C . 12)  (?D . 13)  (?E . 14)  (?F . 15)))
	char inputpos)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (if vm-mime-qp-encoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    start end work-buffer
				    vm-mime-qp-encoder-program
				    vm-mime-qp-encoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status)))
		(if quote-from
		    (save-excursion
		      (set-buffer work-buffer)
		      (goto-char (point-min))
		      (while (re-search-forward "^From " nil t)
			(replace-match "=46rom " t t))))
		(if Q-encoding
		    (save-excursion
		      (set-buffer work-buffer)
		      ;; strip out the line breaks
		      (goto-char (point-min))
		      (while (search-forward "=\n" nil t)
			(delete-char -2))
		      ;; strip out the soft line breaks
		      (goto-char (point-min))
		      (while (search-forward "\n" nil t)
			(delete-char -1)))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq char (char-after inputpos))
	      (cond ((= char ?\n)
		     (vm-insert-char char 1 nil work-buffer)
		     (setq cols 0))
		    ((and (= char 32)
			  (not (= (1+ inputpos) end))
			  (not (= ?\n (char-after (1+ inputpos)))))
		     (vm-insert-char char 1 nil work-buffer)
		     (vm-increment cols))
		    ((or (< char 33) (> char 126)
			 ;; =
			 (= char 61)
			 ;; ?
			 (and Q-encoding (= char 63))
			 ;; _
			 (and Q-encoding (= char 95))
			 (and quote-from (= cols 0)
			      (let ((case-fold-search nil))
				(looking-at "From ")))
			 (and (= cols 0) (= char ?.)
			      (looking-at "\\.\\(\n\\|\\'\\)")))
		     (vm-insert-char ?= 1 nil work-buffer)
		     (vm-insert-char (car (rassq (lsh char -4)
						 hex-digit-alist))
				     1 nil work-buffer)
		     (vm-insert-char (car (rassq (logand char 15)
						 hex-digit-alist))
				     1 nil work-buffer)
		     (setq cols (+ cols 3)))
		    (t (vm-insert-char char 1 nil work-buffer)
		       (vm-increment cols)))
	      (cond ((> cols 70)
		     (setq cols 0)
		     (if Q-encoding
			 nil
		       (vm-insert-char ?= 1 nil work-buffer)
		       (vm-insert-char ?\n 1 nil work-buffer))))
	      (vm-increment inputpos)))
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end)
	  (and (> (- end start) 200)
	       (message "Encoding quoted-printable... done"))
	  (- end start))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-uuencode-decode-region (start end &optional crlf)
  (message "Decoding uuencoded stuff...")
  (let ((work-buffer nil)
	(region-buffer (current-buffer))
	(case-fold-search nil)
	(tempfile (vm-make-tempfile-name)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (insert-buffer-substring region-buffer start end)
	  (goto-char (point-min))
	  (or (re-search-forward "^begin [0-7][0-7][0-7] " nil t)
	      (vm-mime-error "no begin line"))
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (insert tempfile "\n")
	  (goto-char (point-max))
	  (beginning-of-line)
	  ;; Eudora reportedly doesn't terminate uuencoded multipart
	  ;; bodies with a line break. 21 June 1998.
	  ;; Actually it looks like Eudora doesn't understand the
	  ;; multipart newline boundary rule at all and can leave
	  ;; all types of attachments missing a line break.
	  (if (looking-at "^end\\'")
	      (progn
		(goto-char (point-max))
		(insert "\n")))
	  (if (stringp vm-mime-uuencode-decoder-program)
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     ;; use binary coding system in FSF Emacs/MULE
		     (coding-system-for-read (vm-binary-coding-system))
		     (coding-system-for-write (vm-binary-coding-system))
		     (status (apply 'vm-run-command-on-region
				    (point-min) (point-max) nil
				    vm-mime-uuencode-decoder-program
				    vm-mime-uuencode-decoder-switches)))
		(if (not (eq status t))
		    (vm-mime-error "%s" (cdr status))))
	    (vm-mime-error "no uuencode decoder program defined"))
	  (delete-region (point-min) (point-max))
	  (insert-file-contents-literally tempfile)
	  (and crlf
	       (vm-mime-crlf-to-lf-region (point-min) (point-max)))
	  (set-buffer region-buffer)
	  (or (markerp end) (setq end (vm-marker end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))
      (vm-error-free-call 'delete-file tempfile)))
  (message "Decoding uuencoded stuff... done"))

(defun vm-decode-mime-message-headers (m)
  (let ((case-fold-search t)
	(buffer-read-only nil)
	charset need-conversion encoding match-start match-end start end)
    (save-excursion
      (goto-char (vm-headers-of m))
      (while (re-search-forward vm-mime-encoded-word-regexp (vm-text-of m) t)
	(setq match-start (match-beginning 0)
	      match-end (match-end 0)
	      charset (buffer-substring (match-beginning 1) (match-end 1))
	      encoding (buffer-substring (match-beginning 4) (match-end 4))
	      start (match-beginning 5)
	      end (vm-marker (match-end 5)))
	;; don't change anything if we can't display the
	;; character set properly.
	(if (and (not (vm-mime-charset-internally-displayable-p charset))
		 (not (setq need-conversion
			    (vm-mime-can-convert-charset charset))))
	    nil
	  (delete-region end match-end)
	  (condition-case data
	      (cond ((string-match "B" encoding)
		     (vm-mime-B-decode-region start end))
		    ((string-match "Q" encoding)
		     (vm-mime-Q-decode-region start end))
		    (t (vm-mime-error "unknown encoded word encoding, %s"
				      encoding)))
	    (vm-mime-error (apply 'message (cdr data))
			   (goto-char start)
			   (insert "**invalid encoded word**")
			   (delete-region (point) end)))
	  (and need-conversion
	       (setq charset (vm-mime-charset-convert-region
			      charset start end)))
	  (vm-mime-charset-decode-region charset start end)
	  (goto-char end)
	  (delete-region match-start start))))))

(defun vm-decode-mime-encoded-words ()
  (let ((case-fold-search t)
	(buffer-read-only nil)
	charset need-conversion encoding match-start match-end start end)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward vm-mime-encoded-word-regexp nil t)
	(setq match-start (match-beginning 0)
	      match-end (match-end 0)
	      charset (buffer-substring (match-beginning 1) (match-end 1))
	      encoding (buffer-substring (match-beginning 4) (match-end 4))
	      start (match-beginning 5)
	      end (vm-marker (match-end 5)))
	;; don't change anything if we can't display the
	;; character set properly.
	(if (and (not (vm-mime-charset-internally-displayable-p charset))
		 (not (setq need-conversion
			    (vm-mime-can-convert-charset charset))))
	    nil
	  (delete-region end match-end)
	  (condition-case data
	      (cond ((string-match "B" encoding)
		     (vm-mime-B-decode-region start end))
		    ((string-match "Q" encoding)
		     (vm-mime-Q-decode-region start end))
		    (t (vm-mime-error "unknown encoded word encoding, %s"
				      encoding)))
	    (vm-mime-error (apply 'message (cdr data))
			   (goto-char start)
			   (insert "**invalid encoded word**")
			   (delete-region (point) end)))
	  (and need-conversion
	       (setq charset (vm-mime-charset-convert-region
			      charset start end)))
	  (vm-mime-charset-decode-region charset start end)
	  (goto-char end)
	  (delete-region match-start start))))))

(defun vm-decode-mime-encoded-words-in-string (string)
  (if (and vm-display-using-mime
	   (let ((case-fold-search t))
	     (string-match vm-mime-encoded-word-regexp string)))
      (vm-with-string-as-temp-buffer string 'vm-decode-mime-encoded-words)
    string ))

(defun vm-reencode-mime-encoded-words ()
  (let ((charset nil)
	start coding pos q-encoding
	old-size
	(case-fold-search t)
	(done nil))
    (save-excursion
      (setq start (point-min))
      (while (not done)
	(setq charset (get-text-property start 'vm-charset))
	(setq pos (next-single-property-change start 'vm-charset))
	(or pos (setq pos (point-max) done t))
	(if charset
	    (progn
	      (if (setq coding (get-text-property start 'vm-coding))
		  (progn
		    (setq old-size (buffer-size))
		    (encode-coding-region start pos coding)
		    (setq pos (+ pos (- (buffer-size) old-size)))))
	      (setq pos
		    (+ start 
		       (if (setq q-encoding
				 (string-match "^iso-8859-\\|^us-ascii"
					       charset))
			   (vm-mime-Q-encode-region start pos)
			 (vm-mime-B-encode-region start pos))))
	      (goto-char pos)
	      (insert "?=")
	      (setq pos (point))
	      (goto-char start)
	      (insert "=?" charset "?" (if q-encoding "Q" "B") "?")
	      (setq pos (+ pos (- (point) start)))))
	(setq start pos)))))

(defun vm-reencode-mime-encoded-words-in-string (string)
  (if (and vm-display-using-mime
	   (text-property-any 0 (length string) 'vm-string t string))
      (vm-with-string-as-temp-buffer string 'vm-reencode-mime-encoded-words)
    string ))

(fset 'vm-mime-parse-content-header 'vm-parse-structured-header)

(defun vm-mime-get-header-contents (header-name-regexp)
  (let ((contents nil)
	regexp)
    (setq regexp (concat "^\\(" header-name-regexp "\\)\\|\\(^$\\)"))
    (save-excursion
      (let ((case-fold-search t))
	(if (and (re-search-forward regexp nil t)
		 (match-beginning 1)
		 (progn (goto-char (match-beginning 0))
			(vm-match-header)))
	    (vm-matched-header-contents)
	  nil )))))

(defun vm-mime-parse-entity (&optional m default-type default-encoding
				       passing-message-only)
  (catch 'return-value
    (save-excursion
      (if (and m (not passing-message-only))
	  (progn
	    (setq m (vm-real-message-of m))
	    (set-buffer (vm-buffer-of m))))
      (let ((case-fold-search t) version type qtype encoding id description
	    disposition qdisposition boundary boundary-regexp start end
	    multipart-list pos-list c-t c-t-e done p returnval)
	(save-excursion
	  (save-restriction
	    (if (and m (not passing-message-only))
		(progn
		  (setq version (vm-get-header-contents m "MIME-Version:")
			version (car (vm-mime-parse-content-header version))
			type (vm-get-header-contents m "Content-Type:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if type "1.0" nil)) 
			qtype (vm-mime-parse-content-header type ?\; t)
			type (vm-mime-parse-content-header type ?\;)
			encoding (vm-get-header-contents
				  m "Content-Transfer-Encoding:")
			version (if (or version
					vm-mime-require-mime-version-header)
				    version
				  (if encoding "1.0" nil)) 
			encoding (or encoding "7bit")
			encoding (or (car
				      (vm-mime-parse-content-header encoding))
				     "7bit")
			id (vm-get-header-contents m "Content-ID:")
			id (car (vm-mime-parse-content-header id))
			description (vm-get-header-contents
				     m "Content-Description:")
			description (and description
					 (if (string-match "^[ \t\n]*$"
							   description)
					     nil
					   description))
			disposition (vm-get-header-contents
				     m "Content-Disposition:")
			qdisposition (and disposition
					  (vm-mime-parse-content-header
					   disposition ?\; t))
			disposition (and disposition
					 (vm-mime-parse-content-header
					  disposition ?\;)))
		  (widen)
		  (narrow-to-region (vm-headers-of m) (vm-text-end-of m)))
	      (goto-char (point-min))
	      (setq type (vm-mime-get-header-contents "Content-Type:")
		    qtype (or (vm-mime-parse-content-header type ?\; t)
			      default-type)
		    type (or (vm-mime-parse-content-header type ?\;)
			     default-type)
		    encoding (or (vm-mime-get-header-contents
				  "Content-Transfer-Encoding:")
				 default-encoding)
		    encoding (or (car (vm-mime-parse-content-header encoding))
				 default-encoding)
		    id (vm-mime-get-header-contents "Content-ID:")
		    id (car (vm-mime-parse-content-header id))
		    description (vm-mime-get-header-contents
				 "Content-Description:")
		    description (and description (if (string-match "^[ \t\n]*$"
								   description)
						     nil
						   description))
		    disposition (vm-mime-get-header-contents
				 "Content-Disposition:")
		    qdisposition (and disposition
				      (vm-mime-parse-content-header
				       disposition ?\; t))
		    disposition (and disposition
				     (vm-mime-parse-content-header
				      disposition ?\;))))
	    (cond ((null m) t)
		  (passing-message-only t)
		  ((null version)
		   (throw 'return-value 'none))
		  ((or vm-mime-ignore-mime-version (string= version "1.0")) t)
		  (t (vm-mime-error "Unsupported MIME version: %s" version)))
	    ;; deal with known losers
	    ;; Content-Type: text
	    (cond ((and type (string-match "^text$" (car type)))
		   (setq type '("text/plain" "charset=us-ascii")
			 qtype '("text/plain" "charset=us-ascii"))))
	    (cond ((and m (not passing-message-only) (null type))
		   (throw 'return-value
			  (vm-make-layout
			   'type '("text/plain" "charset=us-ascii")
			   'qtype '("text/plain" "charset=us-ascii")
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-headers-of m)
			   'header-end (vm-marker (1- (vm-text-of m)))
			   'body-start (vm-text-of m)
			   'body-end (vm-text-end-of m)
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  ((null type)
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (vm-make-layout
		    'type default-type
		    'qtype default-type
		    'encoding encoding
		    'id id
		    'description description
		    'disposition disposition
		    'qdisposition qdisposition
		    'header-start (vm-marker (point-min))
		    'header-body (vm-marker (1- (point)))
		    'body-start (vm-marker (point))
		    'body-end (vm-marker (point-max))
		    'cache (vm-mime-make-cache-symbol)
		    'message-symbol (vm-mime-make-message-symbol m)
		    ))
		  ((null (string-match "[^/ ]+/[^/ ]+" (car type)))
		   (vm-mime-error "Malformed MIME content type: %s"
				  (car type)))
		  ((and (string-match "^multipart/\\|^message/" (car type))
			(null (string-match "^\\(7bit\\|8bit\\|binary\\)$"
					    encoding))
			(if vm-mime-ignore-composite-type-opaque-transfer-encoding
			    (progn
			      ;; Some mailers declare an opaque
			      ;; encoding on a composite type even
			      ;; though it's only a subobject that
			      ;; uses that encoding.  Deal with it
			      ;; by assuming a proper transfer encoding.
			      (setq encoding "binary")
			      ;; return nil so and-clause will fail
			      nil )
			  t ))
		   (vm-mime-error "Opaque transfer encoding used with multipart or message type: %s, %s" (car type) encoding))
		  ((and (string-match "^message/partial$" (car type))
			(null (string-match "^7bit$" encoding)))
		   (vm-mime-error "Non-7BIT transfer encoding used with message/partial message: %s" encoding))
		  ((string-match "^multipart/digest" (car type))
		   (setq c-t '("message/rfc822")
			 c-t-e "7bit"))
		  ((string-match "^multipart/" (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")) ; below
		  ((string-match "^message/\\(rfc822\\|news\\|external-body\\)"
				 (car type))
		   (setq c-t '("text/plain" "charset=us-ascii")
			 c-t-e "7bit")
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'parts (list
				   (save-restriction
				     (narrow-to-region (point) (point-max))
				     (vm-mime-parse-entity-safe m c-t c-t-e t)))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   )))
		  (t
		   (goto-char (point-min))
		   (or (re-search-forward "^\n\\|\n\\'" nil t)
		       (vm-mime-error "MIME part missing header/body separator line"))
		   (throw 'return-value
			  (vm-make-layout
			   'type type
			   'qtype qtype
			   'encoding encoding
			   'id id
			   'description description
			   'disposition disposition
			   'qdisposition qdisposition
			   'header-start (vm-marker (point-min))
			   'header-end (vm-marker (1- (point)))
			   'body-start (vm-marker (point))
			   'body-end (vm-marker (point-max))
			   'cache (vm-mime-make-cache-symbol)
			   'message-symbol (vm-mime-make-message-symbol m)
			   ))))
	    (setq p (cdr type)
		  boundary nil)
	    (while p
	      (if (string-match "^boundary=" (car p))
		  (setq boundary (car (vm-parse (car p) "=\\(.+\\)"))
			p nil)
		(setq p (cdr p))))
	    (or boundary
		(vm-mime-error
		 "Boundary parameter missing in %s type specification"
		 (car type)))
	    ;; the \' in the regexp is to "be liberal" in the
	    ;; face of broken software that does not add a line
	    ;; break after the final boundary of a nested
	    ;; multipart entity.
	    (setq boundary-regexp
		  (concat "^--" (regexp-quote boundary)
			  "\\(--\\)?[ \t]*\\(\n\\|\\'\\)"))
	    (goto-char (point-min))
	    (setq start nil
		  multipart-list nil
		  done nil)
	    (while (and (not done) (re-search-forward boundary-regexp nil 0))
	      (if (null start)
		  (setq start (match-end 0))
		(and (match-beginning 1)
		     (setq done t))
		(setq pos-list (cons start
				     (cons (1- (match-beginning 0)) pos-list))
		      start (match-end 0))))
	    (if (and (not done)
		     (not vm-mime-ignore-missing-multipart-boundary))
		(vm-mime-error "final %s boundary missing" boundary)
	      (if (and start (not done))
		  (setq pos-list (cons start (cons (point) pos-list)))))
	    (setq pos-list (nreverse pos-list))
	    (while pos-list
	      (setq start (car pos-list)
		    end (car (cdr pos-list))
		    pos-list (cdr (cdr pos-list)))
	      (save-excursion
		(save-restriction
		  (narrow-to-region start end)
		  (setq multipart-list
			(cons (vm-mime-parse-entity-safe m c-t c-t-e t)
			      multipart-list)))))
	    (goto-char (point-min))
	    (or (re-search-forward "^\n\\|\n\\'" nil t)
		(vm-mime-error "MIME part missing header/body separator line"))
	    (vm-make-layout
	     'type type
	     'qtype qtype
	     'encoding encoding
	     'id id
	     'description description
	     'disposition disposition
	     'qdisposition qdisposition
	     'header-start (vm-marker (point-min))
	     'header-end (vm-marker (1- (point)))
	     'body-start (vm-marker (point))
	     'body-end (vm-marker (point-max))
	     'parts (nreverse multipart-list)
	     'cache (vm-mime-make-cache-symbol)
	     'message-symbol (vm-mime-make-message-symbol m)
	     )))))))

(defun vm-mime-parse-entity-safe (&optional m c-t c-t-e p-m-only)
  (or c-t (setq c-t '("text/plain" "charset=us-ascii")))
  (or c-t-e (setq c-t-e "7bit"))
  ;; don't let subpart parse errors make the whole parse fail.  use default
  ;; type if the parse fails.
  (condition-case error-data
      (vm-mime-parse-entity m c-t c-t-e p-m-only)
    (vm-mime-error
     (message "%s" (car (cdr error-data)))
;;; don't sleep, no one cares about MIME syntax errors
;;;     (sleep-for 2)
     (let ((header (if (and m (not p-m-only))
		       (vm-headers-of m)
		     (vm-marker (point-min))))
	   (text (if (and m (not p-m-only))
		     (vm-text-of m)
		   (save-excursion
		     (re-search-forward "^\n\\|\n\\'"
					nil 0)
		     (vm-marker (point)))))
	   (text-end (if (and m (not p-m-only))
			 (vm-text-end-of m)
		       (vm-marker (point-max)))))
     (vm-make-layout
      'type '("error/error")
      'qtype '("error/error")
      'encoding (vm-determine-proper-content-transfer-encoding text text-end)
      ;; cram the error message into the description slot
      'description (car (cdr error-data))
      ;; mark as an attachment to improve the chance that the user
      ;; will see the description.
      'disposition '("attachment")
      'qdisposition '("attachment")
      'header-start header
      'header-end (vm-marker (1- text))
      'body-start text
      'body-end text-end
      'cache (vm-mime-make-cache-symbol)
      'message-symbol (vm-mime-make-message-symbol m)
      )))))

(defun vm-mime-get-xxx-parameter (name param-list)
  (let ((match-end (1+ (length name)))
	(name-regexp (concat (regexp-quote name) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match name-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (substring (car param-list) match-end))))

(defun vm-mime-get-parameter (layout name)
  (vm-mime-get-xxx-parameter name (cdr (vm-mm-layout-type layout))))

(defun vm-mime-get-disposition-parameter (layout name)
  (vm-mime-get-xxx-parameter name (cdr (vm-mm-layout-disposition layout))))

(defun vm-mime-set-xxx-parameter (name value param-list)
  (let ((match-end (1+ (length name)))
	(name-regexp (concat (regexp-quote name) "="))
	(case-fold-search t)
	(done nil))
    (while (and param-list (not done))
      (if (and (string-match name-regexp (car param-list))
	       (= (match-end 0) match-end))
	  (setq done t)
	(setq param-list (cdr param-list))))
    (and (car param-list)
	 (setcar param-list (concat name "=" value)))))

(defun vm-mime-set-parameter (layout name value)
  (vm-mime-set-xxx-parameter name value (cdr (vm-mm-layout-type layout))))

(defun vm-mime-set-qparameter (layout name value)
  (setq value (concat "\"" value "\""))
  (vm-mime-set-xxx-parameter name value (cdr (vm-mm-layout-qtype layout))))

(defun vm-mime-insert-mime-body (layout)
  (vm-insert-region-from-buffer (marker-buffer (vm-mm-layout-body-start layout))
				(vm-mm-layout-body-start layout)
				(vm-mm-layout-body-end layout)))

(defun vm-mime-insert-mime-headers (layout)
  (vm-insert-region-from-buffer (marker-buffer (vm-mm-layout-header-start layout))
				(vm-mm-layout-header-start layout)
				(vm-mm-layout-header-end layout)))

(defvar buffer-display-table)
(defvar standard-display-table)
(defvar buffer-file-type)
(defun vm-make-presentation-copy (m)
  (let ((mail-buffer (current-buffer))
	b mm
	(real-m (vm-real-message-of m))
	(modified (buffer-modified-p)))
    (cond ((or (null vm-presentation-buffer-handle)
	       (null (buffer-name vm-presentation-buffer-handle)))
	   (let ((default-enable-multibyte-characters t))
	     (setq b (generate-new-buffer (concat (buffer-name)
						  " Presentation"))))
	   (save-excursion
	     (set-buffer b)
	     (if (fboundp 'buffer-disable-undo)
		 (buffer-disable-undo (current-buffer))
	       ;; obfuscation to make the v19 compiler not whine
	       ;; about obsolete functions.
	       (let ((x 'buffer-flush-undo))
		 (funcall x (current-buffer))))
	     (setq mode-name "VM Presentation"
		   major-mode 'vm-presentation-mode
		   vm-message-pointer (list nil)
		   vm-mail-buffer mail-buffer
		   mode-popup-menu (and vm-use-menus
					(vm-menu-support-possible-p)
					(vm-menu-mode-menu))
		   ;; Default to binary file type for DOS/NT.
		   buffer-file-type t
		   ;; Tell XEmacs/MULE not to mess with the text on writes.
		   buffer-read-only t
		   mode-line-format vm-mode-line-format)
	     ;; scroll in place messes with scroll-up and this loses
	     (defvar scroll-in-place)
	     (make-local-variable 'scroll-in-place)
	     (setq scroll-in-place nil)
	     (if (fboundp 'set-buffer-file-coding-system)
		 (set-buffer-file-coding-system (vm-binary-coding-system) t))
	     (vm-fsfemacs-nonmule-display-8bit-chars)
	     (if (and vm-mutable-frames vm-frame-per-folder
		      (vm-multiple-frames-possible-p))
		 (vm-set-hooks-for-frame-deletion))
	     (use-local-map vm-mode-map)
	     (vm-toolbar-install-or-uninstall-toolbar)
	     (and (vm-menu-support-possible-p)
		  (vm-menu-install-menus))
	     (run-hooks 'vm-presentation-mode-hook))
	   (setq vm-presentation-buffer-handle b)))
    (setq b vm-presentation-buffer-handle
	  vm-presentation-buffer vm-presentation-buffer-handle
	  vm-mime-decoded nil)
    ;; W3 or some other external mode might set some local colors
    ;; in this buffer; remove them before displaying a different
    ;; message here.
    (if (fboundp 'remove-specifier)
	(progn
	  (remove-specifier (face-foreground 'default) b)
	  (remove-specifier (face-background 'default) b)))
    (save-excursion
      (set-buffer (vm-buffer-of real-m))
      (save-restriction
	(widen)
	;; must reference this now so that headers will be in
	;; their final position before the message is copied.
	;; otherwise the vheader offset computed below will be
	;; wrong.
	(vm-vheaders-of real-m)
	(set-buffer b)
	(widen)
	(let ((buffer-read-only nil)
	      (inhibit-read-only t)
	      (modified (buffer-modified-p)))
	  (unwind-protect
	      (progn
		(erase-buffer)
		(insert-buffer-substring (vm-buffer-of real-m)
					 (vm-start-of real-m)
					 (vm-end-of real-m)))
	    (set-buffer-modified-p modified)))
	(setq mm (copy-sequence m))
	(vm-set-location-data-of mm (vm-copy (vm-location-data-of m)))
	(set-marker (vm-start-of mm) (point-min))
	(set-marker (vm-headers-of mm) (+ (vm-start-of mm)
					  (- (vm-headers-of real-m)
					     (vm-start-of real-m))))
	(set-marker (vm-vheaders-of mm) (+ (vm-start-of mm)
					   (- (vm-vheaders-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-text-of mm) (+ (vm-start-of mm)
				       (- (vm-text-of real-m)
					  (vm-start-of real-m))))
	(set-marker (vm-text-end-of mm) (+ (vm-start-of mm)
					   (- (vm-text-end-of real-m)
					      (vm-start-of real-m))))
	(set-marker (vm-end-of mm) (+ (vm-start-of mm)
				      (- (vm-end-of real-m)
					 (vm-start-of real-m))))
	(setcar vm-message-pointer mm)))))

(fset 'vm-presentation-mode 'vm-mode)
(put 'vm-presentation-mode 'mode-class 'special)

(defvar buffer-file-coding-system)

(defun vm-determine-proper-charset (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (catch 'done
	(goto-char (point-min))
	(if (or vm-xemacs-mule-p 
		(and vm-fsfemacs-mule-p enable-multibyte-characters))
	    (let ((charsets (delq 'ascii (vm-charsets-in-region
					  (point-min) (point-max)))))
	      (cond ((null charsets)
		     "us-ascii")
		    ((cdr charsets)
		     (or (car (cdr
			       (assq (vm-coding-system-name
				      buffer-file-coding-system)
				     vm-mime-mule-coding-to-charset-alist)))
			 "iso-2022-jp"))
		    (t
		     (or (car (cdr
			       (assoc
				(car charsets)
				vm-mime-mule-charset-to-charset-alist)))
			 "unknown"))))
	  (and (re-search-forward "[^\000-\177]" nil t)
	       (throw 'done (or vm-mime-8bit-composition-charset
				"iso-8859-1")))
	  (throw 'done vm-mime-7bit-composition-charset))))))

(defun vm-determine-proper-content-transfer-encoding (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (catch 'done
	(goto-char (point-min))
	(and (re-search-forward "[\000\015]" nil t)
	     (throw 'done "binary"))

	(let ((toolong nil) bol)
	  (goto-char (point-min))
	  (setq bol (point))
	  (while (and (not (eobp)) (not toolong))
	    (forward-line)
	    (setq toolong (> (- (point) bol) 998)
		  bol (point)))
	  (and toolong (throw 'done "binary")))
	 
	(goto-char (point-min))
	(and (re-search-forward "[^\000-\177]" nil t)
	     (throw 'done "8bit"))

	"7bit"))))

(defun vm-mime-types-match (type type/subtype)
  (let ((case-fold-search t))
    (cond ((string-match "/" type)
	   (if (and (string-match (regexp-quote type) type/subtype)
		    (equal 0 (match-beginning 0))
		    (equal (length type/subtype) (match-end 0)))
	       t
	     nil ))
	  ((and (string-match (regexp-quote type) type/subtype)
		(equal 0 (match-beginning 0))
		(equal (save-match-data
			 (string-match "/" type/subtype (match-end 0)))
		       (match-end 0)))))))

(defvar native-sound-only-on-console)

(defun vm-mime-can-display-internal (layout &optional deep)
  (let ((type (car (vm-mm-layout-type layout))))
    (cond ((vm-mime-types-match "image/jpeg" type)
	   (and (vm-image-type-available-p 'jpeg) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/gif" type)
	   (and (vm-image-type-available-p 'gif) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/png" type)
	   (and (vm-image-type-available-p 'png) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/tiff" type)
	   (and (vm-image-type-available-p 'tiff) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xpm" type)
	   (and (vm-image-type-available-p 'xpm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/pbm" type)
	   (and (vm-image-type-available-p 'pbm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "image/xbm" type)
	   (and (vm-image-type-available-p 'xbm) (vm-images-possible-here-p)))
	  ((vm-mime-types-match "audio/basic" type)
	   (and vm-xemacs-p
		(or (featurep 'native-sound)
		    (featurep 'nas-sound))
		(or (device-sound-enabled-p)
		    (and (featurep 'native-sound)
			 (not native-sound-only-on-console)
			 (memq (device-type) '(x gtk))))))
	  ((vm-mime-types-match "multipart" type) t)
	  ((vm-mime-types-match "message/external-body" type)
	   (or (not deep)
	       (vm-mime-can-display-internal
		(car (vm-mm-layout-parts layout)) t)))
	  ((vm-mime-types-match "message" type) t)
	  ((vm-mime-types-match "text/html" type)
	   (and (fboundp 'w3-region)
		vm-mime-use-w3-for-text/html
		;; this because GNUS bogusly sets up autoloads
		;; for w3-region even if W3 isn't installed.
		(fboundp 'w3-about)
		(let ((charset (or (vm-mime-get-parameter layout "charset")
				   "us-ascii")))
		  (vm-mime-charset-internally-displayable-p charset))))
	  ((vm-mime-types-match "text" type)
	   (let ((charset (or (vm-mime-get-parameter layout "charset")
			      "us-ascii")))
	     (or (vm-mime-charset-internally-displayable-p charset)
		 (vm-mime-can-convert-charset charset))))
	  (t nil))))

(defun vm-mime-can-convert (type)
  (or (vm-mime-can-convert-0 type vm-mime-type-converter-alist)
      (vm-mime-can-convert-0 type vm-mime-image-type-converter-alist)))

(defun vm-mime-can-convert-0 (type alist)
  (let (
	;; fake layout. make it the wrong length so an error will
	;; be signaled if vm-mime-can-display-internal ever asks
	;; for one of the other fields
	(fake-layout (make-vector 1 (list nil)))
	best second-best)
    (while (and alist (not best))
      (cond ((and (vm-mime-types-match (car (car alist)) type)
		  (not (vm-mime-types-match (nth 1 (car alist)) type)))
	     (cond ((and (not best)
			 (progn
			   (setcar (aref fake-layout 0) (nth 1 (car alist)))
			   (vm-mime-can-display-internal fake-layout)))
		    (setq best (car alist)))
		   ((and (not second-best)
			 (vm-mime-find-external-viewer (nth 1 (car alist))))
		    (setq second-best (car alist))))))
      (setq alist (cdr alist)))
    (or best second-best)))

(defun vm-mime-convert-undisplayable-layout (layout)
  (catch 'done
    (let ((ooo (vm-mime-can-convert (car (vm-mm-layout-type layout))))
	  ex work-buffer)
      (message "Converting %s to %s..."
	       (car (vm-mm-layout-type layout))
	       (nth 1 ooo))
      (save-excursion
	(setq work-buffer (vm-make-work-buffer " *mime object*"))
	(vm-register-message-garbage 'kill-buffer work-buffer)
	(set-buffer work-buffer)
	;; call-process-region calls write-region.
	;; don't let it do CR -> LF translation.
	(setq selective-display nil)
	(vm-mime-insert-mime-body layout)
	(vm-mime-transfer-decode-region layout (point-min) (point-max))
	(setq ex (call-process-region (point-min) (point-max) shell-file-name
				      t t nil shell-command-switch (nth 2 ooo)))
	(if (not (eq ex 0))
	    (progn
	      (message "Conversion from %s to %s failed (exit code %s)"
		       (car (vm-mm-layout-type layout))
		       (nth 1 ooo)
		       ex)
	      (throw 'done nil)))
	(goto-char (point-min))
	(insert "Content-Type: " (nth 1 ooo) "\n")
	(insert "Content-Transfer-Encoding: binary\n\n")
	(set-buffer-modified-p nil)
	(message "Converting %s to %s... done"
		 (car (vm-mm-layout-type layout))
		 (nth 1 ooo))
	(vm-make-layout
	 'type (append (list (nth 1 ooo)) (cdr (vm-mm-layout-type layout)))
	 'qtype (append (list (nth 1 ooo)) (cdr (vm-mm-layout-type layout)))
	 'encoding "binary"
	 'id (vm-mm-layout-id layout)
	 'description (vm-mm-layout-description layout)
	 'disposition (vm-mm-layout-disposition layout)
	 'qdisposition (vm-mm-layout-qdisposition layout)
	 'header-start (vm-marker (point-min))
	 'header-end (vm-marker (1- (point)))
	 'body-start (vm-marker (point))
	 'body-end (vm-marker (point-max))
	 'cache (vm-mime-make-cache-symbol)
	 'message-symbol (vm-mime-make-message-symbol
			  (vm-mm-layout-message layout))
	 'layout-is-converted t
	 'unconverted-layout layout
	 )))))

(defun vm-mime-can-convert-charset (charset)
  (vm-mime-can-convert-charset-0 charset vm-mime-charset-converter-alist))

(defun vm-mime-can-convert-charset-0 (charset alist)
  (let ((done nil))
    (while (and alist (not done))
      (cond ((and (vm-string-equal-ignore-case (car (car alist)) charset)
		  (vm-mime-charset-internally-displayable-p
		   (nth 1 (car alist))))
	     (setq done t))
	    (t (setq alist (cdr alist)))))
    (and alist (car alist))))

(defun vm-mime-convert-undisplayable-charset (layout)
  (let ((charset (vm-mime-get-parameter layout "charset"))
	ooo work-buffer)
    (setq ooo (vm-mime-can-convert-charset charset))
    (message "Converting charset %s to %s..."
	     charset
	     (nth 1 ooo))
    (save-excursion
      (setq work-buffer (vm-make-work-buffer " *mime object*"))
      (vm-register-message-garbage 'kill-buffer work-buffer)
      (set-buffer work-buffer)
      ;; call-process-region calls write-region.
      ;; don't let it do CR -> LF translation.
      (setq selective-display nil)
      (vm-mime-insert-mime-body layout)
      (vm-mime-transfer-decode-region layout (point-min) (point-max))
      (call-process-region (point-min) (point-max) shell-file-name
			   t t nil shell-command-switch (nth 2 ooo))
      (setq layout
	    (vm-make-layout
	     'type (copy-sequence (vm-mm-layout-type layout))
	     'qtype (copy-sequence (vm-mm-layout-type layout))
	     'encoding "binary"
	     'id (vm-mm-layout-id layout)
	     'description (vm-mm-layout-description layout)
	     'disposition (vm-mm-layout-disposition layout)
	     'qdisposition (vm-mm-layout-qdisposition layout)
	     'header-start (vm-marker (point-min))
	     'header-body (vm-marker (1- (point)))
	     'body-start (vm-marker (point))
	     'body-end (vm-marker (point-max))
	     'cache (vm-mime-make-cache-symbol)
	     'message-symbol (vm-mime-make-message-symbol
			      (vm-mm-layout-message layout))
	     'layout-is-converted t
	     'onconverted-layout layout
	     ))
      (vm-mime-set-parameter layout "charset" (nth 1 ooo))
      (vm-mime-set-qparameter layout "charset" (nth 1 ooo))
      (goto-char (point-min))
      (insert-before-markers "Content-Type: " (car (vm-mm-layout-type layout)))
      (insert-before-markers ";\n\t"
			     (mapconcat 'identity
					(car (vm-mm-layout-type layout))
					";\n\t")
			     "\n")
      (insert-before-markers "Content-Transfer-Encoding: binary\n\n")
      (set-buffer-modified-p nil)
      (message "Converting charset %s to %s... done"
	       charset
	       (nth 1 ooo))
      layout)))

(defun vm-mime-charset-convert-region (charset b-start b-end)
  (let ((b (current-buffer))
	start end oldsize work-buffer ooo)
    (setq ooo (vm-mime-can-convert-charset charset))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer " *mime object*"))
	  (setq oldsize (- b-end b-start))
	  (set-buffer work-buffer)
	  (insert-buffer-substring b b-start b-end)
	  ;; call-process-region calls write-region.
	  ;; don't let it do CR -> LF translation.
	  (setq selective-display nil)
	  (call-process-region (point-min) (point-max) shell-file-name
			       t t nil shell-command-switch (nth 2 ooo))
	  (and vm-fsfemacs-mule-p (set-buffer-multibyte t))
	  (setq start (point-min) end (point-max))
	  (save-excursion
	    (set-buffer b)
	    (goto-char b-start)
	    (insert-buffer-substring work-buffer start end)
	    (delete-region (point) (+ (point) oldsize)))
	  (nth 1 ooo))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-should-display-button (layout dont-honor-content-disposition)
  (if (and vm-honor-mime-content-disposition
	   (not dont-honor-content-disposition)
	   (vm-mm-layout-disposition layout))
      (let ((case-fold-search t))
	(string-match "^attachment$" (car (vm-mm-layout-disposition layout))))
    (let ((i-list vm-auto-displayed-mime-content-types)
	  (type (car (vm-mm-layout-type layout)))
	  (matched nil))
      (if (if (eq i-list t)
	      nil
	    (while (and i-list (not matched))
	      (if (vm-mime-types-match (car i-list) type)
		  (setq matched t)
		(setq i-list (cdr i-list))))
	    (not matched))
	  t
	(setq i-list vm-auto-displayed-mime-content-type-exceptions
	      matched nil)
	(while (and i-list (not matched))
	  (if (vm-mime-types-match (car i-list) type)
	      (setq matched t)
	    (setq i-list (cdr i-list))))
	matched ))))

(defun vm-mime-should-display-internal (layout)
  (let ((i-list vm-mime-internal-content-types)
	(type (car (vm-mm-layout-type layout)))
	(matched nil))
    (if (if (eq i-list t)
	    t
	  (while (and i-list (not matched))
	    (if (vm-mime-types-match (car i-list) type)
		(setq matched t)
	      (setq i-list (cdr i-list))))
	  matched )
	(progn
	  (setq i-list vm-mime-internal-content-type-exceptions
		matched nil)
	  (while (and i-list (not matched))
	    (if (vm-mime-types-match (car i-list) type)
		(setq matched t)
	      (setq i-list (cdr i-list))))
	  (not matched))
      nil )))

(defun vm-mime-find-external-viewer (type)
  (catch 'done
    (let ((list vm-mime-external-content-type-exceptions)
	  (matched nil))
      (while list
	(if (vm-mime-types-match (car list) type)
	    (throw 'done nil)
	  (setq list (cdr list))))
      (setq list vm-mime-external-content-types-alist)
      (while (and list (not matched))
	(if (and (vm-mime-types-match (car (car list)) type)
		 (cdr (car list)))
	    (setq matched (cdr (car list)))
	  (setq list (cdr list))))
      matched )))
(fset 'vm-mime-can-display-external 'vm-mime-find-external-viewer)

(defun vm-mime-delete-button-maybe (extent)
  (let ((buffer-read-only))
    ;; if displayed MIME object should replace the button
    ;; remove the button now.
    (cond ((vm-extent-property extent 'vm-mime-disposable)
	   (delete-region (vm-extent-start-position extent)
			  (vm-extent-end-position extent))
	   (vm-detach-extent extent)))))

(defun vm-decode-mime-message ()
  "Decode the MIME objects in the current message.

The first time this command is run on a message, decoding is done.
The second time, buttons for all the objects are displayed instead.
The third time, the raw, undecoded data is displayed.

If decoding, the decoded objects might be displayed immediately, or
buttons might be displayed that you need to activate to view the
object.  See the documentation for the variables

    vm-auto-displayed-mime-content-types
    vm-auto-displayed-mime-content-type-exceptions
    vm-mime-internal-content-types
    vm-mime-internal-content-type-exceptions
    vm-mime-external-content-types-alist

to see how to control whether you see buttons or objects.

If the variable vm-mime-display-function is set, then its value
is called as a function with no arguments, and none of the
actions mentioned in the preceding paragraphs are taken.  At the
time of the call, the current buffer will be the presentation
buffer for the folder and a copy of the current message will be
in the buffer.  The function is expected to make the message
`MIME presentable' to the user in whatever manner it sees fit."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (if (and (not vm-display-using-mime)
	   (null vm-mime-display-function))
      (error "MIME display disabled, set vm-display-using-mime non-nil to enable."))
  (if vm-mime-display-function
      (progn
	(vm-make-presentation-copy (car vm-message-pointer))
	(set-buffer vm-presentation-buffer)
	(funcall vm-mime-display-function))
    (if vm-mime-decoded
	(if (eq vm-mime-decoded 'decoded)
	    (let ((vm-preview-lines nil)
		  (vm-auto-decode-mime-messages t)
		  (vm-honor-mime-content-disposition nil)
		  (vm-auto-displayed-mime-content-types '("multipart"))
		  (vm-auto-displayed-mime-content-type-exceptions nil))
	      (setq vm-mime-decoded nil)
	      (intern (buffer-name) vm-buffers-needing-display-update)
	      (save-excursion
		(vm-preview-current-message))
	      (setq vm-mime-decoded 'buttons))
	  (let ((vm-preview-lines nil)
		(vm-auto-decode-mime-messages nil))
	    (intern (buffer-name) vm-buffers-needing-display-update)
	    (vm-preview-current-message)))
      (let ((layout (vm-mm-layout (car vm-message-pointer)))
	    (m (car vm-message-pointer)))
	(message "Decoding MIME message...")
	(cond ((stringp layout)
	       (error "Invalid MIME message: %s" layout)))
	(if (vm-mime-plain-message-p m)
	    (error "Message needs no decoding."))
	(or vm-presentation-buffer
	    ;; maybe user killed it
	    (error "No presentation buffer."))
	(set-buffer vm-presentation-buffer)
	(if (and (interactive-p) (eq vm-system-state 'previewing))
	    (let ((vm-display-using-mime nil))
	      (vm-show-current-message)))
	(setq m (car vm-message-pointer))
	(vm-save-restriction
	 (widen)
	 (goto-char (vm-text-of m))
	 (let ((buffer-read-only nil)
	       (modified (buffer-modified-p)))
	   (unwind-protect
	       (save-excursion
		 (and (not (eq (vm-mm-encoded-header m) 'none))
		      (vm-decode-mime-message-headers m))
		 (if (vectorp layout)
		     (progn
		       (vm-decode-mime-layout layout)
		       (delete-region (point) (point-max))))
		 (vm-energize-urls)
		 (vm-highlight-headers-maybe)
		 (vm-energize-headers-and-xfaces))
	     (set-buffer-modified-p modified))))
	(save-excursion (set-buffer vm-mail-buffer)
			(setq vm-mime-decoded 'decoded))
	(intern (buffer-name vm-mail-buffer) vm-buffers-needing-display-update)
	(vm-update-summary-and-mode-line)
	(message "Decoding MIME message... done"))))
  (vm-display nil nil '(vm-decode-mime-message)
	      '(vm-decode-mime-message reading-message)))

(defun vm-decode-mime-layout (layout &optional dont-honor-c-d)
  (let ((modified (buffer-modified-p))
	new-layout file type type2 type-no-subtype (extent nil))
    (unwind-protect
	(progn
	  (if (not (vectorp layout))
	      (progn
		(setq extent layout
		      layout (vm-extent-property extent 'vm-mime-layout))
		(goto-char (vm-extent-start-position extent))))
	  (setq type (downcase (car (vm-mm-layout-type layout)))
		type-no-subtype (car (vm-parse type "\\([^/]+\\)")))
	  (cond ((and vm-infer-mime-types
		      (vm-mime-types-match "application/octet-stream" type)
		      (setq file
			    (or
			     (vm-mime-get-disposition-parameter layout
								"filename")
			     (vm-mime-get-parameter layout "name")))
		      (setq type2 (vm-mime-default-type-from-filename file))
		      (not (vm-mime-types-match type type2)))
		 (vm-set-mm-layout-type layout (list type2))
		 (vm-set-mm-layout-qtype layout
					 (list (concat "\"" type2 "\"")))
		 (setq type (downcase (car (vm-mm-layout-type layout)))
		       type-no-subtype (car (vm-parse type "\\([^/]+\\)")))))
	  (cond ((and (vm-mime-should-display-button layout dont-honor-c-d)
		      (or (condition-case nil
			      (funcall (intern
					(concat "vm-mime-display-button-"
						type))
				       layout)
			    (void-function nil))
			  (condition-case nil
			      (funcall (intern
					(concat "vm-mime-display-button-"
						type-no-subtype))
				       layout)
			    (void-function nil)))))
		((and (vm-mime-should-display-internal layout)
		      (or (condition-case nil
			      (funcall (intern
					(concat "vm-mime-display-internal-"
						type))
				       layout)
			    (void-function nil))
			  (condition-case nil
			      (funcall (intern
					(concat "vm-mime-display-internal-"
						type-no-subtype))
				       layout)
			    (void-function nil)))))
		((vm-mime-types-match "multipart" type)
		 (or (condition-case nil
			 (funcall (intern
				   (concat "vm-mime-display-internal-"
					   type))
				  layout)
		       (void-function nil))
		     (vm-mime-display-internal-multipart/mixed layout)))
		((and (vm-mime-can-display-external type)
		      (vm-mime-display-external-generic layout))
		 (and extent (vm-set-extent-property
			      extent 'vm-mime-disposable nil)))
		((and (not (vm-mm-layout-is-converted layout))
		      (vm-mime-can-convert type)
		      (setq new-layout
			    (vm-mime-convert-undisplayable-layout layout)))
		 ;; a button should always go away if we're doing
		 ;; a conversion.
		 (if extent
		     (vm-set-extent-property extent 'vm-mime-disposable t))
		 (vm-decode-mime-layout new-layout))
		(t (and extent (vm-mime-rewrite-failed-button
				extent
				(or (vm-mm-layout-display-error layout)
				    "no external viewer defined for type")))
		   (if (vm-mime-types-match type "message/external-body")
		       (if (null extent)
			   (vm-mime-display-button-xxxx layout t)
			 (setq extent nil))
		     (vm-mime-display-internal-application/octet-stream
		      (or extent layout)))))
	  (and extent (vm-mime-delete-button-maybe extent)))
      (set-buffer-modified-p modified)))
  t )

(defun vm-mime-display-button-text (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-text (layout)
  (vm-mime-display-internal-text/plain layout))

(defun vm-mime-display-internal-text/html (layout)
  (if (and (fboundp 'w3-region)
	   vm-mime-use-w3-for-text/html)
      (condition-case error-data
	  (let ((buffer-read-only nil)
		(start (point))
		(charset (or (vm-mime-get-parameter layout "charset")
			     "us-ascii"))
		end buffer-size)
	    (message "Inlining text/html, be patient...")
	    (vm-mime-insert-mime-body layout)
	    (setq end (point-marker))
	    (vm-mime-transfer-decode-region layout start end)
	    (vm-mime-charset-decode-region charset start end)
	    ;; w3-region apparently deletes all the text in the
	    ;; region and then insert new text.  This makes the
	    ;; end == start.  The fix is to move the end marker
	    ;; forward with a placeholder character so that when
	    ;; w3-region delete all the text, end will still be
	    ;; ahead of the insertion point and so will be moved
	    ;; forward when the new text is inserted.  We'll
	    ;; delete the placeholder afterward.
	    (goto-char end)
	    (insert-before-markers "z")
	    (w3-region start (1- end))
	    (goto-char end)
	    (delete-char -1)
	    ;; remove read-only text properties
	    (let ((inhibit-read-only t))
	      (remove-text-properties start end '(read-only nil)))
	    (goto-char end)
	    (message "Inlining text/html... done")
	    t )
	(error (vm-set-mm-layout-display-error
		layout
		(format "Inline HTML display failed: %s"
			(prin1-to-string error-data)))
	       (message "%s" (vm-mm-layout-display-error layout))
	       (sleep-for 2)
	       nil ))
    (vm-set-mm-layout-display-error layout "Need W3 to inline HTML")
    (message "%s" (vm-mm-layout-display-error layout))
    nil ))

(defun vm-mime-display-internal-text/plain (layout &optional no-highlighting)
  (let ((start (point)) end need-conversion
	(buffer-read-only nil)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (if (and (not (vm-mime-charset-internally-displayable-p charset))
	     (not (setq need-conversion (vm-mime-can-convert-charset charset))))
	(progn
	  (vm-set-mm-layout-display-error
	   layout (concat "Undisplayable charset: " charset))
	  nil)
      (vm-mime-insert-mime-body layout)
      (setq end (point-marker))
      (vm-mime-transfer-decode-region layout start end)
      (and need-conversion
	   (setq charset (vm-mime-charset-convert-region charset start end)))
      (vm-mime-charset-decode-region charset start end)
      (or no-highlighting (vm-energize-urls-in-message-region start end))
      (if (and vm-fill-paragraphs-containing-long-lines
	       (not no-highlighting))
	  (let ((needmsg (> (- end start) 12000)))
	    (if needmsg
		(message "Searching for paragraphs to fill..."))
	    (vm-fill-paragraphs-containing-long-lines
	     vm-fill-paragraphs-containing-long-lines
	     start end)
	    (if needmsg
		(message "Searching for paragraphs to fill... done"))))
      (goto-char end)
      t )))

(defun vm-mime-display-internal-text/enriched (layout)
  (require 'enriched)
  (let ((start (point)) end
	(buffer-read-only nil)
	(enriched-verbose t)
	(charset (or (vm-mime-get-parameter layout "charset") "us-ascii")))
    (message "Decoding text/enriched, be patient...")
    (vm-mime-insert-mime-body layout)
    (setq end (point-marker))
    (vm-mime-transfer-decode-region layout start end)
    (vm-mime-charset-decode-region charset start end)
    ;; enriched-decode expects a couple of headers at the top of
    ;; the region and will remove anything that looks like a
    ;; header.  Put a header section here for it to eat so it
    ;; won't eat message text instead.
    (goto-char start)
    (insert "Comment: You should not see this header\n\n")
    (condition-case errdata
	(enriched-decode start end)
      (error (vm-set-mm-layout-display-error
	      layout (format "enriched-decode signaled %s" errdata))
	     (message "%s" (vm-mm-layout-display-error layout))
	     (sleep-for 2)
	     nil ))
    (vm-energize-urls-in-message-region start end)
    (goto-char end)
    (message "Decoding text/enriched... done")
    t ))

(defun vm-mime-display-external-generic (layout)
  (let ((program-list (copy-sequence
		       (vm-mime-find-external-viewer
			(car (vm-mm-layout-type layout)))))
	(buffer-read-only nil)
	start
	(coding-system-for-read (vm-binary-coding-system))
	(coding-system-for-write (vm-binary-coding-system))
	(append-file t)
	process	tempfile cache end suffix basename)
    (setq cache (get (vm-mm-layout-cache layout)
		     'vm-mime-display-external-generic)
	  process (nth 0 cache)
	  tempfile (nth 1 cache))
    (if (and (processp process) (eq (process-status process) 'run))
	t
      (cond ((or (null tempfile) (null (file-exists-p tempfile)))
	     (cond (vm-fsfemacs-mule-p
		    (let (work-buffer (target (current-buffer)))
		      (unwind-protect
			  (save-excursion
			    (setq work-buffer (vm-make-work-buffer))
			    (set-buffer work-buffer)
			    (vm-mime-insert-mime-body layout)
			    (vm-mime-transfer-decode-region layout
							    (point-min)
							    (point-max))
			    (set-buffer-multibyte t)
			    (set-buffer target)
			    (setq start (point))
			    (insert-buffer-substring work-buffer)
			    (setq end (point-marker)))
			(and work-buffer (kill-buffer work-buffer)))))
		   (t
		    (setq start (point))
		    (vm-mime-insert-mime-body layout)
		    (setq end (point-marker))
		    (vm-mime-transfer-decode-region layout start end)))
	     (setq suffix (vm-mime-extract-filename-suffix layout)
		   suffix (or suffix
			      (vm-mime-find-filename-suffix-for-type layout)))
	     (setq basename
		   (or (vm-mime-get-disposition-parameter layout "filename")
		       (vm-mime-get-parameter layout "name")))
	     (setq tempfile (vm-make-tempfile suffix basename))
	     (vm-register-message-garbage-files (list tempfile))
	     (let ((buffer-file-type buffer-file-type)
		   (selective-display nil)
		   buffer-file-coding-system)
	       ;; Tell DOS/Windows NT whether the file is binary
	       (setq buffer-file-type
		     (not (vm-mime-text-type-layout-p layout)))
	       ;; Tell XEmacs/MULE not to mess with the bits unless
	       ;; this is a text type.
	       (if (fboundp 'set-buffer-file-coding-system)
		   (if (vm-mime-text-type-layout-p layout)
		       (set-buffer-file-coding-system
			(vm-line-ending-coding-system) nil)
		     (set-buffer-file-coding-system
		      (vm-binary-coding-system) t)))
	       (write-region start end tempfile nil 0)
	       (delete-region start end))))

      ;; expand % specs
      (let ((p program-list)
	    (vm-mf-attachment-file tempfile))
	(while p
	  (if (string-match "\\([^%]\\|^\\)%f" (car p))
	      (setq append-file nil))
	  (setcar p (vm-mime-sprintf (car p) layout))
	  (setq p (cdr p))))

      (message "Launching %s..." (mapconcat 'identity program-list " "))
      (setq process
	    (if (cdr program-list)
		(apply 'start-process
		       (format "view %25s"
			       (vm-mime-sprintf
				(vm-mime-find-format-for-layout layout)
				layout))
		       nil (if append-file
			       (append program-list (list tempfile))
			     program-list))
	      (apply 'start-process
		     (format "view %25s"
			     (vm-mime-sprintf
			      (vm-mime-find-format-for-layout layout)
			      layout))
		     nil
		     (or shell-file-name "sh")
		     shell-command-switch
		     (if append-file
			 (list (concat (car program-list) " " tempfile))
		       program-list))))
      (process-kill-without-query process t)
      (message "Launching %s... done" (mapconcat 'identity
						 program-list
						 " "))
      (if vm-mime-delete-viewer-processes
	  (vm-register-message-garbage 'delete-process process))
      (put (vm-mm-layout-cache layout)
	   'vm-mime-display-external-generic
	   (list process tempfile))))
  t )

(defun vm-mime-display-internal-application/octet-stream (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil)
	    (vm-mf-default-action "save to a file"))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-application/octet-stream layout))))
	 layout nil))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    ;; support old "name" paramater for application/octet-stream
    ;; but don't override the "filename" parameter extracted from
    ;; Content-Disposition, if any.
    (let ((default-filename
	    (if (vm-mime-get-disposition-parameter layout "filename")
		nil
	      (vm-mime-get-parameter layout "name")))
	  (file nil))
      (setq file (vm-mime-send-body-to-file layout default-filename))
      (if vm-mime-delete-after-saving
	  (let ((vm-mime-confirm-delete nil))
	    ;; we don't care if the delete fails
	    (condition-case nil
		(vm-delete-mime-object (expand-file-name file))
	      (error nil))))))
  t )
(fset 'vm-mime-display-button-application/octet-stream
      'vm-mime-display-internal-application/octet-stream)

(defun vm-mime-display-button-application (layout)
  (vm-mime-display-button-xxxx layout nil))

(defun vm-mime-display-button-image (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-audio (layout)
  (vm-mime-display-button-xxxx layout nil))

(defun vm-mime-display-button-video (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-message (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-button-multipart (layout)
  (vm-mime-display-button-xxxx layout t))

(defun vm-mime-display-internal-multipart/mixed (layout)
  (let ((part-list (vm-mm-layout-parts layout)))
    (while part-list
      (vm-decode-mime-layout (car part-list))
      (setq part-list (cdr part-list)))
    t ))

(defun vm-mime-display-internal-multipart/alternative (layout)
  (let (best-layout)
    (cond ((eq vm-mime-alternative-select-method 'best)
	   (let ((done nil)
		 (best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (if (or (vm-mime-can-display-internal (car part-list) t)
		       (vm-mime-find-external-viewer type))
		   (setq best (car part-list)
			 done t)
		 (setq part-list (cdr part-list))))
	     (setq best-layout (or best (car (vm-mm-layout-parts layout))))))
	  ((eq vm-mime-alternative-select-method 'best-internal)
	   (let ((done nil)
		 (best nil)
		 (second-best nil)
		 part-list type)
	     (setq part-list (vm-mm-layout-parts layout)
		   part-list (nreverse (copy-sequence part-list)))
	     (while (and part-list (not done))
	       (setq type (car (vm-mm-layout-type (car part-list))))
	       (cond ((and (vm-mime-can-display-internal (car part-list) t)
			   (vm-mime-should-display-internal (car part-list)))
		      (setq best (car part-list)
			    done t))
		     ((and (null second-best)
			   (vm-mime-find-external-viewer type))
		      (setq second-best (car part-list))))
	       (setq part-list (cdr part-list)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-select-method)
		(eq (car vm-mime-alternative-select-method)
		    'favorite-internal))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-select-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((or (vm-mime-can-display-internal (car part-list) t)
			    (vm-mime-find-external-viewer type))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout))))))
	  ((and (consp vm-mime-alternative-select-method)
		(eq (car vm-mime-alternative-select-method) 'favorite))
	   (let ((done nil)
		 (best nil)
		 (saved-part-list
		  (nreverse (copy-sequence (vm-mm-layout-parts layout))))
		 (favs (cdr vm-mime-alternative-select-method))
		 (second-best nil)
		 part-list type)
	     (while (and favs (not done))
	       (setq part-list saved-part-list)
	       (while (and part-list (not done))
		 (setq type (car (vm-mm-layout-type (car part-list))))
		 (cond ((and (vm-mime-can-display-internal (car part-list) t)
			     (vm-mime-should-display-internal (car part-list)))
			(if (vm-mime-types-match (car favs) type)
			    (setq best (car part-list)
				  done t)
			  (or second-best
			      (setq second-best (car part-list))))))
		 (setq part-list (cdr part-list)))
	       (setq favs (cdr favs)))
	     (setq best-layout (or best second-best
				   (car (vm-mm-layout-parts layout)))))))
    (and best-layout (vm-decode-mime-layout best-layout))))

(defun vm-mime-display-button-multipart/parallel (layout)
  (vm-mime-insert-button
   (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
   (function
    (lambda (layout)
      (save-excursion
	(let ((vm-auto-displayed-mime-content-types t)
	      (vm-auto-displayed-mime-content-type-exceptions nil))
	  (vm-decode-mime-layout layout t)))))
   layout t))

(fset 'vm-mime-display-internal-multipart/parallel
      'vm-mime-display-internal-multipart/mixed)

(defun vm-mime-display-internal-multipart/digest (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-multipart/digest layout))))
	 layout nil))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (generate-new-buffer (format "digest from %s/%s"
					     (buffer-name vm-mail-buffer)
					     (vm-number-of
					      (car vm-message-pointer)))))
    (setq vm-folder-type vm-default-folder-type)
    (let ((ident-header nil))
      (if vm-digest-identifier-header-format
	  (setq ident-header (vm-summary-sprintf
			      vm-digest-identifier-header-format
			      (vm-mm-layout-message layout))))
      (vm-mime-burst-layout layout ident-header))
    (vm-save-buffer-excursion
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )
(fset 'vm-mime-display-button-multipart/digest
      'vm-mime-display-internal-multipart/digest)

(defun vm-mime-display-button-message/rfc822 (layout)
  (let ((buffer-read-only nil))
    (vm-mime-insert-button
     (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
     (function
      (lambda (layout)
	(save-excursion
	  (vm-mime-display-internal-message/rfc822 layout))))
     layout nil)))
(fset 'vm-mime-display-button-message/news
      'vm-mime-display-button-message/rfc822)

(defun vm-mime-display-internal-message/rfc822 (layout)
  (if (vectorp layout)
      (let ((start (point))
	    (buffer-read-only nil))
	(vm-mime-insert-mime-headers (car (vm-mm-layout-parts layout)))
	(insert ?\n)
	(save-excursion
	  (goto-char start)
	  (vm-reorder-message-headers nil vm-visible-headers
				      vm-invisible-header-regexp))
	(save-restriction
	  (narrow-to-region start (point))
	  (vm-decode-mime-encoded-words))
	(vm-mime-display-internal-multipart/mixed layout))
    (goto-char (vm-extent-start-position layout))
    (setq layout (vm-extent-property layout 'vm-mime-layout))
    (set-buffer (generate-new-buffer
		 (format "message from %s/%s"
			 (buffer-name vm-mail-buffer)
			 (vm-number-of
			  (car vm-message-pointer)))))
    (if vm-fsfemacs-mule-p
	(set-buffer-multibyte nil))
    (setq vm-folder-type vm-default-folder-type)
    (vm-mime-burst-layout layout nil)
    (set-buffer-modified-p nil)
    (vm-save-buffer-excursion
     (vm-goto-new-folder-frame-maybe 'folder)
     (vm-mode)
     (if (vm-should-generate-summary)
	 (progn
	   (vm-goto-new-summary-frame-maybe)
	   (vm-summarize))))
    ;; temp buffer, don't offer to save it.
    (setq buffer-offer-save nil)
    (vm-display (or vm-presentation-buffer (current-buffer)) t
		(list this-command) '(vm-mode startup)))
  t )
(fset 'vm-mime-display-internal-message/news
      'vm-mime-display-internal-message/rfc822)

(defun vm-mime-display-internal-message/delivery-status (layout)
  (vm-mime-display-internal-text/plain layout t))

(defun vm-mime-retrieve-external-body (layout)
  "Fetch an external body into the current buffer.
LAYOUT is the MIME layout struct for the message/external-body object."
  (let ((access-method (downcase (vm-mime-get-parameter layout "access-type")))
	(work-buffer (current-buffer)))
    (cond ((string= access-method "local-file")
	   (let ((name (vm-mime-get-parameter layout "name")))
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (not (file-exists-p name))
		 (vm-mime-error "file %s does not exist" name))
	     (condition-case data
		 (insert-file-contents name)
	       (error (signal 'vm-mime-error (cdr data))))))
	  ((and (string= access-method "url")
		vm-url-retrieval-methods)
	   (defvar w3-configuration-directory) ; for bytecompiler
	   (let ((url (vm-mime-get-parameter layout "url"))
		 ;; needed or url-retrieve will bitch
		 (w3-configuration-directory
		  (if (boundp 'w3-configuration-directory)
		      w3-configuration-directory
		    "~")))
	     (if (null url)
		 (vm-mime-error
		  "%s access type missing `url' parameter"
		  access-method))
	     (setq url (vm-with-string-as-temp-buffer
			url
			(function
			 (lambda ()
			   (goto-char (point-min))
			   (while (re-search-forward "[ \t\n]" nil t)
			     (delete-char -1))))))
	     (vm-mime-fetch-url-with-programs url work-buffer)))
	  ((and (or (string= access-method "ftp")
		    (string= access-method "anon-ftp"))
		(or (fboundp 'efs-file-handler-function)
		    (fboundp 'ange-ftp-hook-function)))
	   (let ((name (vm-mime-get-parameter layout "name"))
		 (directory (vm-mime-get-parameter layout "directory"))
		 (site (vm-mime-get-parameter layout "site"))
		 user)
	     (if (null name)
		 (vm-mime-error
		  "%s access type missing `name' parameter"
		  access-method))
	     (if (null site)
		 (vm-mime-error
		  "%s access type missing `site' parameter"
		  access-method))
	     (cond ((string= access-method "ftp")
		    (setq user (read-string
				(format "User name to access %s: "
					site)
				(user-login-name))))
		   (t (setq user "anonymous")))
	     (if (and (string= access-method "ftp")
		      vm-url-retrieval-methods
		      (vm-mime-fetch-url-with-programs
		       (if directory
			   (concat "ftp:////" site "/"
				   directory "/" name)
			 (concat "ftp:////" site "/" name))
		       work-buffer))
		 t
	       (cond (directory
		      (setq directory
			    (concat "/" user "@" site ":" directory))
		      (setq name (expand-file-name name directory)))
		     (t
		      (setq name (concat "/" user "@" site ":"
					 name))))
	       (condition-case data
		   (insert-file-contents name)
		 (error (signal 'vm-mime-error
				(format "%s" (cdr data)))))))))))


(defun vm-mime-display-internal-message/external-body (layout)
  (let ((child-layout (car (vm-mm-layout-parts layout)))
	(access-method (downcase (vm-mime-get-parameter layout "access-type")))
	ob
	(work-buffer nil))
    ;; Normal objects have the header and body in the same
    ;; buffer.  A retrieved external-body has the body in a
    ;; different buffer from the header, so we use this as an
    ;; indicator of whether the retrieval work has been dnoe
    ;; yet.
    (unwind-protect
	(cond
	 ((and (eq access-method "mail-server")
	       (vm-mm-layout-id child-layout)
	       (setq ob (vm-mime-find-leaf-content-id-in-layout-folder
			 layout (vm-mm-layout-id child-layout))))
	  (setq child-layout ob))
	 ((eq (marker-buffer (vm-mm-layout-header-start child-layout))
	      (marker-buffer (vm-mm-layout-body-start child-layout)))
	  (condition-case data
	      (save-excursion
		(setq work-buffer
		      (vm-make-multibyte-work-buffer
		       (format "*%s mime object*"
			       (car (vm-mm-layout-type child-layout)))))
		(set-buffer work-buffer)
		(if (fboundp 'set-buffer-file-coding-system)
		    (set-buffer-file-coding-system
		     (vm-binary-coding-system) t))
		(cond
		 ((or (string= access-method "ftp")
		      (string= access-method "anon-ftp")
		      (string= access-method "local-file")
		      (string= access-method "url"))
		  (vm-mime-retrieve-external-body layout))
		 ((string= access-method "mail-server")
		  (let ((server (vm-mime-get-parameter layout "server"))
			(subject (vm-mime-get-parameter layout "subject")))
		    (if (null server)
			(vm-mime-error
			 "%s access type missing `server' parameter"
			 access-method))
		    (if (not
			 (y-or-n-p
			  (format
			   "Send message to %s to retrieve external body? "
			   server)))
			(error "Aborted"))
		    (vm-mail-internal
		     (format "mail to MIME mail server %s" server)
		     server subject)
		    (mail-text)
		    (vm-mime-insert-mime-body child-layout)
		    (let ((vm-confirm-mail-send nil))
		      (vm-mail-send))
		    (message "Retrieval message sent.  Retry viewing this object after the response arrives.")
		    (sleep-for 2)))
		 (t
		  (vm-mime-error "unsupported access method: %s"
				 access-method)))
		(cond (child-layout
		       (setq work-buffer nil)
		       (vm-set-mm-layout-body-end child-layout
						  (vm-marker (point-max)))
		       (vm-set-mm-layout-body-start child-layout
						    (vm-marker
						     (point-min))))))
	    (vm-mime-error
	     (vm-set-mm-layout-display-error layout (cdr data))
	     (setq child-layout nil)))))
      (and work-buffer (kill-buffer work-buffer)))
    (and child-layout (vm-decode-mime-layout child-layout))))

(defun vm-mime-fetch-url-with-programs (url buffer)
  (and
   (eq t (cond ((if (and (memq 'wget vm-url-retrieval-methods)
			 (condition-case data
			     (vm-run-command-on-region (point) (point)
						       buffer
						       vm-wget-program
						       "-q" "-O" "-" url)
			   (error nil)))
		    t
		  (save-excursion
		    (set-buffer buffer)
		    (erase-buffer)
		    nil )))
	       ((if (and (memq 'w3m vm-url-retrieval-methods)
			 (condition-case data
			     (vm-run-command-on-region (point) (point)
						       buffer
						       vm-w3m-program
						       "-dump_source" url)
			   (error nil)))
		    t
		  (save-excursion
		    (set-buffer buffer)
		    (erase-buffer)
		    nil )))
	       ((if (and (memq 'fetch vm-url-retrieval-methods)
			 (condition-case data
			     (vm-run-command-on-region (point) (point)
						       buffer
						       vm-fetch-program
						       "-o" "-" url)
			   (error nil)))
		    t
		  (save-excursion
		    (set-buffer buffer)
		    (erase-buffer)
		    nil )))
	       ((if (and (memq 'curl vm-url-retrieval-methods)
			 (condition-case data
			     (vm-run-command-on-region (point) (point)
						       buffer
						       vm-curl-program
						       url)
			   (error nil)))
		    t
		  (save-excursion
		    (set-buffer buffer)
		    (erase-buffer)
		    nil )))
	       ((if (and (memq 'lynx vm-url-retrieval-methods)
			 (condition-case data
			     (vm-run-command-on-region (point) (point)
						       buffer
						       vm-lynx-program
						       "-source" url)
			   (error nil)))
		    t
		  (save-excursion
		    (set-buffer buffer)
		    (erase-buffer)
		    nil )))))
   (save-excursion
     (set-buffer buffer)
     (not (zerop (buffer-size))))))

(defun vm-mime-internalize-local-external-bodies (layout)
  (cond ((vm-mime-types-match "message/external-body"
			      (car (vm-mm-layout-type layout)))
	 (if (not (string= (downcase
			    (vm-mime-get-parameter layout "access-type"))
			   "local-file"))
	     nil
	   (let ((work-buffer nil))
	     (unwind-protect
		 (let ((child-layout (car (vm-mm-layout-parts layout)))
		       oldsize
		       (i (1- (length layout))))
		   (save-excursion
		     (setq work-buffer
			   (vm-make-multibyte-work-buffer
			    (format "*%s mime object*"
				    (car (vm-mm-layout-type child-layout)))))
		     (set-buffer work-buffer)
		     (vm-mime-retrieve-external-body layout))
		   (goto-char (vm-mm-layout-body-start child-layout))
		   (setq oldsize (buffer-size))
		   (condition-case data
		       (insert-buffer-substring work-buffer)
		     (error (signal 'vm-mime-error (cdr data))))
		   (goto-char (+ (point) (- (buffer-size) oldsize)))
		   (if (< (point) (vm-mm-layout-body-end child-layout))
		       (delete-region (point)
				      (vm-mm-layout-body-end child-layout))
		     (vm-set-mm-layout-body-end child-layout (point-marker)))
		   (delete-region (vm-mm-layout-header-start layout)
				  (vm-mm-layout-body-start layout))
		   (while (>= i 0)
		     (aset layout i (aref child-layout i))
		     (setq i (1- i)))))
	     (and work-buffer (kill-buffer work-buffer)))))
	((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	 (let ((p (vm-mm-layout-parts layout)))
	   (while p
	     (vm-mime-internalize-local-external-bodies (car p))
	     (setq p (cdr p)))))
	(t nil)))

(defun vm-mime-display-internal-message/partial (layout)
  (if (vectorp layout)
      (let ((buffer-read-only nil))
	(vm-mime-insert-button
	 (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
	 (function
	  (lambda (layout)
	    (save-excursion
	      (vm-mime-display-internal-message/partial layout))))
	 layout nil))
    (message "Assembling message...")
    (let ((parts nil)
	  (missing nil)
	  (work-buffer nil)
	  extent id o number total m i prev part-header-pos
	  p-id p-number p-total p-list)
      (setq extent layout
	    layout (vm-extent-property extent 'vm-mime-layout)
	    id (vm-mime-get-parameter layout "id"))
      (if (null id)
	  (vm-mime-error
	   "message/partial message missing id parameter"))
      (save-excursion
	(set-buffer (marker-buffer (vm-mm-layout-body-start layout)))
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (and (search-forward id nil t)
			(setq m (vm-message-at-point)))
	      (setq o (vm-mm-layout m))
	      (if (not (vectorp o))
		  nil
		(setq p-list (vm-mime-find-message/partials o id))
		(while p-list
		  (setq p-id (vm-mime-get-parameter (car p-list) "id"))
		  (setq p-total (vm-mime-get-parameter (car p-list) "total"))
		  (if (null p-total)
		      nil
		    (setq p-total (string-to-int p-total))
		    (if (< p-total 1)
			(vm-mime-error "message/partial specified part total < 1, %d" p-total))
		    (if total
			(if (not (= total p-total))
			    (vm-mime-error "message/partial specified total differs between parts, (%d != %d)" p-total total))
		      (setq total p-total)))
		  (setq p-number (vm-mime-get-parameter (car p-list) "number"))
		  (if (null p-number)
		      (vm-mime-error
		       "message/partial message missing number parameter"))
		  (setq p-number (string-to-int p-number))
		  (if (< p-number 1)
		      (vm-mime-error "message/partial part number < 1, %d"
				     p-number))
		  (if (and total (> p-number total))
		      (vm-mime-error "message/partial part number greater than expected number of parts, (%d > %d)" p-number total))
		  (setq parts (cons (list p-number (car p-list)) parts)
			p-list (cdr p-list))))
	      (goto-char (vm-mm-layout-body-end o))))))
      (if (null total)
	  (vm-mime-error "total number of parts not specified in any message/partial part"))
      (setq parts (sort parts
			(function
			 (lambda (p q)
			   (< (car p)
			      (car q))))))
      (setq i 0
	    p-list parts)
      (while p-list
	(cond ((< i (car (car p-list)))
	       (vm-increment i)
	       (cond ((not (= i (car (car p-list))))
		      (setq missing (cons i missing)))
		     (t (setq prev p-list
			      p-list (cdr p-list)))))
	      (t
	       ;; remove duplicate part
	       (setcdr prev (cdr p-list))
	       (setq p-list (cdr p-list)))))
      (while (< i total)
	(vm-increment i)
	(setq missing (cons i missing)))
      (if missing
	  (vm-mime-error "part%s %s%s missing"
			 (if (cdr missing) "s" "")
			 (mapconcat
			  (function identity)
			  (nreverse (mapcar 'int-to-string
					    (or (cdr missing) missing)))
			  ", ")
			 (if (cdr missing)
			     (concat " and " (car missing))
			   "")))
      (set-buffer (generate-new-buffer "assembled message"))
      (if vm-fsfemacs-mule-p
	  (set-buffer-multibyte nil))
      (setq vm-folder-type vm-default-folder-type)
      (vm-mime-insert-mime-headers (car (cdr (car parts))))
      (goto-char (point-min))
      (vm-reorder-message-headers
       nil nil
"\\(Encrypted\\|Content-\\|MIME-Version\\|Message-ID\\|Subject\\|X-VM-\\|Status\\)")
      (goto-char (point-max))
      (setq part-header-pos (point))
      (while parts
	(vm-mime-insert-mime-body (car (cdr (car parts))))
	(setq parts (cdr parts)))
      (goto-char part-header-pos)
      (vm-reorder-message-headers
       nil '("Subject" "MIME-Version" "Content-" "Message-ID" "Encrypted") nil)
      (vm-munge-message-separators vm-folder-type (point-min) (point-max))
      (goto-char (point-min))
      (insert (vm-leading-message-separator))
      (goto-char (point-max))
      (insert (vm-trailing-message-separator))
      (set-buffer-modified-p nil)
      (message "Assembling message... done")
      (vm-save-buffer-excursion
       (vm-goto-new-folder-frame-maybe 'folder)
       (vm-mode)
       (if (vm-should-generate-summary)
	   (progn
	     (vm-goto-new-summary-frame-maybe)
	     (vm-summarize))))
      ;; temp buffer, don't offer to save it.
      (setq buffer-offer-save nil)
      (vm-display (or vm-presentation-buffer (current-buffer)) t
		  (list this-command) '(vm-mode startup)))
    t ))
(fset 'vm-mime-display-button-message/partial
      'vm-mime-display-internal-message/partial)

(defun vm-mime-display-internal-image-xxxx (layout image-type name)
  (cond
   (vm-xemacs-p
    (vm-mime-display-internal-image-xemacs-xxxx layout image-type name))
   ((and vm-fsfemacs-p (fboundp 'image-type-available-p))
    (vm-mime-display-internal-image-fsfemacs-21-xxxx layout image-type name))
   (vm-fsfemacs-p 
    (vm-mime-display-internal-image-fsfemacs-19-xxxx layout image-type name))))

(defun vm-mime-display-internal-image-xemacs-xxxx (layout image-type name)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (let ((start (point-marker)) end tempfile g e
	    (selective-display nil)
	    (incremental vm-mime-display-image-strips-incrementally)
	    do-strips
	    (keymap (make-sparse-keymap))
	    (buffer-read-only nil))
	(if (and (setq tempfile (get (vm-mm-layout-cache layout)
				     'vm-mime-display-internal-image-xxxx))
		 (file-readable-p tempfile))
	    nil
	  (vm-mime-insert-mime-body layout)
	  (setq end (point-marker))
	  (vm-mime-transfer-decode-region layout start end)
	  (setq tempfile (vm-make-tempfile))
	  (vm-register-folder-garbage-files (list tempfile))
	  ;; coding system for presentation buffer is binary so
	  ;; we don't need to set it here.
	  (write-region start end tempfile nil 0)
	  (put (vm-mm-layout-cache layout)
	       'vm-mime-display-internal-image-xxxx
	       tempfile)
	  (delete-region start end))
	(if (not (bolp))
	    (insert "\n"))
	(setq do-strips (and (stringp vm-imagemagick-convert-program)
			     vm-mime-use-image-strips))
	(cond (do-strips
	       (condition-case error-data
		   (let ((strips (vm-make-image-strips tempfile
						       (* 2 (font-height
							(face-font 'default)))
						       image-type
						       t incremental))
			 process image-list extent-list
			 start
			 (first t))
		     (define-key keymap 'button3 'vm-menu-popup-image-menu)
		     (setq process (car strips)
			   strips (cdr strips)
			   image-list strips)
		     (vm-register-message-garbage-files strips)
		     (setq start (point))
		     (while strips
		       (setq g (make-glyph
				(list
				 (cons nil
				       (vector 'string
					       ':data
					       (if (or first
						       (null (cdr strips)))
						   (progn
						     (setq first nil)
						     "+-----+")
						 "|image|"))))))
		       (insert " \n")
		       (setq e (vm-make-extent (- (point) 2) (1- (point))))
		       (vm-set-extent-property e 'begin-glyph g)
		       (vm-set-extent-property e 'start-open t)
		       (vm-set-extent-property e 'keymap keymap)
		       (setq extent-list (cons e extent-list))
		       (setq strips (cdr strips)))
		     (setq e (make-extent start (point)))
		     (vm-set-extent-property e 'start-open t)
		     (vm-set-extent-property e 'vm-mime-layout layout)
		     (vm-set-extent-property e 'vm-mime-disposable t)
		     (vm-set-extent-property e 'keymap keymap)
		     (save-excursion
		       (set-buffer (process-buffer process))
		       (set (make-local-variable 'vm-image-list) image-list)
		       (set (make-local-variable 'vm-image-type) image-type)
		       (set (make-local-variable 'vm-image-type-name)
			    name)
		       (set (make-local-variable 'vm-extent-list)
			    (nreverse extent-list)))
		     (if incremental
			 (set-process-filter
			  process
			  'vm-process-filter-display-some-image-strips))
		     (set-process-sentinel
		      process
		      'vm-process-sentinel-display-image-strips))
		 (vm-image-too-small
		  (setq do-strips nil))
		 (error
		  (message "Failed making image strips: %s" error-data)
		  ;; fallback to the non-strips way
		  (setq do-strips nil)))))
	(cond ((not do-strips)
	       (message "Creating %s glyph..." name)
	       (setq g (make-glyph
			(list
			 (cons (list 'win)
			       (vector image-type ':file tempfile))
			 (cons (list 'win)
			       (vector 'string
				       ':data
				       (format "[Unknown/Bad %s image encoding]"
					       name)))
			 (cons nil
			       (vector 'string
				       ':data
				       (format "[%s image]\n" name))))))
	       (message "")
	       ;; XEmacs 21.2 can pixel scroll images (sort of)
	       ;; if the entire image is above the baseline.
	       (set-glyph-baseline g 100)
	       (if (memq image-type '(xbm))
		   (set-glyph-face g 'vm-monochrome-image))
	       (insert " \n")
	       (define-key keymap 'button3 'vm-menu-popup-image-menu)
	       (setq e (vm-make-extent (- (point) 2) (1- (point))))
	       (vm-set-extent-property e 'keymap keymap)
	       (vm-set-extent-property e 'begin-glyph g)
	       (vm-set-extent-property e 'vm-mime-layout layout)
	       (vm-set-extent-property e 'vm-mime-disposable t)
	       (vm-set-extent-property e 'start-open t)))
	t )))

(defvar vm-menu-fsfemacs-image-menu)

(defun vm-mime-display-internal-image-fsfemacs-21-xxxx (layout image-type name)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (let (start end tempfile image work-buffer
	    (selective-display nil)
	    (incremental vm-mime-display-image-strips-incrementally)
	    do-strips
	    (buffer-read-only nil))
	(if (and (setq tempfile (get (vm-mm-layout-cache layout)
				     'vm-mime-display-internal-image-xxxx))
		 (file-readable-p tempfile))
	    nil
	  (unwind-protect
	      (progn
		(save-excursion
		  (setq work-buffer (vm-make-work-buffer))
		  (set-buffer work-buffer)
		  (setq start (point))
		  (vm-mime-insert-mime-body layout)
		  (setq end (point-marker))
		  (vm-mime-transfer-decode-region layout start end)
		  (setq tempfile (vm-make-tempfile))
		  (let ((coding-system-for-write (vm-binary-coding-system)))
		    (write-region start end tempfile nil 0))
		  (put (vm-mm-layout-cache layout)
		       'vm-mime-display-internal-image-xxxx
		       tempfile))
		(vm-register-folder-garbage-files (list tempfile)))
	    (and work-buffer (kill-buffer work-buffer))))
	(if (not (bolp))
	    (insert-char ?\n 1))
	(setq do-strips (and (stringp vm-imagemagick-convert-program)
			     vm-mime-use-image-strips))
	(cond (do-strips
	       (condition-case error-data
		   (let ((strips (vm-make-image-strips
				  tempfile
				  (* 2 (frame-char-height))
				  image-type t incremental))
			 (first t)
			 start o process image-list overlay-list)
		     (setq process (car strips)
			   strips (cdr strips)
			   image-list strips)
		     (vm-register-message-garbage-files strips)
		     (setq start (point))
		     (while strips
		       (if (or first (null (cdr strips)))
			   (progn
			     (setq first nil)
			     (insert "+-----+"))
			 (insert "|image|"))
		       (setq o (make-overlay (- (point) 7) (point)))
		       (overlay-put o 'evaporate t)
		       (setq overlay-list (cons o overlay-list))
		       (insert "\n")
		       (setq strips (cdr strips)))
		     (setq o (make-overlay start (point) nil t nil))
		     (overlay-put o 'vm-mime-layout layout)
		     (overlay-put o 'vm-mime-disposable t)
		     (if vm-use-menus
			 (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu))
		     (save-excursion
		       (set-buffer (process-buffer process))
		       (set (make-local-variable 'vm-image-list) image-list)
		       (set (make-local-variable 'vm-image-type) image-type)
		       (set (make-local-variable 'vm-image-type-name)
			    name)
		       (set (make-local-variable 'vm-overlay-list)
			    (nreverse overlay-list)))
		     (if incremental
			 (set-process-filter
			  process
			  'vm-process-filter-display-some-image-strips))
		     (set-process-sentinel
		      process
		      'vm-process-sentinel-display-image-strips))
		 (vm-image-too-small
		  (setq do-strips nil))
		 (error
		  (message "Failed making image strips: %s" error-data)
		  ;; fallback to the non-strips way
		  (setq do-strips nil)))))
	(cond ((not do-strips)
	       (setq image (list 'image ':type image-type ':file tempfile))
	       ;; insert one char so we can attach the image to it.
	       (insert "z")
	       (put-text-property (1- (point)) (point) 'display image)
	       (clear-image-cache t)
	       (let (o)
		 (setq o (make-overlay (- (point) 1) (point) nil t nil))
		 (overlay-put o 'evaporate t)
		 (overlay-put o 'vm-mime-layout layout)
		 (overlay-put o 'vm-mime-disposable t)
		 (if vm-use-menus
		     (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu)))))
	t )
    nil ))

(defun vm-mime-display-internal-image-fsfemacs-19-xxxx (layout image-type name)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p image-type))
      (catch 'done
	(let ((selective-display nil)
	      start end origfile workfile image work-buffer
	      (hroll (if vm-fsfemacs-mule-p
			 (+ (cdr (assq 'internal-border-width
				       (frame-parameters)))
			    (if (memq (cdr (assq 'vertical-scroll-bars
						 (frame-parameters)))
				      '(t left))
				(vm-fsfemacs-scroll-bar-width)
			      0))
		       (cdr (assq 'internal-border-width
				  (frame-parameters)))))
	      (vroll (cdr (assq 'internal-border-width (frame-parameters))))
	      (reverse (eq (cdr (assq 'background-mode (frame-parameters)))
			   'dark))
	      blob strips
	      dims width height char-width char-height
	      horiz-pad vert-pad trash-list
	      (buffer-read-only nil))
	  (if (and (setq blob (get (vm-mm-layout-cache layout)
				   'vm-mime-display-internal-image-xxxx))
		   (file-exists-p (car blob))
		   (progn
		     (setq origfile (car blob)
			   workfile (nth 1 blob)
			   width (nth 2 blob)
			   height (nth 3 blob)
			   char-width (nth 4 blob)
			   char-height (nth 5 blob))
		     (and (= char-width (frame-char-width))
			  (= char-height (frame-char-height)))))
	      (setq strips (nth 6 blob))
	    (unwind-protect
		(progn
		  (save-excursion
		    (setq work-buffer (vm-make-work-buffer))
		    (set-buffer work-buffer)
		    (if (and origfile (file-exists-p origfile))
			(progn
			  (insert-file-contents origfile)
			  (setq start (point-min)
				end (vm-marker (point-max))))
		      (setq start (point))
		      (vm-mime-insert-mime-body layout)
		      (setq end (point-marker))
		      (vm-mime-transfer-decode-region layout start end)
		      (setq origfile (vm-make-tempfile))
		      (setq trash-list (cons origfile trash-list))
		      (let ((coding-system-for-write (vm-binary-coding-system)))
			(write-region start end origfile nil 0)))
		    (setq dims (condition-case error-data
				   (vm-get-image-dimensions origfile)
				 (error
				  (message "Failed getting image dimensions: %s"
					   error-data)
				  (throw 'done nil)))
			  width (nth 0 dims)
			  height (nth 1 dims)
			  char-width (frame-char-width)
			  char-height (frame-char-height)
			  horiz-pad (if (< width char-width)
					(- char-width width)
				      (% width char-width))
			  horiz-pad (if (zerop horiz-pad)
					horiz-pad
				      (- char-width horiz-pad))
			  vert-pad (if (< height char-height)
				       (- char-height height)
				     (% height char-height))
			  vert-pad (if (zerop vert-pad)
				       vert-pad
				     (- char-height vert-pad)))
		    ;; crop one line from the bottom of the image
		    ;; if vertical padding needed is odd so that
		    ;; the image height plus the padding will be an
		    ;; exact multiple of the char height.
		    (if (not (zerop (% vert-pad 2)))
			(setq height (1- height)
			      vert-pad (1+ vert-pad)))
		    (call-process-region start end
					 vm-imagemagick-convert-program
					 t t nil
					 (if reverse "-negate" "-matte")
					 "-crop"
					 (format "%dx%d+0+0" width height)
					 "-page"
					 (format "%dx%d+0+0" width height)
					 "-mattecolor" "white"
					 "-frame"
					 (format "%dx%d+0+0"
						 (/ (1+ horiz-pad) 2)
						 (/ vert-pad 2))
					 "-"
					 "-")
		    (setq width (+ width (* 2 (/ (1+ horiz-pad) 2)))
			  height (+ height (* 2 (/ vert-pad 2))))
		    (if (null workfile)
			(setq workfile (vm-make-tempfile)
			      trash-list (cons workfile trash-list)))
		    (let ((coding-system-for-write (vm-binary-coding-system)))
		      (write-region (point-min) (point-max) workfile nil 0))
		    (put (vm-mm-layout-cache layout)
			 'vm-mime-display-internal-image-xxxx
			 (list origfile workfile width height
			       char-width char-height)))
		  (and trash-list
		       (vm-register-folder-garbage-files trash-list)))
	      (and work-buffer (kill-buffer work-buffer))))
	  (if (not (bolp))
	      (insert-char ?\n 1))
	  (condition-case error-data
	      (let (o i-start start process image-list overlay-list)
		(if (and strips (file-exists-p (car strips)))
		    (setq image-list strips)
		  (setq strips (vm-make-image-strips workfile char-height
						     image-type t nil
						     hroll vroll)
			process (car strips)
			strips (cdr strips)
			image-list strips)
		  (put (vm-mm-layout-cache layout)
		       'vm-mime-display-internal-image-xxxx
		       (list origfile workfile width height
			     char-width char-height
			     strips))
		  (vm-register-message-garbage-files strips))
		(setq i-start (point))
		(while strips
		  (setq start (point))
		  (insert-char ?\  (/ width char-width))
		  (put-text-property start (point) 'face 'vm-image-placeholder)
		  (setq o (make-overlay start (point) nil t))
		  (overlay-put o 'evaporate t)
		  (setq overlay-list (cons o overlay-list))
		  (insert "\n")
		  (setq strips (cdr strips)))
		(setq o (make-overlay i-start (point) nil t nil))
		(overlay-put o 'vm-mime-layout layout)
		(overlay-put o 'vm-mime-disposable t)
		(if vm-use-menus
		    (overlay-put o 'vm-image vm-menu-fsfemacs-image-menu))
		(if process
		    (save-excursion
		      (set-buffer (process-buffer process))
		      (set (make-local-variable 'vm-image-list) image-list)
		      (set (make-local-variable 'vm-image-type) image-type)
		      (set (make-local-variable 'vm-image-type-name)
			   name)
		      (set (make-local-variable 'vm-overlay-list)
			   (nreverse overlay-list))
		      ;; incremental strip display intentionally
		      ;; omitted because it makes the Emacs 19
		      ;; display completely repaint for each new
		      ;; strip.
		      (set-process-sentinel
		       process
		       'vm-process-sentinel-display-image-strips))
		  (vm-display-image-strips-on-overlay-regions image-list
							      (nreverse
							       overlay-list)
							      image-type)))
	    (error
	     (message "Failed making image strips: %s" error-data)))
	  t ))
    nil ))

(defun vm-get-image-dimensions (file)
  (let (work-buffer width height)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (call-process vm-imagemagick-identify-program nil t nil file)
	  (goto-char (point-min))
	  (or (search-forward " " nil t)
	      (error "no spaces in 'identify' output: %s"
		     (buffer-string)))
	  (if (not (re-search-forward "\\b\\([0-9]+\\)x\\([0-9]+\\)\\b" nil t))
	      (error "file dimensions missing from 'identify' output: %s"
		     (buffer-string)))
	  (setq width (string-to-int (match-string 1))
		height (string-to-int (match-string 2))))
      (and work-buffer (kill-buffer work-buffer)))
    (list width height)))

(defun vm-imagemagick-type-indicator-for (image-type)
  (cond ((eq image-type 'jpeg) "jpeg:")
	((eq image-type 'gif) "gif:")
	((eq image-type 'png) "png:")
	((eq image-type 'tiff) "tiff:")
	((eq image-type 'xpm) "xpm:")
	((eq image-type 'pbm) "pbm:")
	((eq image-type 'xbm) "xbm:")
	(t "")))

(defun vm-make-image-strips (file min-height image-type async incremental
				  &optional hroll vroll)
  (or hroll (setq hroll 0))
  (or vroll (setq vroll 0))
  (let ((process-connection-type nil)
	(i 0)
	(output-type (vm-imagemagick-type-indicator-for image-type))
	image-list dimensions width height starty newfile work-buffer
	quotient remainder adjustment process)
    (setq dimensions (vm-get-image-dimensions file)
	  width (car dimensions)
	  height (car (cdr dimensions)))
    (if (< height min-height)
	(signal 'vm-image-too-small nil))
    (setq quotient (/ height min-height)
	  remainder (% height min-height)
	  adjustment (/ remainder quotient)
	  remainder (% remainder quotient)
	  starty 0)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (goto-char (point-min))
	  (while (< starty height)
	    (setq newfile (vm-make-tempfile))
	    (if async
		(progn
		  (insert vm-imagemagick-convert-program
			  " -crop"
			  (format " %dx%d+0+%d"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1))
				  starty)
			  " -page"
			  (format " %dx%d+0+0"
				  width
				  (+ min-height adjustment
				     (if (zerop remainder) 0 1)))
			  (format " -roll +%d+%d" hroll vroll)
			  " \"" file "\" \"" output-type newfile "\"\n")
		  (if incremental
		      (progn
			(insert "echo XZXX" (int-to-string i) "XZXX\n")))
		  (setq i (1+ i)))
	      (call-process vm-imagemagick-convert-program nil nil nil
			    "-crop"
			    (format "%dx%d+0+%d"
				    width
				    (+ min-height adjustment
				       (if (zerop remainder) 0 1))
				    starty)
			    "-page"
			    (format "%dx%d+0+0"
				    width
				    (+ min-height adjustment
				       (if (zerop remainder) 0 1)))
			    "-roll"
			    (format "+%d+%d" hroll vroll)
			    file (concat output-type newfile)))
	    (setq image-list (cons newfile image-list)
		  starty (+ starty min-height adjustment
			    (if (zerop remainder) 0 1))
		  remainder (if (= 0 remainder) 0 (1- remainder))))
	  (if (not async)
	      nil
	    (goto-char (point-max))
	    (insert "exit\n")
	    (setq process
		  (start-process (format "image strip maker for %s" file)
				 (current-buffer)
				 shell-file-name))
	    (process-send-string process (buffer-string))
	    (setq work-buffer nil))
	  (if async
	      (cons process (nreverse image-list))
	    (nreverse image-list)))
      (and work-buffer (kill-buffer work-buffer)))))

(defvar vm-image-list)
(defvar vm-image-type)
(defvar vm-image-type-name)
(defvar vm-extent-list)
(defvar vm-overlay-list)
(defun vm-process-sentinel-display-image-strips (process what-happened)
  (save-excursion
    (set-buffer (process-buffer process))
    (cond ((and (boundp 'vm-extent-list)
		(boundp 'vm-image-list))
	   (let ((strips vm-image-list)
		 (extents vm-extent-list)
		 (image-type vm-image-type)
		 (type-name vm-image-type-name))
	     (vm-display-image-strips-on-extents strips extents image-type
						 type-name)))
	  ((and (boundp 'vm-overlay-list)
		(overlay-buffer (car vm-overlay-list))
		(boundp 'vm-image-list))
	   (let ((strips vm-image-list)
		 (overlays vm-overlay-list)
		 (image-type vm-image-type))
	     (vm-display-image-strips-on-overlay-regions strips overlays
							 image-type))))
    (kill-buffer (current-buffer))))

(defun vm-display-image-strips-on-extents (strips extents image-type type-name)
  (let (g)
    (while (and strips
		(file-exists-p (car strips))
		(extent-live-p (car extents))
		(extent-object (car extents)))
      (setq g (make-glyph
	       (list
		(cons (list 'win)
		      (vector image-type ':file (car strips)))
		(cons (list 'win)
		      (vector
		       'string
		       ':data
		       (format "[Unknown/Bad %s image encoding]"
			       type-name)))
		(cons nil
		      (vector 'string
			      ':data
			      (format "[%s image]\n" type-name))))))
      (set-glyph-baseline g 50)
      (if (memq image-type '(xbm))
	  (set-glyph-face g 'vm-monochrome-image))
      (set-extent-begin-glyph (car extents) g)
      (setq strips (cdr strips)
	    extents (cdr extents)))))

(defun vm-display-image-strips-on-overlay-regions (strips overlays image-type)
  (let (prop value omodified)
    (save-excursion
      (set-buffer (overlay-buffer (car vm-overlay-list)))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (if (fboundp 'image-type-available-p)
		  (setq prop 'display)
		(setq prop 'face))
	      (while (and strips
			  (file-exists-p (car strips))
			  (overlay-end (car overlays)))
		(if (fboundp 'image-type-available-p)
		    (setq value (list 'image ':type image-type
				      ':file (car strips)
				      ':ascent 50))
		  (setq value (make-face (make-symbol "<vm-image-face>")))
		  (set-face-stipple value (car strips)))
		(put-text-property (overlay-start (car overlays))
				   (overlay-end (car overlays))
				   prop value)
		(setq strips (cdr strips)
		      overlays (cdr overlays))))
	  (set-buffer-modified-p omodified))))))

(defun vm-process-filter-display-some-image-strips (process output)
  (let (which-strips (i 0))
    (while (string-match "XZXX\\([0-9]+\\)XZXX" output i)
      (setq which-strips (cons (string-to-int (match-string 1 output))
			       which-strips)
	    i (match-end 0)))
    (save-excursion
      (set-buffer (process-buffer process))
      (cond ((and (boundp 'vm-extent-list)
		  (boundp 'vm-image-list))
	     (let ((strips vm-image-list)
		   (extents vm-extent-list)
		   (image-type vm-image-type)
		   (type-name vm-image-type-name))
	       (vm-display-some-image-strips-on-extents strips extents
							image-type
							type-name
							which-strips)))
	    ((and (boundp 'vm-overlay-list)
		  (overlay-buffer (car vm-overlay-list))
		  (boundp 'vm-image-list))
	     (let ((strips vm-image-list)
		   (overlays vm-overlay-list)
		   (image-type vm-image-type))
	       (vm-display-some-image-strips-on-overlay-regions
		strips overlays image-type which-strips)))))))

(defun vm-display-some-image-strips-on-extents
  (strips extents image-type type-name which-strips)
  (let (g sss eee)
    (while which-strips
      (setq sss (nthcdr (car which-strips) strips)
	    eee (nthcdr (car which-strips) extents))
      (cond ((and sss
		  (file-exists-p (car sss))
		  (extent-live-p (car eee))
		  (extent-object (car eee)))
	     (setq g (make-glyph
		      (list
		       (cons (list 'win)
			     (vector image-type ':file (car sss)))
		       (cons (list 'win)
			     (vector
			      'string
			      ':data
			      (format "[Unknown/Bad %s image encoding]"
				      type-name)))
		       (cons nil
			     (vector 'string
				     ':data
				     (format "[%s image]\n" type-name))))))
	     (set-glyph-baseline g 50)
	     (if (memq image-type '(xbm))
		 (set-glyph-face g 'vm-monochrome-image))
	     (set-extent-begin-glyph (car eee) g)))
      (setq which-strips (cdr which-strips)))))

(defun vm-display-some-image-strips-on-overlay-regions
  (strips overlays image-type which-strips)
  (let (sss ooo prop value omodified)
    (save-excursion
      (set-buffer (overlay-buffer (car vm-overlay-list)))
      (setq omodified (buffer-modified-p))
      (save-restriction
	(widen)
	(unwind-protect
	    (let ((buffer-read-only nil))
	      (if (fboundp 'image-type-available-p)
		  (setq prop 'display)
		(setq prop 'face))
	      (while which-strips
		(setq sss (nthcdr (car which-strips) strips)
		      ooo (nthcdr (car which-strips) overlays))
		(cond ((and sss
			    (file-exists-p (car sss))
			    (overlay-end (car ooo)))
		       (if (fboundp 'image-type-available-p)
			   (setq value (list 'image ':type image-type
					     ':file (car sss)
					     ':ascent 50))
			 (setq value (make-face (make-symbol
						 "<vm-image-face>")))
			 (set-face-stipple value (car sss)))
		       (put-text-property (overlay-start (car ooo))
					  (overlay-end (car ooo))
					  prop value)))
		(setq which-strips (cdr which-strips))))
	  (set-buffer-modified-p omodified))))))

(defun vm-mime-display-internal-image/gif (layout)
  (vm-mime-display-internal-image-xxxx layout 'gif "GIF"))

(defun vm-mime-display-internal-image/jpeg (layout)
  (vm-mime-display-internal-image-xxxx layout 'jpeg "JPEG"))

(defun vm-mime-display-internal-image/png (layout)
  (vm-mime-display-internal-image-xxxx layout 'png "PNG"))

(defun vm-mime-display-internal-image/tiff (layout)
  (vm-mime-display-internal-image-xxxx layout 'tiff "TIFF"))

(defun vm-mime-display-internal-image/xpm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xpm "XPM"))

(defun vm-mime-display-internal-image/pbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'pbm "PBM"))

(defun vm-mime-display-internal-image/xbm (layout)
  (vm-mime-display-internal-image-xxxx layout 'xbm "XBM"))

(defun vm-mime-frob-image-xxxx (extent &rest convert-args)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 success tempfile
	 (work-buffer nil))
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (vm-make-work-buffer))
	  (set-buffer work-buffer)
	  (setq success
		(eq 0 (apply 'call-process vm-imagemagick-convert-program
			     tempfile t nil
			     (append convert-args (list "-" "-")))))
	  (if success
	      (progn
		(write-region (point-min) (point-max) tempfile nil 0)
		(if (consp blob)
		    (setcar (nthcdr 5 blob) 0))
		(put (vm-mm-layout-cache layout) 'vm-image-modified t))))
      (and work-buffer (kill-buffer work-buffer)))
    (if success
	(progn
	  (vm-mark-image-tempfile-as-message-garbage-once layout tempfile)
	  (vm-mime-display-generic extent)))))

(defun vm-mark-image-tempfile-as-message-garbage-once (layout tempfile)
  (if (get (vm-mm-layout-cache layout) 'vm-message-garbage)
      nil
    (vm-register-message-garbage-files (list tempfile))
    (put (vm-mm-layout-cache layout) 'vm-message-garbage t)))

(defun vm-mime-rotate-image-left (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "-90"))

(defun vm-mime-rotate-image-right (extent)
  (vm-mime-frob-image-xxxx extent "-rotate" "90"))

(defun vm-mime-mirror-image (extent)
  (vm-mime-frob-image-xxxx extent "-flop"))

(defun vm-mime-brighten-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "115"))

(defun vm-mime-dim-image (extent)
  (vm-mime-frob-image-xxxx extent "-modulate" "85"))

(defun vm-mime-monochrome-image (extent)
  (vm-mime-frob-image-xxxx extent "-monochrome"))

(defun vm-mime-revert-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (and (stringp tempfile)
	 (vm-error-free-call 'delete-file tempfile))
    (put (vm-mm-layout-cache layout) 'vm-image-modified nil)
    (vm-mime-display-generic extent)))

(defun vm-mime-larger-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 dims tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (* 2 (car dims)))
				     "x"
				     (int-to-string (* 2 (nth 1 dims)))))))

(defun vm-mime-smaller-image (extent)
  (let* ((layout (vm-extent-property extent 'vm-mime-layout))
	 (blob (get (vm-mm-layout-cache layout)
		    'vm-mime-display-internal-image-xxxx))
	 dims tempfile)
    ;; Emacs 19 uses a different layout cache than XEmacs or Emacs 21+.
    ;; The cache blob is a list in that case.
    (if (consp blob)
	(setq tempfile (car blob))
      (setq tempfile blob))
    (setq dims (vm-get-image-dimensions tempfile))
    (vm-mime-frob-image-xxxx extent
			     "-scale"
			     (concat (int-to-string (/ (car dims) 2))
				     "x"
				     (int-to-string (/ (nth 1 dims) 2))))))

(defun vm-mime-display-internal-audio/basic (layout)
  (if (and vm-xemacs-p
	   (or (featurep 'native-sound)
	       (featurep 'nas-sound))
	   (or (device-sound-enabled-p)
	       (and (featurep 'native-sound)
		    (not native-sound-only-on-console)
		    (memq (device-type) '(x gtk)))))
      (let ((start (point-marker)) end tempfile
	    (selective-display nil)
	    (buffer-read-only nil))
	(if (setq tempfile (get (vm-mm-layout-cache layout)
				'vm-mime-display-internal-audio/basic))
	    nil
	  (vm-mime-insert-mime-body layout)
	  (setq end (point-marker))
	  (vm-mime-transfer-decode-region layout start end)
	  (setq tempfile (vm-make-tempfile))
	  (vm-register-folder-garbage-files (list tempfile))
	  ;; coding system for presentation buffer is binary, so
	  ;; we don't need to set it here.
	  (write-region start end tempfile nil 0)
	  (put (vm-mm-layout-cache layout)
	       'vm-mime-display-internal-audio/basic
	       tempfile)
	  (delete-region start end))
	(start-itimer "audioplayer"
		      (list 'lambda nil (list 'play-sound-file tempfile))
		      1)
	t )
    nil ))

(defun vm-mime-display-generic (layout)
  (save-excursion
    (let ((vm-auto-displayed-mime-content-types t)
	  (vm-auto-displayed-mime-content-type-exceptions nil))
      (vm-decode-mime-layout layout t))))

(defun vm-mime-display-button-xxxx (layout disposable)
  (vm-mime-insert-button
   (vm-mime-sprintf (vm-mime-find-format-for-layout layout) layout)
   (function vm-mime-display-generic)
   layout disposable)
  t )

(defun vm-find-layout-extent-at-point ()
  (cond (vm-fsfemacs-p
	 (let (o-list o retval (found nil))
	   (setq o-list (overlays-at (point)))
	   (while (and o-list (not found))
	     (cond ((overlay-get (car o-list) 'vm-mime-layout)
		    (setq found t)
		    (setq retval (car o-list))))
	     (setq o-list (cdr o-list)))
	   retval ))
	(vm-xemacs-p
	 (extent-at (point) nil 'vm-mime-layout))))

(defun vm-mime-run-display-function-at-point (&optional function dispose)
  "Display the MIME object at point according to its type."
  (interactive)
  ;; save excursion to keep point from moving.  its motion would
  ;; drag window point along, to a place arbitrarily far from
  ;; where it was when the user triggered the button.
  (save-excursion
    (let ((e (vm-find-layout-extent-at-point))
	  retval )
      (cond ((null e) nil)
	    (vm-fsfemacs-p
	     (funcall (or function (overlay-get e 'vm-mime-function))
		      e))
	    (vm-xemacs-p
	     (funcall (or function (extent-property e 'vm-mime-function))
		      e))))))

(defun vm-mime-reader-map-save-file ()
  "Write the MIME object at point to a file."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (file)
    (save-excursion
      (setq file (vm-mime-run-display-function-at-point
		  'vm-mime-send-body-to-file)))
    (if vm-mime-delete-after-saving
	(let ((vm-mime-confirm-delete nil))
	  ;; we don't care if the delete fails
	  (condition-case nil
	      (vm-delete-mime-object (expand-file-name file))
	    (error nil))))
    file ))

(defun vm-mime-reader-map-save-message ()
  "Save the MIME object at point to a folder."
  (interactive)
  ;; make sure point doesn't move, we need it to stay on the tag
  ;; if the user wants to delete after saving.
  (let (folder)
    (save-excursion
      (setq folder (vm-mime-run-display-function-at-point
		    'vm-mime-send-body-to-folder)))
    (if vm-mime-delete-after-saving
	(let ((vm-mime-confirm-delete nil))
	  ;; we don't care if the delete fails
	  (condition-case nil
	      (vm-delete-mime-object folder)
	    (error nil))))))

(defun vm-mime-reader-map-pipe-to-command ()
  "Pipe the MIME object at point to a shell command."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-pipe-body-to-queried-command))

(defun vm-mime-reader-map-pipe-to-printer ()
  "Print the MIME object at point."
  (interactive)
  (vm-mime-run-display-function-at-point 'vm-mime-send-body-to-printer))

(defun vm-mime-reader-map-display-using-external-viewer ()
  "Display the MIME object at point with an external viewer."
  (interactive)
  (vm-mime-run-display-function-at-point
   'vm-mime-display-body-using-external-viewer))

(defun vm-mime-reader-map-display-using-default ()
  "Display the MIME object at point using the `default' face."
  (interactive)
  (vm-mime-run-display-function-at-point 'vm-mime-display-body-as-text))

(defun vm-mime-reader-map-display-object-as-type ()
  "Display the MIME object at point as some other type."
  (interactive)
  (vm-mime-run-display-function-at-point 'vm-mime-display-object-as-type))

;; for the karking compiler
(defvar vm-menu-mime-dispose-menu)

(defun vm-mime-set-image-stamp-for-type (e type)
  (cond
   (vm-xemacs-p
    (vm-mime-xemacs-set-image-stamp-for-type e type))
   (vm-fsfemacs-p
    (vm-mime-fsfemacs-set-image-stamp-for-type e type))))

(defun vm-mime-xemacs-set-image-stamp-for-type (e type)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p 'xpm)
	   (> (device-bitplanes) 7))
      (let ((dir vm-image-directory)
	    (colorful (> (device-bitplanes) 15))
	    (tuples
	     '(("text" "document-simple.xpm" "document-colorful.xpm")
	       ("image" "mona_stamp-simple.xpm" "mona_stamp-colorful.xpm")
	       ("audio" "audio_stamp-simple.xpm" "audio_stamp-colorful.xpm")
	       ("video" "film-simple.xpm" "film-colorful.xpm")
	       ("message" "message-simple.xpm" "message-colorful.xpm")
	       ("application" "gear-simple.xpm" "gear-colorful.xpm")
	       ("multipart" "stuffed_box-simple.xpm"
		"stuffed_box-colorful.xpm")))
	    glyph file sym p)
	(setq file (catch 'done
		     (while tuples
		       (if (vm-mime-types-match (car (car tuples)) type)
			   (throw 'done (car tuples))
			 (setq tuples (cdr tuples))))
		     nil)
	      file (and file (if colorful (nth 2 file) (nth 1 file)))
	      sym (and file (intern file vm-image-obarray))
	      glyph (and sym (boundp sym) (symbol-value sym))
	      glyph (or glyph
			(and file
			     (make-glyph
			      (list
			       (vector 'xpm ':file
				       (expand-file-name file dir))
			       [nothing])))))
	(and sym (not (boundp sym)) (set sym glyph))
	(and glyph (set-extent-begin-glyph e glyph)))))

(defun vm-mime-fsfemacs-set-image-stamp-for-type (e type)
  (if (and (vm-images-possible-here-p)
	   (vm-image-type-available-p 'xpm))
      (let ((dir vm-image-directory)
	    ;; no display-planes function under FSF Emacs before
	    ;; v21, so only try to use it if present.
	    (colorful (if (fboundp 'display-planes)
			  (> (display-planes) 15)
			t))
	    (tuples
	     '(("text" "document-simple.xpm" "document-colorful.xpm")
	       ("image" "mona_stamp-simple.xpm" "mona_stamp-colorful.xpm")
	       ("audio" "audio_stamp-simple.xpm" "audio_stamp-colorful.xpm")
	       ("video" "film-simple.xpm" "film-colorful.xpm")
	       ("message" "message-simple.xpm" "message-colorful.xpm")
	       ("application" "gear-simple.xpm" "gear-colorful.xpm")
	       ("multipart" "stuffed_box-simple.xpm"
		"stuffed_box-colorful.xpm")))
	    file)
	(setq file (catch 'done
		     (while tuples
		       (if (vm-mime-types-match (car (car tuples)) type)
			   (throw 'done (car tuples))
			 (setq tuples (cdr tuples))))
		     nil)
	      file (and file (if colorful (nth 2 file) (nth 1 file)))
	      file (and file (expand-file-name file dir)))
	(if file
	    (save-excursion
	      (let ((buffer-read-only nil))
		(set-buffer (overlay-buffer e))
		(goto-char (overlay-start e))
		(insert "x")
		(move-overlay e (1- (point)) (overlay-end e))
		(put-text-property (1- (point)) (point) 'display
				   (list 'image
					 ':ascent 80
					 ':color-symbols
					   (list
					    (cons "background"
						  (cdr (assq
							'background-color
							(frame-parameters)))))
					 ':type 'xpm
					 ':file file))))))))

(defun vm-mime-insert-button (caption action layout disposable)
  (let ((start (point))	e
	(keymap vm-mime-reader-map)
	(buffer-read-only nil))
    (if (fboundp 'set-keymap-parents)
	(if (current-local-map)
	    (set-keymap-parents keymap (list (current-local-map))))
      (setq keymap (append keymap (current-local-map))))
    (if (not (bolp))
	(insert "\n"))
    (insert caption "\n")
    ;; we must use the same interface that the vm-extent functions
    ;; use.  if they use overlays, then we call make-overlay.
    (if (eq (symbol-function 'vm-make-extent) 'make-overlay)
	;; we MUST have the five arg make-overlay.  overlays must
	;; advance when text is inserted at their start position or
	;; inline text and graphics will seep into the button
	;; overlay and then be removed when the button is removed.
	(setq e (make-overlay start (point) nil t nil))
      (setq e (make-extent start (point)))
      (set-extent-property e 'start-open t)
      (set-extent-property e 'end-open t))
    (vm-mime-set-image-stamp-for-type e (car (vm-mm-layout-type layout)))
    ;; for emacs
    (vm-set-extent-property e 'mouse-face 'highlight)
    (vm-set-extent-property e 'local-map keymap)
    ;; for xemacs
    (vm-set-extent-property e 'highlight t)
    (vm-set-extent-property e 'keymap keymap)
    (vm-set-extent-property e 'balloon-help 'vm-mouse-3-help)
    ;; for all
    (vm-set-extent-property e 'vm-button t)
    (vm-set-extent-property e 'vm-mime-disposable disposable)
    (vm-set-extent-property e 'face vm-mime-button-face)
    (vm-set-extent-property e 'vm-mime-layout layout)
    (vm-set-extent-property e 'vm-mime-function action)))

(defun vm-mime-rewrite-failed-button (button error-string)
  (let* ((buffer-read-only nil)
	 (start (point)))
    (goto-char (vm-extent-start-position button))
    (insert (format "DISPLAY FAILED -- %s\n" error-string))
    (vm-set-extent-endpoints button start (vm-extent-end-position button))
    (delete-region (point) (vm-extent-end-position button))))

(defun vm-mime-send-body-to-file (layout &optional default-filename file)
  (if (not (vectorp layout))
      (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (or default-filename
      (setq default-filename
	    (vm-mime-get-disposition-parameter layout "filename")))
  (and default-filename
       (setq default-filename (file-name-nondirectory default-filename)))
  (let ((work-buffer nil)
	;; evade the XEmacs dialog box, yeccch.
	(use-dialog-box nil)
	(dir vm-mime-attachment-save-directory)
	(done nil))
    (if file
	nil
      (while (not done)
	(setq file
	      (read-file-name
	       (if default-filename
		   (format "Write MIME body to file (default %s): "
			   default-filename)
		 "Write MIME body to file: ")
	       dir default-filename)
	      file (expand-file-name file dir))
	(if (not (file-directory-p file))
	    (setq done t)
	  (if (null default-filename)
	      (error "%s is a directory" file))
	  (setq file (expand-file-name default-filename file)
		done t))))
    (save-excursion
      (unwind-protect
	  (let ((coding-system-for-read (vm-binary-coding-system)))
	    (setq work-buffer (vm-make-work-buffer))
	    (set-buffer work-buffer)
	    (setq selective-display nil)
	    ;; Tell DOS/Windows NT whether the file is binary
	    (setq buffer-file-type (not (vm-mime-text-type-layout-p layout)))
	    ;; Tell XEmacs/MULE not to mess with the bits unless
	    ;; this is a text type.
	    (if (fboundp 'set-buffer-file-coding-system)
		(if (vm-mime-text-type-layout-p layout)
		    (set-buffer-file-coding-system
		     (vm-line-ending-coding-system) nil)
		  (set-buffer-file-coding-system (vm-binary-coding-system) t)))
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
	    (or (not (file-exists-p file))
		(y-or-n-p "File exists, overwrite? ")
		(error "Aborted"))
	    ;; Bind the jka-compr-compression-info-list to nil so
	    ;; that jka-compr won't compress already compressed
	    ;; data.  This is a crock, but as usual I'm getting
	    ;; the bug reports for somebody else's bad code.
	    (let ((jka-compr-compression-info-list nil))
	      (write-region (point-min) (point-max) file nil nil))
	    file )
	(and work-buffer (kill-buffer work-buffer))))))

(defun vm-mime-send-body-to-folder (layout &optional default-filename)
  (if (not (vectorp layout))
      (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (let ((work-buffer nil)
	(type (car (vm-mm-layout-type layout)))
	file)
    (if (not (or (vm-mime-types-match type "message/rfc822")
		 (vm-mime-types-match type "message/news")))
	(vm-mime-send-body-to-file layout default-filename)
      (save-excursion
	(unwind-protect
	    (let ((coding-system-for-read (vm-binary-coding-system))
		  (coding-system-for-write (vm-binary-coding-system)))
	      (setq work-buffer (vm-make-work-buffer))
	      (set-buffer work-buffer)
	      (setq selective-display nil)
	      ;; Tell DOS/Windows NT whether the file is binary
	      (setq buffer-file-type t)
	      ;; Tell XEmacs/MULE not to mess with the bits unless
	      ;; this is a text type.
	      (if (fboundp 'set-buffer-file-coding-system)
		  (set-buffer-file-coding-system
		   (vm-line-ending-coding-system) nil))
	      (vm-mime-insert-mime-body layout)
	      (vm-mime-transfer-decode-region layout (point-min) (point-max))
	      (goto-char (point-min))
	      (insert (vm-leading-message-separator 'mmdf))
	      (goto-char (point-max))
	      (insert (vm-trailing-message-separator 'mmdf))
	      (set-buffer-modified-p nil)
	      (vm-mode t)
	      (let ((vm-check-folder-types t)
		    (vm-convert-folder-types t))
		(setq file (call-interactively 'vm-save-message)))
	      (vm-quit-no-change)
	      file )
	  (and work-buffer (kill-buffer work-buffer)))))))

(defun vm-mime-pipe-body-to-command (command layout &optional discard-output)
  (if (not (vectorp layout))
      (setq layout (vm-extent-property layout 'vm-mime-layout)))
  (let ((output-buffer (if discard-output
			   0
			 (get-buffer-create "*Shell Command Output*")))
	(work-buffer nil))
    (save-excursion
      (if (bufferp output-buffer)
	  (progn
	    (set-buffer output-buffer)
	    (erase-buffer)))
      (unwind-protect
	  (progn
	    (setq work-buffer (vm-make-work-buffer))
	    ;; call-process-region calls write-region.
	    ;; don't let it do CR -> LF translation.
	    (setq selective-display nil)
	    (set-buffer work-buffer)
	    (if vm-fsfemacs-mule-p
		(set-buffer-multibyte nil))
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout (point-min) (point-max))
	    (let ((pop-up-windows (and pop-up-windows
				       (eq vm-mutable-windows t)))
		  (process-coding-system-alist
		   (if (vm-mime-text-type-layout-p layout)
		       nil
		     (list (cons "." (vm-binary-coding-system)))))
		  ;; Tell DOS/Windows NT whether the input is binary
		  (binary-process-input
		   (not
		    (vm-mime-text-type-layout-p layout))))
	      (call-process-region (point-min) (point-max)
				   (or shell-file-name "sh")
				   nil output-buffer nil
				   shell-command-switch command)))
	(and work-buffer (kill-buffer work-buffer)))
      (if (bufferp output-buffer)
	  (progn
	    (set-buffer output-buffer)
	    (if (not (zerop (buffer-size)))
		(vm-display output-buffer t (list this-command)
			    '(vm-pipe-message-to-command))
	      (vm-display nil nil (list this-command)
			  '(vm-pipe-message-to-command)))))))
  t )

(defun vm-mime-pipe-body-to-queried-command (layout &optional discard-output)
  (let ((command (read-string "Pipe object to command: ")))
    (vm-mime-pipe-body-to-command command layout discard-output)))

(defun vm-mime-pipe-body-to-queried-command-discard-output (layout)
  (vm-mime-pipe-body-to-queried-command layout t))

(defun vm-mime-send-body-to-printer (layout)
  (vm-mime-pipe-body-to-command (mapconcat (function identity)
					   (nconc (list vm-print-command)
						  vm-print-command-switches)
					   " ")
				layout))

(defun vm-mime-display-body-as-text (button)
  (let ((vm-auto-displayed-mime-content-types '("text/plain"))
	(vm-auto-displayed-mime-content-type-exceptions nil)
	(layout (copy-sequence (vm-extent-property button 'vm-mime-layout))))
    (vm-set-extent-property button 'vm-mime-disposable t)
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (vm-set-mm-layout-type layout '("text/plain" "charset=us-ascii"))
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-object-as-type (button)
  (let ((vm-auto-displayed-mime-content-types t)
	(vm-auto-displayed-mime-content-type-exceptions nil)
	(old-layout (vm-extent-property button 'vm-mime-layout))
	layout
	(type (read-string "View as MIME type: ")))
    (setq layout (copy-sequence old-layout))
    (vm-set-extent-property button 'vm-mime-layout layout)
    ;; not universally correct, but close enough.
    (setcar (vm-mm-layout-type layout) type)
    (goto-char (vm-extent-start-position button))
    (vm-decode-mime-layout button t)))

(defun vm-mime-display-body-using-external-viewer (button)
  (let ((layout (vm-extent-property button 'vm-mime-layout))
	(vm-mime-external-content-type-exceptions nil))
    (goto-char (vm-extent-start-position button))
    (if (not (vm-mime-find-external-viewer (car (vm-mm-layout-type layout))))
	(error "No viewer defined for type %s"
	       (car (vm-mm-layout-type layout)))
      (vm-mime-display-external-generic layout))))

(defun vm-mime-convert-body-then-display (button)
  (let ((layout (vm-mime-convert-undisplayable-layout
		 (vm-extent-property button 'vm-mime-layout))))
    (if (null layout)
	nil
      (vm-set-extent-property button 'vm-mime-disposable t)
      (vm-set-extent-property button 'vm-mime-layout layout)
      (goto-char (vm-extent-start-position button))
      (vm-decode-mime-layout button t))))

(defun vm-mime-get-button-layout (e)
  (vm-mime-run-display-function-at-point
   (function
    (lambda (e)
      (vm-extent-property e 'vm-mime-layout)))))

(defun vm-mime-scrub-description (string)
  (let ((work-buffer nil))
      (save-excursion
       (unwind-protect
	   (progn
	     (setq work-buffer (vm-make-work-buffer))
	     (set-buffer work-buffer)
	     (insert string)
	     (while (re-search-forward "[ \t\n]+" nil t)
	       (replace-match " "))
	     (buffer-string))
	 (and work-buffer (kill-buffer work-buffer))))))

;; unused
;;(defun vm-mime-layout-description (layout)
;;  (let ((type (car (vm-mm-layout-type layout)))
;;	description name)
;;    (setq description
;;	  (if (vm-mm-layout-description layout)
;;	      (vm-mime-scrub-description (vm-mm-layout-description layout))))
;;    (concat
;;     (if description description "")
;;     (if description ", " "")
;;     (cond ((vm-mime-types-match "multipart/digest" type)
;;	    (let ((n (length (vm-mm-layout-parts layout))))
;;	      (format "digest (%d message%s)" n (if (= n 1) "" "s"))))
;;	   ((vm-mime-types-match "multipart/alternative" type)
;;	    "multipart alternative")
;;	   ((vm-mime-types-match "multipart" type)
;;	    (let ((n (length (vm-mm-layout-parts layout))))
;;	      (format "multipart message (%d part%s)" n (if (= n 1) "" "s"))))
;;	   ((vm-mime-types-match "text/plain" type)
;;	    (format "plain text%s"
;;		    (let ((charset (vm-mime-get-parameter layout "charset")))
;;		      (if charset
;;			  (concat ", " charset)
;;			""))))
;;	   ((vm-mime-types-match "text/enriched" type)
;;	    "enriched text")
;;	   ((vm-mime-types-match "text/html" type)
;;	    "HTML")
;;	   ((vm-mime-types-match "image/gif" type)
;;	    "GIF image")
;;	   ((vm-mime-types-match "image/jpeg" type)
;;	    "JPEG image")
;;	   ((and (vm-mime-types-match "application/octet-stream" type)
;;		 (setq name (vm-mime-get-parameter layout "name"))
;;		 (save-match-data (not (string-match "^[ \t]*$" name))))
;;	    name)
;;	   (t type)))))

(defun vm-mime-layout-contains-type (layout type)
  (if (vm-mime-types-match type (car (vm-mm-layout-type layout)))
      layout
    (let ((p (vm-mm-layout-parts layout))
	  (result nil)
	  (done nil))
      (while (and p (not done))
	(if (setq result (vm-mime-layout-contains-type (car p) type))
	    (setq done t)
	  (setq p (cdr p))))
      result )))

;; breadth first traversal
(defun vm-mime-find-digests-in-layout (layout)
  (let ((layout-list (list layout))
	layout-type
	(result nil))
    (while layout-list
      (setq layout-type (car (vm-mm-layout-type (car layout-list))))
      (cond ((string-match "^multipart/digest\\|message/\\(rfc822\\|news\\)"
			   layout-type)
	     (setq result (nconc result (list (car layout-list)))))
	    ((vm-mime-composite-type-p layout-type)
	     (setq layout-list (nconc layout-list
				      (copy-sequence
				       (vm-mm-layout-parts
					(car layout-list)))))))
      (setq layout-list (cdr layout-list)))
    result ))
  
(defun vm-mime-plain-message-p (m)
  (save-match-data
    (let ((o (vm-mm-layout m))
	  (case-fold-search t))
      (and (eq (vm-mm-encoded-header m) 'none)
	   (or (not (vectorp o))
	       (and (vm-mime-types-match "text/plain"
					 (car (vm-mm-layout-type o)))
		    (let* ((charset (or (vm-mime-get-parameter o "charset")
					"us-ascii")))
		      (vm-mime-default-face-charset-p charset))
		    (string-match "^\\(7bit\\|8bit\\|binary\\)$"
				  (vm-mm-layout-encoding o))))))))

(defun vm-mime-text-type-p (type)
  (let ((case-fold-search t))
    (or (string-match "^text/" type) (string-match "^message/" type))))

(defun vm-mime-text-type-layout-p (layout)
  (or (vm-mime-types-match "text" (car (vm-mm-layout-type layout)))
      (vm-mime-types-match "message" (car (vm-mm-layout-type layout)))))

(defun vm-mime-charset-internally-displayable-p (name)
  (cond ((and vm-xemacs-mule-p (memq (device-type) '(x gtk mswindows)))
	 (or (vm-string-assoc name vm-mime-mule-charset-to-coding-alist)
	     (vm-mime-default-face-charset-p name)))
	((and vm-fsfemacs-mule-p (memq window-system '(x mac win32 w32)))
	 (or (vm-string-assoc name vm-mime-mule-charset-to-coding-alist)
	     (vm-mime-default-face-charset-p name)))
	((vm-multiple-fonts-possible-p)
	 (or (vm-mime-default-face-charset-p name)
	     (vm-string-assoc name vm-mime-charset-font-alist)))
	(t
	 (vm-mime-default-face-charset-p name))))

(defun vm-mime-default-face-charset-p (charset)
  (and (or (eq vm-mime-default-face-charsets t)
	   (and (consp vm-mime-default-face-charsets)
		(vm-string-member charset vm-mime-default-face-charsets)))
       (not (vm-string-member charset
			      vm-mime-default-face-charset-exceptions))))


(defun vm-mime-find-message/partials (layout id)
  (let ((list nil)
	(type (vm-mm-layout-type layout)))
    (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	   (let ((parts (vm-mm-layout-parts layout)) o)
	     (while parts
	       (setq o (vm-mime-find-message/partials (car parts) id))
	       (if o
		   (setq list (nconc o list)))
	       (setq parts (cdr parts)))))
	  ((vm-mime-types-match "message/partial" (car type))
	   (if (equal (vm-mime-get-parameter layout "id") id)
	       (setq list (cons layout list)))))
    list ))

(defun vm-mime-find-leaf-content-id-in-layout-folder (layout id)
  (save-excursion
    (save-restriction
      (let (m (o nil))
	(set-buffer (vm-buffer-of
		     (vm-real-message-of
		      (vm-mm-layout-message layout))))
	(widen)
	(goto-char (point-min))
	(while (and (search-forward id nil t)
		    (setq m (vm-message-at-point)))
	  (setq o (vm-mm-layout m))
	  (if (not (vectorp o))
	      nil
	    (setq o (vm-mime-find-leaf-content-id o id))
	    (if (null o)
		nil
	      ;; if we found it, end the search loop
	      (goto-char (point-max)))))
	o ))))

(defun vm-mime-find-leaf-content-id (layout id)
  (let ((list nil)
	(type (vm-mm-layout-type layout)))
    (catch 'done
      (cond ((vm-mime-composite-type-p (car (vm-mm-layout-type layout)))
	     (let ((parts (vm-mm-layout-parts layout)) o)
	       (while parts
		 (setq o (vm-mime-find-leaf-content-id (car parts) id))
		 (if o
		     (throw 'done o))
		 (setq parts (cdr parts)))))
	    (t
	     (if (equal (vm-mm-layout-id layout) id)
		 (throw 'done layout)))))))

(defun vm-message-at-point ()
  (let ((mp vm-message-list)
	(point (point))
	(done nil))
    (while (and mp (not done))
      (if (and (>= point (vm-start-of (car mp)))
	       (<= point (vm-end-of (car mp))))
	  (setq done t)
	(setq mp (cdr mp))))
    (car mp)))

(defun vm-mime-make-multipart-boundary ()
  (let ((boundary (make-string 10 ?a))
	(i 0))
    (random t)
    (while (< i (length boundary))
      (aset boundary i (aref vm-mime-base64-alphabet
			     (% (vm-abs (lsh (random) -8))
				(length vm-mime-base64-alphabet))))
      (vm-increment i))
    boundary ))

(defun vm-mime-extract-filename-suffix (layout)
  (let ((filename (or (vm-mime-get-disposition-parameter layout "filename")
		      (vm-mime-get-parameter layout "name")))
	(suffix nil) i)
    (if (and filename (string-match "\\.[^.]+$" filename))
	(setq suffix (substring filename (match-beginning 0) (match-end 0))))
    suffix ))

(defun vm-mime-find-filename-suffix-for-type (layout)
  (let ((type (car (vm-mm-layout-type layout)))
	suffix
	(alist vm-mime-attachment-auto-suffix-alist))
    (while alist
      (if (vm-mime-types-match (car (car alist)) type)
	  (setq suffix (cdr (car alist))
		alist nil)
	(setq alist (cdr alist))))
    suffix ))

(defun vm-mime-attach-file (file type &optional charset description
			    no-suggested-filename)
  "Attach a file to a VM composition buffer to be sent along with the message.
The file is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, FILE, is the name of the file to attach.  Second
argument, TYPE, is the MIME Content-Type of the file.  Optional
third argument CHARSET is the character set of the attached
document.  This argument is only used for text types, and it is
ignored for other types.  Optional fourth argument DESCRIPTION
should be a one line description of the file.  Nil means include
no description.  Optional fifth argument NO-SUGGESTED-FILENAME non-nil
means that VM should not add a filename to the Content-Disposition
header created for the object.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use vm-mime-attach-mime-file to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (charset nil)
	 description file default-type type)
     (if (null vm-send-using-mime)
	 (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
     (setq file (vm-read-file-name "Attach file: " nil nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (if (vm-mime-types-match "text" type)
	 (setq charset (completing-read "Character set (default US-ASCII): "
					vm-mime-charset-completion-alist)
	       charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (if (string-match "^[ \t]*$" description)
	 (setq description nil))
     (list file type charset description nil)))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (if (file-directory-p file)
      (error "%s is a directory, cannot attach" file))
  (if (not (file-exists-p file))
      (error "No such file: %s" file))
  (if (not (file-readable-p file))
      (error "You don't have permission to read %s" file))
  (and charset (setq charset (list (concat "charset=" charset))))
  (and description (setq description (vm-mime-scrub-description description)))
  (vm-mime-attach-object file type charset description nil))

(defun vm-mime-attach-mime-file (file type)
  "Attach a MIME encoded file to a VM composition buffer to be sent
along with the message.

The file is not inserted into the buffer until you execute
vm-mail-send or vm-mail-send-and-exit.  A visible tag indicating
the existence of the attachment is placed in the composition
buffer.  You can move the attachment around or remove it entirely
with normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

The first argument, FILE, is the name of the file to attach.
When called interactively the FILE argument is read from the
minibuffer.

The second argument, TYPE, is the MIME Content-Type of the object.

This command is for attaching files that have a MIME
header section at the top.  For files without MIME headers, you
should use vm-mime-attach-file to attach the file."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 file type)
     (if (null vm-send-using-mime)
	 (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
     (setq file (vm-read-file-name "Attach file: " nil nil t)
	   default-type (or (vm-mime-default-type-from-filename file)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (list file type)))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (if (file-directory-p file)
      (error "%s is a directory, cannot attach" file))
  (if (not (file-exists-p file))
      (error "No such file: %s" file))
  (if (not (file-readable-p file))
      (error "You don't have permission to read %s" file))
  (vm-mime-attach-object file type nil nil t))

(defun vm-mime-attach-buffer (buffer type &optional charset description)
  "Attach a buffer to a VM composition buffer to be sent along with
the message.

The buffer contents are not inserted into the composition
buffer and MIME encoded until you execute `vm-mail-send' or
`vm-mail-send-and-exit'.  A visible tag indicating the existence
of the attachment is placed in the composition buffer.  You
can move the attachment around or remove it entirely with
normal text editing commands.  If you remove the attachment
tag, the attachment will not be sent.

First argument, BUFFER, is the buffer or name of the buffer to
attach.  Second argument, TYPE, is the MIME Content-Type of the
file.  Optional third argument CHARSET is the character set of
the attached document.  This argument is only used for text
types, and it is ignored for other types.  Optional fourth
argument DESCRIPTION should be a one line description of the
file.  Nil means include no description.

When called interactively all arguments are read from the
minibuffer.

This command is for attaching files that do not have a MIME
header section at the top.  For files with MIME headers, you
should use vm-mime-attach-mime-file to attach such a file.  VM
will extract the content type information from the headers in
this case and not prompt you for it in the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (charset nil)
	 description file default-type type buffer buffer-name)
     (if (null vm-send-using-mime)
	 (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
     (setq buffer-name (read-buffer "Attach buffer: " nil t)
	   default-type (or (vm-mime-default-type-from-filename buffer-name)
			    "application/octet-stream")
	   type (completing-read
		 (format "Content type (default %s): "
			 default-type)
		 vm-mime-type-completion-alist)
	   type (if (> (length type) 0) type default-type))
     (if (vm-mime-types-match "text" type)
	 (setq charset (completing-read "Character set (default US-ASCII): "
					vm-mime-charset-completion-alist)
	       charset (if (> (length charset) 0) charset)))
     (setq description (read-string "One line description: "))
     (if (string-match "^[ \t]*$" description)
	 (setq description nil))
     (list buffer-name type charset description)))
  (if (null (setq buffer (get-buffer buffer)))
      (error "Buffer %s does not exist." buffer))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (and charset (setq charset (list (concat "charset=" charset))))
  (and description (setq description (vm-mime-scrub-description description)))
  (vm-mime-attach-object buffer type charset description nil))

(defun vm-mime-attach-message (message &optional description)
  "Attach a message from a folder to a VM composition buffer
to be sent along with the message.

The message is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the attachment is placed in the
composition buffer.  You can move the attachment around or remove
it entirely with normal text editing commands.  If you remove the
attachment tag, the attachment will not be sent.

First argument, MESSAGE, is either a VM message struct or a list
of message structs.  When called interactively a message number is read
from the minibuffer.  The message will come from the parent
folder of this composition.  If the composition has no parent,
the name of a folder will be read from the minibuffer before the
message number is read.

If this command is invoked with a prefix argument, the name of a
folder is read and that folder is used instead of the parent
folder of the composition.

If this command is invoked on marked message (via
`vm-next-command-uses-marks') the marked messages in the selected
folder will be attached as a MIME message digest.

Optional second argument DESCRIPTION is a one-line description of
the message being attached.  This is also read from the
minibuffer if the command is run interactively."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command)
	 (result 0)
	 mlist mp default prompt description folder)
     (if (null vm-send-using-mime)
	 (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
     (cond ((or current-prefix-arg (null vm-mail-buffer)
		(not (buffer-live-p vm-mail-buffer)))
	    (let ((dir (if vm-folder-directory
			   (expand-file-name vm-folder-directory)
			 default-directory))
		  file)
	      (let ((last-command last-command)
		    (this-command this-command))
		(setq file (read-file-name "Attach message from folder: "
					   dir nil t)))
	      (save-excursion
		(set-buffer
		 (let ((coding-system-for-read (vm-binary-coding-system)))
		   (find-file-noselect file)))
		(setq folder (current-buffer))
		(vm-mode)
		(setq mlist (vm-select-marked-or-prefixed-messages 0)))))
	   (t
	    (setq folder vm-mail-buffer)
	    (save-excursion
	      (set-buffer folder)
	      (setq mlist (vm-select-marked-or-prefixed-messages 0)))))
     (if (null mlist)
	 (save-excursion
	   (set-buffer folder)
	   (setq default (and vm-message-pointer
			      (vm-number-of (car vm-message-pointer)))
		 prompt (if default
			    (format "Yank message number: (default %s) "
				    default)
			  "Yank message number: "))
	   (while (zerop result)
	     (setq result (read-string prompt))
	     (and (string= result "") default (setq result default))
	     (setq result (string-to-int result)))
	   (if (null (setq mp (nthcdr (1- result) vm-message-list)))
	       (error "No such message."))))
     (setq description (read-string "Description: "))
     (if (string-match "^[ \t]*$" description)
	 (setq description nil))
     (list (or mlist (car mp)) description)))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (if (not (consp message))
      (let* ((buf (generate-new-buffer "*attached message*"))
	     (m (vm-real-message-of message))
	     (folder (vm-buffer-of m)))
	(save-excursion
	  (set-buffer buf)
	  (if vm-fsfemacs-mule-p
	      (set-buffer-multibyte nil))
	  (vm-insert-region-from-buffer folder (vm-headers-of m)
					(vm-text-end-of m))
	  (goto-char (point-min))
	  (vm-reorder-message-headers nil nil
				      vm-internal-unforwarded-header-regexp))
	(and description (setq description
			       (vm-mime-scrub-description description)))
	(vm-mime-attach-object buf "message/rfc822" nil description nil)
	(make-local-variable 'vm-forward-list)
	(setq vm-system-state 'forwarding
	      vm-forward-list (list message))
	(add-hook 'kill-buffer-hook
		  (list 'lambda ()
			(list 'if (list 'eq (current-buffer) '(current-buffer))
			      (list 'kill-buffer buf)))))
    (let ((buf (generate-new-buffer "*attached messages*"))
	  boundary)
      (save-excursion
	(set-buffer buf)
	(setq boundary (vm-mime-encapsulate-messages
			message vm-mime-digest-headers
			vm-mime-digest-discard-header-regexp
			t))
	(goto-char (point-min))
	(insert "MIME-Version: 1.0\n")
	(insert (if vm-mime-avoid-folding-content-type
		    "Content-Type: multipart/digest; boundary=\""
		  "Content-Type: multipart/digest;\n\tboundary=\"")
		boundary "\"\n")
	(insert "Content-Transfer-Encoding: "
		(vm-determine-proper-content-transfer-encoding
		 (point)
		 (point-max))
		"\n\n"))
      (and description (setq description
			     (vm-mime-scrub-description description)))
      (vm-mime-attach-object buf "multipart/digest"
			     (list (concat "boundary=\""
					   boundary "\"")) nil t)
      (make-local-variable 'vm-forward-list)
      (setq vm-system-state 'forwarding
	    vm-forward-list (copy-sequence message))
      (add-hook 'kill-buffer-hook
		(list 'lambda ()
		      (list 'if (list 'eq (current-buffer) '(current-buffer))
			    (list 'kill-buffer buf)))))))

(defun vm-mime-attach-object-from-message (composition)
  "Attach a object from the current message to a VM composition buffer.

The object is not inserted into the buffer and MIME encoded until
you execute `vm-mail-send' or `vm-mail-send-and-exit'.  A visible tag
indicating the existence of the object is placed in the
composition buffer.  You can move the object around or remove
it entirely with normal text editing commands.  If you remove the
object tag, the object will not be sent.

First argument COMPOSITION is the buffer into which the object
will be inserted.  When this function is called interactively
COMPOSITION's name will be read from the minibuffer."
  (interactive
   ;; protect value of last-command and this-command
   (let ((last-command last-command)
	 (this-command this-command))
     (list
      (read-buffer "Attach object to buffer: "
		   (vm-find-composition-buffer) t))))
  (if (null vm-send-using-mime)
      (error "MIME attachments disabled, set vm-send-using-mime non-nil to enable."))
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)

  (let (e layout (work-buffer nil) buf start w)
    (setq e (vm-find-layout-extent-at-point)
	  layout (and e (vm-extent-property e 'vm-mime-layout)))
    (unwind-protect
	(if (null layout)
	    (error "No MIME object found at point.")
	  (save-excursion
	    (setq work-buffer (vm-make-work-buffer))
	    (set-buffer work-buffer)
	    (vm-mime-insert-mime-headers layout)
	    (insert "\n")
	    (setq start (point))
	    (vm-mime-insert-mime-body layout)
	    (vm-mime-transfer-decode-region layout start (point-max))
	    (goto-char (point-min))
	    (vm-reorder-message-headers nil nil "Content-Transfer-Encoding:")
	    (insert "Content-Transfer-Encoding: binary")
	    (set-buffer composition)
	    (vm-mime-attach-object work-buffer
				   (car (vm-mm-layout-type layout))
				   (cdr (vm-mm-layout-type layout))
				   (vm-mm-layout-description layout)
				   t)
	    ;; move windwo point forward so that if this command
	    ;; is used consecutively, the insertions will be in
	    ;; the correct order in the composition buffer.
	    (setq w (vm-get-buffer-window composition))
	    (and w (set-window-point w (point)))
	    (setq buf work-buffer
		  work-buffer nil)
	    (add-hook 'kill-buffer-hook
		      (list 'lambda ()
			    (list 'if (list 'eq (current-buffer)
					    '(current-buffer))
				  (list 'kill-buffer buf))))))
      (and work-buffer (kill-buffer work-buffer)))))

(defun vm-mime-attach-object (object type params description mimed
			      &optional no-suggested-filename)
  (if (not (eq major-mode 'mail-mode))
      (error "Command must be used in a VM Mail mode buffer."))
  (if (vm-mail-mode-get-header-contents "MIME-Version")
      (error "Can't attach MIME object to already encoded MIME buffer."))
  (let (start end e tag-string disposition
	(fb (list vm-mime-forward-local-external-bodies)))
    (if (< (point) (save-excursion (mail-text) (point)))
	(mail-text))
    (setq start (point)
	  tag-string (format "[ATTACHMENT %s, %s]" object
			     (or type "MIME file")))
    (insert tag-string "\n")
    (setq end (1- (point)))
    (if (and (stringp object) (not mimed))
	(progn
	  (if (or (vm-mime-types-match "application" type)
		  (vm-mime-types-match "model" type))
	      (setq disposition (list "attachment"))
	    (setq disposition (list "inline")))
	  (if (not no-suggested-filename)
	      (setq disposition (nconc disposition
				       (list
					(concat "filename=\""
						(file-name-nondirectory object)
						"\""))))))
      (setq disposition (list "unspecified")))
    (cond (vm-fsfemacs-p
	   (put-text-property start end 'front-sticky nil)
	   (put-text-property start end 'rear-nonsticky t)
;; can't be intangible because menu clicking at a position needs
;; to set point inside the tag so that a command can access the
;; text properties there.
;;	   (put-text-property start end 'intangible object)
	   (put-text-property start end 'face vm-mime-button-face)
	   (put-text-property start end 'vm-mime-forward-local-refs fb)
	   (put-text-property start end 'vm-mime-type type)
	   (put-text-property start end 'vm-mime-object object)
	   (put-text-property start end 'vm-mime-parameters params)
	   (put-text-property start end 'vm-mime-description description)
	   (put-text-property start end 'vm-mime-disposition disposition)
	   (put-text-property start end 'vm-mime-encoded mimed))
	  (vm-xemacs-p
	   (setq e (make-extent start end))
	   (vm-mime-set-image-stamp-for-type e (or type "text/plain"))
	   (set-extent-property e 'start-open t)
	   (set-extent-property e 'face vm-mime-button-face)
	   (set-extent-property e 'duplicable t)
	   (let ((keymap (make-sparse-keymap)))
	     (if vm-popup-menu-on-mouse-3
		 (define-key keymap 'button3
		   'vm-menu-popup-attachment-menu))
	     (set-extent-property e 'keymap keymap)
	     (set-extent-property e 'balloon-help 'vm-mouse-3-help))
	   (set-extent-property e 'vm-mime-forward-local-refs fb)
	   (set-extent-property e 'vm-mime-type type)
	   (set-extent-property e 'vm-mime-object object)
	   (set-extent-property e 'vm-mime-parameters params)
	   (set-extent-property e 'vm-mime-description description)
	   (set-extent-property e 'vm-mime-disposition disposition)
	   (set-extent-property e 'vm-mime-encoded mimed)))))

(defun vm-mime-attachment-forward-local-refs-at-point ()
  (cond (vm-fsfemacs-p
	 (let ((fb (get-text-property (point) 'vm-mime-forward-local-refs)))
	   (car fb) ))
	(vm-xemacs-p
	 (let* ((e (extent-at (point) nil 'vm-mime-type))
		(fb (extent-property e 'vm-mime-forward-local-refs)))
	   (car fb) ))))

(defun vm-mime-set-attachment-forward-local-refs-at-point (val)
  (cond (vm-fsfemacs-p
	 (let ((fb (get-text-property (point) 'vm-mime-forward-local-refs)))
	   (setcar fb val) ))
	(vm-xemacs-p
	 (let* ((e (extent-at (point) nil 'vm-mime-type))
		(fb (extent-property e 'vm-mime-forward-local-refs)))
	   (setcar fb val) ))))

(defun vm-mime-attachment-disposition-at-point ()
  (cond (vm-fsfemacs-p
	 (let ((disp (get-text-property (point) 'vm-mime-disposition)))
	   (intern (car disp))))
	(vm-xemacs-p
	 (let* ((e (extent-at (point) nil 'vm-mime-disposition))
		(disp (extent-property e 'vm-mime-disposition)))
	   (intern (car disp))))))

(defun vm-mime-set-attachment-disposition-at-point (sym)
  (cond (vm-fsfemacs-p
	 (let ((disp (get-text-property (point) 'vm-mime-disposition)))
	   (setcar disp (symbol-name sym))))
	(vm-xemacs-p
	 (let* ((e (extent-at (point) nil 'vm-mime-disposition))
		(disp (extent-property e 'vm-mime-disposition)))
	   (setcar disp (symbol-name sym))))))

(defun vm-disallow-overlay-endpoint-insertion (overlay after start end
					       &optional old-size)
  (cond ((null after) nil)
	((= start (overlay-start overlay))
	 (move-overlay overlay end (overlay-end overlay)))
	((= start (overlay-end overlay))
	 (move-overlay overlay (overlay-start overlay) start))))

(defun vm-mime-fake-attachment-overlays (start end)
  (let ((o-list nil)
	(done nil)
	(pos start)
	object props o)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(while (not done)
	  (setq object (get-text-property pos 'vm-mime-object))
	  (setq pos (next-single-property-change pos 'vm-mime-object))
	  (or pos (setq pos (point-max) done t))
	  (if object
	      (progn
		(setq o (make-overlay start pos))
		(overlay-put o 'insert-in-front-hooks
			     '(vm-disallow-overlay-endpoint-insertion))
		(overlay-put o 'insert-behind-hooks
			     '(vm-disallow-overlay-endpoint-insertion))
		(setq props (text-properties-at start))
		(while props
		  (overlay-put o (car props) (car (cdr props)))
		  (setq props (cdr (cdr props))))
		(setq o-list (cons o o-list))))
	  (setq start pos))
	o-list ))))

(defun vm-mime-default-type-from-filename (file)
  (let ((alist vm-mime-attachment-auto-type-alist)
	(case-fold-search t)
	(done nil))
    (while (and alist (not done))
      (if (string-match (car (car alist)) file)
	  (setq done t)
	(setq alist (cdr alist))))
    (and alist (cdr (car alist)))))

(defun vm-remove-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" mail-header-separator "$") nil t)
	(progn
	  (delete-region (match-beginning 0) (match-end 0))
	   t )
      nil )))

(defun vm-add-mail-mode-header-separator ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^$" nil t)
	(replace-match mail-header-separator t t))))

(defun vm-mime-transfer-encode-region (encoding beg end crlf)
  (let ((case-fold-search t)
	(armor-from (and vm-mime-composition-armor-from-lines
			 (let ((case-fold-search nil))
			   (save-excursion
			     (goto-char beg)
			     (re-search-forward "^From " nil t)))))
	(armor-dot (let ((case-fold-search nil))
		     (save-excursion
		       (goto-char beg)
		       (re-search-forward "^\\.\n" nil t)))))
    (cond ((string-match "^binary$" encoding)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((and (not armor-from) (not armor-dot)
		(string-match "^7bit$" encoding)) t)
	  ((string-match "^base64$" encoding) t)
	  ((string-match "^quoted-printable$" encoding) t)
	  ((eq vm-mime-8bit-text-transfer-encoding 'quoted-printable)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable"))
	  ((eq vm-mime-8bit-text-transfer-encoding 'base64)
	   (vm-mime-base64-encode-region beg end crlf)
	   (setq encoding "base64"))
	  ((or armor-from armor-dot)
	   (vm-mime-qp-encode-region beg end nil armor-from)
	   (setq encoding "quoted-printable")))
    (downcase encoding) ))

(defun vm-mime-transfer-encode-layout (layout)
  (let ((list (vm-mm-layout-parts layout))
	(type (car (vm-mm-layout-type layout)))
	(encoding "7bit")
	(vm-mime-8bit-text-transfer-encoding
	 vm-mime-8bit-text-transfer-encoding))
  (cond ((vm-mime-composite-type-p type)
	 ;; MIME messages of type "message" and
	 ;; "multipart" are required to have a non-opaque
	 ;; content transfer encoding.  This means that
	 ;; if the user only wants to send out 7bit data,
	 ;; then any subpart that contains 8bit data must
	 ;; have an opaque (qp or base64) 8->7bit
	 ;; conversion performed on it so that the
	 ;; enclosing entity can use a non-opaque
	 ;; encoding.
	 ;;
	 ;; message/partial requires a "7bit" encoding so
	 ;; force 8->7 conversion in that case.
	 (cond ((memq vm-mime-8bit-text-transfer-encoding
		      '(quoted-printable base64))
		t)
	       ((vm-mime-types-match "message/partial" type)
		(setq vm-mime-8bit-text-transfer-encoding
		      'quoted-printable)))
	 (while list
	   (if (equal (vm-mime-transfer-encode-layout (car list)) "8bit")
	       (setq encoding "8bit"))
	   (setq list (cdr list))))
	(t
	 (if (and (vm-mime-types-match "message/partial" type)
		  (not (memq vm-mime-8bit-text-transfer-encoding
			     '(quoted-printable base64))))
		(setq vm-mime-8bit-text-transfer-encoding
		      'quoted-printable))
	 (setq encoding
	       (vm-mime-transfer-encode-region (vm-mm-layout-encoding layout)
					       (vm-mm-layout-body-start layout)
					       (vm-mm-layout-body-end layout)
					       (vm-mime-text-type-layout-p
						layout)))))
  (if (not (equal encoding (downcase (car (vm-mm-layout-type layout)))))
      (save-excursion
	(save-restriction
	  (goto-char (vm-mm-layout-header-start layout))
	  (narrow-to-region (point) (vm-mm-layout-header-end layout))
	  (vm-reorder-message-headers nil nil "Content-Transfer-Encoding:")
	  (if (not (equal encoding "7bit"))
	      (insert "CONTENT-TRANSFER-ENCODING: " encoding "\n"))
	  encoding )))))

(defun vm-mime-text-description (start end)
  (save-excursion
    (goto-char start)
    (if (looking-at "[ \t\n]*-- \n")
	".signature"
      (if (re-search-forward "^-- \n" nil t)
	  "message body and .signature"
	"message body text"))))
;; tried this but random text in the object tag does't look right.
;;      (skip-chars-forward " \t\n")
;;      (let ((description (buffer-substring (point) (min (+ (point) 20) end)))
;;	    (ellipsis (< (+ (point) 20) end))
;;	    (i nil))
;;	(while (setq i (string-match "[\t\r\n]" description i))
;;	  (aset description i " "))
;;	(cond ((= 0 (length description)) nil)
;;	      (ellipsis (concat description "..."))
;;	      (t description))))))

(defun vm-delete-mime-object (&optional saved-file)
  "Delete the contents of MIME object referred to by the MIME button at point.
The MIME object is replaced by a text/plain object that briefly
describes what was deleted."
  (interactive)
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (if (and (vm-virtual-message-p (car vm-message-pointer))
	   (null (vm-virtual-messages-of (car vm-message-pointer))))
      (error "Can't edit unmirrored virtual messages."))
  (and vm-presentation-buffer
       (set-buffer vm-presentation-buffer))
  (let (layout)
    (cond (vm-fsfemacs-p
	   (let (o-list o (found nil))
	     (setq o-list (overlays-at (point)))
	     (while (and o-list (not found))
	       (setq o (car o-list))
	       (cond ((setq layout (overlay-get o 'vm-mime-layout))
		      (setq found t)
		      (if (eq layout
			      (vm-mime-layout-of
			       (vm-mm-layout-message layout)))
			  (error "Can't delete only MIME object; use vm-delete-message instead."))
		      (if vm-mime-confirm-delete
			  (or (y-or-n-p (vm-mime-sprintf "Delete %t? " layout))
			      (error "Aborted")))
		      (vm-mime-discard-layout-contents layout saved-file)))
	       (setq o-list (cdr o-list)))
	     (if (not found)
		 (error "No MIME button found at point."))
	     (let ((inhibit-read-only t)
		   (buffer-read-only nil))
	       (save-excursion
		 (vm-save-restriction
		  (goto-char (overlay-start o))
		  (insert "Deleted " (vm-mime-sprintf "%d\n" layout))
		  (delete-region (point) (overlay-end o)))))))
	  (vm-xemacs-p
	   (let ((e (extent-at (point) nil 'vm-mime-layout)))
	     (if (null e)
		 (error "No MIME button found at point.")
	       (setq layout (extent-property e 'vm-mime-layout))
	       (if (eq layout (vm-mime-layout-of
			       (vm-mm-layout-message layout)))
		   (error "Can't delete only MIME object; use vm-delete-message instead."))
	       (if vm-mime-confirm-delete
		   (or (y-or-n-p (vm-mime-sprintf "Delete %t? " layout))
		       (error "Aborted")))
	       (let ((inhibit-read-only t)
		     opos
		     (buffer-read-only nil))
		 (save-excursion
		   (vm-save-restriction
		     (goto-char (extent-start-position e))
		     (setq opos (point))
		     (insert "Deleted " (vm-mime-sprintf "%d\n" layout))
		     (delete-region (point) (extent-end-position e))
		     (set-extent-endpoints e opos (point)))))
	       (vm-mime-discard-layout-contents layout saved-file)))))))

(defun vm-mime-discard-layout-contents (layout &optional file)
  (save-excursion
    (let ((inhibit-read-only t)
	  (buffer-read-only nil)
	  (m (vm-mm-layout-message layout))
	  newid new-layout)
      (set-buffer (vm-buffer-of m))
      (vm-save-restriction
	(widen)
	(if (vm-mm-layout-is-converted layout)
	    (setq layout (vm-mm-layout-unconverted-layout layout)))
	(goto-char (vm-mm-layout-header-start layout))
	(cond ((null file)
	       (insert "Content-Type: text/plain; charset=us-ascii\n\n")
	       (vm-set-mm-layout-body-start layout (point-marker))
	       (insert "[Deleted " (vm-mime-sprintf "%d]\n" layout)))
	      (t
	       (insert "Content-Type: message/external-body; access-type=local-file; name=\"" file "\"\n")
	       (insert "Content-Transfer-Encoding: 7bit\n\n")
	       (insert "Content-Type: " (car (vm-mm-layout-qtype layout)))
	       (if (cdr (vm-mm-layout-qtype layout))
		   (let ((p (cdr (vm-mm-layout-qtype layout))))
		     (insert "; " (mapconcat 'identity p "; "))))
	       (insert "\n")
	       (if (vm-mm-layout-qdisposition layout)
		   (let ((p (vm-mm-layout-qdisposition layout)))
		     (insert "Content-Disposition: "
			     (mapconcat 'identity p "; ")
			     "\n")))
	       (if (vm-mm-layout-id layout)
		   (insert "Content-ID: " (vm-mm-layout-id layout) "\n")
		 (setq newid (vm-make-message-id))
		 (insert "Content-ID: " newid "\n"))
	       (insert "Content-Transfer-Encoding: binary\n\n")
	       (insert "[Deleted " (vm-mime-sprintf "%d]\n" layout))
	       (insert "[Saved to " file " on " (system-name) "]\n")))
	(delete-region (point) (vm-mm-layout-body-end layout))
	(vm-set-edited-flag-of m t)
	(vm-set-byte-count-of m nil)
	(vm-set-line-count-of m nil)
	(vm-set-stuff-flag-of m t)
	;; For the dreaded From_-with-Content-Length folders recompute
	;; the message length and make a new Content-Length header.
	(if (eq (vm-message-type-of m) 'From_-with-Content-Length)
	    (let (length)
	      (goto-char (vm-headers-of m))
	      ;; first delete all copies of Content-Length
	      (while (and (re-search-forward vm-content-length-search-regexp
					     (vm-text-of m) t)
			  (null (match-beginning 1))
			  (progn (goto-char (match-beginning 0))
				 (vm-match-header vm-content-length-header)))
		(delete-region (vm-matched-header-start)
			       (vm-matched-header-end)))
	      ;; now compute the message body length
	      (setq length (- (vm-end-of m) (vm-text-of m)))
	      ;; insert the header
	      (goto-char (vm-headers-of m))
	      (insert vm-content-length-header " "
		      (int-to-string length) "\n")))
	;; make sure we get the summary updated.  The 'edited'
	;; flag might already be set and therefore trying to set
	;; it again might not have triggered an update.  We need
	;; the update because the message size has changed.
	(vm-mark-for-summary-update (vm-mm-layout-message layout))
	(cond (file
	       (save-restriction
		 (narrow-to-region (vm-mm-layout-header-start layout)
				   (vm-mm-layout-body-end layout))
		 (setq new-layout (vm-mime-parse-entity-safe))
		 ;; should use accessor and mutator functions
		 ;; to copy the layout struct members, but i'm
		 ;; tired.
		 (let ((i (1- (length layout))))
		   (while (>= i 0)
		     (aset layout i (aref new-layout i))
		     (setq i (1- i))))))
	      (t
	       (vm-set-mm-layout-type layout '("text/x-vm-deleted"))
	       (vm-set-mm-layout-qtype layout '("text/x-vm-deleted"))
	       (vm-set-mm-layout-encoding layout "7bit")
	       (vm-set-mm-layout-id layout nil)
	       (vm-set-mm-layout-description
		layout
		(vm-mime-sprintf "Deleted %d" layout))
	       (vm-set-mm-layout-disposition layout nil)
	       (vm-set-mm-layout-qdisposition layout nil)
	       (vm-set-mm-layout-parts layout nil)
	       (vm-set-mm-layout-display-error layout nil)))))))

(defun vm-mime-encode-composition ()
 "MIME encode the current mail composition buffer.
Attachment tags added to the buffer with vm-mime-attach-file are expanded
and the approriate content-type and boundary markup information is added."
  (interactive)
  (buffer-enable-undo)
  (let ((unwind-needed t)
	(mybuffer (current-buffer)))
    (unwind-protect
	(progn
	  (cond (vm-xemacs-p
		 (vm-mime-xemacs-encode-composition))
		(vm-fsfemacs-p
		 (vm-mime-fsfemacs-encode-composition))
		(t
		 (error "don't know how to MIME encode composition for %s"
			(emacs-version))))
	  (setq unwind-needed nil))
      (and unwind-needed (consp buffer-undo-list)
	   (eq mybuffer (current-buffer))
	   (setq buffer-undo-list (primitive-undo 1 buffer-undo-list))))))

(defvar enriched-mode)

;; Non-XEmacs specific changes to this function should be
;; made to vm-mime-fsfemacs-encode-composition as well.
(defun vm-mime-xemacs-encode-composition ()
  (save-restriction
    (widen)
    (if (not (eq major-mode 'mail-mode))
	(error "Command must be used in a VM Mail mode buffer."))
    (or (null (vm-mail-mode-get-header-contents "MIME-Version:"))
	(error "Message is already MIME encoded."))
    (let ((8bit nil)
	  (just-one nil)
	  (boundary-positions nil)
	  (enriched (and (boundp 'enriched-mode) enriched-mode))
	  forward-local-refs already-mimed layout e e-list boundary
	  type encoding charset params description disposition object
	  opoint-min)
      (mail-text)
      (setq e-list (extent-list nil (point) (point-max))
	    e-list (vm-delete (function
			       (lambda (e)
				 (extent-property e 'vm-mime-object)))
			      e-list t)
	    e-list (sort e-list (function
				 (lambda (e1 e2)
				   (< (extent-end-position e1)
				      (extent-end-position e2))))))
      ;; If there's just one attachment and no other readable
      ;; text in the buffer then make the message type just be
      ;; the attachment type rather than sending a multipart
      ;; message with one attachment
      (setq just-one (and (= (length e-list) 1)
			  (looking-at "[ \t\n]*")
			  (= (match-end 0)
			     (extent-start-position (car e-list)))
			  (save-excursion
			    (goto-char (extent-end-position (car e-list)))
			    (looking-at "[ \t\n]*\\'"))))
      (if (null e-list)
	  (progn
	    (narrow-to-region (point) (point-max))
	    ;; support enriched-mode for text/enriched composition
	    (if enriched
		(let ((enriched-initial-annotation ""))
		  (enriched-encode (point-min) (point-max))))
	    (setq charset (vm-determine-proper-charset (point-min)
						       (point-max)))
	    (if vm-xemacs-mule-p
		(encode-coding-region (point-min) (point-max)
				      buffer-file-coding-system))
	    (setq encoding (vm-determine-proper-content-transfer-encoding
			    (point-min)
			    (point-max))
		  encoding (vm-mime-transfer-encode-region encoding
							   (point-min)
							   (point-max)
							   t))
	    (widen)
	    (vm-remove-mail-mode-header-separator)
	    (goto-char (point-min))
	    (vm-reorder-message-headers
	     nil nil "\\(Content-Type:\\|Content-Transfer-Encoding\\|MIME-Version:\\)")
	    (insert "MIME-Version: 1.0\n")
	    (if enriched
		(insert "Content-Type: text/enriched; charset=" charset "\n")
	      (insert "Content-Type: text/plain; charset=" charset "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n")
	    (vm-add-mail-mode-header-separator))
	(while e-list
	  (setq e (car e-list))
	  (if (or just-one
		  (save-excursion
		    (eq (extent-start-position e)
			(re-search-forward "[ \t\n]*"
					   (extent-start-position e) t))))
	      (delete-region (point) (extent-start-position e))
	    (narrow-to-region (point) (extent-start-position e))
	    (if enriched
		(let ((enriched-initial-annotation ""))
		  (enriched-encode (point-min) (point-max))))
	    (setq charset (vm-determine-proper-charset (point-min)
						       (point-max)))
	    (if vm-xemacs-mule-p
		(encode-coding-region (point-min) (point-max)
				      buffer-file-coding-system))
	    (setq encoding (vm-determine-proper-content-transfer-encoding
			    (point-min)
			    (point-max))
		  encoding (vm-mime-transfer-encode-region encoding
							   (point-min)
							   (point-max)
							   t)
		  description (vm-mime-text-description (point-min)
							(point-max)))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (if enriched
		(insert "Content-Type: text/enriched; charset=" charset "\n")
	      (insert "Content-Type: text/plain; charset=" charset "\n"))
	    (if description
		(insert "Content-Description: " description "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n")
	    (widen))
	  (goto-char (extent-start-position e))
	  (narrow-to-region (point) (point))
	  (setq object (extent-property e 'vm-mime-object))
	  ;; insert the object
	  (cond ((bufferp object)
		 (insert-buffer-substring object))
		((stringp object)
		 (let ((coding-system-for-read
			(if (vm-mime-text-type-p
			     (extent-property e 'vm-mime-type))
			    (vm-line-ending-coding-system)
			  (vm-binary-coding-system)))
		       ;; no transformations!
		       (format-alist nil)
		       ;; no decompression!
		       (jka-compr-compression-info-list nil)
		       ;; don't let buffer-file-coding-system be changed
		       ;; by insert-file-contents.  The
		       ;; value we bind to it to here isn't important.
		       (buffer-file-coding-system (vm-binary-coding-system)))
		   (insert-file-contents object))))
	  ;; gather information about the object from the extent.
	  (if (setq already-mimed (extent-property e 'vm-mime-encoded))
	      (setq layout (vm-mime-parse-entity
			    nil (list "text/plain" "charset=us-ascii")
			    "7bit")
		    type (or (extent-property e 'vm-mime-type)
			     (car (vm-mm-layout-type layout)))
		    params (or (extent-property e 'vm-mime-parameters)
			       (cdr (vm-mm-layout-qtype layout)))
		    forward-local-refs
		        (car (extent-property e 'vm-mime-forward-local-refs))
		    description (extent-property e 'vm-mime-description)
		    disposition
		      (if (not
			   (equal
			    (car (extent-property e 'vm-mime-disposition))
			    "unspecified"))
			  (extent-property e 'vm-mime-disposition)
			(vm-mm-layout-qdisposition layout)))
	    (setq type (extent-property e 'vm-mime-type)
		  params (extent-property e 'vm-mime-parameters)
		  forward-local-refs
		      (car (extent-property e 'vm-mime-forward-local-refs))
		  description (extent-property e 'vm-mime-description)
		  disposition
		    (if (not (equal
			      (car (extent-property e 'vm-mime-disposition))
			      "unspecified"))
			(extent-property e 'vm-mime-disposition)
		      nil)))
	  (cond ((vm-mime-types-match "text" type)
		 (setq encoding
		       (vm-determine-proper-content-transfer-encoding
			(if already-mimed
			    (vm-mm-layout-body-start layout)
			  (point-min))
			(point-max))
		       encoding (vm-mime-transfer-encode-region
				 encoding
				 (if already-mimed
				     (vm-mm-layout-body-start layout)
				   (point-min))
				 (point-max)
				 t))
		 (setq 8bit (or 8bit (equal encoding "8bit"))))
		((vm-mime-composite-type-p type)
		 (setq opoint-min (point-min))
		 (if (not already-mimed)
		     (progn
		       (goto-char (point-min))
		       (insert "Content-Type: " type "\n")
		       ;; vm-mime-transfer-encode-layout will replace
		       ;; this if the transfer encoding changes.
		       (insert "Content-Transfer-Encoding: 7bit\n\n")
		       (setq layout (vm-mime-parse-entity
				     nil (list "text/plain" "charset=us-ascii")
				     "7bit"))
		       (setq already-mimed t)))
		 (and layout (not forward-local-refs)
		      (vm-mime-internalize-local-external-bodies layout))
		 (setq encoding (vm-mime-transfer-encode-layout layout))
		 (setq 8bit (or 8bit (equal encoding "8bit")))
		 (goto-char (point-max))
		 (widen)
		 (narrow-to-region opoint-min (point)))
		(t
		 (and layout (not forward-local-refs)
		      (vm-mime-internalize-local-external-bodies layout))
		 (if already-mimed
		     (setq encoding (vm-mime-transfer-encode-layout layout))
		   (vm-mime-base64-encode-region (point-min) (point-max))
		   (setq encoding "base64"))))
	  (if just-one
	      nil
	    (goto-char (point-min))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (if (not already-mimed)
		nil
	      ;; trim headers
	      (vm-reorder-message-headers nil '("Content-ID:") nil)
	      ;; remove header/text separator
	      (goto-char (1- (vm-mm-layout-body-start layout)))
	      (if (looking-at "\n")
		  (delete-char 1)))
	    (insert "Content-Type: " type)
	    (if params
		(if vm-mime-avoid-folding-content-type
		    (insert "; " (mapconcat 'identity params "; ") "\n")
		  (insert ";\n\t" (mapconcat 'identity params ";\n\t") "\n"))
	      (insert "\n"))
	    (and description
		 (insert "Content-Description: " description "\n"))
	    (if disposition
		(progn
		  (insert "Content-Disposition: " (car disposition))
		  (if (cdr disposition)
		      (insert ";\n\t" (mapconcat 'identity
						 (cdr disposition)
						 ";\n\t")))
		  (insert "\n")))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n"))
	  (goto-char (point-max))
	  (widen)
	  (save-excursion
	    (goto-char (extent-start-position e))
	    (vm-assert (looking-at "\\[ATTACHMENT")))
	  (delete-region (extent-start-position e)
			 (extent-end-position e))
	  (detach-extent e)
	  (if (looking-at "\n")
	      (delete-char 1))
	  (setq e-list (cdr e-list)))
	;; handle the remaining chunk of text after the last
	;; extent, if any.
	(if (or just-one (looking-at "[ \t\n]*\\'"))
	    (delete-region (point) (point-max))
	  (if enriched
	      (let ((enriched-initial-annotation ""))
		(enriched-encode (point) (point-max))))
	  (setq charset (vm-determine-proper-charset (point)
						     (point-max)))
	  (if vm-xemacs-mule-p
	      (encode-coding-region (point) (point-max)
				    buffer-file-coding-system))
	  (setq encoding (vm-determine-proper-content-transfer-encoding
			  (point)
			  (point-max))
		encoding (vm-mime-transfer-encode-region encoding
							 (point)
							 (point-max)
							 t)
		description (vm-mime-text-description (point) (point-max)))
	  (setq 8bit (or 8bit (equal encoding "8bit")))
	  (setq boundary-positions (cons (point-marker) boundary-positions))
	  (if enriched
	      (insert "Content-Type: text/enriched; charset=" charset "\n")
	    (insert "Content-Type: text/plain; charset=" charset "\n"))
	  (if description
	      (insert "Content-Description: " description "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n\n")
	  (goto-char (point-max)))
	(setq boundary (vm-mime-make-multipart-boundary))
	(mail-text)
	(while (re-search-forward (concat "^--"
					  (regexp-quote boundary)
					  "\\(--\\)?$")
				  nil t)
	  (setq boundary (vm-mime-make-multipart-boundary))
	  (mail-text))
	(goto-char (point-max))
	(or just-one (insert "\n--" boundary "--\n"))
	(while boundary-positions
	  (goto-char (car boundary-positions))
	  (insert "\n--" boundary "\n")
	  (setq boundary-positions (cdr boundary-positions)))
	(if (and just-one already-mimed)
	    (progn
	      (goto-char (vm-mm-layout-header-start layout))
	      ;; trim headers
	      (vm-reorder-message-headers nil '("Content-ID:") nil)
	      ;; remove header/text separator
	      (goto-char (vm-mm-layout-header-end layout))
	      (if (looking-at "\n")
		  (delete-char 1))
	      ;; copy remainder to enclosing entity's header section
	      (goto-char (point-max))
	      (insert-buffer-substring (current-buffer)
				       (vm-mm-layout-header-start layout)
				       (vm-mm-layout-body-start layout))
	      (delete-region (vm-mm-layout-header-start layout)
			     (vm-mm-layout-body-start layout))))
	(goto-char (point-min))
	(vm-remove-mail-mode-header-separator)
	(vm-reorder-message-headers
	 nil nil "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(vm-add-mail-mode-header-separator)
	(insert "MIME-Version: 1.0\n")
	(if (not just-one)
	    (insert (if vm-mime-avoid-folding-content-type
			"Content-Type: multipart/mixed; boundary=\""
		      "Content-Type: multipart/mixed;\n\tboundary=\"")
		    boundary "\"\n")
	  (insert "Content-Type: " type)
	  (if params
	      (if vm-mime-avoid-folding-content-type
		  (insert "; " (mapconcat 'identity params "; ") "\n")
		(insert ";\n\t" (mapconcat 'identity params ";\n\t") "\n"))
	    (insert "\n")))
	(if (and just-one description)
	    (insert "Content-Description: " description "\n"))
	(if (and just-one disposition)
	    (progn
	      (insert "Content-Disposition: " (car disposition))
	      (if (cdr disposition)
		  (if vm-mime-avoid-folding-content-type
		      (insert "; " (mapconcat 'identity (cdr disposition) "; ")
			      "\n")
		    (insert ";\n\t" (mapconcat 'identity (cdr disposition)
					       ";\n\t") "\n"))
		(insert "\n"))))
	(if just-one
	    (insert "Content-Transfer-Encoding: " encoding "\n")
	  (if 8bit
	      (insert "Content-Transfer-Encoding: 8bit\n")
	    (insert "Content-Transfer-Encoding: 7bit\n")))))))

;; Non-FSF-Emacs specific changes to this function should be
;; made to vm-mime-xemacs-encode-composition as well.
(defun vm-mime-fsfemacs-encode-composition ()
  (save-restriction
    (widen)
    (if (not (eq major-mode 'mail-mode))
	(error "Command must be used in a VM Mail mode buffer."))
    (or (null (vm-mail-mode-get-header-contents "MIME-Version:"))
	(error "Message is already MIME encoded."))
    (let ((8bit nil)
	  (just-one nil)
	  (boundary-positions nil)
	  (enriched (and (boundp 'enriched-mode) enriched-mode))
	  forward-local-refs already-mimed layout o o-list boundary
	  type encoding charset params description disposition object
	  opoint-min delete-object)
      (mail-text)
      (setq o-list (vm-mime-fake-attachment-overlays (point) (point-max))
	    o-list (vm-delete (function
			       (lambda (o)
				 (overlay-get o 'vm-mime-object)))
			      o-list t)
	    o-list (sort o-list (function
				 (lambda (e1 e2)
				   (< (overlay-end e1)
				      (overlay-end e2))))))
      ;; If there's just one attachment and no other readable
      ;; text in the buffer then make the message type just be
      ;; the attachment type rather than sending a multipart
      ;; message with one attachment
      (setq just-one (and (= (length o-list) 1)
			  (looking-at "[ \t\n]*")
			  (= (match-end 0)
			     (overlay-start (car o-list)))
			  (save-excursion
			    (goto-char (overlay-end (car o-list)))
			    (looking-at "[ \t\n]*\\'"))))
      (if (null o-list)
	  (progn
	    (narrow-to-region (point) (point-max))
	   ;; support enriched-mode for text/enriched composition
	    (if enriched
		(let ((enriched-initial-annotation ""))
		  (enriched-encode (point-min) (point-max))))
	    (setq charset (vm-determine-proper-charset (point-min)
						       (point-max)))
	    (if vm-fsfemacs-mule-p
		(let ((coding-system
		       (car (cdr (vm-string-assoc
				  charset
				  vm-mime-mule-charset-to-coding-alist)))))
		  (if (null coding-system)
		      (error "Can't find a coding system for charset %s"
			     charset)
		    (encode-coding-region (point-min) (point-max)
					  coding-system))))
	    (setq encoding (vm-determine-proper-content-transfer-encoding
			    (point-min)
			    (point-max))
		  encoding (vm-mime-transfer-encode-region encoding
							   (point-min)
							   (point-max)
							   t))
	    (widen)
	    (vm-remove-mail-mode-header-separator)
	    (goto-char (point-min))
	    (vm-reorder-message-headers
	     nil nil "\\(Content-Type:\\|Content-Transfer-Encoding\\|MIME-Version:\\)")
	    (insert "MIME-Version: 1.0\n")
	    (if enriched
		(insert "Content-Type: text/enriched; charset=" charset "\n")
	      (insert "Content-Type: text/plain; charset=" charset "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n")
	    (vm-add-mail-mode-header-separator))
	(while o-list
	  (setq o (car o-list))
	  (if (or just-one
		  (save-excursion
		    (eq (overlay-start o)
			(re-search-forward "[ \t\n]*" (overlay-start o) t))))
	      (delete-region (point) (overlay-start o))
	    (narrow-to-region (point) (overlay-start o))
	   ;; support enriched-mode for text/enriched composition
	    (if enriched
		(let ((enriched-initial-annotation ""))
		  (save-excursion
		    ;; insert/delete trick needed to avoid
		    ;; enriched-mode tags from seeping into the
		    ;; attachment overlays.  I really wish
		    ;; front-advance / rear-advance overlay
		    ;; endpoint properties actually worked.
		    (goto-char (point-max))
		    (insert-before-markers "\n")
		    (enriched-encode (point-min) (1- (point)))
		    (goto-char (point-max))
		    (delete-char -1))))
	    (setq charset (vm-determine-proper-charset (point-min)
						       (point-max)))
	    (if vm-fsfemacs-mule-p
		(let ((coding-system
		       (car (cdr (vm-string-assoc
				  charset
				  vm-mime-mule-charset-to-coding-alist)))))
		  (if (null coding-system)
		      (error "Can't find a coding system for charset %s"
			     charset)
		    (encode-coding-region (point-min) (point-max)
					  coding-system))))
	    (setq encoding (vm-determine-proper-content-transfer-encoding
			    (point-min)
			    (point-max))
		  encoding (vm-mime-transfer-encode-region encoding
							   (point-min)
							   (point-max)
							   t)
		  description (vm-mime-text-description (point-min)
							(point-max)))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (if enriched
		(insert "Content-Type: text/enriched; charset=" charset "\n")
	      (insert "Content-Type: text/plain; charset=" charset "\n"))
	    (if description
		(insert "Content-Description: " description "\n"))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n")
	    (widen))
	  (goto-char (overlay-start o))
	  (narrow-to-region (point) (point))
	  (setq object (overlay-get o 'vm-mime-object))
	  (setq delete-object nil)
	  (cond ((bufferp object)
		 ;; Under Emacs 20.7 inserting a unibyte buffer
		 ;; contents that contain 8-bit characters into a
		 ;; multibyte buffer causes the inserted data to be
		 ;; corrupted with the dreaded \201 corruption.  So
		 ;; we write the data out to disk and let the file
		 ;; be inserted, which gets aoround the problem.
		 (let ((tempfile (vm-make-tempfile)))
		   ;; make note to delete the tempfile after insertion
		   (setq delete-object t)
		   (save-excursion
		     (set-buffer object)
		     (let ((buffer-file-coding-system
			       (vm-binary-coding-system)))
		       (write-region (point-min) (point-max) tempfile nil 0))
		     (setq object tempfile)))))
	  ;; insert the object
	  (cond ((stringp object)
		 ;; as of FSF Emacs 19.34, even with the hooks
		 ;; we've attached to the attachment overlays,
		 ;; text STILL can be inserted into them when
		 ;; font-lock is enabled.  Explaining why is
		 ;; beyond the scope of this comment and I
		 ;; don't know the answer anyway.  This
		 ;; insertion dance work to prevent it.
		 (insert-before-markers " ")
		 (forward-char -1)
		 (let ((coding-system-for-read
			(if (vm-mime-text-type-p
			     (overlay-get o 'vm-mime-type))
			    (vm-line-ending-coding-system)
			  (vm-binary-coding-system)))
		       ;; no transformations!
		       (format-alist nil)
		       ;; no decompression!
		       (jka-compr-compression-info-list nil)
		       ;; don't let buffer-file-coding-system be
		       ;; changed by insert-file-contents.  The
		       ;; value we bind to it to here isn't
		       ;; important.
		       (buffer-file-coding-system (vm-binary-coding-system))
		       ;; For NTEmacs 19: need to do this to make
		       ;; sure CRs aren't eaten.
		       (file-name-buffer-file-type-alist '(("." . t))))
		   (condition-case data
		       (insert-file-contents object)
		     (error
		      (if delete-object
			  (vm-error-free-call 'delete-file object))
		      ;; font-lock could signal this error in FSF
		      ;; Emacs versions prior to 21.0.  Catch it
		      ;; and ignore it.
		      (if (equal data '(error "Invalid search bound (wrong side of point)"))
			  nil
			(signal (car data) (cdr data))))))
		 (if delete-object
		     (vm-error-free-call 'delete-file object))
		 (goto-char (point-max))
		 (delete-char -1)))
	  ;; gather information about the object from the extent.
	  (if (setq already-mimed (overlay-get o 'vm-mime-encoded))
	      (setq layout (vm-mime-parse-entity
			    nil (list "text/plain" "charset=us-ascii")
			    "7bit")
		    type (or (overlay-get o 'vm-mime-type)
			     (car (vm-mm-layout-type layout)))
		    params (or (overlay-get o 'vm-mime-parameters)
			       (cdr (vm-mm-layout-qtype layout)))
		    forward-local-refs
		        (car (overlay-get o 'vm-mime-forward-local-refs))
		    description (overlay-get o 'vm-mime-description)
		    disposition
		    (if (not
			 (equal
			  (car (overlay-get o 'vm-mime-disposition))
			  "unspecified"))
			(overlay-get o 'vm-mime-disposition)
		      (vm-mm-layout-qdisposition layout)))
	    (setq type (overlay-get o 'vm-mime-type)
		  params (overlay-get o 'vm-mime-parameters)
		  forward-local-refs
		      (car (overlay-get o 'vm-mime-forward-local-refs))
		  description (overlay-get o 'vm-mime-description)
		  disposition
		  (if (not (equal
			    (car (overlay-get o 'vm-mime-disposition))
			    "unspecified"))
		      (overlay-get o 'vm-mime-disposition)
		    nil)))
	  (cond ((vm-mime-types-match "text" type)
		 (setq encoding
		       (vm-determine-proper-content-transfer-encoding
			(if already-mimed
			    (vm-mm-layout-body-start layout)
			  (point-min))
			(point-max))
		       encoding (vm-mime-transfer-encode-region
				 encoding
				 (if already-mimed
				     (vm-mm-layout-body-start layout)
				   (point-min))
				 (point-max)
				 t))
		 (setq 8bit (or 8bit (equal encoding "8bit"))))
		((vm-mime-composite-type-p type)
		 (setq opoint-min (point-min))
		 (if (not already-mimed)
		     (progn
		       (goto-char (point-min))
		       (insert "Content-Type: " type "\n")
		       ;; vm-mime-transfer-encode-layout will replace
		       ;; this if the transfer encoding changes.
		       (insert "Content-Transfer-Encoding: 7bit\n\n")
		       (setq layout (vm-mime-parse-entity
				     nil (list "text/plain" "charset=us-ascii")
				     "7bit"))
		       (setq already-mimed t)))
		 (and layout (not forward-local-refs)
		      (vm-mime-internalize-local-external-bodies layout))
		 (setq encoding (vm-mime-transfer-encode-layout layout))
		 (setq 8bit (or 8bit (equal encoding "8bit")))
		 (goto-char (point-max))
		 (widen)
		 (narrow-to-region opoint-min (point)))
		(t
		 (and layout (not forward-local-refs)
		      (vm-mime-internalize-local-external-bodies layout))
		 (if already-mimed
		     (setq encoding (vm-mime-transfer-encode-layout layout))
		   (vm-mime-base64-encode-region (point-min) (point-max))
		   (setq encoding "base64"))))
	  (if just-one
	      nil
	    (goto-char (point-min))
	    (setq boundary-positions (cons (point-marker) boundary-positions))
	    (if (not already-mimed)
		nil
	      ;; trim headers
	      (vm-reorder-message-headers nil '("Content-ID:") nil)
	      ;; remove header/text separator
	      (goto-char (1- (vm-mm-layout-body-start layout)))
	      (if (looking-at "\n")
		  (delete-char 1)))
	    (insert "Content-Type: " type)
	    (if params
		(if vm-mime-avoid-folding-content-type
		    (insert "; " (mapconcat 'identity params "; ") "\n")
		  (insert ";\n\t" (mapconcat 'identity params ";\n\t") "\n"))
	      (insert "\n"))
	    (and description
		 (insert "Content-Description: " description "\n"))
	    (if disposition
		(progn
		  (insert "Content-Disposition: " (car disposition))
		  (if (cdr disposition)
		      (insert ";\n\t" (mapconcat 'identity
						 (cdr disposition)
						 ";\n\t")))
		  (insert "\n")))
	    (insert "Content-Transfer-Encoding: " encoding "\n\n"))
	  (goto-char (point-max))
	  (widen)
	  (save-excursion
	    (goto-char (overlay-start o))
	    (vm-assert (looking-at "\\[ATTACHMENT")))
	  (delete-region (overlay-start o)
			 (overlay-end o))
	  (delete-overlay o)
	  (if (looking-at "\n")
	      (delete-char 1))
	  (setq o-list (cdr o-list)))
	;; handle the remaining chunk of text after the last
	;; extent, if any.
	(if (or just-one (looking-at "[ \t\n]*\\'"))
	    (delete-region (point) (point-max))
	  ;; support enriched-mode for text/enriched composition
	  (if enriched
	      (let ((enriched-initial-annotation ""))
		(enriched-encode (point) (point-max))))
	  (setq charset (vm-determine-proper-charset (point)
						     (point-max)))
	  (if vm-fsfemacs-mule-p
	      (let ((coding-system
		     (car (cdr (vm-string-assoc
				charset
				vm-mime-mule-charset-to-coding-alist)))))
		(if (null coding-system)
		    (error "Can't find a coding system for charset %s"
			   charset)
		  (encode-coding-region (point) (point-max)
					coding-system))))
	  (setq encoding (vm-determine-proper-content-transfer-encoding
			  (point)
			  (point-max))
		encoding (vm-mime-transfer-encode-region encoding
							 (point)
							 (point-max)
							 t)
		description (vm-mime-text-description (point) (point-max)))
	  (setq 8bit (or 8bit (equal encoding "8bit")))
	  (setq boundary-positions (cons (point-marker) boundary-positions))
	  (if enriched
	      (insert "Content-Type: text/enriched; charset=" charset "\n")
	    (insert "Content-Type: text/plain; charset=" charset "\n"))
	  (if description
	      (insert "Content-Description: " description "\n"))
	  (insert "Content-Transfer-Encoding: " encoding "\n\n")
	  (goto-char (point-max)))
	(setq boundary (vm-mime-make-multipart-boundary))
	(mail-text)
	(while (re-search-forward (concat "^--"
					  (regexp-quote boundary)
					  "\\(--\\)?$")
				  nil t)
	  (setq boundary (vm-mime-make-multipart-boundary))
	  (mail-text))
	(goto-char (point-max))
	(or just-one (insert "\n--" boundary "--\n"))
	(while boundary-positions
	  (goto-char (car boundary-positions))
	  (insert "\n--" boundary "\n")
	  (setq boundary-positions (cdr boundary-positions)))
	(if (and just-one already-mimed)
	    (progn
	      (goto-char (vm-mm-layout-header-start layout))
	      ;; trim headers
	      (vm-reorder-message-headers nil '("Content-ID:") nil)
	      ;; remove header/text separator
	      (goto-char (vm-mm-layout-header-end layout))
	      (if (looking-at "\n")
		  (delete-char 1))
	   ;; copy remainder to enclosing entity's header section
	      (goto-char (point-max))
	      (insert-buffer-substring (current-buffer)
				       (vm-mm-layout-header-start layout)
				       (vm-mm-layout-body-start layout))
	      (delete-region (vm-mm-layout-header-start layout)
			     (vm-mm-layout-body-start layout))))
	(goto-char (point-min))
	(vm-remove-mail-mode-header-separator)
	(vm-reorder-message-headers
	 nil nil "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(vm-add-mail-mode-header-separator)
	(insert "MIME-Version: 1.0\n")
	(if (not just-one)
	    (insert (if vm-mime-avoid-folding-content-type
			"Content-Type: multipart/mixed; boundary=\""
		      "Content-Type: multipart/mixed;\n\tboundary=\"")
		    boundary "\"\n")
	  (insert "Content-Type: " type)
	  (if params
	      (if vm-mime-avoid-folding-content-type
		  (insert "; " (mapconcat 'identity params "; ") "\n")
		(insert ";\n\t" (mapconcat 'identity params ";\n\t") "\n"))
	    (insert "\n")))
	(if (and just-one description)
	    (insert "Content-Description: " description "\n"))
	(if (and just-one disposition)
	    (progn
	      (insert "Content-Disposition: " (car disposition))
	      (if (cdr disposition)
		  (if vm-mime-avoid-folding-content-type
		      (insert "; " (mapconcat 'identity (cdr disposition) "; ")
			      "\n")
		    (insert ";\n\t" (mapconcat 'identity (cdr disposition)
					       ";\n\t") "\n"))
		(insert "\n"))))
	(if just-one
	    (insert "Content-Transfer-Encoding: " encoding "\n")
	  (if 8bit
	      (insert "Content-Transfer-Encoding: 8bit\n")
	    (insert "Content-Transfer-Encoding: 7bit\n")))))))

(defun vm-mime-fragment-composition (size)
  (save-restriction
    (widen)
    (message "Fragmenting message...")
    (let ((buffers nil)
	  (total-markers nil)
	  (id (vm-mime-make-multipart-boundary))
	  (n 1)
	  b header-start header-end master-buffer start end)
      (vm-remove-mail-mode-header-separator)
      ;; message/partial must have "7bit" content transfer
      ;; encoding, so force everything to be encoded for
      ;; 7bit transmission.
      (let ((vm-mime-8bit-text-transfer-encoding
	     (if (eq vm-mime-8bit-text-transfer-encoding '8bit)
		 'quoted-printable
	       vm-mime-8bit-text-transfer-encoding)))
	(vm-mime-transfer-encode-layout
	 (vm-mime-parse-entity nil (list "text/plain" "charset=us-ascii")
			       "7bit")))
      (goto-char (point-min))
      (setq header-start (point))
      (search-forward "\n\n")
      (setq header-end (1- (point)))
      (setq master-buffer (current-buffer))
      (goto-char (point-min))
      (setq start (point))
      (while (not (eobp))
	(condition-case nil
	    (progn
	      (forward-char (max (- size 150) 2000))
	      (beginning-of-line))
	  (end-of-buffer nil))
	(setq end (point))
	(setq b (generate-new-buffer (concat (buffer-name) " part "
					     (int-to-string n))))
	(setq buffers (cons b buffers))
	(set-buffer b)
	(make-local-variable 'vm-send-using-mime)
	(setq vm-send-using-mime nil)
	(insert-buffer-substring master-buffer header-start header-end)
	(goto-char (point-min))
	(vm-reorder-message-headers nil nil
         "\\(Content-Type:\\|MIME-Version:\\|Content-Transfer-Encoding\\)")
	(insert "MIME-Version: 1.0\n")
	(insert (format
		 (if vm-mime-avoid-folding-content-type
		     "Content-Type: message/partial; id=%s; number=%d"
		   "Content-Type: message/partial;\n\tid=%s;\n\tnumber=%d")
		 id n))
	(if vm-mime-avoid-folding-content-type
	    (insert (format "; total=" n))
	  (insert (format ";\n\ttotal=" n)))
	(setq total-markers (cons (point) total-markers))
	(insert "\nContent-Transfer-Encoding: 7bit\n")
	(goto-char (point-max))
	(insert mail-header-separator "\n")
	(insert-buffer-substring master-buffer start end)
	(vm-increment n)
	(set-buffer master-buffer)
	(setq start (point)))
      (vm-decrement n)
      (vm-add-mail-mode-header-separator)
      (let ((bufs buffers))
	(while bufs
	  (set-buffer (car bufs))
	  (goto-char (car total-markers))
	  (prin1 n (current-buffer))
	  (setq bufs (cdr bufs)
		total-markers (cdr total-markers)))
	(set-buffer master-buffer))
      (message "Fragmenting message... done")
      (nreverse buffers))))

;; moved to vm-reply.el, not MIME-specific.
(fset 'vm-mime-preview-composition 'vm-preview-composition)

(defun vm-mime-composite-type-p (type)
  (or (vm-mime-types-match "message/rfc822" type)
      (vm-mime-types-match "message/news" type)
      (vm-mime-types-match "multipart" type)))

;; Unused currrently.
;;
;;(defun vm-mime-map-atomic-layouts (function list)
;;  (while list
;;    (if (vm-mime-composite-type-p (car (vm-mm-layout-type (car list))))
;;	(vm-mime-map-atomic-layouts function (vm-mm-layout-parts (car list)))
;;      (funcall function (car list)))
;;    (setq list (cdr list))))

(defun vm-mime-sprintf (format layout)
  ;; compile the format into an eval'able s-expression
  ;; if it hasn't been compiled already.
  (let ((match (assoc format vm-mime-compiled-format-alist)))
    (if (null match)
	(progn
	  (vm-mime-compile-format format)
	  (setq match (assoc format vm-mime-compiled-format-alist))))
    ;; The local variable name `vm-mime-layout' is mandatory here for
    ;; the format s-expression to work.
    (let ((vm-mime-layout layout))
      (eval (cdr match)))))

(defun vm-mime-compile-format (format)
  (let ((return-value (vm-mime-compile-format-1 format 0)))
    (setq vm-mime-compiled-format-alist
	  (cons (cons format (nth 1 return-value))
		vm-mime-compiled-format-alist))))

(defun vm-mime-compile-format-1 (format start-index)
  (or start-index (setq start-index 0))
  (let ((case-fold-search nil)
	(done nil)
	(sexp nil)
	(sexp-fmt nil)
	(last-match-end start-index)
	new-match-end conv-spec)
    (store-match-data nil)
    (while (not done)
      (while
	  (and (not done)
	       (string-match
		"%\\(-\\)?\\([0-9]+\\)?\\(\\.\\(-?[0-9]+\\)\\)?\\([()acdefknNstTx%]\\)"
		format last-match-end))
	(setq conv-spec (aref format (match-beginning 5)))
	(setq new-match-end (match-end 0))
	(if (memq conv-spec '(?\( ?a ?c ?d ?e ?f ?k ?n ?N ?s ?t ?T ?x))
	    (progn
	      (cond ((= conv-spec ?\()
		     (save-match-data
		       (let ((retval (vm-mime-compile-format-1 format
							       (match-end 5))))
			 (setq sexp (cons (nth 1 retval) sexp)
			       new-match-end (car retval)))))
		    ((= conv-spec ?a)
		     (setq sexp (cons (list 'vm-mf-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?c)
		     (setq sexp (cons (list 'vm-mf-text-charset
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?d)
		     (setq sexp (cons (list 'vm-mf-content-description
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?e)
		     (setq sexp (cons (list 'vm-mf-content-transfer-encoding
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?f)
		     (setq sexp (cons (list 'vm-mf-attachment-file
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?k)
		     (setq sexp (cons (list 'vm-mf-event-for-default-action
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?n)
		     (setq sexp (cons (list 'vm-mf-parts-count
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?N)
		     (setq sexp (cons (list 'vm-mf-partial-number
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?s)
		     (setq sexp (cons (list 'vm-mf-parts-count-pluralizer
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?t)
		     (setq sexp (cons (list 'vm-mf-content-type
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?T)
		     (setq sexp (cons (list 'vm-mf-partial-total
					    'vm-mime-layout) sexp)))
		    ((= conv-spec ?x)
		     (setq sexp (cons (list 'vm-mf-external-body-content-type
					    'vm-mime-layout) sexp))))
	      (cond ((and (match-beginning 1) (match-beginning 2))
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-left-justify-string
				'vm-left-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2))))))
		    ((match-beginning 2)
		     (setcar sexp
			     (list
			      (if (eq (aref format (match-beginning 2)) ?0)
				  'vm-numeric-right-justify-string
				'vm-right-justify-string)
			      (car sexp)
			      (string-to-int
			       (substring format
					  (match-beginning 2)
					  (match-end 2)))))))
	      (cond ((match-beginning 3)
		     (setcar sexp
			     (list 'vm-truncate-string (car sexp)
				   (string-to-int
				    (substring format
					       (match-beginning 4)
					       (match-end 4)))))))
	      (setq sexp-fmt
		    (cons "%s"
			  (cons (substring format
					   last-match-end
					   (match-beginning 0))
				sexp-fmt))))
	  (setq sexp-fmt
		(cons (if (eq conv-spec ?\))
			  (prog1 "" (setq done t))
			"%%")
		      (cons (substring format
				       (or last-match-end 0)
				       (match-beginning 0))
			    sexp-fmt))))
	(setq last-match-end new-match-end))
      (if (not done)
	  (setq sexp-fmt
		(cons (substring format last-match-end (length format))
		      sexp-fmt)
		done t))
      (setq sexp-fmt (apply 'concat (nreverse sexp-fmt)))
      (if sexp
	  (setq sexp (cons 'format (cons sexp-fmt (nreverse sexp))))
	(setq sexp sexp-fmt)))
    (list last-match-end sexp)))

(defun vm-mime-find-format-for-layout (layout)
  (let ((p vm-mime-button-format-alist)
	(type (car (vm-mm-layout-type layout))))
    (catch 'done
      (cond ((vm-mime-types-match "error/error" type)
	     (throw 'done "%d"))
	    ((vm-mime-types-match "text/x-vm-deleted" type)
	     (throw 'done "%d")))
      (while p
	(if (vm-mime-types-match (car (car p)) type)
	    (throw 'done (cdr (car p)))
	  (setq p (cdr p))))
      "%-35.35t [%k to %a]" )))

(defun vm-mf-content-type (layout)
  (car (vm-mm-layout-type layout)))

(defun vm-mf-external-body-content-type (layout)
  (car (vm-mm-layout-type (car (vm-mm-layout-parts layout)))))

(defun vm-mf-content-transfer-encoding (layout)
  (vm-mm-layout-encoding layout))

(defun vm-mf-content-description (layout)
  (or (vm-mm-layout-description layout)
      (let ((p vm-mime-type-description-alist)
	    (type (car (vm-mm-layout-type layout))))
	(catch 'done
	  (while p
	    (if (vm-mime-types-match (car (car p)) type)
		(throw 'done (cdr (car p)))
	      (setq p (cdr p))))
	  nil ))
      (vm-mf-content-type layout)))

(defun vm-mf-text-charset (layout)
  (or (vm-mime-get-parameter layout "charset")
      "us-ascii"))

(defun vm-mf-parts-count (layout)
  (int-to-string (length (vm-mm-layout-parts layout))))

(defun vm-mf-parts-count-pluralizer (layout)
  (if (= 1 (length (vm-mm-layout-parts layout))) "" "s"))

(defun vm-mf-partial-number (layout)
  (or (vm-mime-get-parameter layout "number")
      "?"))

(defun vm-mf-partial-total (layout)
  (or (vm-mime-get-parameter layout "total")
      "?"))

(defun vm-mf-attachment-file (layout)
  (or vm-mf-attachment-file ;; for %f expansion in external viewer arg lists
      (vm-mime-get-disposition-parameter layout "filename")
      (vm-mime-get-parameter layout "name")
      "<no suggested filename>"))

(defun vm-mf-event-for-default-action (layout)
  (if (vm-mouse-support-possible-here-p)
      "Click mouse-2"
    "Press RETURN"))

(defun vm-mf-default-action (layout)
  (or vm-mf-default-action
      (let (cons)
	(cond ((or (vm-mime-can-display-internal layout)
		   (vm-mime-find-external-viewer
		    (car (vm-mm-layout-type layout))))
	       (let ((p vm-mime-default-action-string-alist)
		     (type (car (vm-mm-layout-type layout))))
		 (catch 'done
		   (while p
		     (if (vm-mime-types-match (car (car p)) type)
			 (throw 'done (cdr (car p)))
		       (setq p (cdr p))))
		   nil )))
	      ((setq cons (vm-mime-can-convert
			   (car (vm-mm-layout-type layout))))
	       (format "convert to %s and display" (nth 1 cons)))
	      (t "save to a file")))
      ;; should not be reached
      "burn in the raging fires of hell forever"))

(provide 'vm-mime)

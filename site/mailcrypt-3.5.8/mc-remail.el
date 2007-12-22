;; mc-remail.el --- Remailer support for Mailcrypt

;; Copyright (C) 1995 Patrick LoPresti <patl@lcs.mit.edu>

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
;;{{{ Load required packages

(require 'mail-utils)
(require 'sendmail)
(require 'mailcrypt)

(eval-and-compile
  (autoload 'mc-cleanup-recipient-headers "mc-toplev")
  (autoload 'mc-encrypt-message "mc-toplev"))

(eval-and-compile
  (condition-case nil (require 'mailalias) (error nil)))

;;}}}
;;{{{ Functions dealing with remailer structures

(defsubst mc-remailer-create (addr id props pre-encr post-encr)
  "Create a remailer structure.

ADDR is the remailer's Email address, a string.

ID is the remailer's public key ID (a string) or nil if the same as
ADDR.

PROPS is a list of properties, as strings.

PRE-ENCR is a list of pre-encryption functions.  Its elements will be
called with the remailer structure itself as argument.

POST-ENCR is similar, but for post-encryption functions."
(list 'remailer addr id props pre-encr post-encr))

(defsubst mc-remailerp (remailer)
  "Test whether REMAILER is a valid remailer struct."
  (and (listp remailer) (eq 'remailer (car-safe remailer))))

(defsubst mc-remailer-address (remailer)
  "Return the Email address of REMAILER."
  (nth 1 remailer))

(defsubst mc-remailer-userid (remailer)
  "Return the userid with which to look up the public key for REMAILER."
  (or (nth 2 remailer)
      (mc-strip-address (mc-remailer-address remailer))))

(defsubst mc-remailer-properties (remailer)
  "Return the property list for REMAILER"
  (nth 3 remailer))

(defsubst mc-remailer-pre-encrypt-hooks (remailer)
  "Return the list of pre-encryption hooks for REMAILER."
  (nth 4 remailer))

(defsubst mc-remailer-post-encrypt-hooks (remailer)
  "Return the list of post-encryption hooks for REMAILER."
  (nth 5 remailer))

(defun mc-remailer-remove-property (remailer prop)
  (let ((props (append (mc-remailer-properties remailer) nil)))
    (setq props (delete prop props))
    (mc-remailer-create
     (mc-remailer-address remailer)
     (mc-remailer-userid remailer)
     props
     (mc-remailer-pre-encrypt-hooks remailer)
     (mc-remailer-post-encrypt-hooks remailer))))

;;}}}
;;{{{ User variables

(defvar mc-response-block-included-headers
  '("From" "To" "Newsgroups")
  "List of header fields to include in response blocks.

These will be copied into the deepest layer of the response block to
help you identify it when it is used to Email you.")


(defvar mc-remailer-tag "(*REMAILER*)"
  "A string which marks an Email address as belonging to a remailer.")

(defvar mc-levien-file-name "~/.remailers"
  "The file containing a Levien format list of remailers.

The file is read by `mc-read-levien-file' and `mc-reread-levien-file'.

The file should include lines of the following form (other lines
are ignored):

$remailer{\"NAME\"} = \"<EMAIL ADDRESS> PROPERTIES\";

PROPERTIES is a space-separated set of strings.

This format is named after Raphael Levien, who maintained a list of
active remailers. Do \"finger rlist@noisebox.remailer.org\" or
\"finger rlist@mixmaster.shinn.net\" for the latest copy of the
remailer list.")

(defvar mc-remailer-user-chains nil
  "An alist of remailer chains defined by the user.

Format is

((NAME . REMAILER-LIST)
 (NAME . REMAILER-LIST)
 ...)

NAME must be a string.

REMAILER-LIST may be an arbitrary sequence, not just a list.  Its
elements may be any of the following:

1) A remailer structure created by `mc-remailer-create'.  This is
   the base case.

2) A string naming another remailer chain to be spliced in
   at this point.

3) A positive integer N representing a chain to be spliced in at this
   point and consisting of a random permutation of the top N remailers
   as ordered in the file `mc-levien-file-name'.

4) An arbitrary Lisp form to be evaluated, which should
   return another REMAILER-LIST to be recursively processed and
   spliced in at this point.

The complete alist of chains is given by the union of the two lists
`mc-remailer-internal-chains' and `mc-remailer-user-chains'.")

(defvar mc-remailer-internal-chains nil
  "List of \"internal\" remailer chains.

This variable is normally generated automatically from a human-readable
list of remailers; see, for example, the function `mc-reread-levien-file'.

To define your own chains, you probably want to use the variable
`mc-remailer-user-chains'.  See that variable's documentation for
format information.")

(defvar mc-remailer-internal-ranking nil
  "Ordered list of remailers, most reliable first.

This variable is normally generated automatically from a human-readable
list of remailers; see, for example, the function `mc-reread-levien-file'.")

(defvar mc-remailer-user-response-block
  (function
   (lambda (addr lines block)
     (concat
      ";;;\n"
      (format
       "To reply to this message, take the following %d-line block, remove\n"
       lines)
      "leading \"- \" constructs (if any), and place it at the top of a\n"
      (format "message to %s :\n" addr)
      block)))
  "A function called to generate response block text.

Value should be a function taking three arguments (ADDR LINES BLOCK).
ADDR is the address to which the response should be sent.
LINES is the number of lines in the encrypted response block.
BLOCK is the response block itself.
Function should return a string to be inserted into the buffer
by mc-remailer-insert-response-block.")

(defvar mc-remailer-pseudonyms nil
  "*A list of your pseudonyms.

This is a list of strings.  Completion against it will be available
when you are prompted for your pseudonym.")

(defvar mc-remailer-preserved-headers
  '("References" "Followup-to" "In-reply-to" "Subject")
  "*Header fields which are preserved as hashmark headers when rewriting.

This is a list of strings naming the preserved headers.  Note that
\"Newsgroups\" and \"To\" are handled specially and should not be included in
this list.")

;;}}}
;;{{{ Handling Levien format remailer lists

(defun mc-parse-levien-buffer ()
  ;; Parse a buffer in Levien format.
  (goto-char (point-min))
  (let (chains remailer remailer-name ranking)
    (while
	(re-search-forward
	 "^\\$remailer{['\"]\\(.+\\)['\"]}[ \t]*=[ \t]*['\"]\\(.*\\)['\"];"
	 nil t)
      (let ((name (buffer-substring-no-properties
		   (match-beginning 1) (match-end 1)))
	    property-list address
	    (value-start (match-beginning 2))
	    (value-end (match-end 2)))
	(goto-char value-start)
	(while (re-search-forward "[^ \t]+" value-end 'no-error)
	  (setq property-list
		(append
		 property-list
		 (list (buffer-substring-no-properties
			(match-beginning 0) (match-end 0))))))
	(setq address (car property-list)
	      property-list (cdr property-list)
	      remailer-name name)
	(if (not
	     (or (member "mix" property-list)
		 (and (or (member "pgp" property-list)
			  (member "pgp." property-list))
		      (or (member "cpunk" property-list)
			  (member "eric" property-list)))))
	    (setq remailer nil)
	  (setq remailer
		(mc-remailer-create
		 address		; Address
		 (if (member "pgp." property-list)
		     name)		; User ID
		 property-list
		 '(mc-generic-pre-encrypt-function) ; Pre-encrypt hooks
		 '(mc-generic-post-encrypt-function) ; Post-encrypt hooks
		 ))))
      (if (not (null remailer))
	  (setq chains (cons (list remailer-name remailer) chains))))
    (goto-char (point-min))
    (if (re-search-forward "----------" nil t) ; Locate rankings at bottom
	;; Read each word in the rankings section.  Each time we 
	;; hit a remailer we've identified, append it to the ranking
	;; list.  Thus we sort remailers according to rank.
	(while (re-search-forward "^\\([a-zA-Z0-9\\-]+\\) " nil t)
	  (setq remailer-name (buffer-substring-no-properties
			       (match-beginning 1) (match-end 1)))
	  (if (assoc remailer-name chains)
	      (setq ranking (append ranking (list remailer-name))))))
    (cons chains ranking)))

(defun mc-read-levien-file ()
  "Read the Levien format file specified in `mc-levien-file-name'.
Return an alist of length-1 chains, one for each remailer, named
after the remailer.  Only include remailers supporting PGP
encryption."
  (save-excursion
    (if (file-readable-p mc-levien-file-name)
	(prog2
	    (find-file-read-only mc-levien-file-name)
	    (mc-parse-levien-buffer)
	  (bury-buffer)))))

(defun mc-reread-levien-file ()
  "Read the Levien format file specified in `mc-levien-file-name'.

Place result in `mc-remailer-internal-chains' and `mc-remailer-internal-ranking'.

See the documentation for the variable `mc-levien-file-name' for
a description of Levien file format."
  (interactive)
  (let ((parsed-levien-file (mc-read-levien-file)))
    (setq mc-remailer-internal-chains (car parsed-levien-file)
	  mc-remailer-internal-ranking (cdr parsed-levien-file))))

;;}}}
;;{{{ Arbitrary chain choice

(defun mc-remailer-choose-first (n &optional l)
  (cond
   ((= n 0) nil)
   ((null l) (mc-remailer-choose-first n mc-remailer-internal-ranking))
   (t (cons (car l) (mc-remailer-choose-first (1- n) (cdr l))))))

(defun mc-remailer-choose-chain (n)
  (if (null mc-remailer-internal-ranking)
      (error "No ranking information, cannot choose the %d best remailer%s"
	     n (if (> n 1) "s" "")))
  (append (shuffle-vector (vconcat (mc-remailer-choose-first n)))
	  nil))

;;}}}
;;{{{ Canonicalization function

(defun mc-remailer-canonicalize-elmt (elmt chains-alist)
  (cond
   ((mc-remailerp elmt) (list elmt))
   ((stringp elmt)
    (mc-remailer-canonicalize-chain (cdr (assoc elmt chains-alist))
				    chains-alist))
   ((integerp elmt)
    (mc-remailer-canonicalize-chain (mc-remailer-choose-chain elmt)
				    chains-alist))
   (t (mc-remailer-canonicalize-chain (eval elmt) chains-alist))))

(defun mc-remailer-canonicalize-chain (chain &optional chains-alist)
  ;; Canonicalize a remailer chain with respect to CHAINS-ALIST.
  ;; That is, use CHAINS-ALIST to resolve strings.
  ;; Here is where we implement the functionality described in
  ;; the documentation for the variable `mc-remailer-user-chains'.
  (if (null chains-alist)
      (setq chains-alist (mc-remailer-make-chains-alist)))
  (cond
   ((null chain) nil)
   ;; Handle case where chain is actually a string or a single
   ;; remailer.
   ((or (stringp chain) (mc-remailerp chain) (integerp chain))
    (mc-remailer-canonicalize-elmt chain chains-alist))
   (t
    (let ((first (elt chain 0))
	  (rest (cdr (append chain nil))))
      (append
       (mc-remailer-canonicalize-elmt first chains-alist)
       (mc-remailer-canonicalize-chain rest chains-alist))))))

;;}}}
;;{{{ Auxiliaries for mail header munging

(defsubst mc-nuke-field (field &optional bounds)
  ;; Delete all fields exactly matching regexp FIELD from header,
  ;; bounded by BOUNDS.  Default is entire visible region of buffer.
  (mc-get-fields field bounds t))

(defun mc-replace-field (field-name replacement header)
  (save-excursion
    (save-restriction
      (if (not (string-match "^[ \t]" replacement))
	  (setq replacement (concat " " replacement)))
      (if (not (string-match "\n$" replacement))
	  (setq replacement (concat replacement "\n")))
      (let ((case-fold-search t)
	    (field-regexp (regexp-quote field-name)))
	(narrow-to-region (car header) (cdr header))
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" field-regexp ":" mc-field-body-regexp)
	 nil t)
	(mc-nuke-field field-regexp header)
	(insert field-name ":" replacement)))))

(defun mc-find-main-header (&optional ignored)
  ;; Find the main header of the mail message; return as a pair of
  ;; markers (START . END).
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (forward-line -1)
    (cons (copy-marker (point-min)) (copy-marker (point)))))
		
(defun mc-find-colon-header (&optional insert)
  ;; Find the header with a "::" immediately after the
  ;; mail-header-separator.  Return region enclosing header.  Optional
  ;; arg INSERT means insert the header if it does not exist already.
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (if (or (and (looking-at "::\n") (forward-line 1))
	    (and insert
		 (progn
		   (insert-before-markers "::\n\n")
		   (forward-line -1))))
	(let ((start (point)))
	  (re-search-forward "^$" nil 'move)
	  (cons (copy-marker start) (copy-marker (point)))))))

(defun mc-find-hash-header (&optional insert)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (if (or (and (looking-at "##\n") (forward-line 1))
	    (and (looking-at "::\n")
		 (re-search-forward "^\n" nil 'move)
		 (looking-at "##\n")
		 (forward-line 1))
	    (and insert
		 (progn
		   (insert-before-markers "##\n\n")
		   (forward-line -1))))
	(let ((start (point)))
	  (re-search-forward "^$" nil 'move)
	  (cons (copy-marker start) (copy-marker (point)))))))


(defsubst mc-replace-main-field (field replacement)
  (mc-replace-field field replacement (mc-find-main-header t)))

(defsubst mc-replace-hash-field (field replacement)
  (mc-replace-field field replacement (mc-find-hash-header t)))

(defsubst mc-replace-colon-field (field replacement)
  (mc-replace-field field replacement (mc-find-colon-header t)))

(defun mc-recipient-is-remailerp ()
  (let ((to (mc-get-fields "To" (mc-find-main-header))))
    (and to
	 (string-match (regexp-quote mc-remailer-tag) (cdr (car to))))))

;;}}}
;;{{{ Pre-encryption and post-encryption hook defaults

(defun mc-generic-post-encrypt-function (remailer)
  (let ((main-header (mc-find-main-header))
	(colon-header (mc-find-colon-header t)))
    (mc-replace-field "Encrypted" "PGP" colon-header)
    (mc-replace-field
     "To"
     (concat (mc-remailer-address remailer) " " mc-remailer-tag)
     main-header)))

(defun mc-generic-pre-encrypt-function (remailer)
  (let ((addr (mc-remailer-address remailer))
	(props (mc-remailer-properties remailer))
	(main-header (mc-find-main-header))
	(colon-header (mc-find-colon-header t))
	to to-field preserved-regexp preserved)

    (setq preserved-regexp
	  (mc-disjunction-regexp mc-remailer-preserved-headers))
    (setq preserved (mc-get-fields preserved-regexp main-header t))
    (if preserved (goto-char (cdr (mc-find-hash-header t))))
    (mapcar (function
	     (lambda (c)
	       (insert (car c) ":"
		       (mc-eliminate-continuation-lines (cdr c)))))
	    preserved)

    (if (and (mc-find-hash-header) (not (member "hash" props)))
	(error "Remailer %s does not support hashmarks" addr))

    (if (mc-get-fields "Newsgroups" main-header)
	(cond ((not (member "post" props))
	       (error "Remailer %s does not support posting" addr))
	      ((not (member "hash" props))
	       (error "Remailer %s does not support hashmarks" addr))
	      (t (mc-rewrite-news-to-mail remailer)))
      (and (featurep 'mailalias)
	   (not (featurep 'mail-abbrevs))
	   mail-aliases
	   (expand-mail-aliases (car main-header) (cdr main-header)))
      (setq to (mc-strip-addresses
		(mapcar 'cdr (mc-get-fields "To" main-header))))
      (if (string-match "," to)
	  (error "Remailer %s does not support multiple recipients." addr))
      (setq to-field
	    (if (mc-get-fields "From" colon-header)
		"Send-To"
	      (cond
	       ((member "cpunk" props) "Anon-To")
	       (t (error "Remailer %s is not type-1" addr)))))
      (mc-replace-field to-field to colon-header)
      (mc-nuke-field "Reply-to" main-header))))
	
;;}}}
;;{{{ Misc. random

(defun mc-disjunction-regexp (regexps)
  ;; Take a list of regular expressions and return a single
  ;; regular expression which matches anything that any of the
  ;; original regexps match.
  (concat "\\("
	  (mapconcat 'identity regexps "\\)\\|\\(")
	  "\\)"))

; Quiet a warning message when the user hasn't set this.
(defvar gnus-user-from-line nil)

(defun mc-user-mail-address ()
  "Figure out the user's Email address as best we can."
  (mc-strip-address
   (cond ((and (boundp 'gnus-user-from-line)
	       (stringp gnus-user-from-line))
	  gnus-user-from-line)
	 ((stringp mail-default-reply-to) mail-default-reply-to)
	 ((boundp 'user-mail-address) user-mail-address)
	 (t (concat (user-login-name) "@" (system-name))))))

(defun mc-eliminate-continuation-lines (string)
  (while (string-match "\n[\t ]+" string)
    (setq string (replace-match " " t nil string)))
  string)

(defun mc-remailer-make-chains-alist ()
  (if (null mc-remailer-internal-chains)
      (mc-reread-levien-file))
  (append mc-remailer-internal-chains mc-remailer-user-chains))

(defun mc-remailer-insert-pseudonym ()
  "Insert pseudonym as a From field in the hash-mark header.

See the documentation for the variable `mc-remailer-pseudonyms' for
more information."
  (interactive)
  (let ((completion-ignore-case t)
	pseudonym)
    (setq pseudonym
	  (cond ((null mc-remailer-pseudonyms)
		 (read-from-minibuffer "Pseudonym: "))
		(t
		 (completing-read "Pseudonym: "
				  (mapcar 'list mc-remailer-pseudonyms)))))
    (if (not (string-match "\\S +@\\S +" pseudonym))
	(setq pseudonym (concat pseudonym " <x@x.x>")))
    (mc-replace-colon-field "From" pseudonym)))

;;}}}
;;{{{ Mixmaster support
(defvar mc-mixmaster-path nil
  "*Path to the Mixmaster binary.  If defined, Mixmaster chains will
be passed to this program for rewriting.")

(defvar mc-mixmaster-list-path nil
  "*Path to the Mixmaster type2.list file.")

(defun mc-demix (&rest chain)
  "Use arguments as a remailer-list and return a new list with the
\"mix\" property removed from all the elements."
  (mapcar (function (lambda (r) (mc-remailer-remove-property r "mix")))
	  (mc-remailer-canonicalize-chain chain)))

(defun mc-mixmaster-process (beg end recipients preserved mix-chain)
  ;; Run a region through Mixmaster.
  (let (ret)
    (if (not (markerp end))
	(setq end (copy-marker end)))
    (goto-char beg)
    (mapcar (function (lambda (x) (insert x ?\n))) recipients)
    (insert ?\n)
    (mapcar (function (lambda (x) (insert x))) preserved)
    (insert ?\n)
    (setq mix-chain (mapcar (function (lambda (x) (format "%d" x))) mix-chain))
    ;; Handle case of empty message
    (if (< end (point)) (setq end (point)))
    (setq ret
	  (apply 'call-process-region beg end mc-mixmaster-path t t nil
		 "-f" "-o" "stdout" "-l" mix-chain))
    (if (not (eq ret 0)) (error "Mixmaster barfed."))
    (goto-char beg)
    (re-search-forward "^::$")
    (delete-region beg (match-beginning 0))))

(defun mc-mixmaster-build-alist (&optional n)
  ;; Construct an alist mapping Mixmaster Email addresses to integers.
  ;; FIXME; this is terrible
  (let (buf)
    (save-excursion
      (unwind-protect
	  (progn
	    (setq n (or n 1))
	    (setq buf (find-file-noselect mc-mixmaster-list-path))
	    (set-buffer buf)
	    (if (re-search-forward "^[^ \t]+[ \t]+\\([^ \t]+\\)" nil t)
		(cons (cons (buffer-substring-no-properties
			     (match-beginning 1) (match-end 1))
			    n)
		    (mc-mixmaster-build-alist (+ n 1)))))
	(if buf (kill-buffer buf))))))

(defvar mc-mixmaster-alist nil)

(defsubst mc-mixmaster-alist ()
  (or mc-mixmaster-alist
      (setq mc-mixmaster-alist (mc-mixmaster-build-alist))))

(defun mc-mixmaster-translate-chain (chain)
  ;; Take a chain of Mixmaster remailers and convert it to the list
  ;; of integers which represents them.
  (if (or (null chain)
	  (not (member "mix" (mc-remailer-properties (car chain)))))
      nil
    (cons (cdr (assoc (mc-strip-address (mc-remailer-address (car chain)))
		      (mc-mixmaster-alist)))
	  (mc-mixmaster-translate-chain (cdr chain)))))

(defun mc-mixmaster-skip (chain)
  ;; Return the largest possible suffix of CHAIN whose first element
  ;; is not a Mixmaster.
  (cond ((null chain) nil)
	((not (member "mix" (mc-remailer-properties (car chain))))
	 chain)
	(t (mc-mixmaster-skip (cdr chain)))))

(defun mc-rewrite-for-mixmaster (chain &optional pause)
  ;; Rewrite the current mail buffer for a chain of Mixmasters.
  (let ((mix-chain (mc-mixmaster-translate-chain chain))
	(main-header (mc-find-main-header))
	(colon-header (mc-find-colon-header))
	(hash-header (mc-find-hash-header))
        (newsgroups (mc-get-fields "Newsgroups" nil t))
	recipients preserved newsgroups first last rest preserved-regexp)

    ;; Figure out FIRST and LAST. FIRST is the first Mixmaster in the
    ;; chain.  LAST is the last.
    (setq first (car chain)
	  rest chain)
    (while (and rest (member "mix" (mc-remailer-properties (car rest))))
      (setq last (car rest)
	    rest (cdr rest)))
    
    ;; If recipient is not a remailer, deal with hashmark and colon
    ;; headers and get rid of them.
    (if (mc-recipient-is-remailerp)
	nil
      (if hash-header
	  (progn
	    (setq preserved (mc-get-fields nil hash-header))
	    (goto-char (car hash-header))
	    (forward-line -1)
	    (delete-region (point) (+ (cdr hash-header) 1))))
      ;; Preserve pseduonym line...
      (if colon-header
	  (progn
	    (setq preserved
		  (append (mc-get-fields "From" colon-header) preserved))
	    (goto-char (car colon-header))
	    (forward-line -1)
	    (delete-region (point) (+ (cdr colon-header) 1)))))
    
    ;; Expand aliases and get recipients.
    (and (featurep 'mailalias)
	 (not (featurep 'mail-abbrevs))
	 mail-aliases
	 (expand-mail-aliases (car main-header) (cdr main-header)))
    (setq recipients
	  (mc-cleanup-recipient-headers
	   (mapconcat 'cdr (mc-get-fields "To" main-header t) ", ")))
    (if newsgroups
	(setq newsgroups 
	  (mc-cleanup-recipient-headers
	   (cdr 
	    (assoc "Newsgroups" newsgroups)))))
    ;; Mixmaster does not support posting...
;    (if newsgroups
    ;; Now they do! (1998)
    (if (and newsgroups
	     (not (member "post" (mc-remailer-properties last))))
	(error "Remailer %s does not support posting"
	       (mc-remailer-address last)))
    (setq
     recipients
     (append (mapcar
	      (function (lambda (c) (concat "Post: " c))) newsgroups)
	     recipients))

    (mapcar
     (function (lambda (c) (message c))) recipients)

    (setq
     preserved-regexp
     (mc-disjunction-regexp (cons "Subject" mc-remailer-preserved-headers)))

    (setq preserved
	  (append (mc-get-fields preserved-regexp main-header t) preserved))

    ;; Convert preserved header alist to simple list of strings
    (setq preserved
	  (mapcar
	   (function
	    (lambda (c)
	      (concat (car c) ":"
		      (mc-eliminate-continuation-lines (cdr c)))))
	   preserved))

    ;; Do the conversion
    (goto-char (cdr main-header))
    (forward-line 1)
    (mc-mixmaster-process (point) (point-max) recipients preserved
			  mix-chain)

    (mc-replace-field "To"
		      (concat
		       (mc-remailer-address first) " " mc-remailer-tag)
		      main-header)))

;;}}}
;;{{{ High level message rewriting

(defun mc-rewrite-news-to-mail (remailer)
  (let ((main-header (mc-find-main-header))
	newsgroups)
    (setq newsgroups (mc-get-fields "Newsgroups" main-header t))
    (mc-replace-colon-field "Post-To" (cdr (car newsgroups)))
    (mail-mode)))

(defun mc-rewrite-for-remailer (remailer &optional pause)
  ;; Rewrite the current mail buffer for a single remailer.  This
  ;; includes running the pre-encryption hooks, modifying the To:
  ;; field, encrypting with the remailer's public key, and running the
  ;; post-encryption hooks.
  (let ((addr (mc-remailer-address remailer))
	(main-header (mc-find-main-header)))
    ;; If recipient is already a remailer, make sure the "::" and "##"
    ;; headers get to it
    (if (mc-recipient-is-remailerp)
	(progn
	  (goto-char (cdr main-header))
	  (forward-line 1)
	  (insert "::\n\n")))

    (mapcar
     (function (lambda (hook) (funcall hook remailer)))
     (mc-remailer-pre-encrypt-hooks remailer))

    (if pause
	(let ((cursor-in-echo-area t))
	  (message "SPC to encrypt for %s : " addr)
	  (read-char-exclusive)))
    (setq main-header (mc-find-main-header))
    (goto-char (cdr main-header))
    (forward-line 1)
    (if (let ((mc-pgp-always-sign 'never)
	      (mc-encrypt-for-me nil))
	  (mc-encrypt-message (mc-remailer-userid remailer) nil (point)))
	(progn
	  (mapcar
	   (function (lambda (hook) (funcall hook remailer)))
	   (mc-remailer-post-encrypt-hooks remailer))
	  (mc-nuke-field "Comment")
	  (mc-nuke-field "From"))
      (error "Unable to encrypt message to %s"
	     (mc-remailer-userid remailer)))))

(defun mc-rewrite-for-chain (chain &optional pause)
  ;; Rewrite the current buffer for a chain of remailers.
  ;; CHAIN must be in canonical form.
  (let (rest)
    (if mc-mixmaster-path
	(setq rest (mc-mixmaster-skip chain))
      (setq rest chain))
    (if (null chain) nil
      (mc-rewrite-for-chain
       (if (eq rest chain) (cdr rest) rest) pause)
      (if (eq rest chain)
	  (mc-rewrite-for-remailer (car chain) pause)
	(mc-rewrite-for-mixmaster chain pause)))))

(defun mc-unparse-chain (chain)
  ;; Unparse CHAIN into a string suitable for printing.
  (if (null chain)
      nil
    (concat (mc-remailer-address (car chain)) "\n"
	    (mc-unparse-chain (cdr chain)))))

(defun mc-disallow-field (field &optional header)
  (let ((case-fold-search t))
    (if (null header)
	(setq header (mc-find-main-header)))
    (goto-char (car header))
    (if (re-search-forward (concat "^" (regexp-quote field) ":")
			  (cdr header) t)
	
	(progn
	  (goto-char (match-beginning 0))
	  (error "Cannot use a %s field." field)))))

(defun mc-remailer-encrypt-for-chain (&optional pause)
  "Encrypt message for a remailer chain, prompting for chain to use.

With \\[universal-argument], pause before each encryption."
  (interactive "P")
  (let ((chains (mc-remailer-make-chains-alist))
	(buffer (get-buffer-create mc-buffer-name))
	chain-name chain)
    (mc-disallow-field "CC")
    (mc-disallow-field "FCC")
    (mc-disallow-field "BCC")
    (setq chain-name
	  (completing-read
	   "Choose a remailer or chain: " chains nil 'strict-match))
    (setq chain
	  (mc-remailer-canonicalize-chain
	   (cdr (assoc chain-name chains))
	   chains))
    (mc-rewrite-for-chain chain pause)
    (if chain
	(save-excursion
	  (set-buffer buffer)
	  (erase-buffer)
	  (insert "Rewritten for chain `" chain-name "':\n\n"
		  (mc-unparse-chain chain))
	  (message "Done.  See %s buffer for details." mc-buffer-name)))))

;;}}}
;;{{{ Response block generation

(defun mc-remailer-insert-response-block (&optional arg)
  "Insert response block at point, prompting for chain to use.

With \\[universal-argument], enter a recursive edit of the innermost
layer of the block before encrypting it."
  (interactive "p")
  (let (buf main-header to addr block lines)
    (save-excursion
      (setq buf
	    (mc-remailer-make-response-block (if (> arg 1) t)))
      (set-buffer buf)
      (setq main-header (mc-find-main-header))
      (setq to (cdr (car (mc-get-fields "To" main-header))))
      (setq addr (concat "<" (mc-strip-address to) ">"))
      (goto-char (cdr main-header))
      (forward-line 1)
      (setq block (buffer-substring-no-properties
		   (point) (point-max))
	    lines (count-lines (point) (point-max)))
      (kill-buffer buf))
    (let ((opoint (point)))
      (insert (funcall mc-remailer-user-response-block
		       addr lines block))
      (goto-char opoint))
    (mc-nuke-field "Reply-to" (mc-find-main-header))
    (mc-replace-hash-field "Reply-to" addr)))

(defun mc-remailer-make-response-block (&optional recurse)
  ;; Return a buffer which contains a response block
  ;; for the user, and a To: header for the remailer to use.
  (let ((buf (generate-new-buffer " *Remailer Response Block*"))
	(original-buf (current-buffer))
	(mc-mixmaster-path nil)
	all-headers included-regexp included)
    (setq all-headers (mc-find-main-header))
    (setcdr all-headers
	    (max
	     (cdr all-headers)
	     (or (cdr-safe (mc-find-colon-header)) 0)
	     (or (cdr-safe (mc-find-hash-header)) 0)))
    (save-excursion
      (setq
       included-regexp
       (mc-disjunction-regexp mc-response-block-included-headers))
      (setq included (mc-get-fields included-regexp all-headers))
      (set-buffer buf)
      (insert "To: " (mc-user-mail-address) "\n" mail-header-separator "\n")
      (insert ";; Response block created " (current-time-string) "\n")
      (mapcar (function (lambda (c) (insert "; " (car c) ":" (cdr c))))
	      included)
      (if recurse
	  (progn
	    (switch-to-buffer buf)
	    (message "Editing response block ; %s when done."
		     (substitute-command-keys "\\[exit-recursive-edit]"))
	    (recursive-edit)))
      (set-buffer buf)
      (mc-remailer-encrypt-for-chain)
      (switch-to-buffer original-buf))
    buf))

;;}}}

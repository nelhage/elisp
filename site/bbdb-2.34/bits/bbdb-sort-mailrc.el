>>>>> Ronan Waide writes:

>> * birthdays/anniversaries

RW> This /is/ venturing into calendar land. Still, go to yer bbdb buffer
RW> and create a field with C-o. Again, I prefer not to add baggage to the
RW> file format unless it's absolutely necessary. Also, you should be able
RW> to attach bbdb to calendar.el using the bbdb record-dinking hooks so
RW> that it auto-fills your calendar with goop for you. And maybe get
RW> working on calendar-pilot.el...

Well, this isn't really release-ready -- but since someone asks, it
could be a good starting point for someone.  Feel free to
redistribute, or chop up and use the useful bits.

Bng


;;; BBDB-BNG
;;;  Various functions I have added to enhance the big brother database.
;;; Boris Goldowsky, <boris@cs.rochester.edu>
;;;   $Revision: 1.1 $  $Date: 2001/01/24 21:19:08 $
;;;  
;;; This file allows you to do the following things:
;;;  * Sort by firstname or company rather than last name.
;;;  * Mark people's birthdays in emacs's calendar and diary displays.
;;;  * Maintains a file of mail aliases, for use by other mailers,
;;;    automatically updated when the information changes in your database.
;;;  * Make sure that everyone has their username defined as an alias
;;;    for their complete net addresses.
;;;
;;; INSTALLATION:
;;;  Put this file in emacs's load-path, and make sure it gets loaded whenever
;;;  you load BBDB.
;;;  * To use alternate sorting, evaluate (bbdb-sort-by ...) whenever you load
;;;    bbdb.  YOU MUST EITHER ALWAYS DO THIS, OR NEVER DO IT.  When you switch
;;;    over, evaluate (bbdb-resort-database).
;;;  * To make a file of mail-aliases, set bbdb-mail-alias-file to a filename,
;;;    and source that file from your .mailrc.
;;;  * Username-aliases are enabled by default.  Set
;;;   `bbdb-auto-username-alias' to nil if you don't want them.
;;;    You can also use the function `bbdb-add-user-name-as-alias' to
;;;    add such aliases manually.
;;;  * The bbdb/calendar stuff is under development, and may not work.
;;;
;;; EXAMPLE:
;;;  The following code could go in your .emacs:
;;;  (add-hook 'bbdb-load-hook 
;;;            (function (lambda ()
;;;                        (setq bbdb-mail-alias-file 
;;;                                (expand-file-name "~/.mail_aliases")
;;;                        (require 'bbdb-bng)
;;;                        (bbdb-sort-by 'firstname))))

;;; USE:
;;;  If installed as above, these functions operate automatically.

;;; DEPENDENCIES:
;;;  BBDB, of course.
;;;  calendar.el and diary-lib.el are built into recent emacs versions.
;;;  dates.el is available from me.

(provide 'bbdb-bng)

;;;
;;; New birthday stuff.
;;;

(require 'calendar)
(require 'dates)

(if (not (featurep 'diary))  ; the library of many names.
    (or (load "diary-lib" t)
	(load "diary")))

(defvar bbdb/calendar-marker 
  (if (not window-system)
      "^"
    (require 'faces)
    'bold-italic)
  "*How to mark birthdays in calendar.
Can be either a single-character string or a face.")

(add-hook 'list-diary-entries-hook 'bbdb/calendar-list-entries)
(add-hook 'mark-diary-entries-hook 'bbdb/calendar-mark-entries)

(defun bbdb/calendar-mark-entries ()
  (save-excursion
    (set-buffer calendar-buffer)
    (let ((month displayed-month)
	  (year displayed-year))
      (bbdb/calendar-mark-month month year)
      (increment-calendar-month month year -1)
      (bbdb/calendar-mark-month month year)
      (increment-calendar-month month year 2)
      (bbdb/calendar-mark-month month year))))

(defun bbdb/calendar-mark-month (month year)
  (message "Marking birthdays..."
  (let ((days (aref (bbdb/calendar-birthdays) month)))
    (while days
      (mark-visible-calendar-date (list month (car (car days)) year)
				  bbdb/calendar-marker)
      (setq days (cdr days))))
  (message nil)))

(defun bbdb/calendar-list-entries ()
  (message "Listing birthdays..."
  (let* ((bdays (bbdb/calendar-birthdays))
	 (start-date (calendar-absolute-from-gregorian original-date))
	 (end-date (+ number start-date)))
    (calendar-for-loop abs-date from start-date to end-date do
      (let* ((date (calendar-gregorian-from-absolute abs-date))
	     (entries (cdr (assoc (extract-calendar-day date)
				  (aref bdays 
					(extract-calendar-month date))))))
	(while entries
	  (add-to-diary-list date (car entries))
	  (setq entries (cdr entries))))))
  (message nil)))

(defvar bbdb/calendar-birthdays nil
  "Used by function of the same name, which see.")

(defun bbdb/calendar-birthdays ()
  "Returns a vector containing the birthdays in your BBDB.
This is a vector with one element per month:
 [birthdays                               ; identifier in spot 0
  ((4 \"Isaac Newton's birthday\"))       ; Newton's birthday is Jan 4.
  ((11 \"Thomas Edison's birthday\")      ; Edison's is Feb 11.
   (15 \"Galileo's birthday\" \"Susan B. Anthony's birthday\")) ; Both Feb 15.
  ...march through dec...
 ]"
  (or bbdb/calendar-birthdays
      (setq bbdb/calendar-birthdays
	    (let ((cal (make-vector 13 nil))
		  (recs (bbdb-records))
		  birthday-string)
	      (aset cal 0 'birthdays)
	      (while recs
		(if (setq birthday-string
			  (bbdb-record-getprop (car recs) 'birthday))
		    (let ((events (bbdb-split birthday-string ","))
			  (name (bbdb-record-name (car recs))))
		      (while events
			(let ((bday (date-parse (car events))))
			  (if (null bday)
			      (message "Unparsable birthday: %s" (car events))
			    (let* ((date-end (parse-string-end))
				   (eventname (if (eq t date-end) 
						  "birthday"
						(substring (car events)
							   date-end)))
				   (event (concat name "'s "
						  (if (equal "" eventname) 
						      "birthday"
						    eventname)))
				   (month (extract-calendar-month bday))
				   (day (extract-calendar-day bday))
				   (monthlist (aref cal month))
				   (daylist (assoc day monthlist)))
			      (if daylist
				  (setcdr daylist (cons event (cdr daylist)))
				(aset cal month (cons (list day event)
						      monthlist))))))
			(setq events (cdr events)))))
		(setq recs (cdr recs)))
	      cal))))

;;;
;;; Mail alias code
;;;

(defvar bbdb-mail-alias-file nil
  "*File to save mail-aliases into.
Aliases are also kept in the database proper; this is just for the convenience
of other programs that are interested in mail aliases.  For example, you can
use your bbdb mail aliases with ucb mail by including the line
source ~/.mail_aliases
in your .mailrc file.
Set this to nil to avoid storing mail aliases in a file.")

(defvar bbdb-auto-username-alias t
  "*If t, always have a person's username as a mail-alias for them.")

(if bbdb-mail-alias-file
    (add-hook 'bbdb-after-change-hook (function bbdb-check-mail-alias)))

(defun bbdb-add-user-name-as-alias ()
  (interactive)
  (let ((bbdb-auto-username-alias t)
	(this(bbdb-current-record)))
    (bbdb-check-mail-alias this)
    (bbdb-redisplay-one-record this)))

(defun bbdb-record-username (record)
  "Return just the username part of RECORD's first net address,
if it looks like a well-formed internet address; nil otherwise."
  (let ((addr (car (bbdb-record-net record))))
    (if (and addr (string-match "^[a-zA-z0-9]+@" addr))
        (substring addr 0 (1- (match-end 0))))))

(defun bbdb-record-mail-aliases (record)
  (let ((all (bbdb-record-getprop record bbdb-define-all-aliases-field)))
    (if all (bbdb-split all ","))))

(defun bbdb-check-mail-alias (record)
  "Makes sure the person's username is defined as a mail abbrev
for them, and makes sure all their mail abbreves are ready for use."
  (let ((username (bbdb-record-username record))
	(current (bbdb-record-getprop record bbdb-define-all-aliases-field)))
    (if (and current (string-match "\\(,\\)? *\n" current))
	(setq current (replace-match ", " nil nil current)))
    (if (and bbdb-auto-username-alias 
	     username
	     (not (and (boundp 'mail-abbrevs)
		       (intern-soft username mail-abbrevs)))
	     (not (member username (bbdb-record-mail-aliases record))))
	(setq current
	      (if current (concat current ", " username)
		username)))
    (if current
	(bbdb-record-putprop record bbdb-define-all-aliases-field current))

    ;; And make sure aliases are all defined (if any are)
    (if (boundp 'mail-abbrevs)
	(mapcar (function 
		 (lambda (alias)
		   (if (not (intern-soft alias mail-abbrevs))
		       (my-define-mail-abbrev 
			alias (bbdb-dwim-net-address record)))))
		(bbdb-record-mail-aliases record)))))

(defun my-define-mail-abbrev (abbrev address)
  "Defines abbrev, and marks bbdb-mail-alias-file as modified." 
  (define-mail-abbrev abbrev address)
  (save-excursion
    (set-buffer (find-file-noselect bbdb-mail-alias-file))
    (setq buffer-read-only t)
    (set-buffer-modified-p t)
    (make-variable-buffer-local 'local-write-file-hooks)
    (if (not (memq 'bbdb-mail-alias-file-write-hook 
		   local-write-file-hooks))
	(setq local-write-file-hooks '(bbdb-mail-alias-file-write-hook)))))

(defun bbdb-insert-mail-aliases ()
  (let ((begin (point)))
    (if (not (boundp 'mail-abbrevs))
        (bbdb-define-all-aliases))
    (insert-abbrev-table-description 'mail-abbrevs nil)
    (goto-char begin)
    (let ((abbrevs (nth 1 (nth 2 (read (current-buffer))))))
      (setq abbrevs (sort abbrevs (function 
                                   (lambda (x y)
                                     (string-lessp (car x) (car y))))))
      (delete-region begin (point))
      (mapcar (function
               (lambda (abbrev)
                 (let ((alias (car abbrev))
                       (addr  (mapconcat (function simplify-address) 
                                         (bbdb-split (nth 1 abbrev) ",") " ")))
                   (if (not (string-equal alias addr))
                       (insert (format "alias %s\t%s\n" alias addr))))))
              abbrevs))))

(defun simplify-address (addr)
  (let ((addr (car (cdr (mail-extract-address-components addr)))))
    (if (string-match (concat "@" (system-name) "$") addr)
        (substring addr 0 (match-beginning 0))
      addr)))

(defun bbdb-mail-alias-file-write-hook ()
  "Regenerate mail-aliases if necc.  
Call from local-write-file-hooks."
  (let ((buffer-read-only nil))
    (message "Writing aliases...")
    (delete-region (point-min) (point-max))
    (bbdb-insert-mail-aliases)
    (message "Writing aliases...done")
    nil))

;;;
;;; sorting frobnification.
;;;

(defun bbdb-sort-by (field)
  "Tell BBDB which field is the primary sort key.
Currently FIELD must be one of 'firstname 'lastname or 'company.
The first time you use this, use bbdb-resort-database immediately
afterwards.  Then put \(bbdb-sort-by 'firstname), or whichever field is
your choice, on your bbdb-after-load-db-hook."
  (cond ((eq field 'lastname)
         (defun bbdb-record-sortkey (record)
           (or (bbdb-cache-sortkey (bbdb-record-cache record))
               (bbdb-cache-set-sortkey
                (bbdb-record-cache record)
                (downcase
                 (concat (bbdb-record-lastname record)
                         (bbdb-record-firstname record)
                         (bbdb-record-company record)))))))
        ((eq field 'firstname)
         (defun bbdb-record-sortkey (record)
           (or (bbdb-cache-sortkey (bbdb-record-cache record))
               (bbdb-cache-set-sortkey
                (bbdb-record-cache record)
                (downcase
                 (concat (bbdb-record-firstname record)
                         (bbdb-record-lastname record)
                         (bbdb-record-company record)))))))
        ((eq field 'company)
         (defun bbdb-record-sortkey (record)
           (or (bbdb-cache-sortkey (bbdb-record-cache record))
               (bbdb-cache-set-sortkey
                (bbdb-record-cache record)
                (downcase
                 (concat (bbdb-record-company record)
                         (bbdb-record-lastname record)
                         (bbdb-record-firstname record)))))))
        (t (error "Can only sort by firstname lastname or company!"))))

;;; Local Variables:
;;; eval:(put 'calendar-for-loop 'lisp-indent-hook 6)
;;; End:

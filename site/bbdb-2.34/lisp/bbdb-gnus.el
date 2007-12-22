;;; -*- Mode:Emacs-Lisp -*-

;;; This file is part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@netscape.com>.
;;; Interface to GNUS version 3.12 or greater.  See bbdb.texinfo.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;
;; $Id: bbdb-gnus.el,v 1.88 2001/10/14 19:55:09 waider Exp $
;;

(require 'bbdb)
(require 'bbdb-snarf)
(require 'gnus)

(eval-and-compile
  (require 'bbdb-com)
  (require 'rfc822))

;; Cater for older emacs (19.34) with default Gnus installation.
(eval-and-compile
  (condition-case error
      (progn
        (require 'gnus-win)
        (require 'gnus-sum)
        (require 'gnus-art))
    (error nil)))

;;; Compiler hushing
;;; Some of these are probably obsolete variables for older versions
;;; of gnus that should be taken out back and shot.
(eval-when-compile
   (defvar gnus-optional-headers)
   (defvar gnus-Subject-mode-map)
   (defvar gnus-Subject-buffer))

(defun bbdb/gnus-get-message-id ()
  "Return the message-id of the current message."
  (save-excursion
    (set-buffer (get-buffer gnus-article-buffer))
    (set-buffer gnus-original-article-buffer)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward "^Message-ID:\\s-*\\(<.+>\\)" (point-max) t)
          (match-string 1)))))

(defcustom bbdb/gnus-update-records-mode 'annotating
;  '(if (gnus-new-flag msg) 'annotating 'searching)
  "Controls how `bbdb/gnus-update-records' processes email addresses.
Set this to an expression which evaluates either to 'searching or
'annotating.  When set to 'annotating email addresses will be fed to
`bbdb-annotate-message-sender' in order to update existing records or create
new ones.  A value of 'searching will search just for existing records having
the right net.

The default is to annotate only new messages."
  :group 'bbdb-mua-specific-gnus
  :type '(choice (const :tag "annotating all messages"
                        annotating)
                 (const :tag "annotating no messages"
                        searching)
                 (const :tag "annotating only new messages"
                        (if (equal ""
                                   (gnus-summary-article-mark
                                    (gnus-summary-article-number)))
                            'annotating 'searching))
                 (sexp   :tag "user defined")))


;;;###autoload
(defun bbdb/gnus-update-record (&optional offer-to-create)
  "Return the record corresponding to the current GNUS message, creating
or modifying it as necessary.  A record will be created if
bbdb/news-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (let* ((bbdb-get-only-first-address-p t)
         (records (bbdb/gnus-update-records offer-to-create)))
    (if records (car records) nil)))


;;;###autoload
(defun bbdb/gnus-update-records (&optional offer-to-create)
  "Return the records corresponding to the current GNUS message, creating
or modifying it as necessary.  A record will be created if
bbdb/news-auto-create-p is non-nil or if OFFER-TO-CREATE is true
and the user confirms the creation.

The variable `bbdb/gnus-update-records-mode' controls what actions
are performed and it might override `bbdb-update-records-mode'.

When hitting C-g once you will not be asked anymore for new people listed
in this message, but it will search only for existing records.  When hitting
C-g again it will stop scanning."
  (let ((bbdb-update-records-mode (or bbdb/gnus-update-records-mode
                                      bbdb-update-records-mode))
        (bbdb/gnus-offer-to-create offer-to-create)
        ;; here we may distiguish between different type of messages
        ;; for those that have no message id we have to find something
        ;; else as message key.
        (msg-id (bbdb/gnus-get-message-id))
        records cache)
    (save-excursion
      (set-buffer (get-buffer gnus-article-buffer))
      (if (and msg-id (not bbdb/gnus-offer-to-create))
          (setq cache (bbdb-message-cache-lookup msg-id)))

      (if cache
          (setq records (if bbdb-get-only-first-address-p
                            (list (car cache))
                          cache))

        (let ((bbdb-update-records-mode (or bbdb/gnus-update-records-mode
                                            bbdb-update-records-mode)))
          (setq records (bbdb-update-records
                         (bbdb-get-addresses
                          bbdb-get-only-first-address-p
                          (or (if (boundp 'gnus-ignored-from-addresses)
                                  gnus-ignored-from-addresses)
                              bbdb-user-mail-names)
                          'mail-fetch-field)
                         bbdb/news-auto-create-p
                         offer-to-create)))
        (if (and bbdb-message-caching-enabled msg-id)
            (bbdb-encache-message msg-id records))))
    records))

;;;###autoload
(defun bbdb/gnus-annotate-sender (string &optional replace)
  "Add a line to the end of the Notes field of the BBDB record
corresponding to the sender of this message.  If REPLACE is non-nil,
replace the existing notes entry (if any)."
  (interactive (list (if bbdb-readonly-p
             (error "The Insidious Big Brother Database is read-only.")
             (read-string "Comments: "))))
  (gnus-summary-select-article)
  (bbdb-annotate-notes (bbdb/gnus-update-record t) string 'notes replace))

(defun bbdb/gnus-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((record (or (bbdb/gnus-update-record t) (error "unperson"))))
    (bbdb-display-records (list record))
    (if arg
        (bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

;;;###autoload
(defun bbdb/gnus-show-records (&optional address-class)
  "Display the contents of the BBDB for all addresses of this message.
This buffer will be in `bbdb-mode', with associated keybindings."
  (interactive)
  (gnus-summary-select-article)
  (let ((bbdb-get-addresses-headers
         (if address-class
             (list (assoc address-class bbdb-get-addresses-headers))
           bbdb-get-addresses-headers))
        (bbdb/gnus-update-records-mode 'annotating)
        (bbdb-message-cache nil)
        (bbdb-user-mail-names nil)
        (gnus-ignored-from-addresses nil)
        records)
    (setq records (bbdb/gnus-update-records t))
    (if records
        (bbdb-display-records records)
      (bbdb-undisplay-records))
    records))

;;;###autoload
(defun bbdb/gnus-show-all-recipients ()
  "Show all recipients of this message. Counterpart to `bbdb/vm-show-sender'."
  (interactive)
  (bbdb/gnus-show-records 'recipients))

(defun bbdb/gnus-show-sender (&optional show-recipients)
  "Display the contents of the BBDB for the senders of this message.
With a prefix argument show the recipients instead,
with two prefix arguments show all records.
This buffer will be in `bbdb-mode', with associated keybindings."
  (interactive "p")
  (cond ((= 4 show-recipients)
         (bbdb/gnus-show-all-recipients))
        ((= 16 show-recipients)
         (bbdb/gnus-show-records))
        (t
         (if (null (bbdb/gnus-show-records 'authors))
             (bbdb/gnus-show-all-recipients)))))

(defun bbdb/gnus-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the GNUS windows,
displaying the record corresponding to the sender of the current message."
  (let ((bbdb-gag-messages t)
        (records (bbdb/gnus-update-records offer-to-create))
        (bbdb-electric-p nil))

    (when bbdb-use-pop-up
      (let ((b (current-buffer)))
        ;; display the bbdb buffer iff there is a record for this article.
        (if records
            (bbdb-pop-up-bbdb-buffer
             (lambda (w)
               (let ((b (current-buffer)))
                 (set-buffer (window-buffer w))
                 (prog1 (or (eq major-mode 'gnus-Article-mode)
                            (eq major-mode 'gnus-article-mode))
                   (set-buffer b)))))
          (or bbdb-inside-electric-display
              (not (get-buffer-window bbdb-buffer-name))
              (let (w)
                (delete-other-windows)
                (if (assq 'article gnus-buffer-configuration)
                    (gnus-configure-windows 'article)
                  (gnus-configure-windows 'SelectArticle))
                (if (setq w (get-buffer-window
                             (if (boundp 'gnus-summary-buffer)
                                 gnus-summary-buffer
                               gnus-Subject-buffer)))
                    (select-window w)))))
        (set-buffer b))
      (if records (bbdb-display-records records bbdb-pop-up-display-layout)))
    records))

;;
;; Announcing BBDB entries in the summary buffer
;;

(defcustom bbdb/gnus-lines-and-from-length 18
  "*The number of characters used to display From: info in Gnus, if you have
set gnus-optional-headers to 'bbdb/gnus-lines-and-from."
  :group 'bbdb-mua-specific-gnus
  :type 'integer)

(defcustom bbdb/gnus-summary-mark-known-posters t
  "*If t, mark messages created by people with records in the BBDB.
In GNUS, this marking will take place in the subject list (assuming
`gnus-optional-headers' contains `bbdb/gnus-lines-and-from').  In Gnus, the
marking will take place in the Summary buffer if the format code defined by
`bbdb/gnus-summary-user-format-letter' is used in `gnus-summary-line-format'.
This variable has no effect on the marking controlled by
`bbdb/gnus-summary-in-bbdb-format-letter'."
  :group 'bbdb-mua-specific-gnus
  :type '(choice (const :tag "Mark known posters" t)
         (const :tag "Do not mark known posters" nil)))
(defvaralias 'bbdb/gnus-mark-known-posters
  'bbdb/gnus-summary-mark-known-posters)

(defcustom bbdb/gnus-summary-known-poster-mark "+"
  "This is the default character to prefix author names with if
bbdb/gnus-summary-mark-known-posters is t.  If the poster's record has
an entry in the field named by bbdb-message-marker-field, then that will
be used instead."
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom bbdb/gnus-summary-show-bbdb-names t
  "*If both this variable and `bbdb/gnus-summary-prefer-real-names' are true,
then for messages from authors who are in your database, the name
displayed will be the primary name in the database, rather than the
one in the From line of the message.  This doesn't affect the names of
people who aren't in the database, of course.  (`gnus-optional-headers'
must be `bbdb/gnus-lines-and-from' for GNUS users.)"
  :group 'bbdb-mua-specific-gnus
  :type 'boolean)
(defvaralias 'bbdb/gnus-header-show-bbdb-names
  'bbdb/gnus-summary-show-bbdb-names)

(defcustom bbdb/gnus-summary-prefer-bbdb-data t
  "If t, then for posters who are in our BBDB, replace the information
provided in the From header with data from the BBDB."
  :group 'bbdb-mua-specific-gnus
  :type 'boolean)

(defcustom bbdb/gnus-summary-prefer-real-names t
  "If t, then display the poster's name from the BBDB if we have one,
otherwise display his/her primary net address if we have one.  If it
is set to the symbol bbdb, then real names will be used from the BBDB
if present, otherwise the net address in the post will be used.  If
bbdb/gnus-summary-prefer-bbdb-data is nil, then this has no effect.
See `bbdb/gnus-lines-and-from' for GNUS users, or
`bbdb/gnus-summary-user-format-letter' for Gnus users."
  :group 'bbdb-mua-specific-gnus
  :type '(choice (const :tag "Prefer real names" t)
         (const :tag "Prefer network addresses" nil)))
(defvaralias 'bbdb/gnus-header-prefer-real-names
  'bbdb/gnus-summary-prefer-real-names)

(defcustom bbdb/gnus-summary-user-format-letter "B"
  "This is the gnus-user-format-function- that will be used to insert
the information from the BBDB in the summary buffer (using
`bbdb/gnus-summary-get-author').  This format code is meant to replace
codes that insert sender names or addresses (like %A or %n). Unless
you've already got other code using user format B, you might as well
stick with the default.  Additionally, if the value of this variable
is nil, no format function will be installed for
`bbdb/gnus-summary-get-author'.  See also
`bbdb/gnus-summary-in-bbdb-format-letter', which installs a format
code for `bbdb/gnus-summary-author-in-bbdb'"
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom bbdb/gnus-summary-in-bbdb-format-letter "b"
  "This is the gnus-user-format-function- that will be used to insert
`bbdb/gnus-summary-known-poster-mark' (using
`bbdb/gnus-summary-author-in-bbdb') if the poster is in the BBDB, and
\" \" if not.  If the value of this variable is nil, no format code
will be installed for `bbdb/gnus-summary-author-in-bbdb'.  See also
`bbdb/gnus-summary-user-format-letter', which installs a format code
for `bbdb/gnus-summary-get-author'."
  :group 'bbdb-mua-specific-gnus
  :type 'character)

(defcustom bbdb-message-marker-field 'mark-char
  "*The field whose value will be used to mark messages by this user in Gnus."
  :group 'bbdb-mua-specific-gnus
  :type 'symbol)

(defun bbdb/gnus-summary-get-author (header)
  "Given a Gnus message header, returns the appropriate piece of
information to identify the author in a Gnus summary line, depending on
the settings of the various configuration variables.  See the
documentation for the following variables for more details:
  `bbdb/gnus-summary-mark-known-posters'
  `bbdb/gnus-summary-known-poster-mark'
  `bbdb/gnus-summary-prefer-bbdb-data'
  `bbdb/gnus-summary-prefer-real-names'
This function is meant to be used with the user function defined in
  `bbdb/gnus-summary-user-format-letter'"
  (let* ((from (mail-header-from header))
     (data (and bbdb/gnus-summary-show-bbdb-names
            (condition-case ()
            (mail-extract-address-components from)
              (error nil))))
     (name (car data))
     (net (car (cdr data)))
     (record (and data
              (bbdb-search-simple name
               (if (and net bbdb-canonicalize-net-hook)
               (bbdb-canonicalize-address net)
             net)))))
    (if (and record name (member (downcase name) (bbdb-record-net record)))
    ;; bogon!
    (setq record nil))
    (setq name
      (or (and bbdb/gnus-summary-prefer-bbdb-data
           (or (and bbdb/gnus-summary-prefer-real-names
                (and record (bbdb-record-name record)))
               (and record (bbdb-record-net record)
                (nth 0 (bbdb-record-net record)))))
          (and bbdb/gnus-summary-prefer-real-names
           (or (and (equal bbdb/gnus-summary-prefer-real-names 'bbdb)
                net)
               name))
          net from "**UNKNOWN**"))
    (format "%s%s"
        (or (and record bbdb/gnus-summary-mark-known-posters
             (or (bbdb-record-getprop
              record bbdb-message-marker-field)
             bbdb/gnus-summary-known-poster-mark))
        " ")
        name)))

;; DEBUG: (bbdb/gnus-summary-author-in-bbdb "From: simmonmt@acm.org")
(defun bbdb/gnus-summary-author-in-bbdb (header)
  "Given a Gnus message header, returns a mark if the poster is in the BBDB, \" \" otherwise.  The mark itself is the value of the field indicated by `bbdb-message-marker-field' (`mark-char' by default) if the indicated field is in the poster's record, and `bbdb/gnus-summary-known-poster-mark' otherwise."
  (let* ((from (mail-header-from header))
         (data (condition-case ()
                   (mail-extract-address-components from)
                 (error nil)))
         (name (car data))
         (net (cadr data))
         record)
    (if (and data
             (setq record
                   (bbdb-search-simple
                    name (if (and net bbdb-canonicalize-net-hook)
                             (bbdb-canonicalize-address net)
                           net))))
        (or (bbdb-record-getprop
             record bbdb-message-marker-field)
            bbdb/gnus-summary-known-poster-mark) " ")))

;;
;; Gnus-specific snarfing (see also bbdb-snarf.el)
;;

;;;###autoload
(defun bbdb/gnus-snarf-signature ()
  "Snarf signature from the corresponding *Article* buffer."
  (interactive)
  (save-excursion
    (or gnus-article-buffer (error "Not in Gnus!"))
    (set-buffer gnus-article-buffer)
    (save-restriction
      (or (gnus-article-narrow-to-signature) (error "No signature!"))
      (bbdb-snarf-region (point-min) (point-max)))))

;;
;; Scoring
;;

(defcustom bbdb/gnus-score-field 'gnus-score
  "This variable contains the name of the BBDB field which should be
checked for a score to add to the net addresses in the same record."
  :group 'bbdb-mua-specific-gnus-scoring
  :type 'symbol)

(defcustom bbdb/gnus-score-default nil
  "If this is set, then every net address in the BBDB that does not have
an associated score field will be assigned this score.  A value of nil
implies a default score of zero."
  :group 'bbdb-mua-specific-gnus-scoring
  :type '(choice (const :tag "Do not assign default score")
         (integer :tag "Assign this default score" 0)))

(defvar bbdb/gnus-score-default-internal nil
  "Internal variable for detecting changes to
`bbdb/gnus-score-default'.  You should not set this variable directly -
set `bbdb/gnus-score-default' instead.")

(defvar bbdb/gnus-score-alist nil
  "The text version of the scoring structure returned by
bbdb/gnus-score.  This is built automatically from the BBDB.")

(defvar bbdb/gnus-score-rebuild-alist t
  "Set to t to rebuild bbdb/gnus-score-alist on the next call to
bbdb/gnus-score.  This will be set automatically if you change a BBDB
record which contains a gnus-score field.")

(defun bbdb/gnus-score-invalidate-alist (rec)
  "This function is called through `bbdb-after-change-hook',
and sets `bbdb/gnus-score-rebuild-alist' to t if the changed
record contains a gnus-score field."
  (if (bbdb-record-getprop rec bbdb/gnus-score-field)
      (setq bbdb/gnus-score-rebuild-alist t)))

;;;###autoload
(defun bbdb/gnus-score (group)
  "This returns a score alist for GNUS.  A score pair will be made for
every member of the net field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile."
  (list (list
   (condition-case nil
       (read (bbdb/gnus-score-as-text group))
     (error (setq bbdb/gnus-score-rebuild-alist t)
        (message "Problem building BBDB score table.")
        (ding) (sit-for 2)
        nil)))))

(defun bbdb/gnus-score-as-text (group)
  "Returns a SCORE file format string built from the BBDB."
  (cond ((or (cond ((/= (or bbdb/gnus-score-default 0)
            (or bbdb/gnus-score-default-internal 0))
            (setq bbdb/gnus-score-default-internal
              bbdb/gnus-score-default)
            t))
        (not bbdb/gnus-score-alist)
        bbdb/gnus-score-rebuild-alist)
    (setq bbdb/gnus-score-rebuild-alist nil)
    (setq bbdb/gnus-score-alist
      (concat "((touched nil) (\"from\"\n"
          (mapconcat
           (lambda (rec)
             (let ((score (or (bbdb-record-getprop rec
                               bbdb/gnus-score-field)
                      bbdb/gnus-score-default))
               (net (bbdb-record-net rec)))
               (if (not (and score net)) nil
             (mapconcat
              (lambda (addr)
                (format "(\"%s\" %s)\n" addr score))
              net ""))))
           (bbdb-records) "")
          "))"))))
  bbdb/gnus-score-alist)

;;; Posted originally by Colin Rafferty on the <bbdb-info> mailing list
(defun bbdb/gnus-summary-show-all-recipients (not-elided)
  "Display BBDB records for all recipients of the message."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((bbdb-display-layout (or (not not-elided)
                                 bbdb-pop-up-display-layout
                                 bbdb-display-layout))
        (bbdb-auto-notes-alist nil))
    (bbdb/gnus-pop-up-bbdb-buffer nil)
    (set-buffer gnus-article-buffer)
    (bbdb-show-all-recipients)
    ))

;;; from Brian Edmonds' gnus-bbdb.el
;;;
;;; Filing with gnus-folder               REQUIRES (ding) 0.50 OR HIGHER
;;;
;;; To use this feature, you need to put this file somewhere in your
;;; load-path and add the following lines of code to your .gnus file:
;;;
;;; (setq nnmail-split-methods 'bbdb/gnus-split-method)
;;;
;;; You should also examine the variables defvar'd below and customize
;;; them to your taste.  They're listed roughly in descending likelihood
;;; of your wanting to change them.  Once that is done, you need to add
;;; filing information to your BBDB.  There are two fields of interest:
;;;
;;; 1. gnus-private.  This field contains the name of the group in which
;;;    mail to you from any of the addresses associated with this record
;;;    will be filed.  Also, any self-copies of mail you send any of the
;;;    same addresses will be filed here.
;;; 2. gnus-public.  This field is used to keep mail from mailing lists
;;;    out of the private mailboxes.  It should be added to a record for
;;;    the list submission address, and is formatted as follows:
;;;      "group regexp"
;;;    where group is where mail from the list should be filed, and
;;;    regexp is a regular expression which is checked against the
;;;    envelope sender (from the From_ header) to verify that this is
;;;    the copy which came from the list.  For example, the entry for
;;;    the ding mailing list might be:
;;;      "mail.emacs.ding ding-request@ifi.uio.no"
;;;    Yes, the second part *is* a regexp, so those dots may match
;;;    something other than dots.  Sue me.
;;;
;;; Note that you can also specify a gnus-private field for mailing list
;;; addresses, in which case self-copies of mail you send to the list
;;; will be filed there.  Also, the field names can be changed below if
;;; the defaults aren't hip enough for you.  Lastly, if you specify a
;;; gnus-private field for your *own* BBDB record, then all self-copies
;;; of mail you send will be filed to that group.
;;;
;;; This documentation should probably be expanded and moved to a
;;; separate file, but it's late, and *I* know what I'm trying to
;;; say. :)

;;; custom bits
(defcustom bbdb/gnus-split-default-group "mail.misc"
  "*If the BBDB doesn't indicate any group to spool a message to, it will
be spooled to this group.  If bbdb/gnus-split-crosspost-default is not
nil, and if the BBDB did not indicate a specific group for one or more
addresses, messages will be crossposted to this group in addition to any
group(s) which the BBDB indicated."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'string)

(defcustom bbdb/gnus-split-nomatch-function nil
  "*This function will be called after searching the BBDB if no place to
file the message could be found.  It should return a group name (or list
of group names) -- nnmail-split-fancy as provided with Gnus is an
excellent choice."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'function)

(defcustom bbdb/gnus-split-myaddr-regexp
  (concat "^" (user-login-name) "$\\|^"
          (user-login-name) "@\\([-a-z0-9]+\\.\\)*"
          (or gnus-local-domain (message-make-domain)
              (system-name) "") "$")
  "*This regular expression should match your address as found in the
From header of your mail.  You should make sure gnus-local-domain or
gnus-use-generic-from are set before loading this module, if they differ
from (system-name).  If you send mail/news from multiple addresses, then
you'll likely have to set this yourself anyways."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'string)

(defcustom bbdb/gnus-split-crosspost-default nil
  "*If this variable is not nil, then if the BBDB could not identify a
group for every mail address, messages will be filed in
bbdb/gnus-split-default-group in addition to any group(s) which the BBDB
identified."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'boolean)

(defcustom bbdb/gnus-split-private-field 'gnus-private
  "*This variable is used to determine the field to reference to find the
associated group when saving private mail for a network address known to
the BBDB.  The value of the field should be the name of a mail group."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'string)

(defcustom bbdb/gnus-split-public-field 'gnus-public
  "*This variable is used to determine the field to reference to find the
associated group when saving non-private mail (received from a mailing
list) for a network address known to the BBDB.  The value of the field
should be the name of a mail group, followed by a space, and a regular
expression to match on the envelope sender to verify that this mail came
from the list in question."
  :group 'bbdb-mua-specific-gnus-splitting
  :type  'string)

;; The split function works by assigning one of four spooling priorities
;; to each group that is associated with an address in the message.  The
;; priorities are assigned as follows:
;;
;; 0. This priority is assigned when crosspost-default is nil to To/Cc
;;    addresses which have no private group defined in the BBDB.  If the
;;    user's own address has no private group defined, then it will
;;    always be given this priority.
;; 1. This priority is assigned to To/Cc addresses which have a private
;;    group defined in the BBDB.  If crosspost-default is not nil, then
;;    To/Cc addresses which have no private group will also be assigned
;;    this priority.  This is also assigned to the user's own address in
;;    the From position if a private group is defined for it.
;; 2. This priority is assigned to From addresses which have a private
;;    group defined in the BBDB, except for the user's own address as
;;    described under priorities 0 and 1.
;; 3. This priority is assigned to To/Cc addresses which have a public
;;    group defined in the BBDB, and whose associated regular expression
;;    matches the envelope sender (found in the header From_).
;;
;; The split function evaluates the spool priority for each address in
;; the headers of the message, and returns as a list all the groups
;; associated with the addresses which share the highest calculated
;; priority.

;;;#autoload
(defun bbdb/gnus-split-method nil
  "This function expects to be called in a buffer which contains a mail
message to be spooled, and the buffer should be narrowed to the message
headers.  It returns a list of groups to which the message should be
spooled, using the addresses in the headers and information from the
BBDB."
  (let ((prq (list (cons 0 nil) (cons 1 nil) (cons 2 nil) (cons 3 nil))))
    ;; the From: header is special
    (let* ((hdr (or (mail-fetch-field "from") (user-login-name)))
       (rv (bbdb/gnus-split-to-group hdr t)))
      (setcdr (nth (cdr rv) prq) (cons (car rv) nil)))
    ;; do the rest of the headers
    (let ((hdr (or (concat (mail-fetch-field "to" nil t) ", "
               (mail-fetch-field "cc" nil t) ", "
               (mail-fetch-field "apparently-to" nil t)) "")))
      (setq hdr (rfc822-addresses hdr))
      (while hdr
    (let* ((rv (bbdb/gnus-split-to-group (car hdr)))
           (pr (nth (cdr rv) prq)))
      (or (member (car rv) pr) (setcdr pr (cons (car rv) (cdr pr)))))
    (setq hdr (cdr hdr))))
    ;; find the highest non-empty queue
    (setq prq (reverse prq))
    (while (and prq (not (cdr (car prq)))) (setq prq (cdr prq)))
    ;; and return...
    (if (not (or (not (cdr (car prq)))
         (and (equal (cdr (car prq)) (list bbdb/gnus-split-default-group))
                  (symbolp bbdb/gnus-split-nomatch-function)
              (fboundp bbdb/gnus-split-nomatch-function))))
    (cdr (car prq))
      (goto-char (point-min))
      (funcall bbdb/gnus-split-nomatch-function))))

(defun bbdb/gnus-split-to-group (addr &optional source)
  "This function is called from bbdb/gnus-split-method in order to
determine the group and spooling priority for a single address."
  (condition-case tmp
      (progn
    (setq tmp (mail-extract-address-components addr))
    (let* ((nam (car tmp))
           (net (if (not bbdb-canonicalize-net-hook) (car (cdr tmp))
              (bbdb-canonicalize-address (car (cdr tmp)))))
           (rec (bbdb-search-simple nam net))
           pub prv rgx)
      (if (not rec) nil
        (setq prv (bbdb-record-getprop rec bbdb/gnus-split-private-field)
          pub (bbdb-record-getprop rec bbdb/gnus-split-public-field))
        (if (and pub (not source) (string-match "^\\([^ ]+\\) \\(.*\\)$" pub))
        (setq rgx (substring pub (match-beginning 2) (match-end 2))
              pub (substring pub (match-beginning 1) (match-end 1)))
          (setq pub nil)))
      (cond
       ((and rgx pub
         (goto-char (point-min))
         (re-search-forward "^From: \\([^ \n]+\\)[ \n]" nil t)
         (string-match rgx (buffer-substring (match-beginning 1) (match-end 1))))
        (cons pub 3))
       (prv
        (cons prv
          (- 1 (if source -1 0)
             (if (string-match bbdb/gnus-split-myaddr-regexp net) 1 0))))
       (t
        (cons bbdb/gnus-split-default-group
          (if (string-match bbdb/gnus-split-myaddr-regexp net) 0
            (if source 2 (if bbdb/gnus-split-crosspost-default 1 0))))))))
    (error (cons bbdb/gnus-split-default-group 0))))

;;
;; Insinuation
;;

;;;###autoload
(defun bbdb-insinuate-gnus ()
  "Call this function to hook BBDB into GNUS."
  (setq gnus-optional-headers 'bbdb/gnus-lines-and-from)
  (cond ((boundp 'gnus-Article-prepare-hook) ; 3.14 or lower
     (add-hook 'gnus-Article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
     (add-hook 'gnus-Save-newsrc-hook 'bbdb-offer-save)
     (define-key gnus-Subject-mode-map ":" 'bbdb/gnus-show-sender)
     (define-key gnus-Subject-mode-map [(control :)]
       'bbdb/gnus-summary-show-all-recipients)
     (define-key gnus-Subject-mode-map ";" 'bbdb/gnus-edit-notes))
    (t                                   ; 3.15 or higher
     (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
     (add-hook 'gnus-save-newsrc-hook 'bbdb-offer-save)
     (define-key gnus-summary-mode-map ":" 'bbdb/gnus-show-sender)
     (define-key gnus-summary-mode-map ";" 'bbdb/gnus-edit-notes)))

  ;; Set up user field for use in gnus-summary-line-format
  (let ((get-author-user-fun (intern
                  (concat "gnus-user-format-function-"
                      bbdb/gnus-summary-user-format-letter)))
    (in-bbdb-user-fun (intern
               (concat "gnus-user-format-function-"
                   bbdb/gnus-summary-in-bbdb-format-letter))))
                    ; The big one - whole name
    (cond (bbdb/gnus-summary-user-format-letter
       (if (and (fboundp get-author-user-fun)
            (not (eq (symbol-function get-author-user-fun)
                 'bbdb/gnus-summary-get-author)))
           (bbdb-warn
        (format "`gnus-user-format-function-%s' already seems to be in use.
Please redefine `bbdb/gnus-summary-user-format-letter' to a different letter."
            bbdb/gnus-summary-user-format-letter))
         (fset get-author-user-fun 'bbdb/gnus-summary-get-author))))

    ; One tick.  One tick only, please
    (cond (bbdb/gnus-summary-in-bbdb-format-letter
       (if (and (fboundp in-bbdb-user-fun)
            (not (eq (symbol-function in-bbdb-user-fun)
                 'bbdb/gnus-summary-author-in-bbdb)))
           (bbdb-warn
        (format "`gnus-user-format-function-%s' already seems to be in use.
Redefine `bbdb/gnus-summary-in-bbdb-format-letter' to a different letter."
            bbdb/gnus-summary-in-bbdb-format-letter))
         (fset in-bbdb-user-fun 'bbdb/gnus-summary-author-in-bbdb)))))

  ;; Scoring
  (add-hook 'bbdb-after-change-hook 'bbdb/gnus-score-invalidate-alist)
;  (setq gnus-score-find-score-files-function
;   (if (boundp 'gnus-score-find-score-files-function)
;       (cond ((functionp gnus-score-find-score-files-function)
;          (list gnus-score-find-score-files-function
;            'bbdb/gnus-score))
;         ((listp gnus-score-find-score-files-function)
;          (append gnus-score-find-score-files-function
;              'bbdb/gnus-score))
;         (t 'bbdb/gnus-score))
;     'bbdb/gnus-score))
  )

(provide 'bbdb-gnus)

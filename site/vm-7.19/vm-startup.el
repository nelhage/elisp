;;; Entry points for VM
;;; Copyright (C) 1994-1998, 2003 Kyle E. Jones
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

;;(provide 'vm-startup)

(defvar enable-multibyte-characters)

;;;###autoload
(defun vm (&optional folder read-only access-method)
  "Read mail under Emacs.
Optional first arg FOLDER specifies the folder to visit.  It defaults
to the value of vm-primary-inbox.  The folder buffer is put into VM
mode, a major mode for reading mail.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, message additions or deletions will be allowed in the
visited folder.

Visiting the primary inbox normally causes any contents of the system mailbox to
be moved and appended to the resulting buffer.  You can disable this automatic fetching of mail by setting `vm-auto-get-new-mail' to nil.

All the messages can be read by repeatedly pressing SPC.  Use `n'ext and
`p'revious to move about in the folder.  Messages are marked for
deletion with `d', and saved to another folder with `s'.  Quitting VM
with `q' saves the buffered folder to disk, but does not expunge
deleted messages.  Use `###' to expunge deleted messages.

See the documentation for vm-mode for more information."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  ;; set inhibit-local-variables non-nil to protect
  ;; against letter bombs.
  ;; set enable-local-variables to nil for newer Emacses
  (catch 'done
    ;; deduce the access method if none specified
    (if (null access-method)
	(let ((f (or folder vm-primary-inbox)))
	  (cond ((and vm-recognize-imap-maildrops
		      ;; f could be a buffer
		      (stringp f)
		      (string-match vm-recognize-imap-maildrops f))
		 (setq access-method 'imap
		       folder f))
		((and vm-recognize-pop-maildrops
		      ;; f could be a buffer
		      (stringp f)
		      (string-match vm-recognize-pop-maildrops f))
		 (setq access-method 'pop
		       folder f)))))
    (let ((full-startup (not (bufferp folder)))
	  (did-read-index-file nil)
	  folder-buffer first-time totals-blurb
	  folder-name remote-spec
	  preserve-auto-save-file)
      (cond ((eq access-method 'pop)
	     (setq remote-spec (vm-pop-find-spec-for-name folder))
	     (if (null remote-spec)
		 (error "No such POP folder: %s" folder))
	     (setq folder-name folder)
	     ;; Prior to VM 7.11, we computed the cache filename
	     ;; based on the full POP spec including the password
	     ;; if it was in the spec.  This meant that every
	     ;; time the user changed his password, we'd start
	     ;; visiting the wrong (and probably nonexistent)
	     ;; cache file.
	     ;;
	     ;; To fix this we do two things.  First, migrate the
	     ;; user's caches to the filenames based in the POP
	     ;; sepc without the password.  Second, we visit the
	     ;; old password based filename if it still exists
	     ;; after trying to migrate it.
	     ;;
	     ;; For VM 7.16 we apply the same logic to the access
	     ;; methods, pop, pop-ssh and pop-ssl and to
	     ;; authentication method and service port, which can
	     ;; also change and lead us to visit a nonexistent
	     ;; cache file.  The assumption is that these
	     ;; properties of the connection can change and we'll
	     ;; still be accessing the same mailbox on the
	     ;; server.
	     (let ((f-pass (vm-pop-make-filename-for-spec remote-spec))
		   (f-nopass (vm-pop-make-filename-for-spec remote-spec t))
		   (f-nospec (vm-pop-make-filename-for-spec remote-spec t t)))
	       (cond ((or (string= f-pass f-nospec)
			  (file-exists-p f-nospec))
		      nil )
		     ((file-exists-p f-pass)
		      ;; try to migrate
		      (condition-case nil
			  (rename-file f-pass f-nospec)
			(error nil)))
		     ((file-exists-p f-nopass)
		      ;; try to migrate
		      (condition-case nil
			  (rename-file f-nopass f-nospec)
			(error nil))))
	       ;; choose the one that exists, password version,
	       ;; nopass version and finally nopass+nospec
	       ;; version.
	       (cond ((file-exists-p f-pass)
		      (setq folder f-pass))
		     ((file-exists-p f-nopass)
		      (setq folder f-nopass))
		     (t
		      (setq folder f-nospec)))))
	    ((eq access-method 'imap)
	     (setq remote-spec folder
		   folder-name (or (nth 3 (vm-imap-parse-spec-to-list
					   remote-spec))
				   folder)
		   folder (vm-imap-make-filename-for-spec remote-spec))))
      (setq folder-buffer
	    (if (bufferp folder)
		folder
	      (let ((file (or folder (expand-file-name vm-primary-inbox
						       vm-folder-directory))))
		(if (file-directory-p file)
		    ;; MH code perhaps... ?
		    (error "%s is a directory" file)
		  (or (vm-get-file-buffer file)
		      (let ((default-directory
			      (or (and vm-folder-directory
				       (expand-file-name vm-folder-directory))
				  default-directory))
			    (inhibit-local-variables t)
			    (enable-local-variables nil)
			    (enable-local-eval nil)
			    ;; for Emacs/MULE
			    (default-enable-multibyte-characters nil)
			    ;; for XEmacs/Mule
			    (coding-system-for-read
			         (vm-line-ending-coding-system)))
			(message "Reading %s..." file)
			(prog1 (find-file-noselect file)
			  ;; update folder history
			  (let ((item (or remote-spec folder
					  vm-primary-inbox)))
			    (if (not (equal item (car vm-folder-history)))
				(setq vm-folder-history
				      (cons item vm-folder-history))))
			  (message "Reading %s... done" file))))))))
      (set-buffer folder-buffer)
      (cond ((memq access-method '(pop imap))
	     (if (not (equal folder-name (buffer-name)))
		 (rename-buffer folder-name t))))
      (if (and vm-fsfemacs-mule-p enable-multibyte-characters)
	  (set-buffer-multibyte nil))
      ;; for MULE
      ;;
      ;; If the file coding system is not a no-conversion variant,
      ;; make it so by encoding all the text, then setting the
      ;; file coding system and decoding it.  This situation is
      ;; only possible if a file is visited and then vm-mode is
      ;; run on it afterwards.
      ;;
      ;; There are separate code blocks for FSF Emacs and XEmacs
      ;; because the coding systems have different names.
      (defvar buffer-file-coding-system)
      (if (and (or vm-xemacs-mule-p vm-xemacs-file-coding-p)
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-unix)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-dos)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'no-conversion-mac)))
	       (not (eq (get-coding-system buffer-file-coding-system)
			(get-coding-system 'binary))))
	  (let ((buffer-read-only nil)
		(omodified (buffer-modified-p)))
	    (unwind-protect
		(progn
		  (encode-coding-region (point-min) (point-max)
					buffer-file-coding-system)
		  (set-buffer-file-coding-system 'no-conversion nil)
		  (decode-coding-region (point-min) (point-max)
					buffer-file-coding-system))
	      (set-buffer-modified-p omodified))))
      (if (and vm-fsfemacs-mule-p (null buffer-file-coding-system))
	  (set-buffer-file-coding-system 'raw-text nil))
      (if (and vm-fsfemacs-mule-p
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-unix)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-mac)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'raw-text-dos)))
	       (not (eq (coding-system-base buffer-file-coding-system)
			(coding-system-base 'no-conversion))))
	  (let ((buffer-read-only nil)
		(omodified (buffer-modified-p)))
	    (unwind-protect
		(progn
		  (encode-coding-region (point-min) (point-max)
					buffer-file-coding-system)
		  (set-buffer-file-coding-system 'raw-text nil)
		  (decode-coding-region (point-min) (point-max)
					buffer-file-coding-system))
	      (set-buffer-modified-p omodified))))
      (vm-check-for-killed-summary)
      (vm-check-for-killed-presentation)
      ;; If the buffer's not modified then we know that there can be no
      ;; messages in the folder that are not on disk.
      (or (buffer-modified-p) (setq vm-messages-not-on-disk 0))
      (setq first-time (not (eq major-mode 'vm-mode))
	    preserve-auto-save-file (and buffer-file-name
					  (not (buffer-modified-p))
					  (file-newer-than-file-p
					   (make-auto-save-file-name)
					   buffer-file-name)))
      ;; Force the folder to be read only if the auto
      ;; save file contains information the user might not
      ;; want overwritten, i.e. recover-file might be
      ;; desired.  What we want to avoid is an auto-save.
      ;; Making the folder read only will keep
      ;; subsequent actions from modifying the buffer in a
      ;; way that triggers an auto save.
      ;;
      ;; Also force the folder read-only if it was read only and
      ;; not already in vm-mode, since there's probably a good
      ;; reason for this.
      (setq vm-folder-read-only (or preserve-auto-save-file read-only
				    (default-value 'vm-folder-read-only)
				    (and first-time buffer-read-only)))
      ;; If this is not a VM mode buffer then some initialization
      ;; needs to be done 
      (if first-time
	  (progn
	    (buffer-disable-undo (current-buffer))
	    (abbrev-mode 0)
	    (auto-fill-mode 0)
	    ;; If an 8-bit message arrives undeclared the 8-bit
	    ;; characters in it should be displayed using the
	    ;; user's default face charset, rather than as octal
	    ;; escapes.
	    (vm-fsfemacs-nonmule-display-8bit-chars)
	    (vm-mode-internal access-method)
	    (cond ((eq access-method 'pop)
		   (vm-set-folder-pop-maildrop-spec remote-spec))
		  ((eq access-method 'imap)
		   (vm-set-folder-imap-maildrop-spec remote-spec)))
	    ;; If the buffer is modified we don't know if the
	    ;; folder format has been changed to be different
	    ;; from index file, so don't read the index file in
	    ;; that case.
	    (if (not (buffer-modified-p))
		(setq did-read-index-file (vm-read-index-file-maybe)))))

      ;; builds message list, reads attributes if they weren't
      ;; read from an index file.
      (vm-assimilate-new-messages nil (not did-read-index-file) nil t)

      (if (and first-time (not did-read-index-file))
	  (progn
	    (vm-gobble-visible-header-variables)
	    (vm-gobble-bookmark)
	    (vm-gobble-pop-retrieved)
	    (vm-gobble-imap-retrieved)
	    (vm-gobble-summary)
	    (vm-gobble-labels)))

      (if first-time
	  (vm-start-itimers-if-needed))

      ;; make a new frame if the user wants one.  reuse an
      ;; existing frame that is showing this folder.
      (if (and full-startup
	       ;; this so that "emacs -f vm" doesn't create a frame.
	       this-command)
	  (apply 'vm-goto-new-folder-frame-maybe
		 (if folder '(folder) '(primary-folder folder))))

      ;; raise frame if requested and apply startup window
      ;; configuration.
      (if full-startup
	  (let ((buffer-to-display (or vm-summary-buffer
				       vm-presentation-buffer
				       (current-buffer))))
	    (vm-display buffer-to-display buffer-to-display
			(list this-command)
			(list (or this-command 'vm) 'startup))
	    (if vm-raise-frame-at-startup
		(vm-raise-frame))))

      ;; say this NOW, before the non-previewers read a message,
      ;; alter the new message count and confuse themselves.
      (if full-startup
	  (progn
	    ;; save blurb so we can repeat it later as necessary.
	    (setq totals-blurb (vm-emit-totals-blurb))
	    (and buffer-file-name
		 (vm-store-folder-totals buffer-file-name (cdr vm-totals)))))

      (vm-thoughtfully-select-message)
      (vm-update-summary-and-mode-line)
      ;; need to do this after any frame creation because the
      ;; toolbar sets frame-specific height and width specifiers.
      (vm-toolbar-install-or-uninstall-toolbar)

      (and vm-use-menus (vm-menu-support-possible-p)
	   (vm-menu-install-visited-folders-menu))

      (if full-startup
	  (progn
	    (if (and (vm-should-generate-summary)
		     ;; don't generate a summary if recover-file is
		     ;; likely to happen, since recover-file does
		     ;; not work in a summary buffer.
		     (not preserve-auto-save-file))
		(vm-summarize t nil))
	    ;; raise the summary frame if the user wants frames
	    ;; raised and if there is a summary frame.
	    (if (and vm-summary-buffer
		     vm-mutable-frames
		     vm-frame-per-summary
		     vm-raise-frame-at-startup)
		(vm-raise-frame))
	    ;; if vm-mutable-windows is nil, the startup
	    ;; configuration can't be applied, so do
	    ;; something to get a VM buffer on the screen
	    (if vm-mutable-windows
		(vm-display nil nil (list this-command)
			    (list (or this-command 'vm) 'startup))
	      (save-excursion
		(switch-to-buffer (or vm-summary-buffer
				      vm-presentation-buffer
				      (current-buffer)))))))

      (if vm-message-list
	  ;; don't decode MIME if recover-file is
	  ;; likely to happen, since recover-file does
	  ;; not work in a presentation buffer.
	  (let ((vm-auto-decode-mime-messages
		 (and vm-auto-decode-mime-messages
		      (not preserve-auto-save-file))))
	    (vm-preview-current-message)))

      (run-hooks 'vm-visit-folder-hook)

      ;; Warn user about auto save file, if appropriate.
      (if (and full-startup preserve-auto-save-file)
	  (message 
	   (substitute-command-keys
	    "Auto save file is newer; consider \\[recover-file].  FOLDER IS READ ONLY.")))
      ;; if we're not doing a full startup or if doing more would
      ;; trash the auto save file that we need to preserve,
      ;; stop here.
      (if (or (not full-startup) preserve-auto-save-file)
	  (throw 'done t))
      
      (if full-startup
	  (message totals-blurb))

      (if (and vm-auto-get-new-mail
	       (not vm-block-new-mail)
	       (not vm-folder-read-only))
	  (progn
	    (message "Checking for new mail for %s..."
		     (or buffer-file-name (buffer-name)))
	    (if (vm-get-spooled-mail t)
		(progn
		  (setq totals-blurb (vm-emit-totals-blurb))
		  (if (vm-thoughtfully-select-message)
		      (vm-preview-current-message)
		    (vm-update-summary-and-mode-line))))
	    (message totals-blurb)))

      ;; Display copyright and copying info.
      (if (and (interactive-p) (not vm-startup-message-displayed))
	  (progn
	    (vm-display-startup-message)
	    (if (not (input-pending-p))
		(message totals-blurb)))))))

;;;###autoload
(defun vm-other-frame (&optional folder read-only)
  "Like vm, but run in a newly created frame."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (if folder
	  (vm-goto-new-frame 'folder)
	(vm-goto-new-frame 'primary-folder 'folder)))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-other-window (&optional folder read-only)
  "Like vm, but run in a different window."
  (interactive (list nil current-prefix-arg))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm folder read-only)))

(put 'vm-mode 'mode-class 'special)

;;;###autoload
(defun vm-mode (&optional read-only)
  "Major mode for reading mail.

This is VM 7.19.

Commands:
   h - summarize folder contents
   H - summarize contents of all folders
 C-t - toggle threads display

   n - go to next message
   p - go to previous message
   N - like `n' but ignores skip-variable settings
   P - like `p' but ignores skip-variable settings
 M-n - go to next unread message
 M-p - go to previous unread message
 RET - go to numbered message (uses prefix arg or prompts in minibuffer)
 M-g - go to numbered message (uses prefix arg or prompts in minibuffer)
 TAB - go to last message seen
   ^ - go to parent of this message
 M-s - incremental search through the folder

   t - display hidden headers
 SPC - expose message body or scroll forward a page
   b - scroll backward a page
   < - go to beginning of current message
   > - go to end of current message
   [ - go to previous button
   ] - go to next button
   D - decode MIME if not already decoded.  If already decoded,
       display all MIME objects as tags.  If already displaying
       tags, show raw unecoded MIME>

   d - delete message, prefix arg deletes messages forward
 C-d - delete message, prefix arg deletes messages backward
   u - undelete
   k - delete all messages with same subject as the current message

   r - reply (only to the sender of the message)
   R - reply with included text from the current message
 M-r - extract and resend bounced message
   f - followup (reply to all recipients of message)
   F - followup with included text from the current message
   z - forward the current message
   m - send a message
   B - resend the current message to another user.
   c - continue composing the most recent message you were composing

   @ - digestify and mail entire folder contents (the folder is not modified)
   * - burst a digest into individual messages, and append and assimilate these
       messages into the current folder.

   G - sort messages by various keys

   g - get any new mail that has arrived in the system mailbox
       (new mail is appended to the disk and buffer copies of the
       primary inbox.)
   v - visit another mail folder

   e - edit the current message
   j - discard cached information about the current message

   s - save current message in a folder (appends if folder already exists)
   w - write current message to a file without its headers (appends if exists)
   S - save entire folder to disk, does not expunge
   A - save unfiled messages to their vm-auto-folder-alist specified folders
 ### - expunge deleted messages (without saving folder)
   q - quit VM, deleted messages are not expunged, folder is
       saved to disk if it is modified.  new messages are changed
       to be flagged as just unread.
   x - exit VM with no change to the folder

 M N - use marks; the next vm command will affect only marked messages
       if it makes sense for the command to do so.  These commands
       apply and remove marks to messages:

       M M - mark the current message
       M U - unmark the current message
       M m - mark all messages
       M u - unmark all messages
       M C - mark messages matched by a virtual folder selector
       M c - unmark messages matched by a virtual folder selector
       M T - mark thread tree rooted at the current message
       M t - unmark thread tree rooted at the current message
       M S - mark messages with the same subject as the current message
       M s - unmark messages with the same subject as the current message
       M A - mark messages with the same author as the current message
       M a - unmark messages with the same author as the current message
       M R - mark messages within the point/mark region in the summary
       M r - unmark messages within the point/mark region in the summary
       M V - toggle the marked-ness of all messages
       M X - apply the selectors of a named virtual folder to the
             messages in the current folder and mark all messages
             that match those selectors.
       M x - apply the selectors of a named virtual folder to the
             messages in the current folder and unmark all messages
             that match those selectors.
       M ? - partial help for mark commands

   W - prefix for window configuration commands:
       W S - save the current window configuration to a name
       W D - delete a window configuration
       W W - apply a configuration
       W ? - help for the window configuration commands

   V - prefix for virtual folder commands:
       V V - visit a virtual folder (folder must be defined in
             vm-virtual-folder-alist)
       V C - create a virtual folder composed of a subset of
             the current folder's messages.
       V A - create a virtual folder containing all the messages in
             the current folder with the same author as the current message.
       V S - create a virtual folder containing all the messages in
             the current folder with the same subject as the current message.
       V X - apply the selectors of a named virtual folder to the messages in
             the current folder and create a virtual folder
             containing the selected messages.
       V M - toggle whether this virtual folder's messages mirror the
             underlying real messages' attributes.
       V ? - help for virtual folder commands

 C-_ - undo, special undo that retracts the most recent
             changes in message attributes and labels.  Expunges,
             message edits, and saves cannot be undone.  C-x u is
             also bound to this command.

   a - set message attributes

   l - prefix for label commands:
       l a - add labels to message
       l d - delete labels from message

   $ - prefix for MIME commands.  Position the cursor over a MIME
       tag and use these keystrokes to operate on a MIME object.

       RET   - display the MIME object according to its type.
       $ s   - save the MIME object
       $ p   - print the MIME object
       $ |   - pipe the MIME object to a shell command.
       $ RET - display the MIME object's text using the \"default\" face.
       $ e   - display the MIME object with an external viewer.
       $ d   - delete the MIME object from the message.
       $ v   - display the MIME object as some other type.
       $ w   - write the MIME object to a file.
       $ a   - attach the MIME object to a composition buffer.

   L - reload your VM init file, ~/.vm

   % - change a folder to another type

   ? - help

   ! - run a shell command
   | - run a shell command with the current message as input

 M-C - view conditions under which you may redistribute VM
 M-W - view the details of VM's lack of a warranty

Use M-x vm-submit-bug-report to submit a bug report.

Variables:
   vm-arrived-message-hook
   vm-arrived-messages-hook
   vm-auto-center-summary
   vm-auto-decode-mime-messages
   vm-auto-displayed-mime-content-type-exceptions
   vm-auto-displayed-mime-content-types
   vm-auto-folder-alist
   vm-auto-folder-case-fold-search
   vm-auto-get-new-mail
   vm-auto-next-message
   vm-berkeley-mail-compatibility
   vm-burst-digest-messages-inherit-labels
   vm-check-folder-types
   vm-circular-folders
   vm-confirm-new-folders
   vm-confirm-quit
   vm-convert-folder-types
   vm-crash-box
   vm-crash-box-suffix
   vm-default-From_-folder-type
   vm-default-folder-permission-bits
   vm-default-folder-type
   vm-delete-after-archiving
   vm-delete-after-bursting
   vm-delete-after-saving
   vm-delete-empty-folders
   vm-digest-burst-type
   vm-digest-center-preamble
   vm-digest-preamble-format
   vm-digest-send-type
   vm-display-buffer-hook
   vm-display-using-mime
   vm-edit-message-hook
   vm-fill-paragraphs-containing-long-lines
   vm-flush-interval
   vm-folder-directory
   vm-folder-read-only
   vm-folders-summary-database
   vm-folders-summary-directories
   vm-folders-summary-format
   vm-follow-summary-cursor
   vm-forward-message-hook
   vm-forwarded-headers
   vm-forwarding-digest-type
   vm-forwarding-subject-format
   vm-frame-parameter-alist
   vm-frame-per-completion
   vm-frame-per-composition
   vm-frame-per-edit
   vm-frame-per-folder
   vm-frame-per-folders-summary
   vm-frame-per-help
   vm-frame-per-summary
   vm-highlighted-header-face
   vm-highlighted-header-regexp
   vm-honor-mime-content-disposition
   vm-honor-page-delimiters
   vm-icontopbm-program
   vm-image-directory
   vm-imagemagick-convert-program
   vm-imagemagick-identify-program
   vm-imap-auto-expunge-alist
   vm-imap-bytes-per-session
   vm-imap-expunge-after-retrieving
   vm-imap-max-message-size
   vm-imap-messages-per-session
   vm-imap-session-preauth-hook
   vm-index-file-suffix
   vm-in-reply-to-format
   vm-included-text-attribution-format
   vm-included-text-discard-header-regexp
   vm-included-text-headers
   vm-included-text-prefix
   vm-infer-mime-types
   vm-invisible-header-regexp
   vm-jump-to-new-messages
   vm-jump-to-unread-messages
   vm-keep-crash-boxes
   vm-keep-sent-messages
   vm-lynx-program
   vm-mail-check-interval
   vm-mail-header-from
   vm-mail-header-insert-date
   vm-mail-header-insert-message-id
   vm-mail-mode-hook
   vm-mail-send-hook
   vm-make-crash-box-name
   vm-make-spool-file-name
   vm-mime-7bit-composition-charset
   vm-mime-8bit-composition-charset
   vm-mime-8bit-text-transfer-encoding
   vm-mime-alternative-select-method
   vm-mime-attachment-auto-type-alist
   vm-mime-attachment-save-directory
   vm-mime-avoid-folding-content-type
   vm-mime-base64-decoder-program
   vm-mime-base64-decoder-switches
   vm-mime-base64-encoder-program
   vm-mime-base64-encoder-switches
   vm-mime-button-face
   vm-mime-button-format-alist
   vm-mime-charset-converter-alist
   vm-mime-charset-font-alist
   vm-mime-confirm-delete
   vm-mime-decode-for-preview
   vm-mime-default-face-charset-exceptions
   vm-mime-default-face-charsets
   vm-mime-delete-after-saving
   vm-mime-delete-viewer-processes
   vm-mime-digest-discard-header-regexp
   vm-mime-digest-headers
   vm-mime-display-function
   vm-mime-external-content-types-alist
   vm-mime-forward-local-external-bodies
   vm-mime-ignore-composite-type-opaque-transfer-encoding
   vm-mime-ignore-mime-version
   vm-mime-ignore-missing-multipart-boundary
   vm-mime-internal-content-type-exceptions
   vm-mime-internal-content-types
   vm-mime-max-message-size
   vm-mime-qp-decoder-program
   vm-mime-qp-decoder-switches
   vm-mime-qp-encoder-program
   vm-mime-qp-encoder-switches
   vm-mime-require-mime-version-header
   vm-mime-type-converter-alist
   vm-mime-use-image-strips
   vm-mime-use-w3-for-text/html
   vm-mime-uuencode-decoder-program
   vm-mime-uuencode-decoder-switches
   vm-mode-hook
   vm-mosaic-program
   vm-mosaic-program-switches
   vm-move-after-deleting
   vm-move-after-killing
   vm-move-after-undeleting
   vm-move-messages-physically
   vm-mutable-frames
   vm-mutable-windows
   vm-netscape-program
   vm-netscape-program-switches
   vm-page-continuation-glyph
   vm-paragraph-fill-column
   vm-pop-auto-expunge-alist
   vm-pop-bytes-per-session
   vm-pop-expunge-after-retrieving
   vm-pop-folder-alist
   vm-pop-max-message-size
   vm-pop-md5-program
   vm-pop-messages-per-session
   vm-popup-menu-on-mouse-3
   vm-preferences-file
   vm-preview-lines
   vm-preview-read-messages
   vm-primary-inbox
   vm-quit-hook
   vm-recognize-imap-maildrops
   vm-recognize-pop-maildrops
   vm-reply-hook
   vm-reply-ignored-addresses
   vm-reply-ignored-reply-tos
   vm-reply-subject-prefix
   vm-resend-bounced-discard-header-regexp
   vm-resend-bounced-headers
   vm-resend-bounced-message-hook
   vm-resend-discard-header-regexp
   vm-resend-headers
   vm-resend-message-hook
   vm-retrieved-spooled-mail-hook
   vm-rfc1153-digest-discard-header-regexp
   vm-rfc1153-digest-headers
   vm-rfc934-digest-discard-header-regexp
   vm-rfc934-digest-headers
   vm-search-using-regexps
   vm-select-message-hook
   vm-select-new-message-hook
   vm-select-unread-message-hook
   vm-send-digest-hook
   vm-send-using-mime
   vm-skip-deleted-messages
   vm-skip-read-messages
   vm-spool-file-suffixes
   vm-spool-files
   vm-spooled-mail-waiting-hook
   vm-ssh-program
   vm-ssh-program-switches
   vm-ssh-remote-command
   vm-startup-with-summary
   vm-strip-reply-headers
   vm-stunnel-program
   vm-stunnel-program-switches
   vm-stunnel-random-data-method
   vm-subject-significant-chars
   vm-summary-arrow
   vm-summary-format
   vm-summary-highlight-face
   vm-summary-mode-hook
   vm-summary-redo-hook
   vm-summary-show-threads
   vm-summary-thread-indent-level
   vm-tale-is-an-idiot
   vm-temp-file-directory
   vm-thread-using-subject
   vm-toolbar-pixmap-directory
   vm-trust-From_-with-Content-Length
   vm-uncompface-program
   vm-undisplay-buffer-hook
   vm-unforwarded-header-regexp
   vm-url-browser
   vm-url-browser-switches
   vm-url-retrieval-methods
   vm-url-search-limit
   vm-use-menus
   vm-use-toolbar
   vm-virtual-folder-alist
   vm-virtual-mirror
   vm-visible-headers
   vm-visit-folder-hook
   vm-visit-when-saving
   vm-warp-mouse-to-new-frame
   vm-wget-program
   vm-window-configuration-file
"
  (interactive "P")
  (vm (current-buffer) read-only)
  (vm-display nil nil '(vm-mode) '(vm-mode)))

;;;###autoload
(defun vm-visit-folder (folder &optional read-only)
  "Visit a mail file.
VM will parse and present its messages to you in the usual way.

First arg FOLDER specifies the mail file to visit.  When this
command is called interactively the file name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (setq vm-last-visit-folder folder)
  (let ((access-method nil) foo)
    (cond ((and (stringp vm-recognize-pop-maildrops)
		(string-match vm-recognize-pop-maildrops folder)
		(setq foo (vm-pop-find-name-for-spec folder)))
	   (setq folder foo
		 access-method 'pop))
	  ((and (stringp vm-recognize-imap-maildrops)
		(string-match vm-recognize-imap-maildrops folder)
		(setq foo (vm-imap-find-name-for-spec folder)))
	   (setq folder foo
		 access-method 'imap))
	  (t
	   (let ((default-directory (or vm-folder-directory default-directory)))
	     (setq folder (expand-file-name folder)))))
    (vm folder read-only access-method)))

;;;###autoload
(defun vm-visit-folder-other-frame (folder &optional read-only)
  "Like vm-visit-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other frame:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-folder-other-window (folder &optional read-only)
  "Like vm-visit-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((default-directory (if vm-folder-directory
				  (expand-file-name vm-folder-directory)
				default-directory))
	   (default (or vm-last-visit-folder vm-last-save-folder))
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-file-name
	      (format "Visit%s folder in other window:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      default-directory default nil nil 'vm-folder-history)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-folder folder read-only)))

;;;###autoload
(defun vm-visit-pop-folder (folder &optional read-only)
  "Visit a POP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the POP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the POP mailbox.

First arg FOLDER specifies the name of the POP mailbox to visit.
You can only visit mailboxes that are specified in `vm-pop-folder-alist'.
When this command is called interactively the mailbox name is read from the
minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (if (and (equal folder "") (stringp vm-last-visit-pop-folder))
      (setq folder vm-last-visit-pop-folder))
  (if (null (vm-pop-find-spec-for-name folder))
      (error "No such POP folder: %s" folder))
  (setq vm-last-visit-pop-folder folder)
  (vm folder read-only 'pop))

;;;###autoload
(defun vm-visit-pop-folder-other-frame (folder &optional read-only)
  "Like vm-visit-pop-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-pop-folder-other-window (folder &optional read-only)
  "Like vm-visit-pop-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((completion-list (mapcar (function (lambda (x) (nth 1 x)))
				    vm-pop-folder-alist))
	   (default vm-last-visit-pop-folder)
	   (this-command this-command)
	   (last-command last-command))
       (list (vm-read-string
	      (format "Visit%s POP folder:%s "
		      (if current-prefix-arg " read only" "")
		      (if default
			  (format " (default %s)" default)
			""))
	      completion-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-pop-folder folder read-only)))

;;;###autoload
(defun vm-visit-imap-folder (folder &optional read-only)
  "Visit a IMAP mailbox.
VM will present its messages to you in the usual way.  Messages
found in the IMAP mailbox will be downloaded and stored in a local
cache.  If you expunge messages from the cache, the corresponding
messages will be expunged from the IMAP mailbox.

First arg FOLDER specifies the IMAP mailbox to visit.  You can only
visit mailboxes on servers that are listed in `vm-imap-server-list'.
When this command is called interactively the server and mailbox
names are read from the minibuffer.

Prefix arg or optional second arg READ-ONLY non-nil indicates
that the folder should be considered read only.  No attribute
changes, messages additions or deletions will be allowed in the
visited folder."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      vm-imap-server-list t)
	     current-prefix-arg))))
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (vm folder read-only 'imap))

;;;###autoload
(defun vm-visit-imap-folder-other-frame (folder &optional read-only)
  "Like vm-visit-imap-folder, but run in a newly created frame."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      vm-imap-server-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-imap-folder-other-window (folder &optional read-only)
  "Like vm-visit-imap-folder, but run in a different window."
  (interactive
   (save-excursion
     (vm-session-initialization)
     (vm-check-for-killed-folder)
     (vm-select-folder-buffer-if-possible)
     (let ((this-command this-command)
	   (last-command last-command))
       (list (vm-read-imap-folder-name
	      (format "Visit%s IMAP folder: "
		      (if current-prefix-arg " read only" ""))
	      vm-imap-server-list)
	     current-prefix-arg))))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-imap-folder folder read-only)))

(put 'vm-virtual-mode 'mode-class 'special)

(defun vm-virtual-mode (&rest ignored)
  "Mode for reading multiple mail folders as one folder.

The commands available are the same commands that are found in
vm-mode, except that a few of them are not applicable to virtual
folders.

vm-virtual-mode is not a normal major mode.  If you run it, it
will not do anything.  The entry point to vm-virtual-mode is
vm-visit-virtual-folder.")

(defvar scroll-in-place)

;;;###autoload
(defun vm-visit-virtual-folder (folder-name &optional read-only bookmark)
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (not (assoc folder-name vm-virtual-folder-alist))
      (error "No such virtual folder, %s" folder-name))
  (let ((buffer-name (concat "(" folder-name ")"))
	first-time blurb)
    (set-buffer (get-buffer-create buffer-name))
    (setq first-time (not (eq major-mode 'vm-virtual-mode)))
    (if first-time
	(progn
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (setq mode-name "VM Virtual"
		mode-line-format vm-mode-line-format
		buffer-read-only t
		vm-folder-read-only read-only
		vm-label-obarray (make-vector 29 0)
		vm-virtual-folder-definition
		  (assoc folder-name vm-virtual-folder-alist))
	  ;; scroll in place messes with scroll-up and this loses
	  (make-local-variable 'scroll-in-place)
	  (setq scroll-in-place nil)
	  (vm-build-virtual-message-list nil)
	  (use-local-map vm-mode-map)
	  (and (vm-menu-support-possible-p)
	       (vm-menu-install-menus))
	  (add-hook 'kill-buffer-hook 'vm-garbage-collect-folder)
	  (add-hook 'kill-buffer-hook 'vm-garbage-collect-message)
	  ;; save this for last in case the user interrupts.
	  ;; an interrupt anywhere before this point will cause
	  ;; everything to be redone next revisit.
	  (setq major-mode 'vm-virtual-mode)
	  (run-hooks 'vm-virtual-mode-hook)
	  ;; must come after the setting of major-mode
	  (setq mode-popup-menu (and vm-use-menus
				     (vm-menu-support-possible-p)
				     (vm-menu-mode-menu)))
	  (setq blurb (vm-emit-totals-blurb))
	  (if vm-summary-show-threads
	      (vm-sort-messages "thread"))
	  (if bookmark
	      (let ((mp vm-message-list))
		(while mp
		  (if (eq bookmark (vm-real-message-of (car mp)))
		      (progn
			(vm-record-and-change-message-pointer
			 vm-message-pointer mp)
			(vm-preview-current-message)
			(setq mp nil))
		    (setq mp (cdr mp))))))
	  (if (null vm-message-pointer)
	      (if (vm-thoughtfully-select-message)
		  (vm-preview-current-message)
		(vm-update-summary-and-mode-line)))
	  (message blurb)))
    ;; make a new frame if the user wants one.  reuse an
    ;; existing frame that is showing this folder.
    (vm-goto-new-folder-frame-maybe 'folder)
    (if vm-raise-frame-at-startup
	(vm-raise-frame))
    (vm-display nil nil (list this-command) (list this-command 'startup))
    (vm-toolbar-install-or-uninstall-toolbar)
    (if first-time
	(progn
	  (if (vm-should-generate-summary)
	      (progn (vm-summarize t nil)
		     (message blurb)))
	  ;; raise the summary frame if the user wants frames
	  ;; raised and if there is a summary frame.
	  (if (and vm-summary-buffer
		   vm-mutable-frames
		   vm-frame-per-summary
		   vm-raise-frame-at-startup)
	      (vm-raise-frame))
	  ;; if vm-mutable-windows is nil, the startup
	  ;; configuration can't be applied, so do
	  ;; something to get a VM buffer on the screen
	  (if vm-mutable-windows
	      (vm-display nil nil (list this-command)
			  (list (or this-command 'vm) 'startup))
	    (save-excursion
	      (switch-to-buffer (or vm-summary-buffer
				    vm-presentation-buffer
				    (current-buffer)))))))

    ;; check interactive-p so as not to bog the user down if they
    ;; run this function from within another function.
    (and (interactive-p)
	 (not vm-startup-message-displayed)
	 (vm-display-startup-message)
	 (message blurb))))

;;;###autoload
(defun vm-visit-virtual-folder-other-frame (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a newly created frame."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other frame: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'folder))
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-visit-virtual-folder-other-window (folder-name &optional read-only)
  "Like vm-visit-virtual-folder, but run in a different window."
  (interactive
   (let ((last-command last-command)
	 (this-command this-command))
     (vm-session-initialization)
     (list
      (vm-read-string (format "Visit%s virtual folder in other window: "
			      (if current-prefix-arg " read only" ""))
		      vm-virtual-folder-alist)
      current-prefix-arg)))
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-folder nil)
	(vm-search-other-frames nil))
    (vm-visit-virtual-folder folder-name read-only)))

;;;###autoload
(defun vm-mail (&optional to)
  "Send a mail message from within VM, or from without.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (vm-check-for-killed-folder)
  (vm-select-folder-buffer-if-possible)
  (vm-check-for-killed-summary)
  (vm-mail-internal nil to)
  (run-hooks 'vm-mail-hook)
  (run-hooks 'vm-mail-mode-hook))

;;;###autoload
(defun vm-mail-other-frame (&optional to)
  "Like vm-mail, but run in a newly created frame.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (if (vm-multiple-frames-possible-p)
      (vm-goto-new-frame 'composition))
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to))
  (if (vm-multiple-frames-possible-p)
      (vm-set-hooks-for-frame-deletion)))

;;;###autoload
(defun vm-mail-other-window (&optional to)
  "Like vm-mail, but run in a different window.
Optional argument TO is a string that should contain a comma separated
recipient list."
  (interactive)
  (vm-session-initialization)
  (if (one-window-p t)
      (split-window))
  (other-window 1)
  (let ((vm-frame-per-composition nil)
	(vm-search-other-frames nil))
    (vm-mail to)))

(fset 'vm-folders-summary-mode 'vm-mode)
(put 'vm-folders-summary-mode 'mode-class 'special)

;;;###autoload
(defun vm-folders-summarize (&optional display raise)
  "Generate a summary of the folders in your folder directories.
Set `vm-folders-summary-directories' to specify the folder directories.
Press RETURN or click mouse button 2 on an entry in the folders
summary buffer to select a folder."
  (interactive "p\np")
  (vm-session-initialization)
  (vm-check-for-killed-summary)
  (if (not (featurep 'berkeley-db))
      (error "Berkeley DB support needed to run this command"))
  (if (null vm-folders-summary-database)
      (error "'vm-folders-summary-database' must be non-nil to run this command"))
  (if (null vm-folders-summary-buffer)
      (let ((folder-buffer (and (eq major-mode 'vm-mode)
				(current-buffer))))
	(setq vm-folders-summary-buffer
	      (let ((default-enable-multibyte-characters t))
		(get-buffer-create "VM Folders Summary")))
	(save-excursion
	  (set-buffer vm-folders-summary-buffer)
	  (abbrev-mode 0)
	  (auto-fill-mode 0)
	  (vm-fsfemacs-nonmule-display-8bit-chars)
	  (if (fboundp 'buffer-disable-undo)
	      (buffer-disable-undo (current-buffer))
	    ;; obfuscation to make the v19 compiler not whine
	    ;; about obsolete functions.
	    (let ((x 'buffer-flush-undo))
	      (funcall x (current-buffer))))
	  (vm-folders-summary-mode-internal))
	(vm-make-folders-summary-associative-hashes)
	(vm-do-folders-summary)))
  ;; if this command was run from a VM related buffer, select
  ;; the folder buffer in the folders summary, but only if that
  ;; folder has an entry there.
  (and vm-mail-buffer
       (vm-check-for-killed-folder))
  (save-excursion
    (and vm-mail-buffer
	 (vm-select-folder-buffer))
    (vm-check-for-killed-summary)
    (let ((folder-buffer (and (eq major-mode 'vm-mode)
			      (current-buffer)))
	  fs )
      (if (or (null vm-folders-summary-hash) (null folder-buffer)
	      (null buffer-file-name))
	  nil
	(setq fs (symbol-value (intern-soft (vm-make-folders-summary-key
					     buffer-file-name)
					    vm-folders-summary-hash)))
	(if (null fs)
	    nil
	  (vm-mark-for-folders-summary-update buffer-file-name)
	  (set-buffer vm-folders-summary-buffer)
	  (setq vm-mail-buffer folder-buffer)))))
  (if display
      (save-excursion
	(vm-goto-new-folders-summary-frame-maybe)
	(vm-display vm-folders-summary-buffer t
		    '(vm-folders-summarize)
		    (list this-command) (not raise))
	;; need to do this after any frame creation because the
	;; toolbar sets frame-specific height and width specifiers.
	(set-buffer vm-folders-summary-buffer)
	(vm-toolbar-install-or-uninstall-toolbar))
    (vm-display nil nil '(vm-folders-summarize)
		(list this-command)))
  (vm-update-summary-and-mode-line))

(defvar mail-send-actions)

;;;###autoload
(defun vm-compose-mail (&optional to subject other-headers continue
		        switch-function yank-action
			send-actions)
  (interactive)
  (vm-session-initialization)
  (if continue
      (vm-continue-composing-message)
    (let ((buffer (vm-mail-internal
		   (if to
		       (format "message to %s"
			       (vm-truncate-roman-string to 20))
		     nil)
		   to subject)))
      (goto-char (point-min))
      (re-search-forward (concat "^" mail-header-separator "$"))
      (beginning-of-line)
      (while other-headers
	(insert (car (car other-headers)))
	(while (eq (char-syntax (char-before (point))) ?\ )
	  (delete-char -1))
	(while (eq (char-before (point)) ?:)
	  (delete-char -1))
	(insert ": " (cdr (car other-headers)))
	(if (not (eq (char-before (point)) ?\n))
	    (insert "\n"))
	(setq other-headers (cdr other-headers)))
      (cond ((null to)
	     (mail-position-on-field "To"))
	    ((null subject)
	     (mail-position-on-field "Subject"))
	    (t
	     (mail-text)))
      (funcall (or switch-function (function switch-to-buffer))
	       (current-buffer))
      (if yank-action
	  (save-excursion
	    (mail-text)
	    (apply (car yank-action) (cdr yank-action))
	    (push-mark (point))
	    (mail-text)
	    (cond (mail-citation-hook (run-hooks 'mail-citation-hook))
		  (mail-yank-hooks (run-hooks 'mail-yank-hooks))
		  (t (vm-mail-yank-default)))))
      (make-local-variable 'mail-send-actions)
      (setq mail-send-actions send-actions))))

;;;###autoload
(defun vm-submit-bug-report ()
  "Submit a bug report, with pertinent information to the VM bug list."
  (interactive)
  (require 'reporter)
  (vm-session-initialization)
  ;; Use VM to send the bug report.  Could be trouble if vm-mail
  ;; is what the user wants to complain about.  But most of the
  ;; time we'll be fine and users like to use MIME to attach
  ;; stuff to the reports.
  (let ((reporter-mailer '(vm-mail))
	(mail-user-agent 'vm-user-agent))
    (delete-other-windows)
    (reporter-submit-bug-report
     vm-maintainer-address
     (concat "VM " vm-version)
     (list
      'vm-arrived-message-hook
      'vm-arrived-messages-hook
      'vm-auto-center-summary
      'vm-auto-decode-mime-messages
      'vm-auto-displayed-mime-content-type-exceptions
      'vm-auto-displayed-mime-content-types
;; don't send this by default, might be personal stuff in here.
;;      'vm-auto-folder-alist
      'vm-auto-folder-case-fold-search
      'vm-auto-get-new-mail
      'vm-auto-next-message
      'vm-berkeley-mail-compatibility
      'vm-check-folder-types
      'vm-circular-folders
      'vm-confirm-new-folders
      'vm-confirm-quit
      'vm-convert-folder-types
      'vm-crash-box
      'vm-crash-box-suffix
      'vm-default-From_-folder-type
      'vm-default-folder-permission-bits
      'vm-default-folder-type
      'vm-delete-after-archiving
      'vm-delete-after-bursting
      'vm-delete-after-saving
      'vm-delete-empty-folders
      'vm-digest-burst-type
      'vm-digest-identifier-header-format
      'vm-digest-center-preamble
      'vm-digest-preamble-format
      'vm-digest-send-type
      'vm-display-buffer-hook
      'vm-display-using-mime
      'vm-edit-message-hook
      'vm-edit-message-mode
      'vm-fill-paragraphs-containing-long-lines
      'vm-flush-interval
      'vm-folder-directory
      'vm-folder-read-only
      'vm-folders-summary-database
      'vm-folders-summary-directories
      'vm-folders-summary-format
      'vm-follow-summary-cursor
      'vm-forward-message-hook
      'vm-forwarded-headers
      'vm-forwarding-digest-type
      'vm-forwarding-subject-format
      'vm-frame-parameter-alist
      'vm-frame-per-completion
      'vm-frame-per-composition
      'vm-frame-per-edit
      'vm-frame-per-folder
      'vm-frame-per-folders-summary
      'vm-frame-per-help
      'vm-frame-per-summary
      'vm-highlight-url-face
      'vm-highlighted-header-regexp
      'vm-honor-mime-content-disposition
      'vm-honor-page-delimiters
      'vm-icontopbm-program
      'vm-image-directory
      'vm-imagemagick-convert-program
      'vm-imagemagick-identify-program
;; IMAP passwords might be listed here
;;      'vm-imap-auto-expunge-alist
      'vm-imap-bytes-per-session
      'vm-imap-expunge-after-retrieving
      'vm-imap-max-message-size
      'vm-imap-messages-per-session
      'vm-imap-session-preauth-hook
      'vm-in-reply-to-format
      'vm-included-text-attribution-format
      'vm-included-text-discard-header-regexp
      'vm-included-text-headers
      'vm-included-text-prefix
      'vm-index-file-suffix
      'vm-init-file
      'vm-infer-mime-types
      'vm-invisible-header-regexp
      'vm-jump-to-new-messages
      'vm-jump-to-unread-messages
      'vm-keep-crash-boxes
      'vm-keep-sent-messages
      'vm-lynx-program
      'vm-mail-header-from
      'vm-mail-header-insert-date
      'vm-mail-header-insert-message-id
      'vm-mail-hook
      'vm-mail-check-interval
      'vm-mail-mode-hook
      'vm-mail-send-hook
      'vm-make-crash-box-name
      'vm-make-spool-file-name
      'vm-mime-7bit-composition-charset
      'vm-mime-8bit-composition-charset
      'vm-mime-8bit-text-transfer-encoding
      'vm-mime-alternative-select-method
      'vm-mime-attachment-auto-type-alist
      'vm-mime-attachment-save-directory
      'vm-mime-avoid-folding-content-type
      'vm-mime-base64-decoder-program
      'vm-mime-base64-decoder-switches
      'vm-mime-base64-encoder-program
      'vm-mime-base64-encoder-switches
      'vm-mime-button-face
      'vm-mime-button-format-alist
      'vm-mime-charset-converter-alist
      'vm-mime-charset-font-alist
      'vm-mime-confirm-delete
      'vm-mime-decode-for-preview
      'vm-mime-default-face-charset-exceptions
      'vm-mime-default-face-charsets
      'vm-mime-delete-after-saving
      'vm-mime-delete-viewer-processes
      'vm-mime-digest-discard-header-regexp
      'vm-mime-digest-headers
      'vm-mime-display-function
      'vm-mime-external-content-types-alist
      'vm-mime-forward-local-external-bodies
      'vm-mime-ignore-composite-type-opaque-transfer-encoding
      'vm-mime-ignore-mime-version
      'vm-mime-ignore-missing-multipart-boundary
      'vm-mime-internal-content-type-exceptions
      'vm-mime-internal-content-types
      'vm-mime-max-message-size
      'vm-mime-qp-decoder-program
      'vm-mime-qp-decoder-switches
      'vm-mime-qp-encoder-program
      'vm-mime-qp-encoder-switches
      'vm-mime-require-mime-version-header
      'vm-mime-type-converter-alist
      'vm-mime-use-image-strips
      'vm-mime-use-w3-for-text/html
      'vm-mime-uuencode-decoder-program
      'vm-mime-uuencode-decoder-switches
      'vm-mode-hook
      'vm-mode-hooks
      'vm-mosaic-program
      'vm-mosaic-program-switches
      'vm-move-after-deleting
      'vm-move-after-undeleting
      'vm-move-after-killing
      'vm-move-messages-physically
      'vm-movemail-program
      'vm-mutable-frames
      'vm-mutable-windows
      'vm-netscape-program
      'vm-netscape-program-switches
      'vm-page-continuation-glyph
      'vm-paragraph-fill-column
;; POP passwords might be listed here
;;      'vm-pop-auto-expunge-alist
      'vm-pop-bytes-per-session
      'vm-pop-expunge-after-retrieving
;; POP passwords might be listed here
;;      'vm-pop-folder-alist
      'vm-pop-max-message-size
      'vm-pop-messages-per-session
      'vm-pop-md5-program
      'vm-popup-menu-on-mouse-3
      'vm-preferences-file
      'vm-preview-lines
      'vm-preview-read-messages
      'vm-primary-inbox
      'vm-quit-hook
      'vm-recognize-imap-maildrops
      'vm-recognize-pop-maildrops
      'vm-reply-hook
;; don't feed the spammers or crackers
;;      'vm-reply-ignored-addresses
      'vm-reply-ignored-reply-tos
      'vm-reply-subject-prefix
      'vm-resend-bounced-discard-header-regexp
      'vm-resend-bounced-headers
      'vm-resend-bounced-message-hook
      'vm-resend-discard-header-regexp
      'vm-resend-headers
      'vm-resend-message-hook
      'vm-retrieved-spooled-mail-hook
      'vm-rfc1153-digest-discard-header-regexp
      'vm-rfc1153-digest-headers
      'vm-rfc934-digest-discard-header-regexp
      'vm-rfc934-digest-headers
      'vm-search-using-regexps
      'vm-select-message-hook
      'vm-select-new-message-hook
      'vm-select-unread-message-hook
      'vm-send-digest-hook
      'vm-send-using-mime
      'vm-skip-deleted-messages
      'vm-skip-read-messages
;; don't send vm-spool-files by default, might contain passwords
;;      'vm-spool-files
      'vm-spool-file-suffixes
      'vm-spooled-mail-waiting-hook
      'vm-ssh-program
      'vm-ssh-program-switches
      'vm-ssh-remote-command
      'vm-startup-with-summary
      'vm-strip-reply-headers
      'vm-stunnel-program
      'vm-stunnel-program-switches
      'vm-stunnel-random-data-method
      'vm-subject-significant-chars
      'vm-summary-format
      'vm-summary-highlight-face
      'vm-summary-mode-hook
      'vm-summary-mode-hooks
      'vm-summary-redo-hook
      'vm-summary-show-threads
      'vm-summary-thread-indent-level
      'vm-summary-uninteresting-senders
      'vm-summary-uninteresting-senders-arrow
      'vm-tale-is-an-idiot
      'vm-temp-file-directory
      'vm-thread-using-subject
      'vm-toolbar-pixmap-directory
      'vm-trust-From_-with-Content-Length
      'vm-uncompface-program
      'vm-undisplay-buffer-hook
      'vm-unforwarded-header-regexp
      'vm-url-browser
      'vm-url-browser-switches
      'vm-url-retrieval-methods
      'vm-url-search-limit
      'vm-use-menus
      'vm-use-toolbar
      'vm-virtual-folder-alist
      'vm-virtual-mirror
      'vm-visible-headers
      'vm-visit-folder-hook
      'vm-visit-when-saving
      'vm-warp-mouse-to-new-frame
      'vm-wget-program
      'vm-window-configuration-file
;; see what the user had loaded
      'features
      )
     nil
     nil
     "Please change the Subject header to a concise bug description.\nIn this report, remember to cover the basics, that is, what you expected to\nhappen and what in fact did happen.  Please remove these\ninstructions from your message.")
    (save-excursion
      (goto-char (point-min))
      (mail-position-on-field "Subject"))))
;;      (beginning-of-line)
;;      (delete-region (point) (progn (forward-line) (point)))
;;      (insert "Subject: VM " vm-version " induces a brain tumor in the user.\n         It is the tumor that creates the hallucinations.\n"))))

(defun vm-load-init-file (&optional interactive)
  (interactive "p")
  (if (or (not vm-init-file-loaded) interactive)
      (progn
	(and vm-init-file
	     (load vm-init-file (not interactive) (not interactive) t))
	(and vm-preferences-file (load vm-preferences-file t t t))))
  (setq vm-init-file-loaded t)
  (vm-display nil nil '(vm-load-init-file) '(vm-load-init-file)))

(defun vm-check-emacs-version ()
  (cond ((and vm-xemacs-p
	      (or (< emacs-major-version 19)
		  (and (= emacs-major-version 19)
		       (< emacs-minor-version 14))))
	 (error "VM %s must be run on XEmacs 19.14 or a later version."
		vm-version))
	((and vm-fsfemacs-p
	      (or (< emacs-major-version 19)
		  (and (= emacs-major-version 19)
		       (< emacs-minor-version 34))))
	 (error "VM %s must be run on Emacs 19.34 or a later v19 version."
		vm-version))))

(defun vm-set-debug-flags ()
  (or stack-trace-on-error
      debug-on-error
      (setq stack-trace-on-error
	    '(
	      wrong-type-argument
	      wrong-number-of-arguments
	      args-out-of-range
	      void-function
	      void-variable
	      invalid-function
	     ))))

(defun vm-session-initialization ()
  (require 'vm)
  (vm-note-emacs-version)
  (vm-check-emacs-version)
  (add-hook 'kill-emacs-hook 'vm-garbage-collect-global)
;;  (vm-set-debug-flags)
  ;; If this is the first time VM has been run in this Emacs session,
  ;; do some necessary preparations.
  (if (or (not (boundp 'vm-session-beginning))
	  vm-session-beginning)
      (progn
	(random t)
	(vm-load-init-file)
	(if (not vm-window-configuration-file)
	    (setq vm-window-configurations vm-default-window-configuration)
	  (or (vm-load-window-configurations vm-window-configuration-file)
	      (setq vm-window-configurations vm-default-window-configuration)))
	(setq vm-buffers-needing-display-update (make-vector 29 0))
	(setq vm-buffers-needing-undo-boundaries (make-vector 29 0))
	(add-hook 'post-command-hook 'vm-add-undo-boundaries)
	(if (if (fboundp 'find-face)
		(find-face 'vm-monochrome-image)
	      (facep 'vm-monochrome-image))
	    nil
	  (make-face 'vm-monochrome-image)
	  (set-face-background 'vm-monochrome-image "white")
	  (set-face-foreground 'vm-monochrome-image "black"))
	(if (or (not vm-fsfemacs-p)
		;; don't need this face under Emacs 21.
		(fboundp 'image-type-available-p)
		(facep 'vm-image-placeholder))
	    nil
	  (make-face 'vm-image-placeholder)
	  (if (fboundp 'set-face-stipple)
	      (set-face-stipple 'vm-image-placeholder
				(list 16 16
				      (concat "UU\377\377UU\377\377UU\377\377"
					      "UU\377\377UU\377\377UU\377\377"
					      "UU\377\377UU\377\377")))))
	;; default value of vm-mime-button-face is 'gui-button-face
	;; this face doesn't exist by default in FSF Emacs 19.34.
	;; Create it and initialize it to something reasonable.
	(if (and vm-fsfemacs-p (featurep 'faces)
		 (not (facep 'gui-button-face)))
	    (progn
	      (make-face 'gui-button-face)
	      (cond ((eq window-system 'x)
		     (set-face-foreground 'gui-button-face "black")
		     (set-face-background 'gui-button-face "gray75"))
		    (t
		     ;; use primary color names, since fancier
		     ;; names may not be valid.
		     (set-face-foreground 'gui-button-face "white")
		     (set-face-background 'gui-button-face "red")))))
	;; gui-button-face might not exist under XEmacs either.
	;; This can happen if XEmacs is built without window
	;; system support.  In any case, create it anyway.
	(if (and vm-xemacs-p (not (find-face 'gui-button-face)))
	    (progn
	      (make-face 'gui-button-face)
	      (set-face-foreground 'gui-button-face "black" nil '(win))
	      (set-face-background 'gui-button-face "gray75" nil '(win))
	      (set-face-foreground 'gui-button-face "white" nil '(tty))
	      (set-face-background 'gui-button-face "red" nil '(tty))))
	(and (vm-mouse-support-possible-p)
	     (vm-mouse-install-mouse))
	(and (vm-menu-support-possible-p)
	     vm-use-menus
	     (vm-menu-fsfemacs-menus-p)
	     (vm-menu-initialize-vm-mode-menu-map))
	(setq vm-session-beginning nil))))

(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent 'vm-user-agent
      (function vm-compose-mail)	; compose function
      (function vm-mail-send-and-exit)	; send function
      nil				; abort function (kill-buffer)
      nil)				; hook variable (mail-send-hook)
)

(autoload 'reporter-submit-bug-report "reporter")
(autoload 'timezone-make-date-sortable "timezone")
(autoload 'rfc822-addresses "rfc822")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'mail-fetch-field "mail-utils")
(autoload 'mail-position-on-field "mail-utils")
(autoload 'mail-send "sendmail")
(autoload 'mail-mode "sendmail")
(autoload 'mail-extract-address-components "mail-extr")
(autoload 'set-tapestry "tapestry")
(autoload 'tapestry "tapestry")
(autoload 'tapestry-replace-tapestry-element "tapestry")
(autoload 'tapestry-nullify-tapestry-elements "tapestry")
(autoload 'tapestry-remove-frame-parameters "tapestry")
(autoload 'vm-easy-menu-define "vm-easymenu" nil 'macro)
(autoload 'vm-easy-menu-do-define "vm-easymenu")

(provide 'vm-startup)

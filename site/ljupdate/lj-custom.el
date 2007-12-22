;;; lj-custom.el --- Custom declarations for ljupdate

;; Copyright (C) 2002, 2003, 2004, 2005 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is part of ljupdate, a LiveJournal client for Emacs.

;; ljupdate is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; ljupdate is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;

;;; History:
;;

;;; Code:

(defgroup ljupdate nil
  "Emacs LiveJournal client."
  :group 'processes
  :prefix "lj-"
  :link '(url-link "http://edward.oconnor.cx/ljupdate/")
  :link '(url-link "http://community.livejournal.com/ljupdate/"))

(defcustom lj-cache-dir "~/.ljupdate"
  "Directory in which ljupdate saves cached server information."
  :group 'ljupdate
  :type '(directory))

(defcustom lj-cache-login-information nil
  "If non-null, ljupdate will cache the md5 hashes of your paswords.
You might enable this if you don't want to have to log in each time.
However, be warned that ljupdate won't try to update its other cached
information (your friends groups, your journal access list, etc.) if
this is enabled."
  :group 'ljupdate
  :type '(boolean))

(defcustom lj-fill-function 'lj-fill-by-paragraph
  "We use this function to fill your post contents before sending.
When this function is called, the buffer is narrowed to the body.
Set this to `ignore' to send article contents to the server unaltered.
See `lj-fill.el' for several possible values, or write your own!"
  :group 'ljupdate
  :type '(choice (const :tag "Raw (don't fill)" ignore)
                 (const :tag "Default (by paragraph)" lj-fill-by-paragraph)
                 (const :tag "Pipe through shell command"
                        lj-fill-by-shell-command)
                 (function)))

(defcustom lj-fill-by-shell-command-command "cat"
  "Shell command to pipe your LiveJournal post through.

Your post will be filtered through this command.  The output is what will
actually be posted to your LiveJournal.

This only has an effect when you use \"Pipe through shell command\" as your
Lj Fill Function, above."
  :group 'ljupdate
  :type '(string))

(defcustom lj-default-server "www.livejournal.com"
  "LiveJournal server to use by default in various contexts."
  :group 'ljupdate
  :type '(string))

(defcustom lj-default-username nil
  "Username to use by default in various contexts."
  :group 'ljupdate
  :type '(choice string (const nil)))

(defcustom lj-compose-common-hook nil
  "Normal hook run by `lj-compose-header-mode' and `lj-compose-body-mode'.
Note that this hook will be run each time your cursor moves from the headers
to the body and vice-versa."
  :group 'ljupdate
  :type 'hook)

(defcustom lj-compose-init-headers-hook nil
  "Hook to be run after headers have been added to a composition buffer.
Use this hook to insert additional headers into the buffer. The point is
left after the end of the headers."
  :group 'ljupdate
  :type 'hook)

(defcustom lj-compose-init-body-hook nil
  "Hook to be run after a new composition buffer has been initialized.
Use this hook to insert initial contents into the body of the post. The
point is left at the beginning of the body."
  :group 'ljupdate
  :type 'hook)

(defcustom lj-compose-pre-prepare-body-hook nil
  "Hook run by `lj-compose-prepare-body' before running `lj-fill-function'.
The buffer is narrowed to the body when this hook is run."
  :group 'ljupdate
  :type 'hook)

(defcustom lj-compose-post-prepare-body-hook nil
  "Hook run by `lj-compose-prepare-body' after running `lj-fill-function'.
The buffer is narrowed to the body when this hook is run."
  :group 'ljupdate
  :type 'hook)

(defcustom lj-default-headers ""
  "*A string containing header lines to be inserted in outgoing messages.
It is inserted before you edit the message, so you can edit or delete
these lines."
  :group 'ljupdate
  :type 'string)

;; FIXME: use
(defcustom lj-default-access-level "public"
  "Current possible values are ``public'', ``private'', and ``friends''."
  :group 'ljupdate
  :type '(choice (const :tag "Public (anyone)" "public")
                 (const :tag "Private (you only)" "private")
                 (const :tag "Friends-only" "friends")))

;; FIXME: use
(defcustom lj-allow-comments "yes"
  "Whether or not comments on your posts are allowed by default."
  :group 'ljupdate
  :type '(choice (const :tag "Allow comments" "yes")
                 (const :tag "Disallow comments" "no")))

;; FIXME: use
(defcustom lj-default-mail-notification "yes"
  "Non-nil if you should receive comment notification email by default."
  :group 'ljupdate
  :type '(choice (const :tag "Receive Mail Notification" "yes")
                 (const :tag "Do Not Receive Mail Notification" "no")))

(provide 'lj-custom)

;;; lj-custom.el ends here

;;; ljupdate.el --- a LiveJournal client for Emacs -*- emacs-lisp -*-

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

(require 'lj-custom)

(defconst lj-client-revision "25"
  "The Subversion revision of your ljupdate.")

(defun lj-client-version ()
  (format "%sEmacs-ljupdate/4.0.%s"
          (cond ((featurep 'sxemacs) "SX")
                ((featurep 'xemacs) "X")
                (t ""))
          ;; `lj-client-revision' might be a simple number, or it might
          ;; match X:YM?S?, where X and Y are revision numbers.
          (apply 'max
                 (mapcar 'string-to-number
                         (split-string lj-client-revision "[:MS]")))))

(defconst lj-client-version (lj-client-version)
  "The client version to report to the server.")

(provide 'ljupdate)

;;; Generated autoloads follow (made by autoload.el).
;;; ljupdate.in ends here

;;;### (autoloads (lj-compose lj-compose-mode) "lj-compose" "lj-compose.el"
;;;;;;  (18782 35935))
;;; Generated autoloads from lj-compose.el

(autoload 'lj-compose-mode "lj-compose" "\
Major mode for editing LiveJournal posts.

\(fn)" nil nil)

(add-to-list 'auto-mode-alist '("\\.lj\\'" . lj-compose-mode))

(autoload 'lj-compose "lj-compose" "\
Compose a new LiveJournal post.

\(fn)" t nil)

;;;***

;;;### (autoloads (lj-browse-entries lj-html-decode-string) "lj-edit"
;;;;;;  "lj-edit.el" (18782 35935))
;;; Generated autoloads from lj-edit.el

(autoload 'lj-html-decode-string "lj-edit" "\
Not documented

\(fn STRING)" t nil)

(autoload 'lj-browse-entries "lj-edit" "\
Not documented

\(fn)" t nil)

(defalias 'lj-edit-last 'lj-edit-post)

;;;***

;;;### (autoloads (lj-logout lj-login) "lj-login" "lj-login.el" (18782
;;;;;;  35935))
;;; Generated autoloads from lj-login.el

(autoload 'lj-login "lj-login" "\
Logs into SERVER as USERNAME, and return the md5sum of USERNAME's password.

\(fn SERVER USERNAME)" t nil)

(autoload 'lj-logout "lj-login" "\
Logs off of SERVER (as USERNAME).

\(fn SERVER USERNAME)" t nil)

(defalias 'lj-password 'lj-login)

;;;***

;;;### (autoloads nil nil ("http-cookies.el" "http-post.el" "lj-acct.el"
;;;;;;  "lj-compat.el" "lj-custom.el" "lj-fill.el" "lj-maint.el"
;;;;;;  "lj-pcomplete.el" "lj-protocol.el" "lj-util.el") (18829 3655
;;;;;;  21734))

;;;***

;;;### (autoloads (http-get) "http-get" "http-get.el" (18829 3650))
;;; Generated autoloads from http-get.el

(autoload 'http-get "http-get" "\
Get URL in a buffer, and return the process.
You can get the buffer associated with this process using
`process-buffer'.

The optional HEADERS are an alist where each element has the form
\(NAME . VALUE).  Both must be strings and will be passed along with
the request.

With optional argument SENTINEL, the buffer is not shown.  It is the
responsibility of the sentinel to show it, if appropriate.  A sentinel
function takes two arguments, process and message.  It is called when
the process is killed, for example.  This is useful when specifying a
non-persistent connection.  By default, connections are persistent.
Add (\"Connection\" . \"close\") to HEADERS in order to specify a
non-persistent connection.  Usually you do not need to specify a
sentinel, and `ignore' is used instead, to prevent a message being
printed when the connection is closed.

If you want to filter the content as it arrives, bind
`http-filter-pre-insert-hook' and `http-filter-post-insert-hook'.

The optional argument VERSION specifies the HTTP version to use.  It
defaults to version 1.0, such that the connection is automatically
closed when the entire document has been downloaded.  This will then
call SENTINEL, if provided.  If no sentinel is provided, `ignore' will
be used in order to prevent a message in the buffer when the process
is killed.

CONTENT-TYPE is a coding system to use for the encoding of the url
param value.  Its upper case print name will be used for the server.
Possible values are `iso-8859-1' or `euc-jp' and others.

The coding system of the process is set to `binary', because we need to
distinguish between \\r and \\n.  To correctly decode the text later,
use `decode-coding-region' and get the coding system to use from
`http-headers'.

\(fn URL &optional HEADERS SENTINEL VERSION BUFNAME CONTENT-TYPE)" t nil)

;;;***

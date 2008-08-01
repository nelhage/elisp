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

(defconst lj-client-revision "24"
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

;;;### (autoloads (lj-browse-entries lj-html-decode-string) "lj-edit"
;;;;;;  "lj-edit.el" (18345 18526))
;;; Generated autoloads from lj-edit.el

(autoload (quote lj-html-decode-string) "lj-edit" "\
Not documented

\(fn STRING)" t nil)

(autoload (quote lj-browse-entries) "lj-edit" "\
Not documented

\(fn)" t nil)

(defalias (quote lj-edit-last) (quote lj-edit-post))

;;;***

;;;### (autoloads (lj-logout lj-login) "lj-login" "lj-login.el" (18342
;;;;;;  16766))
;;; Generated autoloads from lj-login.el

(autoload (quote lj-login) "lj-login" "\
Logs into SERVER as USERNAME, and return the md5sum of USERNAME's password.

\(fn SERVER USERNAME)" t nil)

(autoload (quote lj-logout) "lj-login" "\
Logs off of SERVER (as USERNAME).

\(fn SERVER USERNAME)" t nil)

(defalias (quote lj-password) (quote lj-login))

;;;***

;;;### (autoloads nil nil ("lj-acct.el" "lj-compat.el" "lj-custom.el"
;;;;;;  "lj-fill.el" "lj-maint.el" "lj-pcomplete.el" "lj-protocol.el"
;;;;;;  "lj-util.el") (18345 18549 706671))

;;;***

;;;### (autoloads (lj-compose lj-compose-mode) "lj-compose" "lj-compose.el"
;;;;;;  (18342 16766))
;;; Generated autoloads from lj-compose.el

(autoload (quote lj-compose-mode) "lj-compose" "\
Major mode for editing LiveJournal posts.

\(fn)" nil nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.lj\\'" . lj-compose-mode)))

(autoload (quote lj-compose) "lj-compose" "\
Compose a new LiveJournal post.

\(fn)" t nil)

;;;***

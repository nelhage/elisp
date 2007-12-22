;;; lj-login.el --- lj protocol login support for ljupdate

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

(require 'cl)

(require 'lj-compat)
(require 'lj-custom)
(require 'lj-acct)
(require 'lj-protocol)
(require 'lj-util)

;; from ljupdate.el
(eval-when-compile (defvar lj-client-version))

(defvar lj-last-server nil
  "The last LJ server we used during this Emacs session.")

(defvar lj-last-username nil
  "The last LJ username we used during this Emacs session.")

(defun lj-process-login-response (server username info)
  "Process SERVER's login information returned when we logged in as USERNAME.
Argument INFO is the bundle of values returned by the server."

  (let ((name (gethash "name" info))
        (access-count (lj-number (gethash "access_count" info 0)))
        (pickw-count (lj-number (gethash "pickw_count" info 0)))
        (frgrp-maxnum (lj-number (gethash "frgrp_maxnum" info 0)))
        (mood-count (lj-number (gethash "mood_count" info 0)))
        (message (gethash "message" info)))

    (when message
      (message "%s" message)
      (sit-for 2))

    (when name
      (lj-user-put server username :name name))

    (let ((access-list '()))
      (dotimes (access-num access-count)
        (let ((name (gethash (format "access_%d" (1+ access-num)) info)))
          (push name access-list)))
      (lj-user-put server username :access access-list))

    (let ((pickw-list '()))
      (dotimes (pickw-num pickw-count)
        (let ((name (gethash (format "pickw_%d" (1+ pickw-num)) info)))
          (push name pickw-list)))
      (lj-user-put server username :pics pickw-list))

    (let ((frgrp-alist '()))
      (dotimes (frgrp-num frgrp-maxnum)
        (let ((name (gethash (format "frgrp_%d_name" (1+ frgrp-num)) info))
              (sort (gethash (format "frgrp_%d_sortorder" (1+ frgrp-num)) info)))
          (when name
            (push (cons name (1+ frgrp-num)) frgrp-alist))))
      (lj-user-put server username :friends-groups frgrp-alist))

    (let ((mood-max (or (lj-number (lj-server-get server :mood-max)) 0))
          (mood-alist (lj-server-get server :moods)))
      (dotimes (mood-num mood-count)
        (let ((name (gethash (format "mood_%d_name" (1+ mood-num)) info))
              (id (lj-number (or (gethash (format "mood_%d_id" (1+ mood-num)) info)
                                 0))))
          (when (> id mood-max)
            (lj-server-put server :mood-max id))
          (push (cons name id) mood-alist)))
      (lj-server-put server :moods mood-alist))))

(defun lj-attempt-login-once (server username password)
  "Try to log in to SERVER with USERNAME and PASSWORD.
Returns a boolean indicating whether or not the login attempt succeeded.
PASSWORD is the downcased MD5sum of the user's password."
  (message "Logging into `%s' as `%s'. Please wait." server username)

  (let ((challenge (lj-getchallenge server)))
    (let* ((auth-response (lj-md5 (concat challenge password)))
           (response
            (lj-protocol-send-request
             server
             `(("mode"           . "login")
               ("ver"            . ,(if (eq lj-coding-system 'utf-8)
                                        "1"
                                      "0"))
               ("clientversion"  . ,lj-client-version)
               ("user"           . ,username)
               ("auth_method"    . "challenge")
               ("auth_challenge" . ,challenge)
               ("auth_response"  . ,auth-response)
               ("getmoods"       . ,(format "%s"
                                            (or (lj-server-get server :mood-max)
                                                0)))
               ("getpickws"      . "1"))))) ; get userpics
      (if (hash-table-p response)
          (cond ((string= (gethash "success" response) "OK")
                 (lj-process-login-response server username response)
                 t)
                ((string= (gethash "success" response) "FAIL")
                 (message "Logging into `%s' failed; error message is `%s'."
                          server (gethash "errmsg" response))
                 nil)
		(t
                 (message
		  "Logging into `%s' failed (empty response); please try again later."
		  server)))
        (message "Logging into `%s' failed (null response); please try again later."
                 server)
        nil))))

(defun lj-attempt-login (server username explicit-login)
  "Attempt to log into SERVER (as USERNAME) once.

If EXPLICIT-LOGIN is non-nil, the user has requested this login
explicitly, so we message useful feedback to the echo area."
  (let ((tries 0)
        (logged-in nil)
        (password nil))
    (while (and (not logged-in) (< tries 3))
      (setq password
            (lj-md5 (read-passwd
                     (format "Password for %s@%s: " username server))))
      (setq tries (+ tries 1)
            logged-in (lj-attempt-login-once server username password)))
    (if logged-in
        (progn
          (setq lj-last-username username
                lj-last-server   server)
          (lj-user-put server username :password password)
          (when explicit-login
            (message "Successfully logged in as %s@%s." username server))
          password)
      (when explicit-login
        (message "Login failure for %s@%s." username server)
        nil))))

(defun lj-read-server ()
  "Read a server name from the user."
  (let ((guess (or lj-last-server lj-default-server "www.livejournal.com")))
    (completing-read "Server: "
                     (mapcar (lambda (item) (cons item item))
                             (lj-servers))
                     nil nil guess nil guess nil)))

(defun lj-read-username (&optional server)
  "Read a username (of SERVER, if supplied) from the user."
  (completing-read "Username: "
                   (mapcar (lambda (item) (cons item item))
                           (lj-users (or server
                                         lj-last-server
                                         lj-default-server)))
                   nil nil lj-default-username nil lj-default-username nil))

(defun lj-read-server-username-pair ()
  "Read a server and a username at that server from the user."
  (let ((server (lj-read-server)))
    (list server (lj-read-username server))))

;;;###autoload
(defun lj-login (server username)
  "Logs into SERVER as USERNAME, and return the md5sum of USERNAME's password."
  (interactive (lj-read-server-username-pair))
  (or (lj-user-get server username :password)
      (lj-attempt-login server username (interactive-p))
      (error "Unable to log into %s as %s" server username)))

;;;###autoload
(defun lj-logout (server username)
  "Logs off of SERVER (as USERNAME)."
  (interactive (lj-read-server-username-pair))
  (lj-user-rem server username :password))

;; Internally, I call this to get the password for the given user@host.
;; So let's make code calling this easier to read.
;;;###autoload
(defalias 'lj-password 'lj-login)

(provide 'lj-login)

;;; lj-login.el ends here

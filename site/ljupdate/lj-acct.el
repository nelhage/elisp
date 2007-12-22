;;; lj-acct.el --- LiveJournal account handling code for ljupdate

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

;; backing store

(defvar lj-acct-hash nil
  "Hash in which internal account information is stored.")

(defun lj-make-acct-hash ()
  "Create a new value for variable `lj-acct-hash'."
  (make-hash-table :test 'equal))

(defun lj-acct-hash ()
  "Return the hash table in which internal account information is stored.
Will attempt to load our cached configuration if it is available."
  (when (null lj-acct-hash)
    (lj-cache-load))
  (or lj-acct-hash
      (setq lj-acct-hash (lj-make-acct-hash))))

;; sever/user property getters/setters

(defun lj-servers ()
  "Return a list of LiveJournal servers that we know about."
  (let ((servers '()))
    (maphash (lambda (server server-hash)
               (push server servers))
             (lj-acct-hash))
    (nreverse servers)))

(defun lj-server-get (server property)
  "Fetch the value of SERVER's PROPERTY."
  (let ((server-hash (gethash server (lj-acct-hash))))
    (when server-hash
      (gethash property server-hash))))

(defun lj-users (server)
  "Return a list of users on SERVER whose accounts we can use."
  (let ((server-hash (gethash server (lj-acct-hash)))
        (users '()))
    (when server-hash
      (maphash (lambda (user user-hash)
                 (when (and (stringp user)
                            (hash-table-p user-hash))
                   (push user users)))
               server-hash)
      users)))

(defun lj-server-put (server property value)
  "Set SERVER' value of PROPERTY to VALUE."
  (let ((server-hash (gethash server (lj-acct-hash))))
    (unless server-hash
      (setq server-hash (make-hash-table :test 'equal))
      (puthash server server-hash (lj-acct-hash)))
    (puthash property value server-hash)))

(defun lj-server-rem (server property)
  "Remove SERVER's PROPERTY."
  (let ((server-hash (gethash server (lj-acct-hash))))
    (when server-hash
      (remhash property server-hash))))

(defun lj-user-get (server username property)
  "Fetch SERVER's value of USERNAME's PROPERTY."
  (let ((user-hash (lj-server-get server username)))
    (when user-hash
      (gethash property user-hash))))

(defun lj-user-put (server username property value)
  "Set SERVER's value of USERNAME's PROPERTY to VALUE."
  (let ((user-hash (lj-server-get server username)))
    (unless user-hash
      (setq user-hash (make-hash-table :test 'equal))
      (lj-server-put server username user-hash))
    (puthash property value user-hash)))

(defun lj-user-rem (server username property)
  "Remove SERVER's USERNAME's PROPERTY."
  (let ((user-hash (lj-server-get server username)))
    (when user-hash
      (remhash property user-hash))))

;; serialization / deserialization routines

(defun lj-hash-from-alist (alist)
  "Return a new hash table with the same mappings as in ALIST."
  (let ((hash (make-hash-table :test 'equal)))
    (mapcar (lambda (element)
              (puthash (car element) (cdr element) hash))
            alist)
    hash))

(defun lj-alist-from-hash (hash)
  "Return a new alist with the same mapping as in HASH."
  (let ((alist '()))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             hash)
    alist))

;; loading and saving cache

(defun lj-cache-file (&optional filename)
  "Return the absolute path to FILENAME.
If FILENAME is nil, returns the absolute path to the file named
\"cache\" in `lj-cache-dir'."
  (if filename
      (expand-file-name filename)
    (expand-file-name "cache" lj-cache-dir)))

(defun lj-cache-load (&optional filename)
  "Load server and user information out of cache FILENAME.
We use our default cache location if FILENAME is nil."
  (setq filename (lj-cache-file filename))
  (when (file-readable-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (eval-buffer))))

(defvar lj-cache-format 1
  "Version of the cache file format.")

(defun lj-cache-save-forms ()
  "Return Lisp forms which would restore this ljupdate config if evalled."
  (let ((forms '()))
    (push '(setq lj-cache-format 1) forms)
    (push '(setq lj-acct-hash (lj-make-acct-hash)) forms)
    (maphash (lambda (server server-hash)
               (push `(lj-server-put ,server :mood-max
                                     ,(or (lj-server-get server :mood-max)
                                          "0"))
                     forms)
               (push `(lj-server-put ,server :moods
                                     ',(lj-server-get server :moods))
                     forms)
               (maphash (lambda (username user-hash)
                          (when (stringp username)
                            ;; handle users
                            (mapc (lambda (field)
                                    (let ((val (lj-user-get server username field)))
                                      (when val
                                        (push `(lj-user-put
                                                ,server ,username ,field
                                                ;; Conservatively quoting everything
                                                ',val)
                                              forms))))
                                  '(:name :access :pics :friends-groups))
                            (let ((pass (lj-user-get server username :password)))
                              (when (and pass lj-cache-login-information)
                                (push `(lj-user-put
                                        ,server ,username :password
                                        ,pass)
                                      forms)))))
                        server-hash))
             (lj-acct-hash))
    (nreverse forms)))

(defun lj-make-directory (directory &optional parents modes)
  "Create DIRECTORY.
If PARENTS is non-null, create any parent directories as necessary.
If MODES is null, 0700 are used."
  (let ((umask (default-file-modes)))
    (unwind-protect
        (progn
          (set-default-file-modes (or modes ?\700))
          (make-directory directory parents))
      (set-default-file-modes umask))))

(defun lj-cache-save (&optional filename)
  "Save server and user information out to cache FILENAME.
We use our default cache location if FILENAME is nil."
  (setq filename (lj-cache-file filename))
  (let ((dir (file-name-directory filename)))
    (unless (file-exists-p dir)
      (lj-make-directory dir t))
    (unless (file-directory-p dir)
      (error "File `%s' is not a directory" dir)))
  (unless (file-writable-p filename)
    (error "Unable to write to `%s'" filename))
  (find-file filename nil)
  (delete-region (point-min) (point-max))
  (insert ";; -*- emacs-lisp -*-\n"
          ";; ljupdate configuration cache file\n")
  (let ((standard-output (current-buffer)))
    (mapc (lambda (form)
            (prin1 form)
            (terpri))
          (lj-cache-save-forms)))
  (save-buffer)
  (kill-buffer (current-buffer)))

(add-hook 'kill-emacs-hook 'lj-cache-save)

(provide 'lj-acct)
;;; lj-acct.el ends here

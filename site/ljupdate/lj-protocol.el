;;; lj-protocol.el --- "flat" protocol support for ljupdate

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

(require 'http-post)

(require 'lj-compat)
(require 'lj-util)

(defun lj-protocol-server-url (hostname)
  "Return the URL to the LJ protocol's \"flat\" interface on HOSTNAME."
  (concat "http://" hostname "/interface/flat"))

(defsubst lj-this-line ()
  "Return a string containing the current line in the current buffer."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun lj-protocol-send-request (server request)
  "Send to SERVER a REQUEST via the LiveJournal protocol.
If the request succeeds, this returns a hash table whose keys and values
contain the server's response. Or, if the request was unsuccessful, this
returns nil ."
  (let ((process (http-post (lj-protocol-server-url server) request
                            lj-coding-system '(("Connection" . "close"))
                            'ignore 1.0 nil " *LiveJournal response*")))
    (while (accept-process-output process))
    (with-current-buffer (process-buffer process)
      ;; (if (and (stringp http-status-code) (= http-status-code 200)) ; HTTP 200 OK
      (let ((response (make-hash-table :test 'equal))
	    (have-frobbed nil))
        (goto-char (point-min))
        (let ((on-variable-name-line t)
              var)
          (while (< (point) (point-max))
            (cond (on-variable-name-line (setq var (lj-this-line)))
                  (t
		   (puthash var (decode-coding-string (string-make-unibyte (lj-this-line)) lj-coding-system) response)
                   (setq have-frobbed t)))
            (forward-line 1)
            (setq on-variable-name-line (not on-variable-name-line))))
        (if have-frobbed
            (prog1 response
              (kill-buffer (current-buffer)))
          (rename-buffer "*LiveJournal debug*"))))))

(defun lj-getchallenge (server)
  "Get an authentication challenge from SERVER."
  (let* ((response (lj-protocol-send-request
                    server '(("mode" . "getchallenge"))))
         (challenge (and (hash-table-p response) (gethash "challenge" response))))
    (unless (stringp challenge)
      (error "Unable to connect to %s" server))
    challenge))

(provide 'lj-protocol)

;;; lj-protocol.el ends here

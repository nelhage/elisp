;;; lj-edit.el --- post editing for ljupdate
;; Copyright (C) 2002, 2003, 2004, 2005 Edward O'Connor <ted@oconnor.cx>
;; Copyright (C) 2006 Paul Huff <paul.huff@gmail.com>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Author: Paul Huff <paul.huff@gmail.com>
;; Keywords: convenience

;; This file is an addition to ljupdate, a LiveJournal client for Emacs.

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
(require 'message)
(require 'sendmail)

(require 'lj-compose)
(require 'lj-custom)
(require 'lj-acct)
(require 'lj-fill)
(require 'lj-pcomplete)
(require 'lj-protocol)
(require 'lj-login)
(require 'lj-util)

(defun lj-edit-post (&optional edit-itemid)
  (interactive)
  (message (if edit-itemid
	       (concat "Editing id: " edit-itemid)
	     "Editing last post."))
  (let* ((edit-server (or lj-last-server lj-default-server
                          "www.livejournal.com"))
         (edit-username (or lj-last-username lj-default-username ""))
         (edit-request (list '("mode" . "getevents")
                             '("auth_method" . "challenge")
                             '("ver" . "1")
                             '("selecttype" . "one")
                             (cons "itemid" (if edit-itemid 
						edit-itemid
					      "-1"))
                             ))
         (edit-challenge (lj-getchallenge edit-server)))
    (when (string= edit-username "")
      (setq edit-username (read-from-minibuffer (format "Username @%s: " edit-server))))
    (add-to-list 'edit-request (cons "user" edit-username))
    (add-to-list 'edit-request (cons "auth_challenge" edit-challenge))
    (add-to-list 'edit-request
                 (cons "auth_response"
                       (lj-md5 (concat edit-challenge (lj-password edit-server edit-username)))))
    (let ((edit-response (lj-protocol-send-request edit-server edit-request)))
      (with-output-to-temp-buffer "lj-list"
        (set-buffer "lj-list")
        ;;        (lj-list-props response)
        (if (not (eq (get-buffer "*LiveJournal*") nil))
            (kill-buffer (get-buffer "*LiveJournal*")))
        (lj-compose)
        (goto-char (- 1 (lj-compose-find-separator)))
        (lj-add-props edit-response edit-server edit-username)
        (goto-char (+ 1 (lj-compose-find-separator)))
        (insert (replace-regexp-in-string "\r" "" (decode-coding-string (string-make-unibyte (lj-html-decode-string (gethash "events_1_event" edit-response))) lj-coding-system)))
	)))(delete-windows-on "lj-list"))

(defun lj-add-prop (prop value)
  (let ((found-field (message-position-on-field prop)))
    (beginning-of-line)
    (re-search-forward (concat "^" (regexp-quote prop) ":") nil t)
    (kill-region (point) (line-end-position))
    (insert (concat " " value))))

(defun lj-add-props-helper (response n)
  (if (> n 0)
      (progn
        (let ((prop_name (gethash (concat "prop_" (number-to-string n) "_name") response))
              (prop_value (gethash (concat "prop_" (number-to-string n) "_value") response)))
          (cond ((string= prop_name "current_mood") (lj-add-prop "Mood" prop_value))
                ((string= prop_name "current_music") (lj-add-prop "Music" prop_value))
                ((string= prop_name "taglist") (lj-add-prop "Tags" prop_value))
                ((string= prop_name "picture_keyword") (lj-add-prop "Picture" prop_value))
                ((and (string= prop_name "opt_nocomments") (string= prop_value "1"))
                 (lj-add-prop "Allow-Comments" "no"))
                ((and (string= prop_name "opt_noemail") (string= prop_value "1"))
                 (lj-add-prop "Receive-Mail-Notification" "no"))
                ))
        (lj-add-props-helper response (- n 1)))))

(defun lj-add-props (response edit-server edit-username)
  (let ((subject (gethash "events_1_subject" response)))
    (when subject
      (lj-add-prop "Subject" subject)))
  (let ((time (gethash "events_1_eventtime" response)))
    (when time
      (lj-add-prop "Time" time)))
  (let ((itemid (gethash "events_1_itemid" response)))
    (when itemid
      (lj-add-prop "Itemid" itemid)))

  (let* ((access (gethash "events_1_security" response))
         (allowmask (gethash "events_1_allowmask" response)))
    (if (stringp access)
        (cond ((string-match "public" access)
               (lj-add-prop "Access" "public"))
              ((string-match "private" access)
               (lj-add-prop "Access" "private"))
              ((string-match "usemask" access)
               (if (eq (truncate (log (string-to-number allowmask) 2)) 0)
                   (lj-add-prop "Access" "friends")
                 (lj-add-prop "Access" (car (rassoc (truncate (log (string-to-number allowmask) 2)) (lj-user-get edit-server edit-username :friends-groups)))))))))
  (lj-add-props-helper response (string-to-number (gethash "prop_count" response))))
;;;###autoload
;; (defun lj-edit-submit ()
;;   "Submit this entry to the server."
;;   (interactive)
;;   (let* ((buf (current-buffer))
;;          ;; The text of the entry.
;;          (event (lj-compose-prepare-body))

;;          ;; Some convenience variables for oft-used headers
;;          (server (lj-compose-fetch-field "Server"))
;;          (user   (lj-compose-fetch-field "User"))
;;          (itemid (lj-compose-fetch-field "Itemid"))
;;          ;; The current time -- or use the specified time if it exists
;;          (time (lj-compose-fetch-field "Time"))
;;          (timestamp (if (eq nil time)
;;                         ()
;;                       (date-to-time (concat time " " (cadr (current-time-zone))))))

;;          (time (split-string (format-time-string "%Y:%m:%d:%H:%M" timestamp) "[:]"))
;;          (year (pop time))
;;          (month (pop time))
;;          (day (pop time))
;;          (hour (pop time))
;;          (minute (pop time))

;;          ;; LJ Authentication information
;;          challenge

;;          ;; The actual request packet, and the response we receive from
;;          ;; the server.
;;          (request (list '("mode"        . "editevent")
;;                         '("auth_method" . "challenge")
;;                         '("ver"         . "1")
;;                         (cons "itemid" itemid)
;;                         (cons "year" year)
;;                         (cons "mon" month)
;;                         (cons "day" day)
;;                         (cons "hour" hour)
;;                         (cons "min" minute)
;;                         (cons "event" event))))

;;     ;; Build up the request packet.
;;     (add-to-list 'request (cons "user" user))

;;     (let ((subject (lj-compose-fetch-field "Subject")))
;;       (when subject
;;         (add-to-list 'request (cons "subject" subject))))
;;     ;; FIXME: use moodid if available
;;     (let ((mood (lj-compose-fetch-field "Mood")))
;;       (when mood
;;         (add-to-list 'request (cons "prop_current_mood" mood))))

;;     (let ((tags (lj-compose-fetch-field "Tags")))
;;       (when tags
;;         (add-to-list 'request (cons "prop_taglist" tags))))

;;     (let ((music (lj-compose-fetch-field "Music")))
;;       (when music
;;         (add-to-list 'request (cons "prop_current_music" music))))

;;     (let ((community (lj-compose-fetch-field "Community")))
;;       (when community
;;         (add-to-list 'request (cons "usejournal" community))))

;;     (let ((picture (lj-compose-fetch-field "Picture")))
;;       (when picture
;;         (add-to-list 'request (cons "prop_picture_keyword" picture))))

;;     (let ((comments (lj-compose-fetch-field "Allow-Comments")))
;;       (when (and comments (string-match "[Nn][Oo]" comments))
;;         (add-to-list 'request '("prop_opt_nocomments" . "1"))))

;;     (let ((email (lj-compose-fetch-field "Receive-Mail-Notification")))
;;       (when (and email (string-match "[Nn][Oo]" email))
;;         (add-to-list 'request '("prop_opt_noemail" . "1"))))

;;     (let* ((access (lj-compose-fetch-field "Access"))
;;            (friends-group-number
;;             (cdr (assoc access (lj-user-get server user :friends-groups)))))
;;       (if (stringp access)
;;           (cond ((string-match "public" access)
;;                  (add-to-list 'request '("security" . "public")))
;;                 ((string-match "private" access)
;;                  (add-to-list 'request '("security" . "private")))
;;                 ((string-match "friends" access)
;;                  (add-to-list 'request '("allowmask" . "1"))
;;                  (add-to-list 'request '("security" . "usemask")))
;;                 (friends-group-number
;;                  (add-to-list 'request (cons "allowmask"
;;                                              (lj-exp2 friends-group-number)))
;;                  (add-to-list 'request '("security" . "usemask")))
;;                 (t
;;                  (warn "Unable to understand Access: %s; presuming private.")
;;                  (add-to-list 'request '("security" . "private"))))
;;         (add-to-list 'request '("security" . "public"))))

;;     ;; Actually talk to the LJ server.
;;     (message "Connecting to `%s' as `%s'. Please wait." server user)
;;     (setq challenge (lj-getchallenge server))

;;     (add-to-list 'request (cons "auth_challenge" challenge))
;;     (add-to-list 'request
;;                  (cons "auth_response"
;;                        (lj-md5 (concat challenge (lj-password server user)))))

;;     (message "Submitting to `%s' as `%s'. Please wait." server user)

;;     (let ((response (lj-protocol-send-request server request)))
;;       (set-buffer buf) ; return to the *LiveJournal* buffer
;;       (if (and (hash-table-p response)
;;                (string= (gethash "success" response) "OK"))
;;           (progn
;;             (set-buffer-modified-p nil)
;;             (message "Successfully posted as %s." (gethash "url" response))
;;             t)
;;         (let ((errmsg (gethash "errmsg" response)))
;;           (if errmsg
;;               (message "Posting to %s failed: %s" server errmsg)
;;             (message "Posting to %s failed!" server)))
;;         nil))))

(defun lj-html-decode-string (string)
  (interactive)
  (let ((string (replace-regexp-in-string "%\\([0-9A-F]\\{2\\}\\)" (lambda (match) (char-to-string (string-to-number (substring match 1) 16))) string)))
    (replace-regexp-in-string "+" " " string)))

(defun lj-list-props (response n)
  (interactive)
  (if (> n 0)
      (progn
        (insert (gethash (concat "prop_" (number-to-string n) "_name") response))
        (insert "\t")
        (insert (gethash (concat "prop_" (number-to-string n) "_value") response))
        (insert "\n")
        (lj-list-props response (- n 1)))))

(defun lj-insert-entry-into-entry-list (hash n)
  (lexical-let* ((event_string (concat "events_" (number-to-string n)))
                 (event_subject (concat event_string "_subject"))
                 (event_time (concat event_string "_eventtime"))
                 (event_itemid_string (concat event_string "_itemid"))
                 (event_itemid (gethash event_itemid_string hash))
                 (button_start -1)
                 (button_end -1)
                 (which_event n))
    (if (<= n (string-to-number (gethash "events_count" hash)))
        (progn
          (insert-button (concat (gethash event_time hash) " - "
                                 (if (gethash event_subject hash)
                                     (gethash event_subject hash)
                                   "(no subject)")) 'action (lambda (event) (lj-edit-post event_itemid)))
          (insert "\n")
          (lj-insert-entry-into-entry-list hash (+ n 1))))))

(defun lj-get-last-n (n)
  (let* ((server (or lj-last-server lj-default-server
                     "www.livejournal.com"))
         (username (or lj-last-username lj-default-username ""))
         (request (list '("mode" . "getevents")
                        '("auth_method" . "challenge")
                        '("ver" . "1")
                        '("selecttype" . "lastn")
                        (cons "howmany" (number-to-string n))))
         (challenge (lj-getchallenge server)))
    (when (string= username "")
      (setq username (read-from-minibuffer (format "Username @%s: " server))))
    (add-to-list 'request (cons "user" username))
    (add-to-list 'request (cons "auth_challenge" challenge))
    (add-to-list 'request
                 (cons "auth_response"
                       (lj-md5 (concat challenge (lj-password server username)))))
    (let ((response (lj-protocol-send-request server request)))
      (with-output-to-temp-buffer "lj-list"
        (set-buffer "lj-list")
        (lj-insert-entry-into-entry-list response 1)
        (print-help-return-message)))))

;;;###autoload
(defun lj-browse-entries ()
  (interactive)
  (lj-get-last-n 10))

;;;###autoload
(defalias 'lj-edit-last 'lj-edit-post)

(provide 'lj-edit)

(provide 'lj-edit)

;;; lj-edit.el ends here

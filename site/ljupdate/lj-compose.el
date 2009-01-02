;;; lj-compose.el --- post composition for ljupdate

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
(require 'message)
(require 'sendmail)

(require 'lj-custom)
(require 'lj-acct)
(require 'lj-compat)
(require 'lj-fill)
(require 'lj-pcomplete)
(require 'lj-protocol)
(require 'lj-login)
(require 'lj-util)

(eval-when-compile
  ;; for `viper-change-state'
  (require 'viper-cmd)
  ;; from viper-init.el
  (defvar viper-current-state)
  ;; from viper.el
  (defvar viper-mode))

;;; Utilities

(defun lj-compose-fetch-field (field)
  "Return this buffer's value of FIELD."
  (save-excursion
    (save-restriction
      (widen)
      (message-narrow-to-headers)
      (message-fetch-field field))))

(defun lj-this-header ()
  "Return the header of line at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^:]+\\)[:]")
      (match-string 1))))

(defun lj-this-server ()
  "Return the current value of the Server header."
  (lj-compose-fetch-field "Server"))

(defun lj-this-user ()
  "Return the current value of the User header."
  (lj-compose-fetch-field "User"))

;;; Code for submitting this post to LiveJournal.

(defun lj-compose-prepare-body ()
  "Massage this buffer's body for submittal to LiveJournal and return as string."
  (save-excursion
    (save-restriction
      (widen)
      (message-goto-body)
      (narrow-to-region (point) (point-max))
      (run-hooks 'lj-compose-pre-prepare-body-hook)
      (funcall lj-fill-function)
      (run-hooks 'lj-compose-post-prepare-body-hook)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun lj-compose-submit ()
  "Submit this entry to the server."
  (interactive)
  (let* ((buf (current-buffer))
         ;; The text of the entry.
         (event (lj-compose-prepare-body))

         ;; Some convenience variables for oft-used headers
         (server (lj-compose-fetch-field "Server"))
         (user   (lj-compose-fetch-field "User"))

         ;; The current time -- or use the specified time if it exists
         (time-field (lj-compose-fetch-field "Time"))
         (timestamp (if (eq nil time-field)
                        ()
                      (date-to-time (concat time-field " " (cadr (current-time-zone))))))

         (time (split-string (format-time-string "%Y:%m:%d:%H:%M" timestamp) "[:]"))
         (year (pop time))
         (month (pop time))
         (day (pop time))
         (hour (pop time))
         (minute (pop time))

         ;; LJ Authentication information
         challenge

         ;; The actual request packet, and the response we receive from
         ;; the server.
         (request (list '("auth_method" . "challenge")
                        '("ver"         . "1")
                        (cons "year" year)
                        (cons "mon" month)
                        (cons "day" day)
                        (cons "hour" hour)
                        (cons "min" minute)
                        (cons "event" event))))

    ;; Build up the request packet.
    (add-to-list 'request (cons "user" user))
    (let ((itemid (lj-compose-fetch-field "Itemid")))
      (if itemid
          (progn (add-to-list 'request (cons "itemid" itemid))
                 (add-to-list 'request '("mode" . "editevent")))
        (add-to-list 'request '("mode" . "postevent"))))
    (let ((subject (lj-compose-fetch-field "Subject")))
      (when subject
        (add-to-list 'request (cons "subject" subject))))

    ;; FIXME: use moodid if available
    (let ((mood (lj-compose-fetch-field "Mood")))
      (when mood
        (add-to-list 'request (cons "prop_current_mood" mood))))

    (let ((location (lj-compose-fetch-field "Location")))
      (when location
        (add-to-list 'request (cons "prop_current_location" location))))

    (let ((tags (lj-compose-fetch-field "Tags")))
      (when tags
        (add-to-list 'request (cons "prop_taglist" tags))))

    (let ((music (lj-compose-fetch-field "Music")))
      (when music
        (add-to-list 'request (cons "prop_current_music" music))))

    (let ((community (lj-compose-fetch-field "Community")))
      (when community
        (add-to-list 'request (cons "usejournal" community))))

    (let ((picture (lj-compose-fetch-field "Picture")))
      (when picture
        (add-to-list 'request (cons "prop_picture_keyword" picture))))

    (let ((comments (lj-compose-fetch-field "Allow-Comments")))
      (when (and comments (string-match "[Nn][Oo]" comments))
        (add-to-list 'request '("prop_opt_nocomments" . "1"))))

    (let ((preformatted (lj-compose-fetch-field "Preformatted")))
      (when (and preformatted (string-match "[Yy][Ee][Ss]" preformatted))
        (add-to-list 'request '("prop_opt_preformatted" . "1"))))

    (let ((email (lj-compose-fetch-field "Receive-Mail-Notification")))
      (when (and email (string-match "[Nn][Oo]" email))
        (add-to-list 'request '("prop_opt_noemail" . "1"))))

    (let* ((access (lj-compose-fetch-field "Access"))
           (friends-group-number
            (cdr (assoc access (lj-user-get server user :friends-groups)))))
      (if (stringp access)
          (cond ((string-match "public" access)
                 (add-to-list 'request '("security" . "public")))
                ((string-match "private" access)
                 (add-to-list 'request '("security" . "private")))
                ((string-match "friends" access)
                 (add-to-list 'request '("allowmask" . "1"))
                 (add-to-list 'request '("security" . "usemask")))
                (friends-group-number
                 (add-to-list 'request (cons "allowmask"
                                             (lj-exp2 friends-group-number)))
                 (add-to-list 'request '("security" . "usemask")))
                (t
                 (lj-warn
                  "Unable to understand Access: %s; presuming private."
                  access)
                 (add-to-list 'request '("security" . "private"))))
        (add-to-list 'request '("security" . "public"))))

    ;; Actually talk to the LJ server.
    (message "Connecting to `%s' as `%s'. Please wait." server user)
    (setq challenge (lj-getchallenge server))

    (add-to-list 'request (cons "auth_challenge" challenge))
    (add-to-list 'request
                 (cons "auth_response"
                       (lj-md5 (concat challenge (lj-password server user)))))

    (message "Submitting to `%s' as `%s'. Please wait." server user)

    (let ((response (lj-protocol-send-request server request)))
      (set-buffer buf)              ; return to the *LiveJournal* buffer
      (if (and (hash-table-p response)
               (string= (gethash "success" response) "OK"))
          (progn
            (set-buffer-modified-p nil)
            (message "Successfully posted as %s." (gethash "url" response))
            t)
        (let ((errmsg (gethash "errmsg" response)))
          (if errmsg
              (message "Posting to %s failed: %s" server errmsg)
            (message "Posting to %s failed!" server)))
        nil))))

(defun lj-compose-submit-then-exit ()
  "Submit this entry to the server, and exit if successful."
  (interactive)
  (when (lj-compose-submit)
    (quit-window)))

;;; Code for handling the separator between headers and body.

(defvar lj-compose-header/body-marker nil
  "The marker between the lj message's header and body sections.
Anything before this marker will be in `message-mode' and anything below
in `html-mode'.")
(make-variable-buffer-local 'lj-compose-header/body-marker)
(put 'lj-compose-header/body-marker 'permanent-local t)

(defun lj-compose-find-separator ()
  "If non-null, the position of mail-header-separator in this buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-quote mail-header-separator) nil t)))

(defun lj-compose-propertize-separator (&optional pos)
  "Puts the `mail-header-separator' property on the header separator."
  (save-excursion
    (goto-char (or pos (lj-compose-find-separator)))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (put-text-property beg end 'category 'mail-header-separator))))

(defun lj-compose-mark-separator (&optional pos)
  "Initialize `lj-compose-header/body-marker' "
  (set (make-local-variable 'lj-compose-header/body-marker)
       (let ((marker (make-marker))
             (sep-pos (or pos (lj-compose-find-separator))))
         (lj-compose-propertize-separator sep-pos)
         (set-marker marker sep-pos)
         marker)))

;;; Major modes for editing LiveJournal posts.

(defun lj-compose-check-mode ()
  "Ensure we're using the correct major mode for this part of the buffer."
  (let ((there (if (and (boundp 'lj-compose-header/body-marker)
                        (markerp lj-compose-header/body-marker))
                   (marker-position lj-compose-header/body-marker)
                 (lj-compose-mark-separator)))
        (here (point))
        (lj-saved-viper-state (and (boundp 'viper-current-state)
                                   viper-current-state)))
    (cond ((and (< here there)
                (not (eq major-mode 'lj-compose-header-mode)))
           (lj-compose-header-mode))
          ((and (> here there)
                (not (eq major-mode 'lj-compose-body-mode)))
           (lj-compose-body-mode)))
    (when (and (boundp 'viper-mode) viper-mode)
      (viper-change-state lj-saved-viper-state))))

(define-derived-mode lj-compose-header-mode message-mode "LJ:H"
  (mml-mode -1)
  (set (make-local-variable 'message-auto-save-directory) "~/.ljupdate/drafts")
  (lj-pcomplete-setup)
  (define-key lj-compose-header-mode-map "\t" 'pcomplete)
  (run-hooks 'lj-compose-common-hook)
  (add-hook 'post-command-hook 'lj-compose-check-mode nil t))

(define-derived-mode lj-compose-body-mode html-mode "LJ:B"
  (run-hooks 'lj-compose-common-hook)
  (add-hook 'post-command-hook 'lj-compose-check-mode nil t))

;;;###autoload
(defun lj-compose-mode ()
  "Major mode for editing LiveJournal posts."
  (lj-compose-mark-separator)
  (lj-compose-check-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lj\\'" . lj-compose-mode))

;;; Key bindings.

(define-key lj-compose-header-mode-map (kbd "C-c C-s") 'lj-compose-submit)
(define-key lj-compose-body-mode-map (kbd "C-c C-s") 'lj-compose-submit)

(define-key lj-compose-header-mode-map (kbd "C-c C-c") 'lj-compose-submit-then-exit)
(define-key lj-compose-body-mode-map (kbd "C-c C-c") 'lj-compose-submit-then-exit)

;; (define-key lj-compose-body-mode-map (kbd "C-c <TAB>") 'lj-complete-body)

;; Ensure that unwanted Message bindings get shadowed.
;; I should probably do this in a nicer way.
(mapc (lambda (key)
        (define-key lj-compose-header-mode-map key 'undefined))
      (list (kbd "C-c C-a")     (kbd "C-c C-e")       (kbd "C-c C-f a")
            (kbd "C-c C-f s")   (kbd "C-c C-f t")     (kbd "C-c C-f w")
            (kbd "C-c C-f x")   (kbd "C-c C-f C-a")   (kbd "C-c C-f C-b")
            (kbd "C-c C-f C-c") (kbd "C-c C-f C-d")   (kbd "C-c C-f C-f")
            (kbd "C-c C-f C-k") (kbd "C-c C-f C-n")   (kbd "C-c C-f C-o")
            (kbd "C-c C-f C-r") (kbd "C-c C-f C-t")   (kbd "C-c C-f C-u")
            (kbd "C-c C-f <RET>")       ; (kbd "C-c C-f <TAB>")
            (kbd "C-c C-j")     (kbd "C-c C-l")       (kbd "C-c C-n")
            (kbd "C-c C-q")     (kbd "C-c C-r")       (kbd "C-c C-t")
            (kbd "C-c C-u")     (kbd "C-c C-v")       (kbd "C-c C-w")
            (kbd "C-c C-y")     (kbd "C-c C-z")       (kbd "C-c <ESC> f")
            (kbd "C-c <ESC> h") (kbd "C-c <ESC> m")   (kbd "C-c <ESC> n")
            (kbd "C-c <ESC> r") (kbd "C-c <ESC> y") ; (kbd "C-c <TAB>")
            ))

;; Ensure that unwanted HTML mode bindings get shadowed.
(mapc (lambda (key)
        (define-key lj-compose-body-mode-map key 'undefined))
      (list (kbd "C-c C-v")))

;;; `lj-compose' is the major interactive entry point into this file.

;;;###autoload
(defun lj-compose ()
  "Compose a new LiveJournal post."
  (interactive)

  ;; Create the composition buffer.
  (switch-to-buffer (get-buffer-create "*LiveJournal*"))

  (unless (buffer-modified-p)
    (delete-region (point-min) (point-max))
    (lj-compose-populate-buffer)
    (goto-char (point-min))
    (lj-compose-header-mode)
    (if (or lj-last-username lj-default-username)
        (message-position-on-field "Subject")
      (message-position-on-field "User"))))

(defun lj-compose-populate-buffer (&optional values)
  "Populate the current buffer as a LiveJournal post."
  ;; Insert the essential headers.
  (unless (hash-table-p values)
    (setq values (make-hash-table)))
  (insert "Server: " (or (gethash :server values)
                         lj-last-server
                         lj-default-server
                         "www.livejournal.com")
          "\n"

          "User: " (or (gethash :username values)
                       lj-last-username
                       lj-default-username
                       "")
          "\n"

          "Community: " (or (gethash :community values) "") "\n"
          "Mood: " (or (gethash :mood values) "") "\n"
          "Location: " (or (gethash :location values) "") "\n"
          "Picture: " (or (gethash :picture values) "") "\n"
          "Access: " (or (gethash :access values) "public") "\n"
          "Subject: " (or (gethash :subject values) "") "\n"
          "Tags: " (or (gethash :tags values) "") "\n")

  ;; Give the user an opportunity to add additional headers to the
  ;; buffer.
  (insert lj-default-headers)
  (run-hooks 'lj-compose-init-headers-hook)

  (insert mail-header-separator)
  (lj-compose-mark-separator)
  (insert "\n")

  (insert (gethash :body values ""))

  ;; Give the user an opportunity to pre-populate the buffer in some
  ;; way.
  (run-hooks 'lj-compose-init-body-hook)

  ;; The user hasn't actually done anything to this buffer, so it
  ;; shouldn't be marked as modified.
  (set-buffer-modified-p nil))

(provide 'lj-compose)

;;; lj-compose.el ends here

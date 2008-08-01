;;; http-cookies.el --- simple HTTP cookies implementation

;; Copyright (C) 2004, David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.0
;; Keywords: hypermedia

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Implementation of old netscape cookies (used by maybe all servers) and
;; version 1 cookies.
;;
;; See http://www.faqs.org/rfcs/rfc2109.html and
;; http://wp.netscape.com/newsref/std/cookie_spec.html

;;; Change log:

;;; TODO:

;;    - whitelist
;;    - blacklist
;;    - reading from file, saving to file
;;    - expire

;;; Code:

(require 'time-date)

(defconst http-cookies-version "1.0.0")

(defgroup http-emacs ()
  "Simple HTTP client implementation in elisp.")

(defcustom http-emacs-use-cookies nil
  "Use cookies in the http-emacs package. *EXPERIMENTAL*"
  :type 'boolean
  :group 'http-emacs)

(defcustom http-emacs-cookie-file "~/.emacs-cookies"
  "*File where to store the cookies."
  :type 'file
  :group 'http-emacs)

(defconst http-token-value-regexp
  "^[ \t]*\\(.*?\\)[ \t]*=[ \t]*\"?\\(.*?\\)\"?[ \t]*;?[ \t]*$"
  "Regexp to match a token=\"value\"; in a cookie.")

(defvar http-cookies-accept-functions
  '(http-cookie-check-path
    http-cookie-check-domain
    http-cookie-check-hostname)
  "*List of functions used to determine if we accept a cookie or not.
If one of these function returns nil the cookie will be rejected.  Each
function can access the free variables `cookie', `host' (from the url)
`path' (from the URL) and `url' to make its decision.")

(defvar http-cookies-host-hash
  (make-hash-table :test 'equal)
  "Hash to look up cookies by host name.")

(defvar http-cookies-domain-hash
  (make-hash-table :test 'equal)
  "Hash to look up cookies by domain.")



;; functions for parsing the header

(defun http-cookies-ns-to-rfc (line)
  "Make the header value LINE a bit more RFC compatible.
Make old netscape cookies a bit more RFC 2109 compatible by quoting
the \"expires\" value.  We need this to be able to properly split
the header value if there is more than one cookie."
  (let ((start 0))
    (while (string-match "expires[ \t]*=[ \t]*\\([^\";]+?\\)\\(;\\|$\\)"
                         line start)
      (setq start (match-end 0))
      (setq line (replace-match "\"\\1\"" t nil line 1)))
    line))

(defun http-cookies-find-char-in-string (char string &optional start)
  "Return the first position of CHAR in STRING.
If START is non-nil start at position START."
  (unless start
    (setq start 0))
  (let ((i start) (len (length string)) pos)
    (while (and (not pos) (< i len))
      (when (= (aref string i) char)
        (setq pos i))
      (setq i (1+ i)))
    pos))

(defun http-cookies-find-quoted-strings (header-value)
  "Return list of positions of quoted strings in HEADER_VALUE.
Return a list of pairs with the beginning and end of quoted strings
in a \"Set-cookie: \" header value."
  (let ((start 0) qstring-pos)
    (while (string-match "=[ \t]*\\(\".*?[^\\]\"\\)" header-value start)
      (add-to-list 'qstring-pos (cons (match-beginning 1) (1- (match-end 1))))
      (setq start (match-end 1)))
    qstring-pos))

(defun http-cookies-split-string (header-value sep-char)
  "Split the HEADER-VALUE at the character SEP-CHAR.
Ignores SEP-CHAR if it is in a quoted string.  Return a list of the
substrings."
  (let ((qstrings (http-cookies-find-quoted-strings header-value))
        (start 0) (beg 0) pos in-qstring strings)
    (while (setq pos (http-cookies-find-char-in-string
                      sep-char header-value start))
      (unless (= pos start)           ; ignore empty strings
        ;; check if pos is in a quoted string
        (dolist (qstring-pos qstrings)
          (unless in-qstring
            (when (and (> pos (car qstring-pos)) (< pos (cdr qstring-pos)))
              (setq in-qstring t))))
        (if in-qstring
            (setq in-qstring nil)
          (add-to-list 'strings (substring header-value beg pos))
          (setq beg (1+ pos))))
      (setq start (1+ pos)))
    ;; add the last token
    (add-to-list 'strings (substring header-value beg))
    strings))

(defun http-cookies-parse-cookie (string)
  "Parse one cookie.
Return an alist ((NAME . VALUE) (attr1 . value1) (attr2 . value2) ...)
or nil on error."
  (let (attrs error)
    (dolist (attr (http-cookies-split-string string ?\;))
      (if (string-match http-token-value-regexp attr)
          (add-to-list 'attrs (cons (match-string 1 attr)
                                    (match-string 2 attr)))
        ;; match the secure attribute
        (if (string-match "[ \t]*\\([a-zA-Z]+\\)[ \t]*"  attr)
            (add-to-list 'attrs (cons (match-string 1 attr) t))
          (setq error t)
          (message "Cannot parse cookie %s" string))))
    (unless error
      attrs)))

(defun http-cookies-set (url headers)
  "Set the cookies from the response to a request of URL.
Set HEADERS to the headers of the response."
  (let ((host (http-cookies-url-host url)) (path (http-cookies-url-path url))
        header-value cookie)
    ;; The server may send several "Set-Cookie:" headers.
    (dolist (line headers)
      (when (equal (car line) "set-cookie")
        (setq header-value (http-cookies-ns-to-rfc (cdr line)))
        ;; there may be several cookies separated by ","
        (dolist (raw-cookie (http-cookies-split-string header-value ?\,))
          (setq cookie (http-cookies-parse-cookie raw-cookie))
          ;; (message "%s" raw-cookie)
          (when (http-cookies-accept)
            ;; (message "accepted")
            (http-cookies-store host cookie)))))))



;; storing cookies

(defun http-cookies-name (cookie)
  "Return the name of the COOKIE."
  (car (car cookie)))

(defun http-cookies-path (cookie)
  "Return the value of the path attribute of the COOKIE."
  (let ((attr (or (assoc "path" cookie) (assoc "Path" cookie))))
    (when attr
      (cdr attr))))

(defun http-cookies-domain (cookie)
  "Return the value of the domain attribute of the COOKIE."
  (let ((attr (or (assoc "domain" cookie) (assoc "Domain" cookie))))
    (when attr
      (cdr attr))))

(defun http-cookies-expires (cookie)
  "Return the value of the expires attribute of the COOKIE."
  (let ((attr (assoc "expires" cookie)))
    (when attr
      (cdr attr))))

(defun http-cookies-max-age (cookie)
  "Return the value of the Max-Age attribute of the COOKIE."
  (let ((attr (assoc "Max-Age" cookie)))
    (when attr
      (cdr attr))))

(defun http-cookies-version (cookie)
  "Return the value of the version attribute of the COOKIE."
  (let ((version (assoc "Version" cookie)))
    (when version
      (if (equal version "1")
          t
        (message "Cookie version %s not supported." version)
        nil))))

(defun http-cookies-equal (c1 c2)
  "Return non nil if the given cookies are equal.
Old netscape cookies are equal if the name and path attributes are equal.
Version 1 cookies are equal if name path and domain are equal."
  (if (and (http-cookies-version c1) (http-cookies-version c2))
      ;; version 1 cookies
      (and (equal (http-cookies-name c1) (http-cookies-name c2))
           (equal (http-cookies-path c1) (http-cookies-path c2))
           (equal (http-cookies-domain c1) (http-cookies-domain c2)))
    ;; netscape cookies
    (and (equal (http-cookies-name c1) (http-cookies-name c2))
         (equal (http-cookies-path c1) (http-cookies-path c2)))))

(defun http-cookies-expired (expire-string)
  "Return non nil if EXPIRE-STRING is in the past."
  (> (time-to-seconds (time-since expire-string)) 0.0))

(defun http-cookies-remove (cookie key table)
  "Remove cookies \"equal\" to COOKIE from the list stored with KEY in TABLE."
  (let ((cookie-list (gethash key table)) new-list)
    (dolist (entry cookie-list)
      (unless (http-cookies-equal entry cookie)
        (add-to-list 'new-list entry)))
    (when cookie-list
      (remhash key table)
      (puthash key new-list table))))

(defun http-cookies-store (host cookie)
  "Store the given COOKIE from HOST in the hash tables.
Remove cookie from the tables if the given COOKIE expires in the past or
has an \"Max-Age\" of 0."
  (let ((domain (http-cookies-domain cookie))
        (max-age (http-cookies-max-age cookie))
        (expires (http-cookies-expires cookie))
        (cookie-list))
    ;; remove an possible "equal" old cookie
    (http-cookies-remove cookie host http-cookies-host-hash)
    (when domain
      (http-cookies-remove cookie domain http-cookies-domain-hash))
    ;; check if expires is in the past or Max-Age is zero
    (unless (or (and max-age (= (string-to-number max-age) 0))
                (and expires (http-cookies-expired expires)))
      ;; convert "Max-Age" to "expire"
      (when max-age
        ;; this value does not have to be in the "right" format
        ;; it's enough if `parse-time-string' can parse it
        (setq expires (format-time-string
                       "%Y-%m-%d %T %z"
                       (time-add (current-time) (seconds-to-time max-age))
                       t))
        (setcdr (assoc "Max-Age" cookie) expires)
        (setcar (assoc "Max-Age" cookie) "expires"))
      (setq cookie-list (gethash host http-cookies-host-hash))
      (add-to-list 'cookie-list cookie)
      (puthash host cookie-list http-cookies-host-hash)
      (when domain
        (setq cookie-list (gethash domain http-cookies-domain-hash))
        (add-to-list 'cookie-list cookie)
        (puthash domain cookie-list http-cookies-domain-hash)))))



;; building the header to send back the cookie

(defun http-cookies-cookie-to-string (cookie)
  "Return the cookie as a string to be used as a header value."
  (let* ((name (http-cookies-name cookie))
         (value (cdr (assoc name cookie)))
         (path (http-cookies-path cookie))
         (domain (http-cookies-domain cookie))
         (string))
    (if (http-cookies-version cookie)
        ;; version 1 cookie
        (progn
          (setq string (concat "$Version = \"1\"; " name " = \"" value "\""))
          (when path
            (setq string (concat string "; $Path = \"" path "\"")))
          (when domain
            (setq string (concat string "; $Domain = \"" domain "\""))))
      ;; netscape cookies
      (setq string (concat name "=" value)))))

(defun http-cookies-cookie-in-list (cookie list)
  "Return non-nil if a cookie \"equal\" to the given COOKIE is in LIST."
  (let ((in-list))
    (dolist (element list)
      (unless in-list
        (setq in-list (http-cookies-equal cookie element))))
    in-list))

(defun http-cookies-path-depth (cookie)
  "Return the number of dashes in the path attribute of the cookie."
  (let ((patch http-cookies-path cookie) (n 0) (start 0))
    (while (setq start (http-cookies-find-char-in-string ?\/ path start))
      (setq n (1+ n)))
    n))

(defun http-cookie-path-depth-less (c1 c2)
  "Return non nil if the path depth of cookie C1 is less than C2."
  (< (http-cookies-path-depth c1) (http-cookies-path-depth c2)))

(defun http-cookies-build-header (url)
  "Return a pair (\"Cookie\" . <header value>).
Use this to send back cookies to the given URL."
  (let ((host (http-cookies-url-host url)) (domain) (cookie-list) (string))
    (when (string-match "^[^.]+\\(\\..+\\)" host)
      (setq domain (match-string 1 host))
      (dolist (cookie (gethash host http-cookies-host-hash))
        (unless (http-cookies-expired (http-cookies-expires cookie))
          (add-to-list 'cookie-list cookie)))
      (dolist (cookie (gethash domain http-cookies-domain-hash))
        (unless (or (http-cookies-cookie-in-list cookie cookie-list)
                    (http-cookies-expired (http-cookies-expires cookie)))
          (add-to-list 'cookie-list cookie)))
      (setq cookie-list (sort cookie-list 'http-cookies-path-depth-less))
      (dolist (cookie cookie-list)
        (if string
            (setq string (concat string "; "
                                 (http-cookies-cookie-to-string cookie)))
          (setq string (http-cookies-cookie-to-string cookie)))))
    (cons "Cookie" string)))



;; extract parts of the url

(defun http-cookies-url-host (url)
  "Return the hostname of URL"
  (unless (string-match
           "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
           url)
    (error "Cannot parse URL %s." url))
  (match-string 1 url))

(defun http-cookies-url-path (url)
  "Return the path of the URL."
  (unless (string-match
           "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
           url)
    (error "Cannot parse URL %s." url))
  (concat "/" (or (match-string 4 url) "")))



;; functions to check the cookie (implementation of 4.3.2 of RFC 2109)

(defun http-cookies-accept ()
  "Return non nil if the cookie should be accepted.
The tests are based on the functions in `http-cookies-accept-functions'."
  (let ((accept t))
    (dolist (fun http-cookies-accept-functions)
      (when accept
        (setq accept (funcall fun))))
    accept))

(defun http-cookie-check-path ()
  "Return nil if the \"path\" attribute is not a prefix of th URL."
  (let ((cookie-path (cdr (assoc "path" cookie))))
    (if cookie-path
        (if (string-match (concat "^" cookie-path) path)
            t
          (message "Rejecting cookie: path attribute \"%s\" is not a prefix\
 of the URL %s." cookie-path url)
          nil)
      t)))

(defun http-cookie-check-domain ()
  "Return nil if the domain is bogus.
Return nil if the domain does not start with a \".\" or does not contain
an embedded dot."
  (let ((domain (cdr (assoc "domain" cookie))))
    (if domain
        (if (string-match "^\\.[^.]+\\.[^.]+" domain)
            t
          (message "Rejection cookie: domain \"%s\" does not start with a dot\
 or does not contain an embedded dot." domain)
          nil)
      t)))

(defun http-cookie-check-hostname ()
  "Return nil if the domain doesn't match the host.
Return nil if the domain attribute does not match the host name or the
host name without the domain attribute still contains one or more dots."
  ;; FIXME: hostname might be an IP address
  (let ((domain (cdr (assoc "domain" cookie))))
    (if (not domain)
        t
      (when (string-match (concat domain "$") host)
        (not (http-cookies-find-char-in-string
              ?\. (substring host 0 (match-beginning 0))))))))



(provide 'http-cookies)

;;; http-cookies.el ends here

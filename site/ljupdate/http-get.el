;;; http-get.el --- simple HTTP GET

;; Copyright (C) 2002, 2003 Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;;         Pierre Gaston <pierre@gaston-karlaouzou.com>
;;         David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.15
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?HttpGet

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

;; Use `http-get' to download an URL.

;;; Change log:

;; 1.0.15
;;   - made `http-parse-headers' RFC 2616 compatible (removing whitespaces,
;;     headers may spawn several line)
;;   - log message headers
;;   - made most variables buffer local with `make-variable-buffer-local'
;; 1.0.14
;;   - Removed attempt to fix bug in 1.0.12, not needed anymore since 1.0.13.
;; 1.0.13
;;   - The string is now not anymore decoded in the http-filter.
;;     You have to run `http-decode' yourself.
;; 1.0.12
;;   - Hopefully fixed the bug with inserting "half" multi byte chars.
;; 1.0.11
;;   - Added (setq string (string-make-unibyte string)) to http-filter
;;     this seems to solve problems with multi byte chars.
;;   - Fixed bug when building the headers.
;;   - Fixed indentation (please guys, read the coding conventions in the
;;     elisp manual)
;;   - Replaced string-bytes with length (string-bytes shouldn't be needed
;;     anymore as we force the string to be unibyte)
;; 1.0.10
;;   - Fix some codings problems again.
;; 1.0.9
;;   - Added better coding support.
;; 1.0.8
;;   - Rewrote the parser.
;;   - Correction to the http 1.0 usage.
;; 1.0.3
;;   - Move http-url-encode from http-post.el to http-get.el.
;;   - Add a param to http-get to specify the encoding of the params in the url.

;;; Code:

(require 'hexl)
(require 'http-cookies)

(defvar http-get-version "1.0.15")

;; Proxy
(defvar http-proxy-host nil
  "*If nil dont use proxy, else name of proxy server.")

(defvar http-proxy-port nil
  "*Port number of proxy server.  Default is 80.")

(defvar http-coding 'iso-8859-1
   "Default coding to be use when the string is inserted in the buffer.
This coding will be modified on Finding the content-type header")
(make-variable-buffer-local 'http-coding)

(defvar  http-filter-pre-insert-hook '(http-parser)
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, before it is
inserted into the buffer.  If you want to modify the string that gets
inserted, modify the variable `string' which is dynamically bound to
what will get inserted in the end.  The string will be inserted at
the `process-mark', which you can get by calling \(process-mark proc).
`proc' is dynamically bound to the process, and the current buffer
is the very buffer where the string will be inserted.")

(defvar http-filter-post-insert-hook  nil
  "Hook run by the `http-filter'.
This is called whenever a chunk of input arrives, after it has been
inserted, but before the `process-mark' has moved.  Therefore, the new
text lies between the `process-mark' and point.  You can get the values
of the `process-mark' by calling (process-mark proc).  Please take care
to leave point at the right place, eg.  by wrapping your code in a
`save-excursion'.")

(defun http-filter (proc string)
  "Filter function for HTTP buffers.
See `http-filter-pre-insert-hook' and `http-filter-post-insert-hook'
for places where you can do your own stuff such as HTML rendering.
Argument PROC is the process that is filtered.
Argument STRING is the string outputted by the process."
  ;; emacs seems to screw this sometimes
  (when (fboundp 'string-make-unibyte)
    (setq string (string-make-unibyte string)))
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	" Insert the text, advancing the process marker."
	(goto-char (process-mark proc))
	(run-hooks 'http-filter-pre-insert-hook)
        ;; Note: the string is inserted binary in a unibyte buffer
        (insert string)
	(run-hooks 'http-filter-post-insert-hook)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defvar http-status-code nil
  "The status code returned for the current buffer.
This is set by the function `http-headers'.")
(make-variable-buffer-local 'http-status-code)

(defvar http-reason-phrase nil
  "The reason phrase returned for the `http-status-code'.
This is set by the function `http-headers'.")
(make-variable-buffer-local 'http-reason-phrase)

(defvar http-headers nil
  "An alist of the headers that have been parsed and removed from the buffer.
The headers are stored as an alist.
This is set by the function `http-headers'.")
(make-variable-buffer-local 'http-headers)

(defvar http-parser-state 'status-line
  "Parser status.")
(make-variable-buffer-local 'http-parser-state)

(defvar http-unchunk-chunk-size  0
  "Size of the current unfinished chunk.")
(make-variable-buffer-local 'http-unchunk-chunk-size)

(defvar http-not-yet-parsed  ""
  "Received bytes that have not yet been parsed.")
(make-variable-buffer-local 'http-not-yet-parsed)

(defvar http-host ""
  "The host to which we have sent the request.")
(make-variable-buffer-local 'http-host)

(defvar http-url ""
  "The requested URL.")
(make-variable-buffer-local 'http-url)

(defun http-parser ()
  "Simple parser for http message.
Parse the status line, headers and chunk."
  (let ((parsed-string (concat http-not-yet-parsed string)) content-type)
    (setq string "")
    (setq http-not-yet-parsed "")
    (while (> (length parsed-string) 0)
      (cond

       ((eq http-parser-state 'status-line)
	;; parsing status line
	(if (string-match "HTTP/[0-9.]+ \\([0-9]+\\) \\(.*\\)\r\n"
                          parsed-string)
	    (progn
              (setq http-status-code
                    (string-to-number (match-string 1 parsed-string)))
              (setq http-reason-phrase (match-string 2 parsed-string))
              (setq http-parser-state 'header)
              (setq parsed-string (substring parsed-string (match-end 0))))
	  ;; status line not found
          (setq http-not-yet-parsed parsed-string)
          (setq parsed-string "")))

       ((eq http-parser-state 'header)
	;; parsing headers
	(if (string-match "\r\n\r\n" parsed-string)
	    (let ((end-headers (match-end 0)))
	      (setq http-headers
                    (http-parse-headers
                     (substring parsed-string 0 (match-beginning 0))))
	      (if (string= "chunked"
			   (cdr (assoc "transfer-encoding" http-headers)))
		  (setq http-parser-state 'chunked)
		(setq http-parser-state 'dump))
	      (when (and
		     (setq content-type
			   (cdr (assoc "content-type" http-headers)))
		     (string-match "charset=\\(.*\\)" content-type))
		(setq http-coding
                      (intern-soft (downcase (match-string 1 content-type)))))
	      (setq parsed-string (substring parsed-string end-headers))
              ;; set cookies
              (when http-emacs-use-cookies
                  (http-cookies-set http-url http-headers)))
	  ;; we don't have all the headers yet
	  (setq http-not-yet-parsed parsed-string)
	  (setq parsed-string "")))

       ((eq http-parser-state 'chunked)
	;; parsing chunked content
        (if (> (length parsed-string) http-unchunk-chunk-size)
	    (progn
	      (setq string (concat string
                                   (substring parsed-string 0
                                              http-unchunk-chunk-size)))
	      (setq parsed-string
                    (substring parsed-string http-unchunk-chunk-size))
	      (setq http-unchunk-chunk-size 0)

	      (if (string-match  "\\([0-9a-f]+\\)[^\r^\b]*\\(\r\n\\)"
                                 parsed-string)
		  (if (> (setq http-unchunk-chunk-size
                               (hexl-hex-string-to-integer
                                (match-string 1 parsed-string)))
			 0)
		      (setq parsed-string
                            (substring parsed-string (match-end 2)))
		    ;; chunk 0 found we just burry it
		    (setq parsed-string "")
		    (setq http-parser-state 'trailer))
                ;; we don't have the next chunk-size yet
		(setq http-not-yet-parsed parsed-string)
		(setq parsed-string "")))
	  ;; the current chunk is not finished yet
	  (setq string  (concat string parsed-string))
	  (setq http-unchunk-chunk-size
                (- http-unchunk-chunk-size (length parsed-string)))
          (setq parsed-string "")))

       ((eq http-parser-state 'trailer)
        ;; parsing trailer
        (setq  parsed-string ""))

       ((eq http-parser-state 'dump)
	 (setq  string parsed-string)
	 (setq  parsed-string ""))))))


(defun http-parse-headers (header-string)
  "Parse the header string.
Argument HEADER-STRING A string containing a header list."
  ;; headers may spawn several line if the nth, n>1, line starts with
  ;; at least one whitespace
  (setq header-string (replace-regexp-in-string "\r\n[ \t]+" " "
                                                header-string))
  (let ((lines-list (split-string header-string "\r\n")))
    (mapcar (lambda (line)
	      (if (string-match ":[ \t]+\\(.*?\\)[ \t]*$" line)
                    (cons (downcase (substring line 0 (match-beginning 0)))
                          (match-string 1 line))
		line))
            lines-list)))


;; URL encoding for parameters
(defun http-url-encode (str content-type)
  "URL encode STR using CONTENT-TYPE as the coding system."
  (apply 'concat
	 (mapcar (lambda (c)
		   (if (or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (string c)
		     (format "%%%02x" c)))
		 (encode-coding-string str content-type))))


(defun http-decode-buffer ()
  "Decode buffer according to the buffer local variable `http-coding'."
  (when (and
	 (fboundp 'set-buffer-multibyte)
	 (fboundp 'multibyte-string-p))
    (when (multibyte-string-p (decode-coding-string "test" http-coding))
      (set-buffer-multibyte t)))
  (decode-coding-region (point-min) (point-max) http-coding))

;; Debugging
(defvar http-log-function 'ignore
  "Function to call for log messages.")

(defun http-log (str)
  "Log STR using `http-log-function'.
The default value just ignores STR."
  (funcall http-log-function str))


(defun http-get-debug (url &optional headers version)
  "Debug the call to `http-get'."
  (interactive "sURL: ")
  (let* ((http-log-function (lambda (str)
			      (save-excursion
				;; dynamic binding -- buf from http-get is used
				(set-buffer buf)
				(insert str))))
	 proc)
    (when (get-buffer "*Debug HTTP-GET*")
      (kill-buffer "*Debug HTTP-GET*"))
    (setq proc (http-get url headers nil version))
    (set (make-local-variable 'http-filter-pre-insert-hook) nil)
    (set (make-local-variable 'http-filter-post-insert-hook) nil)
    (rename-buffer "*Debug HTTP-GET*")))


;; The main function

;;;###autoload
(defun http-get (url &optional headers sentinel version bufname content-type)
  "Get URL in a buffer, and return the process.
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
Add \(\"Connection\" . \"close\") to HEADERS in order to specify a
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
`http-headers'."
  (interactive "sURL: ")
  (setq version (or version 1.0))
  (let* (host dir file port proc buf command start-line (message-headers "") )
    (unless (string-match
	     "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
             url)
      (error "Cannot parse URL %s." url))
    (unless bufname
      (setq bufname (format "*HTTP GET %s *" url)))

    (setq host (match-string 1 url)
	  port (or (and (setq port (match-string 3 url))
                        (string-to-int port)) 80)
	  dir (or (match-string 4 url) "")
	  file (or (match-string 5 url) "")
	  buf (get-buffer-create bufname)
	  proc (open-network-stream
                (concat "HTTP GET " url) buf
                (if http-proxy-host http-proxy-host host)
                (if http-proxy-port http-proxy-port port) ))
    (if sentinel
	(set-buffer buf)
      (switch-to-buffer buf))
    (erase-buffer)
    (kill-all-local-variables)
    (with-current-buffer buf
      (setq http-host host)
      (setq http-url url))
    (if content-type
	(setq file
	      (replace-regexp-in-string
	       "=[^&]+"
	       (lambda (param)
		 (concat "="
			 (http-url-encode (substring param 1) content-type)))
	       file)))
    (setq start-line
	  (concat (format "GET %s%s%s HTTP/%.1f\r\n"
                          (if http-proxy-host
                              (concat "http://" host "/") "/") dir file version)
		  (format "Host: %s\r\n" host)))
    (when http-emacs-use-cookies
      (let ((cookie (http-cookies-build-header url)))
        (when cookie (add-to-list 'headers cookie))))
    (when headers
      (setq message-headers (mapconcat (lambda (pair)
					 (concat (car pair) ": " (cdr pair)))
				       headers
				       "\r\n")))
    ;; mapconcat doesn't append the \r\n for the final line
    (setq command (format "%s%s\r\n\r\n" start-line message-headers))
    (http-log (format "Connecting to %s %d\nCommand:\n%s\n" host port command))
    (http-log message-headers)
    (set-process-sentinel proc (or sentinel 'ignore))
    (set-process-coding-system proc 'binary 'binary) ; we need \r\n
    ;; we need this to be able to correctly decode the buffer with
    ;; decode-coding-region later
    (when (fboundp 'set-buffer-multibyte)
      (with-current-buffer buf (set-buffer-multibyte nil)))
    (set-process-filter proc 'http-filter)
    (set-marker (process-mark proc) (point-max))
    (process-send-string proc command)

    proc))


;; needed for xemacs.  c&p from gnu emacs cvs sources
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string &optional
                                          fixedcase literal subexp start)
    (let ((l (length string))
          (start (or start 0))
          matches str mb me)
      (save-match-data
        (while (and (< start l) (string-match regexp string start))
          (setq mb (match-beginning 0)
                me (match-end 0))
          (when (= me mb) (setq me (min l (1+ mb))))
          (string-match regexp (setq str (substring string mb me)))
          (setq matches
                (cons (replace-match (if (stringp rep)
                                         rep
                                       (funcall rep (match-string 0 str)))
                                     fixedcase literal str subexp)
                      (cons (substring string start mb)
                            matches)))
          (setq start me))
        (setq matches (cons (substring string start l) matches))
        (apply #'concat (nreverse matches))))))

(provide 'http-get)

;;; http-get.el ends here

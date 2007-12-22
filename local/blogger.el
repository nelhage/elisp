;;; blogger.el - Post to blogs that support the Blogger API.

;; Copyright (C) 2002 Mark A. Hershberger.
;; Based on code Copyright (C) 2001 by Simon Kittle.

;; Author: mah@everybody.org
;; Version: 1.0

;; This file is not yet part of GNU Emacs.

;; blogger.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; blogger.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; blogger.el implements the Blogger API to talk to weblog software.
;;
;; For ease of use:
;; I use the following commands in my .emacs file:
;;
;; (load-file "blogger.el")
;; (global-set-key "\C-cbs" 'blogger-start-post)
;;
;; C-c b s will switch to a new buffer where you can compose a
;; post.
;;
;; C-x C-s    -- post-and-publish current buffer to the blog.
;;               Calling blogger-save-post with an prefix argument
;;               (i.e. C-u C-x C-s) will prompt for which weblog
;;               to use.
;;
;; C-c C-c    -- identical to C-x C-s, but will also bury the buffer.
;;
;; C-c C-n    -- post (but not publish) the current message and
;;               load the next message.
;;
;; C-c C-p    -- post (but not publish) the current message and
;;               load the previous message.
;;
;; C-c C-k    -- delete the current post.
;;
;; M-g        -- synchronise blogger.el's idea of the posts available
;;               with the blogger server.
;;
;; C-c C-t m  -- edit the main template.
;;
;; C-c C-t a  -- edit the Archive Index template
;;
;;
;; Notes:
;; ----
;;
;; This code is based on Simon Kittle's blogger.el
;; (http://www.tswoam.co.uk/files/blogger.el.txt), but where his
;; code calls a Perl program, this code uses xml-rpc.el.  As of
;; this writing, my mods to xml-rpc.el are needed to add support
;; for boolean types.  You can get xml-rpc.el from
;; <http://mah.everybody.org/hacks/emacs/xml-rpc.el.txt>.
;;
;; Differences between SK's blogger.el and this one:
;;
;; - Doesn't need any external programs.  Uses xml-rpc.el.
;; - I've added a bunch of defcustom's here to make this integrate
;;   better with Emacs Customization interface. 
;; - Created a *blogger* mode.
;; - Made selection of a blog more intuitive.  It queries the
;;   Blogger server and allows the user to choose the name of the
;;   blog from a list.
;; - Prompt for blog on initial post if blogger-id isn't set.
;; - Can "ping" http://weblogs.com/ and http://blo.gs/ whenever
;;   you update.
;; - Can "scroll" through posts on the blogger server and edit them.
;;
;; Todo:
;;
;;  * RSS creation
;;  * Implement Manila interface.
;;
;; Bugs:
;;
;;  * When you delete a post it gets deleted, but it doesn't
;;  disappear from your post ring until you sync (M-g) with the server.


(require 'xml-rpc)
(require 'ring)

(defgroup blogger nil
  "Edit Blogger sites with Emacs."
  :group 'emacs)

(defcustom blogger-app-key "07C72E6970E0FBA5DE21BA9F4800C44534C19870"
  "The appkey to send to blogger.  Generally this shouldn't be changed."
  :group 'blogger
  :type 'string)

(defcustom blogger-username nil
  "Your blogger username.  You will be prompted if this is left nil."
  :group 'blogger
  :type 'string)

(defcustom blogger-password nil
  "Your password.  You will be prompted if this is left nil."
  :group 'blogger
  :type 'string)

(defcustom blogger-url "http://plant.blogger.com/api/RPC2"
  "Server you want to Use.  If this is a Blogger site, leave this
at the default.  Otherwise, you will need to change it."
  :group 'blogger
  :type 'string)

(defcustom blogger-id nil
  "Your Blog ID.  If the server is a blogger server, you can
leave this nil and select which blog you wish to post to at
post-time.  If it is a Manila site, you need to provide the URL
of your site."
  :group 'blogger
  :type 'string)

(defcustom blogger-max-entries 20
  "Maximum number of posts that can be editted.  There may be a
server-side limitation to this number."
  :group 'blogger
  :type 'string)

(defcustom blogger-weblog-ping-urls '("http://rpc.weblogs.com/RPC2")
  "List of URLs to ping using the XML-RPC interface defined at 
<http://www.xmlrpc.com/weblogsCom>."
  :group 'blogger
  :type 'list)

(defcustom blogger-default-message-count 5
  "Default number of messages to retrieve at a time from blogger"
  :group 'blogger
  :type 'integer)

(defvar blogger-message-list nil
  "List of blogger messages that we know about. Chronological
order, with newest first.")

(defvar blogger-userid nil
  "ID of logged in user.")

(defvar *blogger* nil
  "The blogger buffer where we compose posts")

(defvar blogger-mode-hook nil
  "Hook to run after starting up blogger mode.")

(defvar blogger-new-post-hook '(blogger-ping-weblogs)
  "Hook to run after sending a new post.  Typically, this is
where you would put blogger-ping-weblogs to let weblog
aggregators know that you have updated.")

(defvar blogger-edit-post-hook nil
  "Hook to run after updating a new post.")

(defvar blogger-mode-map nil
  "Keymap for blogger-mode.")

(defvar blogger-post-ring nil
  "Ring that holds all the posts")

(defvar blogger-ring-index 0
  "Pointer to the index on the ring")

(defvar blogger-current-post nil
  "Holds the post from the ring that is currently being editted.")

(unless blogger-mode-map
  (setq blogger-mode-map (copy-keymap text-mode-map))
  (define-key blogger-mode-map "\C-c\C-c" 'blogger-send-post)
  (define-key blogger-mode-map "\C-x\C-s" 'blogger-save-post)
  (define-key blogger-mode-map "\C-c\C-n" 'blogger-next-post)
  (define-key blogger-mode-map "\C-c\C-p" 'blogger-prev-post)
  (define-key blogger-mode-map "\C-c\C-k" 'blogger-delete-post)
  (define-key blogger-mode-map "\M-g"     'blogger-refetch-posts))

;;;###autoload
(defun blogger-mode ()
  "Major mode for editing text for Blogger.  Based on text-mode."
  (interactive)
  (text-mode)
  (use-local-map blogger-mode-map)
  (setq mode-name "blogger")
  (setq major-mode 'blogger-mode)
  (setq blogger-post-ring (make-ring blogger-max-entries))
  (run-hooks 'blogger-mode-hook))

;;;###autoload
(defun blogger-start-post()
  "*Start creating a blogger post in the *blogger* buffer"
  (interactive)
  (setq *blogger* (switch-to-buffer "*blogger*"))
  (blogger-mode)
  (setq blogger-ring-index nil)
  (erase-buffer))

;;;###autoload
(defun blogger-send-post (&optional arg)
  "Publish the current message.  With optional argument prompts
for blog to use."
  (interactive)
  (blogger-save-post arg)
  (bury-buffer))

;;;###autoload
(defun blogger-save-post (&optional arg)
  "Publish the current message.  With optional argument prompts
for blog to use."
  (interactive)
  (if (not (equal (current-buffer) *blogger*))
      (message 
       "You are not in the *blogger* buffer.")
    (cond ((buffer-modified-p)
	   (blogger-username arg)
	   (blogger-password arg)
	   (blogger-id arg)
	   (cond (blogger-ring-index
		  (setcdr
		   (assoc 'content 
			  (ring-ref blogger-post-ring blogger-ring-index))
		   (buffer-string))
		  (blogger-send-edits t)
		  (set-buffer-modified-p nil))
		 (t (blogger-new-post (buffer-string) t))))
	   (t (message "Nothing to post")))))

(defun blogger-username (&optional prompt)
  "Get the username.  If you've not yet logged in then prompt for
it"
  (setq blogger-username
	(if (or prompt (not blogger-username))
	      (read-from-minibuffer "Username: " blogger-username)
	    blogger-username)))

(defun blogger-password (&optional prompt)
  "Get the password.  If you've not yet logged in then prompt for
it"
  (setq blogger-password
	(if (or prompt (not blogger-password))
	    (if blogger-password
		(read-passwd "Password for blog: " nil blogger-password)
	      (read-passwd "Password for blog: " nil))
	    blogger-password)))

(defun blogger-id (&optional prompt)
  "Get the blogger ID."
  (setq blogger-id
	(if (or (not blogger-id) prompt)
	    (blogger-select-blog)
	  blogger-id)))
 
(defun blogger-send-edits (publishp)
  "Posts edits to a message specified by msgid.  If publishp is
non-nil, publishes the message as well."
  (xml-rpc-method-call
   blogger-url
   'blogger.editPost
   blogger-app-key
   (cdr (assoc 'postid (ring-ref blogger-post-ring blogger-ring-index)))
   (blogger-username)
   (blogger-password)
   (url-insert-entities-in-string 
    (cdr (assoc 'content (ring-ref blogger-post-ring blogger-ring-index))))
   publishp)
  (run-hooks 'blogger-edit-post-hook))

(defun blogger-new-post (message publishp)
  "Post a new message.  If publishp is non-nil, publishes the
message as well."
  (let* ((msgid (xml-rpc-method-call
		 blogger-url
		 'blogger.newPost
		 blogger-app-key
		 (blogger-id)
		 (blogger-username)
		 (blogger-password)
		 (url-insert-entities-in-string message)
		 publishp)))
	  (ring-insert blogger-post-ring
		       (list (cons 'postid msgid)
			     (cons 'userid nil) ; Cause there
						; seems to be
						; some sort of
						; server problem.
						; Otherwise we
						; would say
						; "(blogger-get-userid)"
			     (cons 'content message)
			     (cons 'date-created nil))))
  (setq blogger-ring-index 0) 
  (run-hooks 'blogger-new-post-hook))

(defun blogger-select-blog ()
  "Allows the user to select a blog and returns the blog ID."
  (blogger-id-from-blogname
   (let* ((bloglist (blogger-blog-list-names)))
     (read-from-minibuffer 
      "Blog: " (elt bloglist 0) nil nil (cons 'bloglist 1)))))

(defun blogger-id-from-blogname (name)
  "Returns the blog id given the name."
  (cdr (assoc name
	 (mapcar 
	  (lambda (blog)
	    (cons (cdr (assoc "blogName" blog))
		  (cdr (assoc "blogid" blog))))
	  (blogger-blog-alist)))))

(defun blogger-blog-from-id (id)
  "Returns the blog name given the id."
  (cdr (assoc id
	 (mapcar 
	  (lambda (blog)
	    (cons (cdr (assoc "blogid" blog))
		  (cdr (assoc "blogName" blog))))
	  (blogger-blog-alist)))))

(defun blogger-url-from-id (id)
  "Returns the blog URL given the id."
  (cdr (assoc id
	      (mapcar
	       (lambda (blog)
		 (cons (cdr (assoc "blogid" blog))
		       (cdr (assoc "url" blog))))
	       (blogger-blog-alist)))))

(defun blogger-blog-list-names ()
  "Returns a list of Blog names."
  (mapcar 
   (lambda (blog)
     (cdr (assoc "blogName" blog)))
   (blogger-blog-alist)))

(defun blogger-blog-alist ()
  "Returns the alist of blogs owned by a user."
  (xml-rpc-method-call 
   blogger-url
   'blogger.getUsersBlogs
   blogger-app-key
   (blogger-username)
   (blogger-password)))

(defun blogger-ping-weblogs (&optional id)
  "Pings the blog aggregators listed in blogger-weblog-ping-urls
when you post an update."
  (mapcar
   (lambda (url)
     (xml-rpc-method-call-async
      'blogger-handle-weblog-ping-response
      url
      'weblogUpdates.ping
      (blogger-blog-from-id 
       (or id blogger-id)				)
      (blogger-url-from-id 
       (or id blogger-id))))
   blogger-weblog-ping-urls))

(defun blogger-handle-weblog-ping-response (resp)
  "Handle the response from a weblog ping.  Print a message with the result."
  (message (cdr (assoc "message" (cdr resp)))))

;(defun blogger-buffer-to-html ()
;  "Translate the current buffer into html suitable for Blogger.
;\(not yet implemented\)")

;;;###autoload
(defun blogger-next-post ()
  "Edit the contents of the next message.  Don't change the
buffer if this is the last message."
  (interactive)
  (if (buffer-modified-p)
      (blogger-send-post nil))
  (unless blogger-message-list
    (blogger-list-messages))
  (setq blogger-ring-index (+ (if blogger-ring-index blogger-ring-index -1) 1))
  (if (ring-empty-p blogger-post-ring)
      (blogger-list-messages))
  (blogger-edit-message
   (ring-ref blogger-post-ring blogger-ring-index)))

;;;###autoload
(defun blogger-prev-post ()
  "Edit the contents of the previous message.  Don't change the
buffer if this is the first message."
  (interactive)
  (if (buffer-modified-p)
      (blogger-send-post nil))
  (unless blogger-message-list
    (blogger-list-messages))
  (setq blogger-ring-index (- (if blogger-ring-index blogger-ring-index 1) 1))
  (if (ring-empty-p blogger-post-ring)
      (blogger-list-messages))
  (blogger-edit-message
   (ring-ref blogger-post-ring blogger-ring-index)))

;;;###autoload
(defun blogger-delete-post ()
  "Delete the message."
  (interactive)
  (unless blogger-ring-index
    (message "You must have a blogger message loaded first."))
  (if (y-or-n-p "Do you really want to delete this message? ")
      (let* ((msgid (cdr 
		     (assoc 'postid 
			    (ring-ref blogger-post-ring blogger-ring-index)))))
	(xml-rpc-method-call
	 blogger-url
	 'blogger.deletePost
	 blogger-app-key
	 msgid
	 (blogger-username)
	 (blogger-password)
	 t))
    (ring-remove blogger-post-ring blogger-ring-index)
    (blogger-edit-message
     (ring-ref blogger-post-ring blogger-ring-index))))

(defun blogger-list-messages (&optional count)
  "Return a list of messages that the Blogger server has.  COUNT specifies
how many of the most recent messages to get.  If COUNT is not
specified, then the default \(blogger-default-message-list\) is
assumed."
  (setq blogger-message-list 
	(mapcar 
	 (lambda (post)
	   (ring-insert-at-beginning  blogger-post-ring
				      (blogger-message-to-struct post)))
	 (xml-rpc-method-call
	  blogger-url
	  'blogger.getRecentPosts
	  blogger-app-key
	  (blogger-id)
	  (blogger-username)
	  (blogger-password)
	  (or count blogger-default-message-count)))))

(defun blogger-edit-message (msg)
  "Edit a Message.  MSG specifies which message to edit."
  (set-buffer *blogger*)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (cdr (assoc 'content msg)))
  (set-buffer-modified-p nil)
;  (or (eq (blogger-get-userid) (cdr (assoc 'userid msg)))
;      (setq buffer-read-only t))
  (pop-to-buffer *blogger*))

(defun blogger-message-to-struct (msg)
  "Convert the result of the xml-rpc call to a structure we
like."
  (list
   (cons 'postid       (cdr (assoc "postid" msg)))
   (cons 'author-name  (cdr (assoc "authorName" msg)))
   (cons 'userid       (cdr (assoc "userid" msg)))
   (cons 'date-created (cdr (assoc "dateCreated" msg)))
   (cons 'content      (cdr (assoc "content" msg)))))

(defun blogger-get-userid ()
  "Get information on user."
  (or blogger-userid
      (setq blogger-userid 
	    (cdr
	     (assoc "userid"
		    (xml-rpc-method-call
		     blogger-url
		     'blogger.getUserInfo
		     blogger-app-key
		     (blogger-username)
		     (blogger-password)))))))

;;;###autoload
(defun blogger-refetch-posts ()
  "Sync the post ring with what is on the blogger server."
  (interactive)
  (setq blogger-post-ring (make-ring blogger-max-entries))
  (blogger-list-messages)
  (setq blogger-ring-index 0)
  (blogger-edit-message
   (ring-ref blogger-post-ring blogger-ring-index)))

(provide 'blogger)

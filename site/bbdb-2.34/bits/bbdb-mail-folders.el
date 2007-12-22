From: Geoffroy Ville <ville@isr.umd.edu>
Subject: bbdb/mail-folders
Date: 20 Nov 1998 00:00:00 GMT
Message-ID: <6azlnl56h9v.fsf@einstein.isr.umd.edu>
Sender: ville@einstein.isr.umd.edu
Organization: University of Maryland, College Park
X-Url: http://www.cenaath.cena.dgac.fr/~ville/
Newsgroups: gnu.emacs.sources,gnu.emacs.vm.info


Just thought I would repost this piece of code after today's
improvement. Cengiz told me he does not use it hence does not maintain it
anymore.

I do not remember where I got it from originally, thus this post in sources
and vm.info.

For BBDB users, this code allows you to have several mail-folder by default
for a given author. Saving one mail creates automaically the entry if none, or
allows you to select which among the existing one you want, or add a new one.
Very useful when several people you know are involved in many different
projects. After a while, a typical entry would look like this:

   mail-folders: ("~/Mail/project1" "~/Mail/project2" "~/Mail/personal")

My 2 cts addition is an expand file-name to get rid of possible duplicate
paths to the same file and a file-name-abbrevation to keep it ~/Mail for
example (very useful for me because I changed sites twice in the recent years
and had different home directories).

I'm just *sharing* the code and will not have time to maintain it further. But
if it's buggy or outdated by some new feature of I_do_not_know_what, please
tell me :-) 

--- bbdb-mail-folders.el ---
;A while back Roland posted advices to enable a mail-folder
;property. This property was used as the default folder name while
;saving messages in vm.

;I have improved that in two ways:
;1. It is now a list of folder names, the first one on this list
;   becomes the default folder name and the other names are pushed to
;   the file-name history so that one can scroll through them using the
;   history mechanisms. This is useful if you are saving mail from a
;   person to more than one folder.
;2. This property is created and updated automatically when a message
;   is saved to a folder so that the list is in MRU (most recently
;   used) order. This is useful, because I am lazy to  set the
;   mail-folder property by hand.

;I renamed the property to mail-folders so that it does not break with
;the existing mail-folder property.

;Enjoy. Bug fixes are welcome.

;Cengiz

;-- 
;Cengiz Alaettinoglu       Information Sciences Institute
;(310) 822-1511            University of Southern California
;http://www.isi.edu/div7/people/cengiz.home

;$Modified: Fri Nov 20 11:41:56 1998 by ville@isr.umd.edu $
; GV: - always expand filename to avoid duplicate similar path
;     - use abbreviation alist for home directory (comes from mode-line)

(defvar bbdb/vm-mail-folders-file-name-history nil "")

(defvar bbdb/file-name-abbreviation-alist
      (list 
       (cons  (concat "^" (expand-file-name "~") "/")  "~/")
       )
)

(defadvice vm-save-message (around bbdb/vm-mail-folders activate compile)
  "cache"
  (let* ((folder-name "")
         (record (bbdb/vm-update-record nil))
         (mail-folders (and record (bbdb-record-getprop record 'mail-folders)))
         (folder-list (and mail-folders (car (read-from-string mail-folders)))))
    ad-do-it
    (setq folder-name (ad-get-arg 0))
    (setq folder-name (expand-file-name folder-name vm-folder-directory))
    (setq folder-name (string-replace-regexp-alist
		       folder-name bbdb/file-name-abbreviation-alist))
    (setq file-name-history 
          (append (list folder-name) bbdb/vm-mail-folders-file-name-history))
    (and record
         (progn
           (setq folder-list (delete folder-name folder-list))
           (setq folder-list (append (list folder-name) folder-list))
           (bbdb-record-putprop record 'mail-folders 
                                (prin1-to-string folder-list))
           )
         )
    )
  )

(defadvice vm-auto-select-folder (around bbdb/vm-mail-folders activate compile)
  "If the message sender's BBDB entry has a `mail-folder' property, use that."
  (let* ((record (bbdb/vm-update-record nil))
         (mail-folders (and record (bbdb-record-getprop record 'mail-folders)))
         (folder-list (and mail-folders (car (read-from-string mail-folders))))
         (folder-name (and folder-list (car folder-list))))
    (setq bbdb/vm-mail-folders-file-name-history file-name-history)
    (and (cdr folder-list)
         (setq file-name-history 
               (append (cdr folder-list) file-name-history)))
    (if folder-name 
        (setq ad-return-value (file-name-nondirectory folder-name))
      ad-do-it)
    )
  )

(provide 'bbdb-mail-folders)

--- end ---

Enjoy,

-- Geoffroy

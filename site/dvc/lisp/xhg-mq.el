;;; xhg-mq.el --- dvc integration for hg's mq

;; Copyright (C) 2006-2009 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; For more information on mq see:
;;   http://www.selenic.com/mercurial/wiki/index.cgi/MqTutorial

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `xhg-qinit'
;;    Run hg qinit.
;;  `xhg-qnew'
;;    Run hg qnew.
;;  `xhg-qrefresh'
;;    Run hg qrefresh.
;;  `xhg-qrefresh-header'
;;    Run hg qrefresh --message.
;;  `xhg-qrefresh-edit-message-done'
;;    Use the current buffer content as parameter for hg qrefresh --message.
;;  `xhg-qrefresh-edit-message-mode'
;;    Major mode to edit the mq header message for the current patch.
;;  `xhg-qpop'
;;    Run hg qpop.
;;  `xhg-qpush'
;;    Run hg qpush.
;;  `xhg-qapplied'
;;    Run hg qapplied.
;;  `xhg-qunapplied'
;;    Run hg qunapplied.
;;  `xhg-qseries'
;;    Run hg qseries.
;;  `xhg-qdiff'
;;    Run hg qdiff.
;;  `xhg-qdelete'
;;    Run hg qdelete
;;  `xhg-qconvert-to-permanent'
;;    Convert all applied patchs in permanent changeset.
;;  `xhg-qrename'
;;    Run hg qrename
;;  `xhg-qtop'
;;    Run hg qtop.
;;  `xhg-qnext'
;;    Run hg qnext.
;;  `xhg-qprev'
;;    Run hg qprev.
;;  `xhg-qheader'
;;    Run hg qheader.
;;  `xhg-qsingle'
;;    Merge applied patches in a single patch satrting from "qbase".
;;  `xhg-qimport'
;;    Run hg qimport
;;  `xhg-mq-export-via-mail'
;;    Prepare an email that contains a mq patch.
;;  `xhg-mq-show-stack'
;;    Show the mq stack.
;;  `xhg-qdiff-at-point'
;;    Show the diff for a given patch.
;;  `xhg-mq-mode'
;;    Major mode for xhg mq interaction.
;;  `xhg-mq-edit-series-file'
;;    Edit the mq patch series file
;;

;; The following commands are available for hg's mq:
;; X qapplied      print the patches already applied
;;   qclone        clone main and patch repository at same time
;;   qcommit       commit changes in the queue repository
;; X qdelete       remove a patch from the series file
;; X qdiff         diff of the current patch
;;   qfold         fold the named patches into the current patch
;;   qgoto         push or pop patches until named patch is at top of stack
;;   qguard        set or print guards for a patch
;; X qheader       Print the header of the topmost or specified patch
;; X qimport       import a patch
;; X qinit         init a new queue repository
;; X qnew          create a new patch
;; X qnext         print the name of the next patch
;; X qpop          pop the current patch off the stack
;; X qprev         print the name of the previous patch
;; X qpush         push the next patch onto the stack
;; X qrefresh      update the current patch
;; X qrename       rename a patch
;;   qrestore      restore the queue state saved by a rev
;;   qsave         save current queue state
;;   qselect       set or print guarded patches to push
;; X qseries       print the entire series file
;; X qtop          print the name of the current patch
;; X qunapplied    print the patches not yet applied

;;; Code:

(defvar xhg-mq-submenu
  '("mq"
    ["Show mq stack"  xhg-mq-show-stack t]
    ["mq refresh"  xhg-qrefresh t]
    ["mq diff"  xhg-qdiff t]
    ["mq push"  xhg-qpush t]
    ["mq pop"  xhg-qpop t]
    ["mq applied"  xhg-qapplied t]
    ["mq unapplied"  xhg-qunapplied t]
    ["mq series"  xhg-qseries t]
    ["mq delete"  xhg-qdelete t]
    ["mq rename"  xhg-qrename t]
    ["mq header"  xhg-qheader t]
    "--"
    ["mq init" xhg-qinit t]
    ["mq new"  xhg-qnew t]
    ))

(defvar xhg-mq-sub-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?A] 'xhg-qapplied)
    (define-key map [?U] 'xhg-qunapplied)
    (define-key map [?S] 'xhg-qseries)
    (define-key map [?s] 'xhg-mq-show-stack)
    (define-key map [?e] 'xhg-mq-edit-series-file)
    (define-key map [?h] 'xhg-qheader)
    (define-key map [?H] 'xhg-qrefresh-header)
    (define-key map [?R] 'xhg-qrefresh)
    (define-key map [?M] 'xhg-qrename)
    (define-key map [?P] 'xhg-qpush) ;; mnemonic: stack gets bigger
    (define-key map [?p] 'xhg-qpop)  ;; mnemonic: stack gets smaller
    (define-key map [?t] 'xhg-qtop)
    (define-key map [?+] 'xhg-qnext)
    (define-key map [?-] 'xhg-qprev)
    (define-key map [?=] 'xhg-qdiff)
    (define-key map [?d] 'xhg-qdelete)
    (define-key map [?N] 'xhg-qnew)
    (define-key map [?E] 'xhg-mq-export-via-mail)
    (define-key map [?x] 'xhg-qsingle)
    (define-key map [?C] 'xhg-qconvert-to-permanent)
    map)
  "Keymap used for xhg-mq commands.")

(defvar xhg-mq-cookie nil  "Ewoc cookie for xhg mq buffers.")

;;;###autoload
(defun xhg-qinit (&optional dir qinit-switch)
  "Run hg qinit.
When called without a prefix argument run hg qinit -c, otherwise hg qinit."
  (interactive
   (list (progn (setq qinit-switch (if current-prefix-arg "" "-c"))
                (expand-file-name (dvc-read-directory-name (format "Directory for hg qinit %s: " qinit-switch)
                                                           (or default-directory
                                                               (getenv "HOME")))))
         qinit-switch))
  (let ((default-directory dir))
    (dvc-run-dvc-sync 'xhg (list "qinit" qinit-switch)
                      :finished (dvc-capturing-lambda
                                    (output error status arguments)
                                  (message "hg qinit finished")))))

(defun xhg-qnew-name-patch ()
  "Return a default name for a new patch based on last revision number"
  (let ((cur-patch (xhg-qtop))
        (cur-rev (xhg-dry-tip))
        (patch-name)
        (patch-templ-regex "\\(patch-r[0-9]+\\)"))
    (if cur-patch
        (if (string-match patch-templ-regex cur-patch)
            (setq patch-name
                  (replace-regexp-in-string "\\([0-9]+\\)"
                                            (int-to-string
                                             (+ (string-to-number cur-rev) 1))
                                            cur-patch))
            (setq patch-name
                  (replace-regexp-in-string "\\([0-9]+\\)"
                                            (int-to-string
                                             (+ (string-to-number cur-rev) 1))
                                            "patch-r0")))
        (setq patch-name
              "Initial-patch"))
    patch-name))

;;;###autoload
(defun xhg-qnew (patch-name &optional commit-description force)
  "Run hg qnew.
Asks for the patch name and an optional commit description.
If the commit description is not empty, run hg qnew -m \"commit description\"
When called with a prefix argument run hg qnew -f."
  (interactive
   (list (read-from-minibuffer "qnew patch name: " nil nil nil nil (xhg-qnew-name-patch))
         (read-from-minibuffer "qnew commit message (empty for none): " nil nil nil nil
                               "New patch, edit me when done with <M-x xhg-qrefresh-header>")
         current-prefix-arg))
  (when (string= commit-description "")
    (setq commit-description nil))
  (dvc-run-dvc-sync 'xhg (list "qnew"
                               (when force "-f")
                               (when commit-description "-m")
                               (when commit-description (concat "\"" commit-description "\""))
                               patch-name)))

;;;###autoload
(defun xhg-qrefresh ()
  "Run hg qrefresh."
  (interactive)
  (let ((top (xhg-qtop)))
    (dvc-run-dvc-sync 'xhg (list "qrefresh"))
    (message (format "hg qrefresh for %s finished" top))))

;;;###autoload
(defun xhg-qrefresh-header ()
  "Run hg qrefresh --message."
  (interactive)
  (let ((cur-message (xhg-qheader))
        (cur-dir default-directory))
    (dvc-buffer-push-previous-window-config)
    (pop-to-buffer (get-buffer-create (format "*xhg header for %s*" (xhg-qtop))))
    (setq default-directory (dvc-tree-root cur-dir))
    (erase-buffer)
    (insert cur-message)
    (xhg-qrefresh-edit-message-mode)
    (message "Edit the message and hit C-c C-c to accept it.")))

(defun xhg-qrefresh-edit-message-done ()
  "Use the current buffer content as parameter for hg qrefresh --message."
  (interactive)
  (let ((logfile-name (make-temp-file "xhg-qrefresh"))
        (new-message (buffer-substring-no-properties (point-min) (point-max)))
        (message-buf))
    (save-excursion
      (find-file logfile-name)
      (setq message-buf (current-buffer))
      (insert new-message)
      (save-buffer))
    (dvc-run-dvc-sync 'xhg (list "qrefresh" "--logfile" logfile-name))
    (kill-buffer message-buf)
    (delete-file logfile-name)
    (let ((dvc-buffer-quit-mode 'kill))
      (dvc-buffer-quit))))

(defvar xhg-qrefresh-edit-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?c)] 'xhg-qrefresh-edit-message-done)
    map)
  "Keymap used in a xhg qrefresh edit message buffer.")

(define-derived-mode xhg-qrefresh-edit-message-mode fundamental-mode
  "xhg qrefresh edit message"
  "Major mode to edit the mq header message for the current patch."
  (dvc-install-buffer-menu))

;;;###autoload
(defun xhg-qpop (&optional all)
  "Run hg qpop.
When called with a prefix argument run hg qpop -a."
  (interactive
   (list current-prefix-arg))
  (let ((curbuf (current-buffer)))
    (message (format "qpop -> %s"
                     (dvc-run-dvc-sync 'xhg (list "qpop" (when all "-a"))
                                       :finished 'dvc-output-buffer-handler
                                       :error (lambda (output error status arguments)
                                                (if (eq status 1)
                                                    (message "no patches applied")
                                                  (message "error status: %d" status))))))
    (xhg-mq-maybe-refresh-patch-buffer)
    (pop-to-buffer curbuf)))

;;;###autoload
(defun xhg-qpush (&optional all)
  "Run hg qpush.
When called with a prefix argument run hg qpush -a."
  (interactive
   (list current-prefix-arg))
  (let ((curbuf (current-buffer)))
    (message (format "qpush -> %s"
                     (dvc-run-dvc-sync 'xhg (list "qpush" (when all "-a"))
                                       :finished 'dvc-output-buffer-handler
                                       :error (lambda (output error status arguments)
                                                (if (eq status 1)
                                                    (message "patch series fully applied")
                                                  (message "error status: %d" status))))))
    (xhg-mq-maybe-refresh-patch-buffer)
    (pop-to-buffer curbuf)))

(defun xhg-mq-maybe-refresh-patch-buffer ()
  (let ((patch-buffer (dvc-get-buffer 'xhg 'patch-queue)))
    (when patch-buffer
      (with-current-buffer patch-buffer
        (dvc-generic-refresh)))))

(defun xhg-mq-printer (elem)
  "Print an element ELEM of the mq patch list."
  (insert (dvc-face-add (car elem) (cadr elem))))

(defun xhg-process-mq-patches (cmd-list header refresh-function &optional only-show)
  (let ((patches (delete "" (dvc-run-dvc-sync 'xhg cmd-list
                                              :finished 'dvc-output-buffer-split-handler))))
    (when only-show
      (let ((curbuf (current-buffer)))
        (pop-to-buffer (dvc-get-buffer-create 'xhg 'patch-queue))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert header)
          (set (make-local-variable 'xhg-mq-cookie)
               (ewoc-create (dvc-ewoc-create-api-select #'xhg-mq-printer)))
          (put 'xhg-mq-cookie 'permanent-local t)
          (dolist (patch patches)
            (ewoc-enter-last xhg-mq-cookie (list patch nil))))
        (xhg-mq-mode)
        (setq dvc-buffer-refresh-function refresh-function)
        (goto-char (point-min))
        (forward-line 1)
        (pop-to-buffer curbuf)))
    patches))

;;;###autoload
(defun xhg-qapplied ()
  "Run hg qapplied."
  (interactive)
  (xhg-process-mq-patches '("qapplied") "hg qapplied:" 'xhg-qapplied (interactive-p)))

;;;###autoload
(defun xhg-qunapplied ()
  "Run hg qunapplied."
  (interactive)
  (xhg-process-mq-patches '("qunapplied") "hg qunapplied:" 'xhg-qunapplied (interactive-p)))

;;;###autoload
(defun xhg-qseries ()
  "Run hg qseries."
  (interactive)
  (xhg-process-mq-patches '("qseries") "hg series:" 'xhg-qseries (interactive-p)))

;;;###autoload
(defun xhg-qdiff (&optional file)
  "Run hg qdiff."
  (interactive)
  (let ((curbuf (current-buffer)))
    (dvc-run-dvc-display-as-info 'xhg (list "qdiff" file) nil (format "hg qdiff %s:\n" (xhg-qtop)))
    (with-current-buffer "*xhg-info*"
      (diff-mode))
    (pop-to-buffer curbuf)))

;;;###autoload
(defun xhg-qdelete (patch)
  "Run hg qdelete"
  (interactive (list
                (let ((unapplied (xhg-qunapplied)))
                  (if unapplied
                      (dvc-completing-read "Delete mq patch: " unapplied nil t
                                           (car (member (xhg-mq-patch-name-at-point) unapplied)))
                    (message "No unapplied patch to delete from the mq series file")
                    nil))))
  (when patch
    (dvc-run-dvc-sync 'xhg (list "qdelete" patch))
    (xhg-mq-maybe-refresh-patch-buffer)))

;;;###autoload
(defun xhg-qconvert-to-permanent (&optional force)
  "Convert all applied patchs in permanent changeset.
Run the command hg qdelete -r qbase:qtip
Called with prefix-arg, do not prompt for confirmation"
  (interactive)
  (let ((tip (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      (list "tip" "--template" "#rev#"))
               (buffer-string)))
        (confirm))
    (if current-prefix-arg
        (progn
          (dvc-run-dvc-sync 'xhg (list "qdelete" "-r" "qbase:qtip"))
          (message "All patchs converted to permanent changeset: now at rev %s" tip))
      (setq confirm (read-string "Really add permanent changesets to this repo?\(y/n\): "))
      (if (equal confirm "y")
          (progn
            (dvc-run-dvc-sync 'xhg (list "qdelete" "-r" "qbase:qtip"))
            (message "All patchs converted to permanent changeset: now at rev %s" tip))
        (message "Operation cancelled")))))

;;;###autoload
(defun xhg-qrename (from to)
  "Run hg qrename"
  (interactive (let ((old-name (or (xhg-mq-patch-name-at-point) (xhg-qtop))))
                 (list
                  old-name
                  (if old-name
                      (read-from-minibuffer (format "Rename mq patch '%s' to: " old-name) old-name)
                    (message "No mq patch to rename found")
                    nil))))
  (message "Running hg qrename %s %s" from to)
  (when (and from to)
    (dvc-run-dvc-sync 'xhg (list "qrename" from to))))

;;;###autoload
(defun xhg-qtop ()
  "Run hg qtop."
  (interactive)
  (let ((top (dvc-run-dvc-sync 'xhg '("qtop")
                               :finished 'dvc-output-buffer-handler
                               :error (lambda (output error status arguments)
                                        nil))))
    (when (interactive-p)
      (if top
          (message "Mercurial qtop: %s" top)
        (message "Mercurial qtop: no patches applied")))
    top))

;;;###autoload
(defun xhg-qnext ()
  "Run hg qnext."
  (interactive)
  (let ((next (dvc-run-dvc-sync 'xhg '("qnext")
                                :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qnext: %s" next))
    next))

;;;###autoload
(defun xhg-qprev ()
  "Run hg qprev."
  (interactive)
  (let ((prev (dvc-run-dvc-sync 'xhg '("qprev")
                                :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qprev: %s" prev))
    prev))

;;;###autoload
(defun xhg-qheader (&optional patch)
  "Run hg qheader."
  (interactive
   (list
    (xhg-mq-patch-name-at-point)))
  (let ((header (dvc-run-dvc-sync 'xhg (list "qheader" patch)
                                  :finished 'dvc-output-buffer-handler)))
    (when (interactive-p)
      (message "Mercurial qheader: %s" header))
    header))

(defun xhg-mq-patch-file-name (patch)
  (concat (xhg-tree-root) "/.hg/patches/" patch))

;;;###autoload
(defun* xhg-qsingle (file &optional (start-from "qbase"))
  "Merge applied patches in a single patch starting from \"qbase\".
If prefix arg, merge applied patches starting from revision number or patch-name."
  (interactive "FPatchName: ")
  (when (and current-prefix-arg (interactive-p))
    (let ((series (xhg-qseries)))
      (setq start-from (completing-read "PatchName: "
                                        series nil t
                                        (car (member (xhg-mq-patch-name-at-point) series))))))
  (let* ((base (with-temp-buffer
                 (apply #'call-process "hg" nil t nil
                        `("parents"
                          "-r"
                          ,start-from
                          "--template"
                          "#rev#"))
                 (buffer-string)))
         (patch (with-temp-buffer
                  (apply #'call-process "hg" nil t nil
                         (list "diff"
                               "-r"
                               base
                               "-r"
                               "qtip"
                               (when xhg-export-git-style-patches "--git")))
                  (buffer-string)))
         (applied (split-string
                   (with-temp-buffer
                     (apply #'call-process "hg" nil t nil
                            (list "qapplied" "-s"))
                     (buffer-string)) "\n")))
    (when (not (equal start-from "qbase"))
      (let (pos elm)
        (catch 'break
          (dolist (i applied)
            (when (string-match start-from i)
              (throw 'break
                (setq elm i)))))
        (setq pos (position elm applied))
        (setq applied (subseq applied pos))))
    (find-file file)
    (goto-char (point-min))
    (erase-buffer)
    (insert (format "##Merge of all patches applied from revision %s\n" base))
    (mapc #'(lambda (x)
              (insert (concat "## " x "\n")))
          applied)
    (insert patch)
    (save-buffer)
    (kill-buffer (current-buffer))
    (message "Ok patch extracted from rev %s to tip in %s" base file)))

;;;###autoload
(defun xhg-qimport (patch &optional push)
  "Run hg qimport"
  (interactive (list (read-file-name "Import hg qpatch: "
                                     nil
                                     nil
                                     t
                                     (when
                                         (eq major-mode 'dired-mode)
                                       (file-name-nondirectory (dired-get-filename))))))
  (if current-prefix-arg
      (progn
        (and (dvc-run-dvc-sync 'xhg (list "qimport" (expand-file-name patch)))
             (dvc-run-dvc-sync 'xhg (list "qpush")))
        (message "Ok patch %s added" patch))
    (dvc-run-dvc-sync 'xhg (list "qimport" (expand-file-name patch)))
    (message "Ok patch %s added ; don't forget to qpush" patch)))

;; --------------------------------------------------------------------------------
;; Higher level functions
;; --------------------------------------------------------------------------------

;;;###autoload
(defun xhg-mq-export-via-mail (patch &optional single)
  "Prepare an email that contains a mq patch.
`xhg-submit-patch-mapping' is honored for the destination email address and the project name
that is used in the generated email."
  (interactive (list
                (let ((series (xhg-qseries)))
                  (dvc-completing-read (if current-prefix-arg
                                           "Send single patch from: "
                                         "Send mq patch via mail: ") series nil t
                                         (car (member (xhg-mq-patch-name-at-point) series))))))
  (let ((file-name)
        (destination-email "")
        (base-file-name nil)
        (subject)
        (log))
    (dolist (m xhg-submit-patch-mapping)
      (when (string= (dvc-uniquify-file-name (car m)) (dvc-uniquify-file-name (xhg-tree-root)))
        ;;(message "%S" (cadr m))
        (setq destination-email (car (cadr m)))
        (setq base-file-name (cadr (cadr m)))))
    (message "Preparing an email for the mq patch '%s' for '%s'" patch destination-email)
    (if (or current-prefix-arg single)
        (let ((pname (format "single-from-%s-to-tip.patch" patch)))
          (setq file-name (concat (dvc-uniquify-file-name dvc-temp-directory)
                                  pname))
          (xhg-qsingle file-name patch)
          (setq log
                (with-temp-buffer
                  (let (beg end)
                    (insert-file-contents file-name)
                    (goto-char (point-min))
                    (setq beg (point))
                    (when (re-search-forward "^diff" nil t)
                      (setq end (point-at-bol)))
                    (replace-regexp-in-string "^#*" "" (buffer-substring beg end)))))
          (setq subject pname))
        (setq file-name (concat (dvc-uniquify-file-name dvc-temp-directory)
                                (or base-file-name "") "-" patch ".patch"))
        (copy-file (xhg-mq-patch-file-name patch) file-name t t))

    (require 'reporter)
    (delete-other-windows)
    (reporter-submit-bug-report
     destination-email
     nil
     nil
     nil
     nil
     (if (or current-prefix-arg single)
         log
         dvc-patch-email-message-body-template))
    (unless (or current-prefix-arg single)
      (setq subject (if base-file-name (concat base-file-name ": " patch) patch)))

    ;; delete emacs version - its not needed here
    (delete-region (point) (point-max))

    (mml-attach-file file-name "text/x-patch")
    (goto-char (point-min))
    (mail-position-on-field "Subject")
    (insert (concat "[MQ-PATCH] " subject))
    (when (search-forward "<<LOG-START>>" nil t)
      (forward-line 1))
    (find-file-other-window file-name)
    (other-window -1)))

;;;###autoload
(defun xhg-mq-show-stack ()
  "Show the mq stack."
  (interactive)
  (xhg-process-mq-patches '("qseries") "hg stack:" 'xhg-mq-show-stack (interactive-p))
  (let ((applied (xhg-qapplied))
        (unapplied (xhg-qunapplied))
        (top (xhg-qtop))
        (top-pos))
    (with-current-buffer (dvc-get-buffer 'xhg 'patch-queue)
      (let ((buffer-read-only nil)
            (old-applied-patches (progn (goto-char (point-min)) (next-line 1)
                                        (split-string (buffer-substring-no-properties (point) (- (point-max) 1)))))
            (act-patches (append applied unapplied)))
        (dolist (u unapplied)
          (goto-char (point-min))
          (when (re-search-forward (concat "^" u "$") nil t)
            (setcar (cdr (xhg-mq-ewoc-data-at-point)) nil)))
        (dolist (a applied)
          (goto-char (point-min))
          (when (re-search-forward (concat "^" a "$") nil t)
            (setcar (cdr (xhg-mq-ewoc-data-at-point)) 'dvc-move)))
        (dolist (p old-applied-patches)
          (when (not (member p act-patches))
            (goto-char (point-min))
            (when (re-search-forward (concat "^" p "$") nil t)
              (message "Patch %s no longer present" p)
              (dvc-ewoc-delete xhg-mq-cookie (ewoc-locate xhg-mq-cookie)))))
        (when top
          (goto-char (point-min))
          (when (re-search-forward (concat "^" top "$") nil t)
            (setq top-pos (line-beginning-position))
            (setcar (cdr (xhg-mq-ewoc-data-at-point)) 'bold)))
        (ewoc-refresh xhg-mq-cookie)
        (when top-pos
          (goto-char top-pos))))))

(defun xhg-qdiff-at-point (&optional patch)
  "Show the diff for a given patch."
  (interactive)
  (let ((patch-name (or patch (xhg-mq-patch-name-at-point)))
        (cur-buf (current-buffer)))
    (find-file-other-window (xhg-mq-patch-file-name patch-name))
    (toggle-read-only 1)
    (diff-mode)
    (pop-to-buffer cur-buf)))

;; --------------------------------------------------------------------------------
;; the xhg mq mode
;; --------------------------------------------------------------------------------

(defvar xhg-mq-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (dvc-prefix-buffer ?L) 'dvc-open-internal-log-buffer)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map [?g] 'dvc-generic-refresh)
    (define-key map [?e] 'xhg-mq-edit-series-file)
    (define-key map [down] 'xhg-mq-next)
    (define-key map [up] 'xhg-mq-previous)
    (define-key map [?P] 'xhg-qpush) ;; mnemonic: stack gets bigger
    (define-key map [?p] 'xhg-qpop)  ;; mnemonic: stack gets smaller
    (define-key map [?=] 'xhg-qdiff-at-point)
    (define-key map [?E] 'xhg-mq-export-via-mail)
    (define-key map [?M] 'xhg-qrename)
    (define-key map [?x] 'xhg-qsingle)
    (define-key map [?C] 'xhg-qconvert-to-permanent)
    (define-key map [?Q] xhg-mq-sub-mode-map)
    map)
  "Keymap used in a xhg mq buffer.")

(easy-menu-define xhg-mq-mode-menu xhg-mq-mode-map
  "`xhg-mq-mode' menu"
  xhg-mq-submenu)

(define-derived-mode xhg-mq-mode fundamental-mode
  "xhg mq mode"
  "Major mode for xhg mq interaction."
  (dvc-install-buffer-menu)
  (toggle-read-only 1))

(defun xhg-mq-ewoc-data-at-point ()
  (if (or (= (dvc-line-number-at-pos) 1)
          (eq (line-beginning-position) (line-end-position))
          (not (eq major-mode 'xhg-mq-mode)))
      nil
    (ewoc-data (ewoc-locate xhg-mq-cookie))))

(defun xhg-mq-patch-name-at-point ()
  "Return the patch name at point in a xhg mq buffer."
  (car (xhg-mq-ewoc-data-at-point)))

(defun xhg-mq-edit-series-file ()
  "Edit the mq patch series file"
  (interactive)
  (find-file-other-window (concat (dvc-tree-root) "/.hg/patches/series"))
  (message "You can carefully reorder the patches in the series file. Comments starting with '#' and empty lines are allowed."))

(defun xhg-mq-next ()
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (unless (xhg-mq-ewoc-data-at-point)
      (goto-char pos))))

(defun xhg-mq-previous ()
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (unless (xhg-mq-ewoc-data-at-point)
      (goto-char pos))))

(provide 'xhg-mq)
;;; xhg-mq.el ends here

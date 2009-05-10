;;; dvc-bookmarks.el --- The bookmark system for DVC

;; Copyright (C) 2006-2008 by all contributors

;; Authors: Stefan Reichoer, <stefan@xsteve.at>
;;          Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;; This file provides a hierachical bookmark system for DVC

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `dvc-cur-date-string'
;;    Return current date under string form ==>2008.03.16
;;  `dvc-bookmarks-set-tree-properties'
;;    color value is one of the dvc-faces ==> dvc-buffer, dvc-nested-tree, etc...
;;  `dvc-bookmarks-toggle-time-stamp'
;;    Toggle show/don't show time-stamp
;;  `dvc-bookmarks-toggle-partner-url'
;;    Toggle show/don't show partners urls
;;  `dvc-bookmarks'
;;    Display the *dvc-bookmarks* buffer.
;;  `dvc-bookmarks-mode'
;;    Mode to display DVC bookmarks.
;;  `dvc-bookmarks-quit'
;;    Clean dvc-bookmarks-hidden-subtree
;;  `dvc-bookmarks-add'
;;    Add a DVC bookmark named BOOKMARK-NAME, directory BOOKMARK-LOCAL-DIR.
;;  `dvc-bookmarks-dired-add-project'
;;    Add a DVC bookmark from dired
;;  `dvc-bookmarks-edit'
;;    Change the current DVC bookmark's BOOKMARK-NAME and/or LOCAL-DIR.
;;  `dvc-bookmarks-status'
;;    Run `dvc-status' for bookmark at point.
;;  `dvc-bookmarks-diff'
;;    Run `dvc-diff' for bookmark at point.
;;  `dvc-bookmarks-pull'
;;    Pull from partner at point or default into current bookmark.
;;  `dvc-bookmarks-merge'
;;    Merge from partner at point into current bookmark.
;;  `dvc-bookmarks-yank'
;;    Choose to yank marked or at point
;;  `dvc-bookmarks-yank-from-list-to-sub'
;;    Yank from list ==> sublist
;;  `dvc-bookmarks-yank-from-sub-to-list'
;;    Yank from sublist ==> list
;;  `dvc-bookmarks-yank-from-sub-to-sub'
;;    Yank from one sublist to another sublist,
;;  `dvc-bookmarks-yank-from-list-to-list'
;;    Yank inside dvc-bookmark-alist: list ==> list
;;  `dvc-bookmarks-show-or-hide-subtree'
;;    Hide subtree when called with no argument
;;  `dvc-bookmarks-delete-at-point'
;;    Destructive kill and delete function
;;  `dvc-bookmarks-kill'
;;    Choose to kill marked entry or entry at point
;;  `dvc-bookmarks-delete'
;;    Choose to delete marked entry or entry at point
;;  `dvc-bookmarks-add-empty-tree'
;;    Add a new family to your bookmarks
;;  `dvc-bookmarks-toggle-mark-entry'
;;    Mark or unmark the current bookmark entry.
;;  `dvc-bookmarks-unmark-all'
;;    Unmark all bookmarks.
;;  `dvc-bookmarks-hg-convert-from-marked'
;;    Call `xhg-convert' with current dvc-bookmark as target and
;;  `dvc-bookmarks-save'
;;    Save `dvc-bookmark-alist' to the file `dvc-bookmarks-file-name'.
;;

;;; History:

;;

;;; Code:
(require 'dvc-core)
(require 'dvc-state)
(require 'ewoc)
(eval-when-compile (require 'cl))

;; this were the settings used for tla
;; ;; Generated file. Do not edit!!!
;; (setq
;; tla-bookmarks-alist
;; '(("dvc"
;;   (local-tree "/home/srei/work/tla/xtla")
;;   (location "stefan@xsteve.at--public-2005" "dvc" "dev" "0" nil)
;;   (timestamp . "Wed Apr 27 10:45:31 2005"))
;;  ("emacs-muse"
;;   (local-tree "/home/srei/work/tla/emacs-muse")
;;   (location "mwolson@gnu.org--2006" "muse" "main" "1.0" nil)
;;   (timestamp . "Fri Dec 10 07:05:56 2004"))))

;; what I want to have:
;; hierachical tree of bookmarks
;; support for different dvc's
;; short name for working copy/branch
;; local-tree
;; timestamp => bookmark-creation-date?
;; different colors
;; optional: dvc: xhg, bzr,...
;; bookmark editing via C-k, C-y (just like in gnus)

;; saved under ~/.dvc/bookmarks.el

;; a data structure for testing purposes
(defvar dvc-bookmark-alist
  '(("hg"
     (local-tree "~/work/hg/hg"))
    ("work-stuff"
     (children
      ("home-dir"
       (local-tree "~/"))
      ("another-dir"
       (local-tree "~/work")))))
  "The bookmarks used for dvc")
;;(pp-to-string dvc-bookmark-alist)

(defvar dvc-bookmarks-file-name "dvc-bookmarks.el" "The file that holds the dvc bookmarks")

(defvar dvc-bookmarks-show-partners t
  "If non-nil, display partners.
Must be non-nil for some featurs of dvc-bookmarks to work.")

(defvar dvc-bookmarks-mode-hook '()
  "*Hooks run when entering dvc-bookmarks-mode'.")

(defvar dvc-bookmarks-loaded nil "Whether `dvc-bookmark-alist' has been loaded from `dvc-bookmarks-file-name'.")
(defvar dvc-bookmarks-cookie nil "The ewoc cookie for the *dvc-bookmarks* buffer.")
;;(defvar dvc-bookmarks-marked-entry nil "A marked bookmark entry for some special operations.")

(defvar dvc-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    ;;(define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map dvc-keyvec-quit 'dvc-bookmarks-quit)
    (define-key map [return] 'dvc-bookmarks-goto)
    (define-key map "\C-x\C-f" 'dvc-bookmarks-find-file-in-tree)
    (define-key map "\C-m"   'dvc-bookmarks-goto)
    (define-key map "\C-o"   'dvc-bookmarks-goto-other-window)
    (define-key map "g"      'dvc-bookmarks)
    (define-key map "h"      'dvc-buffer-pop-to-partner-buffer)
    (define-key map "j"      'dvc-bookmarks-jump)
    (define-key map "n"      'dvc-bookmarks-next)
    (define-key map "p"      'dvc-bookmarks-previous)
    (define-key map "a"      'dvc-bookmarks-add)
    (define-key map "At"     'dvc-bookmarks-add-empty-tree)
    (define-key map "e"      'dvc-bookmarks-edit)
    (define-key map "\C-y"   'dvc-bookmarks-yank)
    (define-key map "\C-k"   'dvc-bookmarks-kill)
    (define-key map "D"      'dvc-bookmarks-delete)
    (define-key map "H"      'dvc-bookmarks-show-or-hide-subtree)
    (define-key map "S"      'dvc-bookmarks-set-tree-properties)
    (define-key map "s"      'dvc-bookmarks-status)
    (define-key map "d"      'dvc-bookmarks-diff)
    (define-key map "c"      'dvc-bookmarks-log-edit)
    (define-key map "C"      'dvc-bookmarks-hg-convert-from-marked)
    (define-key map "l"      'dvc-bookmarks-changelog)
    (define-key map "L"      'dvc-bookmarks-log)
    (define-key map "Mm"     'dvc-bookmarks-missing)
    (define-key map "Mf"     'dvc-bookmarks-pull)
    (define-key map "Mp"     'dvc-bookmarks-push)
    (define-key map "Mx"     'dvc-bookmarks-merge)
    (define-key map "#"      'dvc-bookmarks-toggle-mark-entry)
    (define-key map "U"      'dvc-bookmarks-unmark-all)
    (define-key map "."      'dvc-bookmarks-show-info-at-point)
    (define-key map "\C-x\C-s" 'dvc-bookmarks-save)
    (define-key map "Ap"     'dvc-bookmarks-add-partner)
    (define-key map "Rp"     'dvc-bookmarks-remove-partner)
    (define-key map "Tp"     'dvc-bookmarks-toggle-partner-visibility)
    (define-key map "Td"     'dvc-bookmarks-toggle-time-stamp)
    (define-key map "Tu"     'dvc-bookmarks-toggle-partner-url)
    (define-key map "An"     'dvc-bookmarks-add-nickname)
    (define-key map "Am"     'dvc-bookmarks-add-push-location) ;; mnemonic: Add mirror
    (define-key map "Rm"     'dvc-bookmarks-remove-push-location)
    map)
  "Keymap used in `dvc-bookmarks-mode'.")

(easy-menu-define dvc-bookmarks-mode-menu dvc-bookmarks-mode-map
  "`dvc-bookmarks-mode' menu"
  `("dvc-bookmarks"
    ["Go to working copy" dvc-bookmarks-goto t]
    ["DVC diff" dvc-bookmarks-diff t]
    ["DVC status" dvc-bookmarks-status t]
    ["DVC changelog" dvc-bookmarks-changelog t]
    ["DVC log" dvc-bookmarks-log t]
    ["DVC missing" dvc-bookmarks-missing t]
    ["DVC pull" dvc-bookmarks-pull t]
    ["DVC push" dvc-bookmarks-push t]
    ["DVC merge" dvc-bookmarks-merge t]
    "--"
    ["Add new bookmark" dvc-bookmarks-add t]
    ["Edit current bookmark" dvc-bookmarks-edit t]
    ["Add partner" dvc-bookmarks-add-partner t]
    ["Remove partner" dvc-bookmarks-remove-partner t]
    ["Delete bookmark" dvc-bookmarks-delete t]
    ["Add/edit partner Nickname" dvc-bookmarks-add-nickname t]
    ["Add Push location" dvc-bookmarks-add-push-location t]
    ["Remove Push location" dvc-bookmarks-remove-push-location t]
    "--"
    ("Toggle visibility"
     ["Partners"    dvc-bookmarks-toggle-partner-visibility
      :style toggle :selected dvc-bookmarks-show-partners])
    "--"
    ["Save bookmarks" dvc-bookmarks-save t]
    ))

;; This data structure represents a single entry in the bookmarks
;; list.  There is one of these associated with each ewoc node.
(defstruct dvc-bookmark
  name                                  ; a string
  indent                                ; an integer
  elem)                                 ; the cdr is an alist

(defun dvc-bookmark-properties (bookmark)
  (cdr (dvc-bookmark-elem bookmark)))

(defsetf dvc-bookmark-properties (bookmark) (val)
  `(setcdr (dvc-bookmark-elem ,bookmark) ,val))

;; This data structure represents a partner of a bookmark.
(defstruct (dvc-bookmark-partner
            (:type list))
  url
  nickname)

(defun dvc-assq-all (key alist)
  "Return an alist containing all associations from ALIST matching KEY."
  (delete nil (mapcar '(lambda (e)
                         (when (and (listp e) (eq (car e) key))
                           e))
                      alist)))

(defun make-dvc-bookmark-from-assoc (assoc indent)
  "Create a `dvc-bookmark' from the association ASSOC.
The indent is taken from INDENT."
  (make-dvc-bookmark
   :name (car assoc)
   :indent indent
   :elem assoc))

(defun dvc-bookmark-key-value (bookmark key)
  "Return the association from the property of BOOKMARK matching KEY."
  (assq key (dvc-bookmark-properties bookmark)))

(defun dvc-bookmark-value (bookmark key)
  "Return the value of the property of BOOKMARK matching KEY."
  (cadr (dvc-bookmark-key-value bookmark key)))

(defun dvc-bookmark-partners (bookmark)
  "Return a list of the partners of BOOKMARK.
Each element is a `dvc-bookmark-partner' structure."
  (mapcar 'cdr
          (dvc-assq-all 'partner (dvc-bookmark-properties bookmark))))

(defun dvc-bookmark-partners-by-url (bookmark)
  "Return an alist of the partners of BOOKMARK.
The car of each association is the URL of the partner and the cdr
is the `dvc-bookmark-partner' itself."
  (mapcar (lambda (p) (cons (dvc-bookmark-partner-url p) p))
          (dvc-bookmark-partners bookmark)))

(defun dvc-bookmark-partner-urls (bookmark)
  "Return a list of the partner urls of BOOKMARK."
  (mapcar 'dvc-bookmark-partner-url (dvc-bookmark-partners bookmark)))

(defun dvc-bookmark-partner-url-from-nick (bookmark)
  "Return an alist of partners of BOOKMARK with nickname as key"
  (let ((partner-alist (mapcar (lambda (p) (reverse p))
                               (dvc-bookmark-partners bookmark))))
    partner-alist))

(defun dvc-bookmark-unmask-nickname-at-point ()
  "Get nickname at point even when urls are masked"
  (save-excursion
    (let ((nickname))
      ;;(goto-char (line-beginning-position))
      (end-of-line)
      (when (looking-back "\\[.+\\]")
        (setq nickname (replace-regexp-in-string "\\]" ""
                                                 (replace-regexp-in-string
                                                  "\\["
                                                  ""
                                                  (match-string 0)))))
      nickname)))

(defun dvc-bookmark-get-hidden-url-at-point ()
  "Get url of partner at point even if partner urls
are masked."
  (let ((url (cadr (assoc (dvc-bookmark-unmask-nickname-at-point)
                          (dvc-bookmark-partner-url-from-nick (dvc-bookmarks-current-bookmark))))))
    (when (stringp url)
      (cond ((string-match "~/" url)
             (expand-file-name url))
            (t
             url)))))

;; dvc-bookmarks-properties
(defvar dvc-bookmarks-prop-file
  (dvc-config-file-full-path "dvc-bookmarks-properties.el" t))

(defvar dvc-bookmarks-cache (make-hash-table)
  "init dvc-bookmarks hash-table properties")

(defun set-dvc-bookmarks-cache ()
  "Load cache file or create cache file if don't exist"
  (save-excursion
    (if (file-exists-p dvc-bookmarks-prop-file)
        (load dvc-bookmarks-prop-file)
      (find-file dvc-bookmarks-prop-file)
      (goto-char (point-min))
      (erase-buffer)
      (insert ";;; dvc-bookmarks-cache -*- mode: emacs-lisp; coding: utf-8; -*-")
      (save-buffer)
      (quit-window))))

;;(set-dvc-bookmarks-cache)

(defmacro hash-get-items (hash-table)
  "Get the list of all keys/values of hash-table
values are given under string form"
  `(let ((li-items nil))
     (maphash #'(lambda (x y) (push (list x y) li-items))
              ,hash-table)
     li-items))

(defmacro hash-get-symbol-keys (hash-table)
  "Get the list of all the keys in hash-table
keys are given under string form"
  `(let ((li-keys nil)
         (li-all (hash-get-items ,hash-table)))
     (setq li-keys (mapcar #'car li-all))
     li-keys))

(defmacro hash-has-key (key hash-table)
  "check if hash-table have key key
key here must be a symbol and not a string"
  `(let ((keys-list (hash-get-symbol-keys ,hash-table)))
     (if (memq ,key keys-list)
         t
       nil)))

(defun dvc-cur-date-string ()
  "Return current date under string form ==>2008.03.16"
  (interactive)
  (let ((year (nth 5 (decode-time (current-time))))
        (month (nth 4 (decode-time (current-time))))
        (day (nth 3 (decode-time (current-time))))
        (str-day-date ""))
    (setq str-day-date
          (concat (int-to-string year)
                  "."
                  (if (< (length (substring (int-to-string (/ (float month) 100)) 2)) 2)
                      (concat (substring (int-to-string (/ (float month) 100)) 2) "0")
                      (substring (int-to-string (/ (float month) 100)) 2))
                  "."
                  (if (< (length (substring (int-to-string (/ (float day) 100)) 2)) 2)
                      (concat (substring (int-to-string (/ (float day) 100)) 2) "0")
                      (substring (int-to-string (/ (float day) 100)) 2))))
    str-day-date))

(defvar dvc-table-face '("dvc-id"
                         "dvc-excluded"
                         "dvc-nested-tree"
                         "dvc-mark"
                         "dvc-revision-name"
                         "dvc-source"
                         "dvc-unknown"
                         "dvc-separator"
                         "dvc-highlight"
                         "dvc-copy"
                         "dvc-duplicate"))

(defun dvc-bookmarks-set-tree-properties (color state)
  "color value is one of the dvc-faces ==> dvc-buffer, dvc-nested-tree, etc...
See dvc-defs.el.
state values can be closed or open"
  (interactive
   (let* ((current-tree (aref (dvc-bookmarks-current-bookmark) 1))
          (current-color (if (hash-has-key (intern current-tree)
                                           dvc-bookmarks-cache)
                             (cdr (assoc
                                   'color
                                   (gethash (intern current-tree)
                                            dvc-bookmarks-cache)))))
          (current-state (if (hash-has-key (intern current-tree)
                                           dvc-bookmarks-cache)
                             (cdr (assoc
                                   'state
                                   (gethash (intern current-tree)
                                            dvc-bookmarks-cache)))))
          (def-color (dvc-completing-read  "Color: "
                                           dvc-table-face
                                           nil
                                           t
                                           (format "%s" current-color)
                                           'minibuffer-history))
          (def-state (dvc-completing-read "State: "
                                          '("open" "closed")
                                          nil
                                          t
                                          current-state
                                          'minibuffer-history)))
     (list def-color def-state)))
  (let* ((current-tree (aref (dvc-bookmarks-current-bookmark) 1))
         (time-stamp (if (equal (cdr (assoc
                                      'state
                                      (gethash (intern current-tree)
                                               dvc-bookmarks-cache)))
                                state)
                         (cdr (assoc
                               'time-stamp
                               (gethash (intern current-tree)
                                        dvc-bookmarks-cache)))
                       (dvc-cur-date-string)))
         (new-entry (concat
                     (format "(puthash '%S '((color . %S) (state . %S) (time-stamp . %S))"
                             (intern current-tree)
                             (intern color)
                             state
                             time-stamp)
                     " dvc-bookmarks-cache)")))
    (save-excursion
      (find-file dvc-bookmarks-prop-file)
      (goto-char (point-min))
      (setq case-fold-search nil)
      (if (hash-has-key (intern current-tree)
                        dvc-bookmarks-cache)
          (when (re-search-forward current-tree)
            (beginning-of-line)
            (kill-line)
            (insert new-entry))
        (goto-char (point-max))
        (forward-line)
        (insert new-entry))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (set-dvc-bookmarks-cache)
  (dvc-bookmarks))


(defun dvc-bookmarks-ignore-closed-trees ()
  "If state of tree is closed don't print all children"
  (ewoc-filter dvc-bookmarks-cookie #'(lambda (x)
                                        (or (assoc (aref x 1) dvc-bookmark-alist)
                                            (not (hash-has-key (intern (dvc-get-parent-elm (aref x 1)
                                                                                           dvc-bookmark-alist))
                                                               dvc-bookmarks-cache))
                                            (equal (cdr (assoc
                                                         'state
                                                         (gethash (intern (dvc-get-parent-elm (aref x 1)
                                                                                              dvc-bookmark-alist))
                                                                  dvc-bookmarks-cache)))
                                                   "open")))))

(add-hook 'dvc-bookmarks-mode-hook 'dvc-bookmarks-ignore-closed-trees)

(defvar dvc-bookmarks-show-time-stamp t)
(defun dvc-bookmarks-toggle-time-stamp ()
  "Toggle show/don't show time-stamp"
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (if dvc-bookmarks-show-time-stamp
        (setq dvc-bookmarks-show-time-stamp nil)
      (setq dvc-bookmarks-show-time-stamp t))
    (dvc-bookmarks)
    (goto-char beg)
    (beginning-of-line)))

(defun dvc-bookmarks-printer (data)
  (let* ((entry (dvc-bookmark-name data))
         (indent (dvc-bookmark-indent data))
         (partners (and dvc-bookmarks-show-partners
                        (dvc-bookmark-partners data)))
         (nick-name)
         (partner-string)
         (date (cadr
                (assoc 'time-stamp
                       (assoc entry
                              (cadr (assoc (dvc-get-parent-elm entry dvc-bookmark-alist )
                                           dvc-bookmark-alist))))))
         (entry-string (if (hash-has-key (intern entry) dvc-bookmarks-cache)
                           (format "%s%s" (make-string indent ? ) (concat  entry
                                                                           " ["
                                                                           (cdr (assoc
                                                                                 'state
                                                                                 (gethash (intern entry)
                                                                                          dvc-bookmarks-cache)))
                                                                           "]["
                                                                           (cdr (assoc
                                                                                 'time-stamp
                                                                                 (gethash (intern entry)
                                                                                          dvc-bookmarks-cache)))
                                                                           "]"))
                         (format "%s%s" (make-string indent ? ) (if (and dvc-bookmarks-show-time-stamp
                                                                         date)
                                                                    (concat entry
                                                                            " ["
                                                                            date
                                                                            "]")
                                                                  entry)))))
    ;;(dvc-trace "dvc-bookmarks-printer - data: %S, partners: %S" data partners)
;;;     (when (and dvc-bookmarks-marked-entry (string= dvc-bookmarks-marked-entry entry))
;;;       (setq entry-string (dvc-face-add entry-string 'dvc-marked)))
    (if (assoc entry dvc-bookmark-alist)
        (if (hash-has-key (intern entry) dvc-bookmarks-cache)
            (setq entry-string (dvc-face-add entry-string
                                             (cdr (assoc
                                                   'color
                                                   (gethash (intern entry)
                                                            dvc-bookmarks-cache)))))
          (setq entry-string (dvc-face-add entry-string dvc-bookmarks-face-tree)))
      (setq entry-string (dvc-face-add entry-string dvc-bookmarks-face-subtree)))
    (when (and dvc-bookmarks-marked-entry-list
               (member entry dvc-bookmarks-marked-entry-list))
      (setq entry-string (dvc-face-add entry-string 'dvc-marked)))
    (insert entry-string)
    (when partners
      (dolist (p partners)
        (setq nick-name (dvc-bookmark-partner-nickname p))
        (setq partner-string (format "\n%sPartner %s%s"
                                     (make-string (+ 2 indent) ? )
                                     (if nick-name
                                         (if dvc-bookmarks-show-partner-url
                                             (dvc-bookmark-partner-url p)
                                           "")
                                       (dvc-bookmark-partner-url p))
                                     (if nick-name (format "  [%s]" nick-name) "")))
        (setq partner-string (dvc-face-add partner-string dvc-bookmarks-face-partner))
        (insert partner-string)))))

(defvar dvc-bookmarks-show-partner-url t
  "Define if we show partners url or not")

(defun dvc-bookmarks-toggle-partner-url ()
  "Toggle show/don't show partners urls"
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (if dvc-bookmarks-show-partner-url
        (setq dvc-bookmarks-show-partner-url nil)
      (setq dvc-bookmarks-show-partner-url t))
    (dvc-bookmarks)
    (goto-char beg)
    (beginning-of-line)))

(defun dvc-bookmarks-add-to-cookie (elem indent &optional node)
  (let ((curr (or node (ewoc-locate dvc-bookmarks-cookie)))
        (data (make-dvc-bookmark-from-assoc elem indent))
        (enter-function (if (eq (dvc-line-number-at-pos) 1) 'ewoc-enter-before 'ewoc-enter-after)))
    (cond ((or (assoc 'children elem)
               (and dvc-bookmarks-show-partners
                    (assoc 'partner elem)))
           (setq node
                 (if curr
                     (apply enter-function (list dvc-bookmarks-cookie curr data))
                   (let ((n (ewoc-enter-last dvc-bookmarks-cookie data)))
                     (forward-line 1)
                     n)))
           (dolist (child (reverse (cdr (assoc 'children elem))))
             (dvc-bookmarks-add-to-cookie child (+ indent 2) node)))
          (t
           (if curr
               (apply enter-function (list dvc-bookmarks-cookie curr data))
             (ewoc-enter-last dvc-bookmarks-cookie data))))
    (forward-line 2)))

;;;###autoload
(defun dvc-bookmarks (&optional arg)
  "Display the *dvc-bookmarks* buffer.
With prefix argument ARG, reload the bookmarks file from disk."
  (interactive "P")
  (dvc-bookmarks-load-from-file arg)
  (when (eq (hash-table-count dvc-bookmarks-cache) 0)
    (set-dvc-bookmarks-cache))
  (dvc-switch-to-buffer (get-buffer-create "*dvc-bookmarks*"))
  (let ((cur-pos (point)))
    (toggle-read-only 0)
    (erase-buffer)
    (set (make-local-variable 'dvc-bookmarks-cookie)
         (ewoc-create (dvc-ewoc-create-api-select
                       #'dvc-bookmarks-printer)))
    (put 'dvc-bookmarks-cookie 'permanent-local t)
    (dolist (entry dvc-bookmark-alist)
      (dvc-bookmarks-add-to-cookie entry 0))
    (if (eq major-mode 'dvc-bookmarks-mode)
        (goto-char cur-pos)
      (goto-char (point-min))))
  (dvc-bookmarks-mode))

(defun dvc-bookmarks-mode ()
  "Mode to display DVC bookmarks.

\\{dvc-bookmarks-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map dvc-bookmarks-mode-map)
  (setq major-mode 'dvc-bookmarks-mode)
  (setq mode-name "dvc-bookmarks")
  (toggle-read-only 1)
  (run-hooks 'dvc-bookmarks-mode-hook))

(defun dvc-bookmarks-quit ()
  "Clean dvc-bookmarks-hidden-subtree
and quit"
  (interactive)
  (setq dvc-bookmarks-hidden-subtree nil)
  (dvc-buffer-quit))

(defun dvc-bookmarks-show-info-at-point ()
  (interactive)
  (message "%S" (dvc-bookmarks-current-bookmark)))

(defun dvc-bookmarks-current-bookmark ()
  (ewoc-data (ewoc-locate dvc-bookmarks-cookie)))

(defun dvc-bookmarks-invalidate-current-bookmark ()
  "Regenerate the text for the bookmark under point."
  (ewoc-invalidate dvc-bookmarks-cookie (ewoc-locate dvc-bookmarks-cookie)))

(defun dvc-bookmarks-current-value (key)
  (dvc-bookmark-value (dvc-bookmarks-current-bookmark) key))

(defun dvc-bookmarks-current-key-value (key)
  (dvc-bookmark-key-value (dvc-bookmarks-current-bookmark) key))

(defun dvc-bookmarks-add (bookmark-name bookmark-local-dir)
  "Add a DVC bookmark named BOOKMARK-NAME, directory BOOKMARK-LOCAL-DIR."
  (interactive
   (let* ((bmk-name (read-string "DVC bookmark name: "))
          (bmk-loc (dvc-read-directory-name (format "DVC bookmark %s directory: " bmk-name))))
     (list bmk-name bmk-loc)))
  (let* ((date (dvc-cur-date-string))
         (elem (list bookmark-name
                     (list 'local-tree bookmark-local-dir)
                     (list 'time-stamp date)))
         (data (make-dvc-bookmark-from-assoc elem 0)))
    (dvc-bookmarks)
    (add-to-list 'dvc-bookmark-alist elem t)
    (ewoc-enter-last dvc-bookmarks-cookie data)))

(defun dvc-bookmarks-member-p (elm)
  "Predicate to test if `elm' is member
of dvc-bookmark-alist
`elm' is a string"
  (catch 'break
    (dolist (x dvc-bookmark-alist)
      (dolist (i (cdadr x))
        (when (string= elm (car i))
          (throw 'break t))))))

;;;###autoload
(defun dvc-bookmarks-dired-add-project ()
  "Add a DVC bookmark from dired"
  (interactive)
  (let ((ori-list (dired-get-marked-files))
        (bname-list))
    (save-excursion
      (dvc-bookmarks)
      (dolist (i ori-list)
        (if (not (dvc-bookmarks-member-p (file-name-nondirectory i)))
            (push i bname-list)))
    (if (yes-or-no-p (format "Add %s bookmarks to DVC-BOOKMARKS? "
                             (length bname-list)))
        (progn
          (dolist (i bname-list)
            (let ((bname (file-name-nondirectory i)))
              (when (file-directory-p i)
                (dvc-bookmarks-add bname i))))
          (dvc-bookmarks-save)
          (dvc-bookmark-goto-name (file-name-nondirectory (car (last bname-list)))))
        (message "Operation aborted")))))

(defun dvc-bookmarks-edit (bookmark-name bookmark-local-dir &optional bmk-time-stamp)
  "Change the current DVC bookmark's BOOKMARK-NAME and/or LOCAL-DIR."
  (interactive
   (let* ((old-name (dvc-bookmark-name (dvc-bookmarks-current-bookmark)))
          (cur-data (dvc-bookmarks-current-bookmark))
          (old-local-tree (dvc-bookmarks-current-value 'local-tree))
          (old-date (dvc-bookmarks-current-value 'time-stamp))
          (is-child (equal (first (split-string (aref cur-data 1) "-"))
                           "child"))
          (bmk-name (read-string "DVC bookmark name: " old-name))
          (bmk-loc (dvc-read-directory-name
                    (format "DVC bookmark %s directory: " bmk-name)
                    old-local-tree))
          (bmk-tmstp (unless is-child
                       (read-string "DVC bookmark time-stamp: " old-date))))
     (list bmk-name bmk-loc bmk-tmstp)))
  (if (assoc (aref (dvc-bookmarks-current-bookmark) 1) dvc-bookmark-alist)
      (error "Tree edition is not implemented yet! Sorry!")
    (let* ((node (ewoc-locate dvc-bookmarks-cookie))
           (old-data (ewoc-data node))
           (old-indent (dvc-bookmark-indent old-data))
           (elem (dvc-bookmark-elem old-data)))
      (setcar elem bookmark-name)
      (setcdr elem (cons (list 'local-tree bookmark-local-dir)
                         (cons (list 'time-stamp bmk-time-stamp)
                               (assq-delete-all 'time-stamp
                                                (assq-delete-all 'local-tree
                                                                 (cdr elem))))))
      (ewoc-set-data node (make-dvc-bookmark-from-assoc elem old-indent))
      (ewoc-invalidate dvc-bookmarks-cookie node))))

(defun dvc-bookmarks-next ()
  (interactive)
  (forward-line 1))

(defun dvc-bookmarks-previous ()
  (interactive)
  (forward-line -1))

(defun dvc-bookmarks-goto ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (find-file local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-goto-other-window ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (find-file-other-window local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-find-file-in-tree ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory (file-name-as-directory local-tree)))
          (find-file (read-file-name "Find file in bookmarked tree: ")))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-status ()
  "Run `dvc-status' for bookmark at point."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (dvc-status local-tree)
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-diff ()
  "Run `dvc-diff' for bookmark at point."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree))
        (partner (dvc-bookmark-get-hidden-url-at-point)))
    (if local-tree
        (if partner
            (progn (message "Running dvc diff for %s, against %s"
                            (dvc-bookmark-name (dvc-bookmarks-current-bookmark))
                            partner)
                   (let ((default-directory local-tree))
                     (dvc-diff-against-url partner)))
          (dvc-diff nil local-tree))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-log-edit ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-log-edit))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-changelog ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-changelog))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-log ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-log))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-missing ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((partner (dvc-bookmark-get-hidden-url-at-point)))
          (message "Running dvc missing for %s, against %s"
                   (dvc-bookmark-name (dvc-bookmarks-current-bookmark))
                   partner)
          (sit-for 1)
          (dvc-missing partner local-tree))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-pull ()
  "Pull from partner at point or default into current bookmark."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree)
              (partner (dvc-bookmark-get-hidden-url-at-point))
              (nickname (dvc-bookmark-unmask-nickname-at-point)))
          (message (if partner
                       (if nickname
                           (format "Pulling from %s, using URL %s" nickname partner)
                         (format "Pulling from %s" partner))
                     "Pulling from default location"))
          (dvc-pull partner))
      (message "No local-tree defined for this bookmark entry."))))

(defun dvc-bookmarks-push ()
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (if local-tree
        (let ((default-directory local-tree))
          (dvc-push))
      (message "No local-tree defined for this bookmark entry."))))

(defvar dvc-bookmarks-merge-template "Merged from %s: ")
(defun dvc-bookmarks-merge ()
  "Merge from partner at point into current bookmark."
  (interactive)
  (let ((local-tree (dvc-bookmarks-current-value 'local-tree)))
    (setq local-tree (dvc-uniquify-file-name local-tree))
    (if local-tree
        (let ((default-directory local-tree)
              (partner (dvc-bookmarks-partner-at-point t))
              (nickname (dvc-bookmarks-nickname-at-point)))
          (setq dvc-memorized-log-header (when nickname (format dvc-bookmarks-merge-template nickname)))
          (setq dvc-memorized-log-message nil)
          (message (if nickname
                       (format "Merging from %s, using URL %s" nickname partner)
                     (format "Merging from %s" partner)))
          (dvc-merge partner))
      (message "No local-tree defined for this bookmark entry."))))

;; backend functions to yank
(defun dvc-get-index-el-list (elm lis)
  "Get index of element in list"
  (let ((n 0)
        (index 0))
    (if (member elm lis)
        (progn
          (dolist (x lis)
            (when (equal x elm)
              (setq index n))
            (setq n (+ n 1)))
          index)
      (error "No element %s in %s" elm lis))))

(defun dvc-move-element-in-list (name-elm lis where)
  "move element `name-elm' of list `lis' to index `where'
in list `lis'.
`name-elm' have the form of element in list.
`lis' is a LIST
`where' is an INTEGER"
  (let* ((index-elm (dvc-get-index-el-list name-elm lis))
         (start-part-list (subseq lis 0 where))
         (mod-list (append (remove name-elm start-part-list)
                           (cons name-elm
                                 (remove name-elm (subseq lis where))))))
    mod-list))

(defun dvc-add-to-list-at-ind (elm lis where)
  "Add `elm' in `lis' at index `where'"
  (let ((cons-list (cons elm lis))
        (appended-list nil))
    (setq appended-list
          (dvc-move-element-in-list elm cons-list (+ 1 where)))
    appended-list))

(defun dvc-move-elm-in-list-or-sublist (name-elm lis where &optional subtree)
  "move element `name-elm' of list `lis' to index `where' in list `lis'

elm ==> any element of a list
lis ==> the main list
where ==> a number, index to use of list or sublist
subtree ==> the sublist:
any quoted list or function that return a sublist of lis

Examples:

,----
| ELISP> (setq *A* '((1 2 3 4) a b c d e f))
| ((1 2 3 4)
|  a b c d e f)
|
| ELISP> (dvc-move-elm-in-list-or-sublist 'a *A* 1 '(1 2 3 4))
| ((1 a 2 3 4)
|  b c d e f)
|
| ELISP> (dvc-move-elm-in-list-or-sublist 1 *A* 2 '(1 2 3 4))
| ((2 3 4)
|  a b 1 c d e f)
|
| ELISP> (dvc-move-elm-in-list-or-sublist 'e *A* 1)
| ((1 2 3 4)
|  e a b c d f)
`----

"
  (let* ((subtree-index (when subtree
                          (dvc-get-index-el-list subtree lis)))
         (list-to-use (if subtree
                          (nth subtree-index lis)
                        lis))
         (modif-list (if (member name-elm lis)
                         (dvc-add-to-list-at-ind name-elm list-to-use where)
                       (dvc-move-element-in-list name-elm list-to-use where))))
    (if subtree
        (cond ((member name-elm lis)
               (dvc-add-to-list-at-ind modif-list (remove subtree (remove name-elm lis)) subtree-index))
              ((member name-elm subtree)
               (let ((append-list (dvc-add-to-list-at-ind name-elm (remove subtree lis) where)))
                 (dvc-add-to-list-at-ind (remove name-elm subtree) append-list subtree-index)))
              (t
               (dvc-add-to-list-at-ind modif-list (remove subtree lis) subtree-index)))
      modif-list)))

(defun dvc-get-parent-elm (elm list)
  "Return the name of sublist where current element is"
  (let ((head nil))
    (dolist (x list)
      (when (member (assoc elm (assoc 'children x))
                    (assoc 'children x))
        (setq head (car x))))
    head))

;; Yanking

(defun dvc-bookmarks-yank ()
  "Choose to yank marked or at point"
  (interactive)
  (if dvc-bookmarks-marked-entry-list
      (dvc-bookmarks-yank-all-marked-at-point)
      (dvc-bookmarks-really-yank)))


(defun dvc-bookmarks-really-yank ()
  "Check which function call and call it"
  ;(interactive)
  (let* ((killed-elm (aref dvc-bookmarks-tmp-yank-item 3))
         (yank-point (aref (dvc-bookmarks-current-bookmark) 3))
         (parent-elm (if (and (member killed-elm dvc-bookmark-alist)
                              (not (member yank-point dvc-bookmark-alist)))
                         (dvc-get-parent-elm (aref (dvc-bookmarks-current-bookmark) 1)
                                             dvc-bookmark-alist)
                       (dvc-get-parent-elm (aref dvc-bookmarks-tmp-yank-item 1)
                                           dvc-bookmark-alist)))
         (child-alist (cadr (assoc parent-elm dvc-bookmark-alist)))
         (cur-pos (point)))
    (cond ((and (member killed-elm dvc-bookmark-alist)
                (member yank-point dvc-bookmark-alist))
           (dvc-bookmarks-yank-from-list-to-list))
          ((and (member killed-elm dvc-bookmark-alist)
                (member yank-point child-alist))
           (dvc-bookmarks-yank-from-list-to-sub))
          ((and (member killed-elm child-alist)
                (member yank-point dvc-bookmark-alist))
           (dvc-bookmarks-yank-from-sub-to-list))
          ((and (not (member killed-elm dvc-bookmark-alist))
                (not (member yank-point dvc-bookmark-alist)))
           (dvc-bookmarks-yank-from-sub-to-sub))
          (t (message "This yank is not implemented yet sorry!")))
    (goto-char cur-pos)))

(defun dvc-bookmarks-yank-from-list-to-sub ()
  "Yank from list ==> sublist"
  (interactive)
  (let* ((elm-to-move (aref dvc-bookmarks-tmp-yank-item 3))
         (elm-at-point (aref (dvc-bookmarks-current-bookmark) 3))
         (parent (dvc-get-parent-elm (aref (dvc-bookmarks-current-bookmark) 1)
                                     dvc-bookmark-alist))
         (sublist (assoc parent dvc-bookmark-alist))
         ;; get index of sub and store it
         (sub-index (dvc-get-index-el-list sublist dvc-bookmark-alist))
         (child-dvc-bookmark-alist (cadr sublist))
         (alist-nosub (remove sublist dvc-bookmark-alist))
         (which-list (cond ((member elm-at-point child-dvc-bookmark-alist)
                            child-dvc-bookmark-alist)
                           ((member elm-at-point sublist)
                            sublist)
                           (t dvc-bookmark-alist)))
         (yank-index (dvc-get-index-el-list elm-at-point which-list))
         ;; move elm at the root of sublist
         (tmp-alist (dvc-move-elm-in-list-or-sublist elm-to-move
                                                     dvc-bookmark-alist
                                                     1
                                                     sublist)))
    ;; now move elm in the '(children)
    (setq sublist
          (dvc-move-elm-in-list-or-sublist elm-to-move
                                           (assoc parent tmp-alist)
                                           (+ 1 yank-index)
                                           child-dvc-bookmark-alist))
    (when (not (consp (nth 1 sublist))) ; hack to fix a small bug in backend func
      (setq sublist (remove (nth 1 sublist) sublist)))
    ;; replace the sublist modified to initial place
    (setq dvc-bookmark-alist
          (dvc-add-to-list-at-ind sublist alist-nosub sub-index))
    (setq dvc-bookmark-alist
          (remove elm-to-move dvc-bookmark-alist))
    (ewoc-refresh dvc-bookmarks-cookie))
  (dvc-bookmarks-save)
  (dvc-bookmarks))

(defun dvc-bookmarks-yank-from-sub-to-list ()
  "Yank from sublist ==> list"
  (interactive)
  (let* ((elm-to-move (aref dvc-bookmarks-tmp-yank-item 3))
         (elm-at-point (aref (dvc-bookmarks-current-bookmark) 3))
         (parent (dvc-get-parent-elm (aref dvc-bookmarks-tmp-yank-item 1)
                                     dvc-bookmark-alist))
         (sublist (assoc parent dvc-bookmark-alist))
         ;; get index of sublist and store it
         (sub-index (dvc-get-index-el-list sublist dvc-bookmark-alist))
         (child-dvc-bookmark-alist (cadr sublist))
         (alist-nosub (remove sublist dvc-bookmark-alist))
         (yank-index (dvc-get-index-el-list elm-at-point dvc-bookmark-alist))
         ;; now move elm out of '(children)
         (tmp-sublist (dvc-move-elm-in-list-or-sublist elm-to-move
                                                       sublist
                                                       1
                                                       child-dvc-bookmark-alist))
         (tmp-alist nil))
    ;; replace the sublist modified to initial place
    (setq tmp-alist (dvc-add-to-list-at-ind tmp-sublist alist-nosub sub-index))
    ;; now move elm to root of dvc-bookmark-alist
    (if (member elm-to-move child-dvc-bookmark-alist)
        ;; elm-to-move was in child
        (setq dvc-bookmark-alist (dvc-move-elm-in-list-or-sublist elm-to-move
                                                                  tmp-alist
                                                                  yank-index
                                                                  tmp-sublist))
      ;; elm-to-move was in sublist ("home-dir"...)
      (setq dvc-bookmark-alist (dvc-move-elm-in-list-or-sublist elm-to-move
                                                                dvc-bookmark-alist
                                                                yank-index
                                                                sublist)))
    (ewoc-refresh dvc-bookmarks-cookie))
  (dvc-bookmarks-save)
  (dvc-bookmarks))

(defun dvc-bookmarks-yank-from-sub-to-sub ()
  "Yank from one sublist to another sublist,
or in the same sublist"
  (interactive)
  (let* ((elm-to-move (aref dvc-bookmarks-tmp-yank-item 3))
         (elm-at-point (aref (dvc-bookmarks-current-bookmark) 3))
         (parent-from (dvc-get-parent-elm (aref dvc-bookmarks-tmp-yank-item 1)
                                          dvc-bookmark-alist))
         (parent-to (dvc-get-parent-elm (aref (dvc-bookmarks-current-bookmark) 1)
                                        dvc-bookmark-alist))
         (sublist1 (assoc parent-from dvc-bookmark-alist))
         (sublist2 (assoc parent-to dvc-bookmark-alist))
         (sub-index (dvc-get-index-el-list sublist1 dvc-bookmark-alist))
         (sub-index2 (dvc-get-index-el-list sublist2 dvc-bookmark-alist))
         ;; index point (yank here + 1)
         (yank-index (dvc-get-index-el-list elm-at-point (cadr sublist2)))
         (yank-index-sub-in-sub nil)
         ;; dvc-bookmark-alist without sublist1
         (alist-nosub (remove sublist1 dvc-bookmark-alist))
         ;; initial sublist with elm-to-move at root of sublist
         (tmp-sublist (dvc-move-elm-in-list-or-sublist elm-to-move
                                                       sublist1
                                                       1
                                                       (cadr sublist1)))
         ;; replace sublist1 modified to initial place
         (tmp-alist (dvc-add-to-list-at-ind tmp-sublist
                                            alist-nosub
                                            sub-index))
         ;; the new alist without sub2
         (alist-nosub2 nil))
    ;; check now if we yank in the same sub or an external one
    (if (equal parent-from parent-to)
        ;; we yank in the same sub
        (progn
          ;; move elm-to-move in child
          (setq yank-index-sub-in-sub
                (dvc-get-index-el-list elm-at-point (cadr tmp-sublist)))
          (setq sublist1
                (dvc-move-elm-in-list-or-sublist elm-to-move
                                                 tmp-sublist
                                                 (+ 1 yank-index-sub-in-sub)
                                                 (cadr tmp-sublist)))
          (setq dvc-bookmark-alist
                (dvc-add-to-list-at-ind sublist1
                                        alist-nosub
                                        sub-index)))
      ;; else: we yank in another sub
      ;; now move elm-to-move to root of dvc-bookmark-alist
      (setq tmp-alist
            (dvc-move-elm-in-list-or-sublist elm-to-move
                                             tmp-alist
                                             1
                                             tmp-sublist))
      ;; now move elm-to-move to root of sub2
      (setq tmp-alist
            (dvc-move-elm-in-list-or-sublist elm-to-move
                                             tmp-alist
                                             1
                                             sublist2))

      ;; now move elm-to-move to child of sub2 at yank-index
      (setq sublist2
            (dvc-move-elm-in-list-or-sublist elm-to-move
                                             (assoc parent-to tmp-alist)
                                             (+ 1 yank-index)
                                             (cadr sublist2)))
      ;; create now a new dvc-bookmark-alist with the sub2 modified
      (when (not (consp (nth 1 sublist2))) ; hack to fix a small bug in backend func
        (setq sublist2 (remove (nth 1 sublist2) sublist2)))
      ;; at this point we have just to remove elm-to-move from sub1
      (setq dvc-bookmark-alist
            (dvc-add-to-list-at-ind (remove elm-to-move (assoc parent-from tmp-alist))
                                    alist-nosub
                                    sub-index))
      ;; set an alist without old sub2
      (setq alist-nosub2
            (remove (assoc parent-to dvc-bookmark-alist)
                    dvc-bookmark-alist))
      ;; add new sublist2 to the alist without sub2
      (setq dvc-bookmark-alist
            (dvc-add-to-list-at-ind sublist2
                                    alist-nosub2
                                    sub-index2)))
    (ewoc-refresh dvc-bookmarks-cookie))
  (dvc-bookmarks-save)
  (dvc-bookmarks))

(defun dvc-bookmarks-yank-from-list-to-list ()
  "Yank inside dvc-bookmark-alist: list ==> list"
  (interactive)
  (let* ((elm-to-move (assoc (dvc-bookmark-name dvc-bookmarks-tmp-yank-item)
                             dvc-bookmark-alist))
         (elm-at-point (assoc (dvc-bookmark-name (dvc-bookmarks-current-bookmark))
                              dvc-bookmark-alist))
         (yank-index (dvc-get-index-el-list elm-at-point dvc-bookmark-alist)))
    (setq dvc-bookmark-alist (dvc-move-element-in-list elm-to-move dvc-bookmark-alist (+ 1 yank-index)))
    (ewoc-refresh dvc-bookmarks-cookie))
  (dvc-bookmarks-save)
  (dvc-bookmarks))


(defvar dvc-bookmarks-hidden-subtree nil
  "List of all hidden subtrees")

(defun dvc-bookmarks-show-or-hide-subtree (&optional show)
  "Hide subtree when called with no argument
show subtree when called with prefix argument (C-u)"
  (interactive "P")
  (let ((current-tree (aref (dvc-bookmarks-current-bookmark) 1))
        (parent))
    (when (member (assoc current-tree dvc-bookmark-alist) dvc-bookmark-alist) ;check if we are really on a tree
      (if current-prefix-arg
          (progn
            (setq dvc-bookmarks-hidden-subtree (remove current-tree dvc-bookmarks-hidden-subtree))
            (dvc-bookmarks))
        (add-to-list 'dvc-bookmarks-hidden-subtree current-tree))
      (ewoc-filter dvc-bookmarks-cookie #'(lambda (x)
                                            (setq parent (dvc-get-parent-elm (aref x 1) dvc-bookmark-alist))
                                            (if (not (member parent dvc-bookmarks-hidden-subtree))
                                                t
                                              nil))))))

(defvar dvc-bookmarks-tmp-yank-item '("hg" (local-tree "~/work/hg/hg")))

(defun dvc-bookmarks-delete-at-point ()
  "Destructive kill and delete function
do not use it to kill/yank, use dvc-bookmarks-kill instead"
  (interactive)
  (let ((init-place (point))
        (current-bookmark))
    (dvc-bookmarks-kill-at-point)
    (setq current-bookmark (dvc-bookmark-name dvc-bookmarks-tmp-yank-item))
    (if (assoc (dvc-bookmark-name dvc-bookmarks-tmp-yank-item) dvc-bookmark-alist)
        (progn
          (setq dvc-bookmark-alist (remove (assoc (dvc-bookmark-name dvc-bookmarks-tmp-yank-item) dvc-bookmark-alist)
                                           dvc-bookmark-alist))
          (ewoc-refresh dvc-bookmarks-cookie)
          (dvc-bookmarks-save)
          ;;(dvc-bookmarks)
          (if (hash-has-key (intern current-bookmark)
                            dvc-bookmarks-cache)
              (save-excursion
                (find-file dvc-bookmarks-prop-file)
                (setq case-fold-search nil)
                (goto-char (point-min))
                (when (re-search-forward current-bookmark)
                  (beginning-of-line)
                  (kill-line)
                  (delete-blank-lines)
                  (save-buffer)
                  (kill-buffer (current-buffer)))
                (set-dvc-bookmarks-cache))))
      (message "Please move first this element to root and then delete it"))
    (dvc-bookmarks)
    (goto-char init-place)))

(defun dvc-bookmarks-kill-at-point ()
  "kill or cut bookmark at point"
  (setq dvc-bookmarks-tmp-yank-item (dvc-bookmarks-current-bookmark))
  (let ((buffer-read-only nil)
        (current-tree (aref (dvc-bookmarks-current-bookmark) 1))
        (parent))
    (if (member (assoc current-tree dvc-bookmark-alist) dvc-bookmark-alist)
        (ewoc-filter dvc-bookmarks-cookie #'(lambda (x)
                                              (setq parent (dvc-get-parent-elm (aref x 1) dvc-bookmark-alist))
                                              (when (not (equal current-tree (aref x 1)))
                                                (if (not (equal parent current-tree))
                                                    t
                                                  nil))))
      (dvc-ewoc-delete dvc-bookmarks-cookie (ewoc-locate dvc-bookmarks-cookie)))))

(defun dvc-bookmarks-kill ()
  "Choose to kill marked entry or entry at point"
  (interactive)
  (if dvc-bookmarks-marked-entry-list
      (dvc-bookmarks-kill-all-marked)
      (dvc-bookmarks-kill-at-point)))

(defun dvc-bookmarks-delete ()
  "Choose to delete marked entry or entry at point"
  (interactive)
  (if dvc-bookmarks-marked-entry-list
      (if (yes-or-no-p (format "Really delete %s bookmarks?"
                               (length dvc-bookmarks-marked-entry-list)))
          (dvc-bookmarks-delete-all-marked)
          (message "Action aborted"))
      (if (yes-or-no-p "Really delete this bookmarks?")
          (dvc-bookmarks-delete-at-point)
          (message "Action aborted"))))

(defun dvc-bookmarks-add-empty-tree (name)
  "Add a new family to your bookmarks"
  (interactive "sName: ")
  (let ((child-name (concat "child-" name)))
    (if (not (assoc name dvc-bookmark-alist))
        (progn
          (add-to-list 'dvc-bookmark-alist
                       (list name
                             `(children
                               (,child-name
                                (local-tree "~/")))) t)
          (ewoc-refresh dvc-bookmarks-cookie))
      (error "Tree %s already exist please choose another name" name)))
  (dvc-bookmarks-save)
  (dvc-bookmarks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Marked bookmark code ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dvc-bookmarks-marked-entry-list nil
  "List of marked bookmarks")

(defun dvc-bookmarks-toggle-mark-entry ()
  "Mark or unmark the current bookmark entry.
And add it to the `dvc-bookmarks-marked-entry-list'"
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (bmk-name (dvc-bookmark-name cur-data))
         (has-children (dvc-bookmarks-current-value 'children))
         (is-child (equal (first (split-string (aref cur-data 1) "-"))
                          "child")))
    ;; (message "bmk-name: %s has-children: %s" bmk-name has-children)
    (unless (or has-children
                is-child)
      (if (member bmk-name dvc-bookmarks-marked-entry-list)
          (progn
            (message "Unmarking bookmark entry %s" bmk-name)
            (setq dvc-bookmarks-marked-entry-list
                  (remove bmk-name dvc-bookmarks-marked-entry-list)))
          (message "Marking bookmark entry %s" bmk-name)
          (push bmk-name dvc-bookmarks-marked-entry-list))
      (dvc-bookmarks-goto-next)
      (dvc-bookmarks-reload))))

(defun dvc-bookmarks-reload ()
  "Remember the last position and reload dvc-bookmarks"
  (let ((last-pos (dvc-bookmark-name (dvc-bookmarks-current-bookmark))))
    (dvc-bookmarks)
    (dvc-bookmark-goto-name last-pos)))

(defun dvc-bookmarks-goto-next ()
  "Go to next bookmark even if there is
closed tree(s) behind; in this case jump over
partners will not be performed"
  (let (flag-fwdl)
    (save-excursion
      (when (re-search-backward "closed" nil t)
            (setq flag-fwdl t)))
    (if flag-fwdl
        (forward-line 1)
        (ewoc-goto-next dvc-bookmarks-cookie 1))))

(defun dvc-bookmarks-unmark-all ()
  "Unmark all bookmarks."
  (interactive)
  (setq dvc-bookmarks-marked-entry-list nil)
  (message "Unmarking all")
  (dvc-bookmarks-reload))

(defun dvc-bookmarks-marked-p ()
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (bmk-name (dvc-bookmark-name cur-data))
         (has-children (dvc-bookmarks-current-value 'children)))
    (unless has-children
      (if (member bmk-name dvc-bookmarks-marked-entry-list)
          t
          nil))))

(defun dvc-bookmarks-apply-func-on-marked (fn)
  (dolist (i dvc-bookmarks-marked-entry-list)
    (dvc-bookmark-goto-name i)
    (funcall fn)))

(defun dvc-bookmarks-delete-all-marked ()
  (interactive)
  (dvc-bookmarks-apply-func-on-marked 'dvc-bookmarks-delete-at-point)
  (setq dvc-bookmarks-marked-entry-list nil))

(defvar dvc-bookmarks-kill-ring nil)
(defun dvc-bookmarks-kill-all-marked ()
  "Kill all marked entry and put them in the
`dvc-bookmarks-kill-ring'"
  (setq dvc-bookmarks-kill-ring nil)
  (dolist (i dvc-bookmarks-marked-entry-list)
    (dvc-bookmark-goto-name i)
    (dvc-bookmarks-kill-at-point)
    (push dvc-bookmarks-tmp-yank-item
          dvc-bookmarks-kill-ring)))

(defun dvc-bookmarks-yank-all-marked-at-point ()
  "Yank all marked entries at point
and reinit `dvc-bookmarks-kill-ring'"
  (if dvc-bookmarks-kill-ring
      (progn
        (dolist (i dvc-bookmarks-kill-ring)
          (setq dvc-bookmarks-tmp-yank-item i)
          (dvc-bookmarks-really-yank)
          (dvc-bookmark-goto-name (aref i 1)))
        (setq dvc-bookmarks-kill-ring nil)
        (setq dvc-bookmarks-tmp-yank-item nil))
      (message "Did you forget to kill? (C-k)")))

(defun dvc-bookmarks-get-marked-with-name (name)
  (when (and dvc-bookmarks-marked-entry-list
             (member name dvc-bookmarks-marked-entry-list))
    (save-excursion
      (dvc-bookmark-goto-name name)
      (dvc-bookmarks-current-bookmark))))

(defun dvc-bookmarks-marked-value (key name)
  "Get the value of a marked bookmark for key."
  (let ((marked-bookmark (dvc-bookmarks-get-marked-with-name name)))
    (when marked-bookmark
      (dvc-bookmark-value marked-bookmark key))))

(defun dvc-bookmarks-hg-convert-from-marked ()
  "Call `xhg-convert' with current dvc-bookmark as target and
marked dvc-bookmark as source."
  (interactive)
  (let* ((target (dvc-bookmarks-current-value 'local-tree))
         (cur-dvc (dvc-bookmarks-active-dvc-at-point))
         (marked (car dvc-bookmarks-marked-entry-list))
         (source (dvc-bookmarks-marked-value 'local-tree marked)))
    (when (eq cur-dvc 'xhg)
      (if (= (length dvc-bookmarks-marked-entry-list) 1)
          (if (y-or-n-p (format "Convert <%s> to <%s>?"
                                source
                                (propertize target
                                            'face 'dvc-id)))
              (xhg-convert source target))
          (message "Please mark ONE source to convert from!")))))

(defun dvc-bookmarks-active-dvc-at-point ()
  (let ((path (dvc-bookmarks-current-value 'local-tree))
        (current-dvc))
    (save-excursion
      (find-file path)
      (setq current-dvc (dvc-current-active-dvc))
      (kill-buffer (current-buffer)))
    current-dvc))

(defun dvc-bookmarks-save ()
  "Save `dvc-bookmark-alist' to the file `dvc-bookmarks-file-name'."
  (interactive)
  (dvc-save-state '(dvc-bookmark-alist)
                  (dvc-config-file-full-path dvc-bookmarks-file-name t)
                  t))

(defun dvc-bookmarks-load-from-file (&optional force)
  "Load bookmarks from the file `dvc-bookmarks-file-name'.

If FORCE is non-nil, reload the file even if it was loaded before."
  (when (or force (not dvc-bookmarks-loaded))
    (dvc-load-state (dvc-config-file-full-path
                     dvc-bookmarks-file-name t))
    (setq dvc-bookmarks-loaded t)))

(defun dvc-bookmark-name-1 (entry &optional parent-name)
  (cond ((assoc 'children entry)
         (let ((names))
           (dolist (child (cdr (assoc 'children entry)))
             (add-to-list 'names (car (dvc-bookmark-name-1 child (car entry)))))
           names))
        (t
         (list (concat (if parent-name (concat  parent-name "/") "") (car entry))))))

(defun dvc-bookmark-names ()
  "Return a list with all dvc bookmark names."
  (let ((names))
    (dolist (entry dvc-bookmark-alist)
      (setq names (append names (dvc-bookmark-name-1 entry))))
    names))

(defun dvc-bookmark-local-tree-mapping-1 (entry)
  (cond ((assoc 'children entry)
         (let ((tree-mapping))
           (dolist (child (cdr (assoc 'children entry)))
             (add-to-list 'tree-mapping (car (dvc-bookmark-local-tree-mapping-1 child))))
           tree-mapping))
        (t
         (list (list (dvc-uniquify-file-name (cadr (assoc 'local-tree (cdr entry)))) (car entry))))))

;; (dvc-bookmark-local-tree-mapping)

(defun dvc-bookmark-local-tree-mapping ()
  "Return an alist that maps from working copies to bookmark names."
  (let ((tree-mapping))
    (dolist (entry dvc-bookmark-alist)
      (setq tree-mapping (append tree-mapping (dvc-bookmark-local-tree-mapping-1 entry))))
    tree-mapping))


(defun dvc-bookmark-goto-name (name)
  (let ((cur-pos (point))
        (name-list (split-string name "/"))
        (prefix ""))
    (goto-char (point-min))
    (dolist (name name-list)
      (setq name (concat prefix name))
      (setq prefix (concat "  " prefix))
      (search-forward name))
    (beginning-of-line-text)))

(defun dvc-bookmarks-jump ()
  (interactive)
  (dvc-bookmark-goto-name (dvc-completing-read "Jump to dvc bookmark: "
                                               (dvc-bookmark-names))))

(defun dvc-bookmarks-get-partner-urls ()
  (dvc-bookmark-partner-urls (dvc-bookmarks-current-bookmark)))

(defun dvc-bookmarks-add-partner ()
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (partner-url (read-string (format "Add partner to '%s': "
                                           (dvc-bookmark-name cur-data)))))
    (if (not (member partner-url (dvc-bookmarks-get-partner-urls)))
        (progn
          (setf (dvc-bookmark-properties cur-data)
                (append (dvc-bookmark-properties cur-data)
                        (list (cons 'partner
                                    (make-dvc-bookmark-partner :url partner-url)))))
          (dvc-trace "dvc-bookmarks-add-partner %s" cur-data)
          (dvc-bookmarks-invalidate-current-bookmark))
      (message "%s is already a partner for %s"
               partner-url (dvc-bookmark-name cur-data)))))

(defun dvc-bookmarks-remove-partner ()
  (interactive)
  (let* ((cur-data (dvc-bookmarks-current-bookmark))
         (partners-alist (dvc-bookmark-partners-by-url cur-data))
         (partner-to-remove (dvc-completing-read
                             (format "Remove partner from %s: "
                                     (dvc-bookmark-name cur-data))
                             (mapcar 'car partners-alist)
                             nil t nil nil
                             (dvc-bookmarks-partner-at-point))))
    (setf (dvc-bookmark-properties cur-data)
          (delete (cons 'partner (cdr (assoc partner-to-remove partners-alist)))
                  (dvc-bookmark-properties cur-data)))
    (dvc-bookmarks-invalidate-current-bookmark)))

(defun dvc-bookmarks-toggle-partner-visibility ()
  (interactive)
  (setq dvc-bookmarks-show-partners (not dvc-bookmarks-show-partners))
  (dvc-bookmarks))

(defun dvc-bookmarks-partner-at-point (&optional expand-file-name-when-possible)
  (save-excursion
    (let ((partner-url))
      (goto-char (line-beginning-position))
      (when (looking-at "  +Partner \\(.+?\\)\\(  \\[.+\\)?$")
        (setq partner-url (match-string 1))
        (when (and expand-file-name-when-possible (file-directory-p partner-url))
          (setq partner-url (expand-file-name partner-url))))
      partner-url)))

(defun dvc-bookmarks-nickname-at-point ()
  (save-excursion
    (let ((nickname))
      (goto-char (line-beginning-position))
      (when (looking-at "  +Partner \\(.+?\\)  \\[\\(.+\\)?\\]$")
        (setq nickname (match-string 2)))
      nickname)))

(defun dvc-bookmarks-add-nickname ()
  (interactive)
  ;;(message "dvc-bookmarks-add-nickname %S" (dvc-bookmarks-current-bookmark))
  (let* ((url-at-point (dvc-bookmarks-partner-at-point))
         (bookmark (dvc-bookmarks-current-bookmark))
         (partner (cdr (assoc url-at-point
                              (dvc-bookmark-partners-by-url bookmark)))))
    (if partner
        (progn
          (setf (dvc-bookmark-partner-nickname partner)
                (read-string (format "Nickname for %s: " url-at-point)
                             (dvc-bookmark-partner-nickname partner)))
          (dvc-bookmarks-invalidate-current-bookmark)
          (message "Added nickname %s to the partner %s"
                   (dvc-bookmark-partner-nickname partner) url-at-point))
      (error "No partner with URL '%s'" url-at-point))))

(defun dvc-bookmarks-add-push-location ()
  (interactive)
  (let* ((push-locations (dvc-bookmarks-current-value 'push-locations))
         (cur-data (dvc-bookmarks-current-bookmark))
         (push-location (read-string (format "Add push location to '%s': " (dvc-bookmark-name cur-data)))))
    (if (not (member push-location push-locations))
        (progn
          (if (null push-locations)
              (progn
                (setq push-locations (list 'push-locations (list push-location)))
                (setf (dvc-bookmark-properties cur-data)
                      (append (dvc-bookmark-properties cur-data)
                              (list push-locations))))
            (setcdr push-locations (append (cdr push-locations)
                                           (list push-location)))))
      (message "%s is already a push-location for %s"
               push-location (dvc-bookmark-name cur-data)))))

(defun dvc-bookmarks-remove-push-location ()
  (interactive)
  (let* ((push-locations (dvc-bookmarks-current-key-value 'push-locations))
         (cur-data (dvc-bookmarks-current-bookmark))
         (location-to-remove (dvc-completing-read "Remove push location: " (cadr push-locations)))
         (new-push-locations (delete location-to-remove (cadr push-locations))))
    (if new-push-locations
        (setcdr push-locations (list new-push-locations))
      (setf (dvc-bookmark-properties cur-data)
            (delete push-locations (dvc-bookmark-properties cur-data))))))

;;;###autoload
(defun dvc-bookmarks-current-push-locations ()
  (let* ((tree-mapping (dvc-bookmark-local-tree-mapping))
         (bookmark-name (cadr (assoc (dvc-tree-root) tree-mapping)))
         (push-locations))
    (when bookmark-name
      (save-window-excursion
        (with-current-buffer "*dvc-bookmarks*"
          (dvc-bookmark-goto-name bookmark-name)
          (setq push-locations (dvc-bookmarks-current-value 'push-locations)))))
    ;;(message "bookmark-name: %s -> push-locations: %S" bookmark-name push-locations)
    push-locations))


;; (dvc-bookmarks-load-from-file t)

(provide 'dvc-bookmarks)
;;; dvc-bookmarks.el ends here

;;; dvc-defs.el --- Common definitions for DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributors: Matthieu Moy, <Matthieu.Moy@imag.fr>

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

;; This file provides the low-level functions used by the DVC interfaces
;; to distributed revison control systems.


;;; History:

;; This file holds general useful functions, previously only used for DVC.

;;; Code:

(eval-and-compile
  (require 'font-lock))

(require 'dvc-site)

(defmacro dvc-first-set (arg1 arg2)
  "Returns ARG1 if set, non-nil, and not the empty string.
Otherwise, return ARG2. ARG1 must be a variable."
  (declare (indent 1) (debug (symbolp form)))
  `(or (and ,(boundp arg1) (when (not (string= ,arg1 ""))
                             ,arg1))
       ,arg2))

(unless (fboundp 'executable-find)
  (autoload 'executable-find "executable"))

(defvar dvc-registered-backends nil "The list of registered dvc backends.")

(defgroup dvc nil
  "Decentralized Version Control interface for Emacs."
  :group 'tools
  :prefix "dvc-")

;; Common settings for all dvc's
(defcustom dvc-select-priority '()
  "A list that defines the priority of the available dvc's.
If a project uses more than one dvc, use this list to select the primary dvc.

Possible values include: 'tla, 'baz, 'xhg, 'xgit, 'bzr, 'xmtn"
  :type '(repeat (choice (const :tag "tla" tla)
                         (const :tag "baz" baz)
                         (const :tag "xhg" xhg)
                         (const :tag "xgit" xgit)
                         (const :tag "bzr" bzr)
                         (const :tag "xmtn" xmtn)
                         (symbol :tag "Other")))
  :group 'dvc)

(defcustom dvc-prompt-active-dvc nil
  "If non-nil, prompt for the active dvc when more than one is
found for the current directory. The back-ends considered are
given in dvc-select-priority (it must be non-nil - it should be
restricted it to only those back-ends actually used). Otherwise,
use the first one found; dvc-select-priority sets the search
order."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-highlight t
  "*Use highlighting for DVC buffers."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-confirm-add t
  "*If non-nil, prompt for confirmation in dvc-add-files."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-confirm-ignore t
  "*If non-nil, prompt for confirmation in dvc-ignore-files."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-confirm-update t
  "*If non-nil, prompt for confirmation in dvc-update."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-log-last-n nil
  "*If non-nil, limit log listings to last n entries."
  :type '(choice boolean integer)
  :group 'dvc)

(defcustom dvc-status-display-known nil
  "If non-nil, display files with 'known' status in dvc-status buffer."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-status-display-ignored nil
  "If non-nil, display files with 'ignored' status in dvc-status buffer."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-fileinfo-printer-interface 'full
  "How to display info about the working tree in DVC diff and status buffers.

The default is 'full, which uses explanatory text when listing
the status of the tree.

Another option is 'terse, which uses a single letter to indicate
the status of each file.

Alternatively, you may set this to the name of a custom function
which, given a fileinfo argument, produces the status list in the
current buffer."
  :group 'dvc
  :type '(choice (const :tag "Full" full)
                 (const :tag "Terse" terse)
                 (symbol :tag "Other")))

(defcustom dvc-completing-read-function 'auto
  "Function to call when prompting user to choose between a list of options.
This should take the same arguments as `completing-read'.
Possible values are `completing-read' and `ido-completing-read'.
Note that you must set `ido-mode' if using`ido-completing-read'.
When set to 'auto, use `ido-completing-read' when ido-mode is enabled,
otherwise `completing-read'."
  :type 'function
  :group 'dvc)

;; --------------------------------------------------------------------------------
;; Keybindings
;; --------------------------------------------------------------------------------
;; --------------------------------------------------------------------------------
;; Faces
;; --------------------------------------------------------------------------------

(defgroup dvc-faces nil
  "This group contains faces defined for DVC."
  :group 'dvc)

(defface dvc-revision-name
  '((((type tty) (class color)) (:foreground "lightblue" :weight light))
    (((class color) (background light)) (:foreground "blue4"))
    (((class color) (background dark)) (:foreground "lightskyblue1"))
    (t (:weight bold)))
  "Face to highlight DVC revision names."
  :group 'dvc-faces)

(defface dvc-repository-name
  '((t (:inherit dvc-revision-name)))
  "Face to highlight DVC repository name."
  :group 'dvc-faces)

(defface dvc-local-directory
  '((t (:inherit dvc-repository-name)))
  "Face to highlight DVC local directory."
  :group 'dvc-faces)

(defface dvc-buffer
  '((t (:inherit dvc-repository-name)))
  "Face to highlight buffer names printed in DVC's buffer."
  :group 'dvc-faces)

(defface dvc-marked
  '((((type tty) (class color)) (:foreground "magenta" :weight light))
    (((class color) (background light)) (:foreground "magenta"))
    (((class color) (background dark)) (:foreground "yellow"))
    (t (:weight bold)))
  "Face to highlight a marked entry in DVC buffers"
  :group 'dvc-faces)

(defface dvc-excluded
  '((((type tty) (class color)) (:foreground "orchid" :weight light))
    (((class color) (background light)) (:foreground "orchid"))
    (((class color) (background dark)) (:foreground "gold")))
  "Face to highlight an excluded entry in DVC buffers"
  :group 'dvc-faces)

(defface dvc-bookmark-name
  '((t (:inherit dvc-repository-name)))
  "Face to highlight DVC revision names."
  :group 'dvc-faces)

(defface dvc-id
  '((t (:inherit dvc-keyword)))
  "Face to highlight an arch id."
  :group 'dvc-faces)

(defface dvc-separator
  '((((type tty)) (:underline t :weight bold))
    ;;(((background light)) (:strike-through t :weight bold))
    ;;(((background dark))  (:strike-through t :weight bold)))
    (((background light)) (:underline t :weight bold))
    (((background dark))  (:underline t :weight bold)))
  "Face to highlight separators."
  :group 'dvc-faces)

(defface dvc-keyword
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight keywords."
  :group 'dvc-faces)

(defface dvc-comment
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight comments."
  :group 'dvc-faces)

(defface dvc-ignored
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight precious entries."
  :group 'dvc-faces)

(defface dvc-unrecognized
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight unrecognized entries."
  :group 'dvc-faces)

(defface dvc-duplicate
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight files with duplicate IDs."
  :group 'dvc-faces)

(defface dvc-source
  '((t (:inherit font-lock-string-face)))
  "Face to highlight source code entries."
  :group 'dvc-faces)

(defface dvc-nested-tree
  '((t (:inherit font-lock-type-face)))
  "Face to highlight nested trees."
  :group 'dvc-faces)

(defface dvc-to-add
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight a file that should probably be added to the archive."
  :group 'dvc-faces)

(defface dvc-broken-link
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight a broken link."
  :group 'dvc-faces)

(defface dvc-unmerged
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight unmerged patches."
  :group 'dvc-faces)

(defface dvc-header
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight header in log mode for example."
  :group 'dvc-faces)

(defface dvc-conflict
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight conflicts."
  :group 'dvc-faces)

(defface dvc-unknown
  '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight unknown status modification."
  :group 'dvc-faces)

(defface dvc-modified
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight modified files."
  :group 'dvc-faces)

(defface dvc-copy
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight copied files/directories."
  :group 'dvc-faces)

(defface dvc-move
  '((t (:inherit font-lock-constant-face)))
  ;; Same font as dvc-added, different from dvc-modified, so it stands out in a typical list.
  "Face to highlight moved files/directory."
  :group 'dvc-faces)

(defface dvc-deleted
  '((t (:inherit font-lock-warning-face)))
  "Face to highlight deleted files."
  :group 'dvc-faces)

(defface dvc-added
  '((t (:inherit font-lock-constant-face)))
  "Face to highlight added files."
  :group 'dvc-faces)

(defface dvc-meta-info
  '((t (:inherit font-lock-comment-face)))
  "Face to highlight files with meta-info changes."
  :group 'dvc-faces)

(defface dvc-messages
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight messages in DVC buffers."
  :group 'dvc-faces)

(defface dvc-highlight
  '((((class color) (background dark)) (:background "darkblue"))
    (((class color) (background light)) (:background "gold")))
  "Face to use as an alternative to `highlight' face.
If there could be more than two highlighted things, the user will confuse.
In such case use this face."
  :group 'dvc-faces)

(defface dvc-mark
  '((((class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:foreground "green3" :bold t))
    (t (:bold t)))
  "DVC face used to highlight marked file indicator."
  :group 'dvc-faces)

(defcustom dvc-bookmarks-face-tree 'dvc-keyword
  "DVC face used in bookmarks to highlight main tree entry's"
  :type 'face
  :group 'dvc-faces)

(defcustom dvc-bookmarks-face-subtree 'dvc-comment
  "DVC face used in bookmarks to highlight subtree entry's"
  :type 'face
  :group 'dvc-faces)

(defcustom dvc-bookmarks-face-partner 'dvc-revision-name
  "DVC face used in bookmarks to highlight partner entry's"
  :type 'face
  :group 'dvc-faces)

(defcustom dvc-button-face 'bold
  "DVC face used to highlight buttons.

An button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it."
  :type 'face
  :group 'dvc-faces)

(defcustom dvc-mouse-face 'highlight
  "DVC face used to highlight buttons.

Buttons will be displayed in this face when the cursor is above
them."
  :type 'face
  :group 'dvc-faces)

(defcustom dvc-switch-to-buffer-mode 'pop-to-buffer
  "*Mode for switching to DVC buffers.
Recommended settings are: 'pop-to-buffer, and 'show-in-other-window
and 'single-window"
  :type '(choice (const pop-to-buffer)
                 (const single-window)
                 (const dedicated-frame)
                 (const show-in-other-window))
  :group 'dvc)

(defgroup dvc-file-actions nil
  "This group contains items manipulating finding, saving and
reverting files."
  :group 'dvc)

(defcustom dvc-do-not-prompt-for-save nil
  "*Whether or not DVC will prompt before saving.

If non nil, DVC will not prompt you before saving buffers of the
working local tree."
  :type 'boolean
  :group 'dvc-file-actions)

(defcustom dvc-automatically-revert-buffers t
  "*Whether or not DVC will automatically revert buffers.

If non nil, DVC will automatically revert unmodified buffers after an
arch operation modifying the file."
  :type 'boolean
  :group 'dvc-file-actions)

(defgroup dvc-internal nil
  "This group contains items used mainly for debugging."
  :group 'dvc)

(defcustom dvc-log-commands t
  "*Non nil means log all DVC commands in the buffer *dvc-log*."
  :type 'boolean
  :group 'dvc-internal)

(defcustom dvc-log-buffer " *dvc-log*"
  "*Name of the buffer in which DVC logs main events."
  :type 'string
  :group 'dvc-internal)

(defcustom dvc-read-project-tree-mode 'sometimes
  "*Mode for prompting for project tree directories. Possible values are:

- always: always prompt.

- unless-specified: If a directory is given as an argument, use
  it; otherwise prompt. Some commands modify this to use the
  current project tree without prompt; then a user arg forces a
  prompt.

- sometimes: If a command is run inside a project tree, the tree
  root is used. Otherwise, prompt.

- never: If a command is run inside a project tree, use the tree
  root. Otherwise, raise an error."
  :type '(choice (const always)
                 (const unless-specified)
                 (const sometimes)
                 (const never))
  :group 'dvc)

(defcustom dvc-read-directory-mode 'sometimes
  "*How prompting project directories should be done.

Works similarly to `dvc-read-project-tree-mode', but this one is used
for commands like `tla-inventory' for which a subdirectory of a
project tree is accepted."
  :type '(choice (const always)
                 (const sometimes)
                 (const never))
  :group 'dvc)

(defcustom dvc-switch-to-buffer-first t
  "*Switch to newly created buffer on creation of buffers?

If non-nil, DVC commands implementing this feature will switch to the
newly created buffer when the command is called. Further (potentially
asynchronous) processes are run without modifying your
window-configuration. Otherwise, DVC will switch to the new buffer on
command completion."
  :type 'boolean
  :group 'dvc)

(defcustom dvc-buffer-quit-mode 'kill
  "*How *dvc-...* buffer should be killed.
If the value is 'kill, buffers are actually killed. Otherwise, just
bury them."
  :type '(choice (const kill)
                 (const bury))
  :group 'dvc)

(defcustom dvc-log-insert-last t
  "*If non-nil, insert changelog entries at the end of the log file."
  :type 'boolean
  :group 'dvc)

(defvar dvc-test-mode nil
  "Set non-nil in unit tests; bypasses confirmation prompts.")

(defvar dvc-buffer-marked-file-list nil
  "List of marked and not hidden files in the current buffer.

This variable is buffer-local.")
(make-variable-buffer-local 'dvc-buffer-marked-file-list)

(defvar dvc-buffer-all-marked-file-list nil
  "List of marked files, including hidden ones, in the current buffer.

`dvc-buffer-marked-file-list' is a subset of this one.

This variable is buffer-local.")
(make-variable-buffer-local 'dvc-buffer-all-marked-file-list)
;; FIXME: dvc-buffer-all-marked-file-list is only used by tla, and it
;; never actually differs from dvc-buffer-marked-file-list

(defvar dvc-patch-email-message-body-template
  (concat
   "Please change the Subject header to a concise description of your patch.\n"
   "Please describe your patch between the LOG-START and LOG-END markers:\n"
   "<<LOG-START>>\n"
   "\n"
   "<<LOG-END>>\n"
   "\n")
  "A template that is used for functions to send patches via email.
It should contain a <<LOG-START>> and a <<LOG-END>> marker to allow
automatic log message extraction.")

;;
;; Executable location
;;
(defcustom dvc-diff-executable (dvc-first-set
                                   dvc-site-diff-executable
                                 "diff")
  "*The name of the diff executable."
  :type 'string
  :group 'dvc)

(defcustom dvc-patch-executable (dvc-first-set
                                    dvc-site-patch-executable
                                  "patch")
  "*The name of the patch executable."
  :type 'string
  :group 'dvc)

;; end executable

;;
;; DVC tips
;;
;;
;; Tips
;;
(defgroup dvc-tips nil
  "\"Tip of the day\" feature for DVC"
  :group 'dvc)

(defcustom dvc-tips-enabled t
  "*Set this to nil to disable tips."
  :type 'boolean
  :group 'dvc-tips)


;; end tips mode

(provide 'dvc-defs)
;;; dvc-defs.el ends here

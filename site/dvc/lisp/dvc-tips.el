;;; dvc-tips.el --- "Tip of the day" feature for DVC.

;; Copyright (C) 2004-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords: convenience

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

;; To raise the learning curve for DVC users. Some commands can
;; (optionaly) pop-up a buffer with a tip. Currently, `dvc-commit'
;; does this.


;;; History:
;;
;; Created on October 2004 by Matthieu MOY

;;; Code:

(defconst dvc-tips-array
  [
   "Welcome to DVC. I'm the tip buffer. I will appear from time to time
to show you interesting features that you may have missed! Disable me
by setting the variable `dvc-tips-enabled' to nil.

Press q to exit, n to view next tip, p to view previous tip."
   "DVC.el provides high level interfaces to various distributed revision
control systems. It currently supports:
* bzr: bzr
* tla: tla (Gnu Arch)
* xhg: hg (Mercurial)
* xmtn: Monotone
* xgit: git
* xdarcs: darcs"
   "The following functions are the main entry points:
M-x dvc-status
M-x dvc-diff
M-x dvc-changelog
"
   "Most interesting commands are available through a global keyboard
shortcut. Try \"C-x V C-h\" to get a list"
   "DVC.el provides several major modes for different buffers. Each mode
has its own keybindings. Get help with \"C-h m\""
   "When DVC.el is loaded, C-M-h in a minibuffer prompt gives you help
about the command being run."
   "Report bugs using M-x dvc-submit-bug-report RET"
   "Submitting a patch is very easy:
Just make the needed changes to your checked out copy and do
M-x dvc-submit-patch RET"
   "You can add changelog style comments to your commit log by \"C-x V a\"."
   "Currently the tips are mostly tailored towards tla since Xtla was
the starting point for DVC.el

We accept new tips and like to integrate them to the tips list.
Please send your tip to dvc-dev@gna.org."
   "For the available tla commands Xtla provides a corresponding interactive
function.
Some examples:

M-x tla-inventory   ... runs tla inventory
M-x tla-undo        ... runs tla undo
M-x tla-changes     ... runs tla changes

Xtla prompts for the needed parameters."
   "When you are prompted for an archive, category, branch, version or
revision name, lots of keybindings are available. Get a list with \"C-h\"."
   "Xtla allows you to manage a list of bookmarks. Try \"C-x V b\" and add
bookmarks from the menu.  You may also add bookmarks from an archives,
category, version or revision buffer as well as from the tla-browse
buffer."
   "From the bookmark buffer, you can select some bookmarks and make
them partners with M-p. Afterwards, pressing 'M m' on a bookmark will
show you the missing patches from his partners."
   "You can enable ispell, flyspell or other useful mode for editing
log files by \"M-x customize-variable RET tla-log-edit-mode-hook RET\"."
   "By default, Xtla caches any log file you retrieve with
`tla-revlog' or `tla-cat-archive-log' in ~/.arch-log-library. This
speeds up many Xtla operations.

You can disable this by setting `tla-log-library-greedy' to nil."
   "Xtla can use both tla and bazaar implementations of GNU Arch.
\"M-x customize-variable RET tla-arch-branch RET\" to choose.
\"M-x tla-use-tla RET\" and \"M-x tla-use-baz RET\" to switch.

See bazaar homepage for more info on bazaar:
http://bazaar.canonical.com/"
   "Xtla is highly customizable.
Start it by \"M-x customize-group RET xtla RET\"."
   "In a *tla-changes* buffer you can quickly jump to the source file by
\"RET\", or view the source file in another window by \"v\", or start
an ediff session by \"e\" to inspect/reject parts of the changes."
   "In a *tla-changes* buffer, you can quickly jump from the list of
files to the corresponding patch hunk, and come back with \"j\""
   "From a revision list buffer or a *tla-changes* buffer, \"=\" will
show the diffs for the thing at point. M-= and M-RET allows you to
navigate in this diff while keeping your cursor in the same buffer."
   "After committing, you can review the last committed patch with
\"M-x tla-changes-last-revision RET\".

Usefull to review and fix a patch you've just merged without mixing
manual modifications and merge in the same patch."
   "After a merge, typing \"C-c m\" in the log buffer will generate
for you a summary line, keyword and body. This is highly
customizable."
   "You've got a nice, graphical, archive browser one M-x tla-browse
RET away."
   "In the bookmark buffer, pressing \"C-x C-f\" starts with the local
tree of the bookmark at point for the default directory."
   "SMerge mode is an Emacs minor mode usefull to resolve conflicts
after a --three-way merge. Xtla will enter this mode automatically
when you open a file with conflicts. Type M-x tla-conflicts-finish RET
to exit smerge mode and delete the corresponding .rej file."
   "\"C-x V e\" in a source file will open an ediff session with the
unmodified version of the file. From here, you can undo patch hunks
one by one with the key \"b\""
   "In the *tree-lint* buffer, with your cursor on a message, most
commands will apply to all the files listed under this message."
   "M-x baz-annotate RET will show you an annotated version of your
source file to track the origin of each line of code."
   "After running M-x baz-annotate RET, you cat run

M-x baz-trace-line RET => Gives you the revision in which the line was
                          last modified in the minibuffer.

M-x baz-trace-line-show-log RET => Displays the log file of this
                                   revision."
   "Xtla is well integrated with Gnus. Revision names are buttonized,
you can apply a changeset sent to you as attachment easily, ...

Add

  (tla-insinuate-gnus)

to your ~/.gnus.el or your ~/.emacs.el."
   ]
  "List of tips. Add more !")

(defvar dvc-tips-number 0
  "Number of the last tip viewed.
Will be saved in state.el")

(defun dvc-tips-message-number (number)
  "Return the message number NUMBER, as a string."
  (let ((number (mod number (length dvc-tips-array))))
    (aref dvc-tips-array number)))

;;
;; Tips mode
;;
(defvar dvc-tips-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    (define-key map dvc-keyvec-next 'dvc-tips-next-tip)
    (define-key map dvc-keyvec-previous 'dvc-tips-previous-tip)
    (define-key map [?c] 'dvc-tips-customize)
    map))

(define-derived-mode dvc-tips-mode fundamental-mode "dvc-tips"
  "Major mode for buffers displaying tip of the day.

Commands:
\\{dvc-tips-mode-map}"
  (toggle-read-only 1))


(defun dvc-tips-popup-number (number &optional noswitch)
  "Pops up tip number NUMBER."
  (let ((message (dvc-tips-message-number number)))
    (switch-to-buffer (dvc-get-buffer-create 'dvc 'tips))
    (dvc-tips-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (dvc-face-add
               "*************************   Did you know?   *************************"
               'dvc-messages)
              "\n\n")
      (insert message)
      (newline 2)
      (insert (dvc-face-add
               "*********************************************************************"
               'dvc-messages))
      (goto-char (point-min))
      )
    (when (and (not noswitch) (eq dvc-switch-to-buffer-mode 'single-window))
      ;; If mode is single-window, switch to another window (and if
      ;; necessary, split the frame) anyway.
      (when (= (length (window-list)) 1)
        (split-window-vertically))
      (other-window 1))))

(defun dvc-tips-popup-maybe ()
  "Pop up a buffer with a tip if tips are enabled.

see `dvc-tips-enabled'."
  (when dvc-tips-enabled
    (dvc-tips-popup)))

(defcustom dvc-tips-function nil
  "*Alternate function to show a tip.

Must insert the text in the current buffer"
  :type 'function
  :group 'dvc-tips)

(defun dvc-tips-make-function-from-exec (shell-command header footer)
  "Make a lisp function from a shell command.

SHELL-COMMAND is the name of a shell command, return a function
suitable for `dvc-tips-function'."
  `(lambda ()
     "Function to display a message."
     (interactive)
     (insert ,header
             (shell-command-to-string ,shell-command)
             ,footer)))

(defun dvc-tips-make-fortune-from-exec (shell-command)
  "Wrapper for `dvc-tips-make-function-from-exec'.

Shows a nice header and footer in addition.

Try

\(setq dvc-tips-function (dvc-tips-make-fortune-from-exec \"fortune\"))
"
  (dvc-tips-make-function-from-exec
   shell-command
   (concat (dvc-face-add
            "****************************   Fortune   ****************************"
            'dvc-messages) "\n\n")
   (concat "\n"
           (dvc-face-add
            "*********************************************************************"
            'dvc-messages))))

;;;###autoload
(defun dvc-tips-popup (&optional direction noswitch)
  "Pop up a buffer with a tip message.

Don't use this function from Xtla. Use `dvc-tips-popup-maybe'
instead."
  (interactive)
  (let ((work-dir default-directory))
    (if dvc-tips-function
        (progn
          (switch-to-buffer (dvc-get-buffer-create 'dvc 'tips))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (funcall dvc-tips-function))
          (dvc-tips-mode))
      (dvc-load-state)
      (dvc-tips-popup-number dvc-tips-number noswitch)
      (setq dvc-tips-number
            (mod (+ dvc-tips-number (or direction 1)) (length dvc-tips-array)))
      (dvc-save-state))
    (setq default-directory work-dir))) ;; set the default-directory in the tips buffer to the current working dir

(defun dvc-tips-next-tip ()
  "Show next tip."
  (interactive)
  (dvc-tips-popup 1 t))

(defun dvc-tips-previous-tip ()
  "Show previous tip."
  (interactive)
  (dvc-tips-popup -1 t))

(defun dvc-tips-customize ()
  "Run customize group for dvc-tips."
  (interactive)
  (customize-group 'dvc-tips))

(provide 'dvc-tips)
;;; dvc-tips.el ends here

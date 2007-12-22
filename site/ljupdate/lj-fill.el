;;; lj-fill.el --- various filling methods for livejournal posts

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
;; The intent is for several different filling methods to live here.
;; Currently, there are only two recommended values of
;; `lj-fill-function': `lj-fill-by-paragraph' and `ignore'.
;; Read the `lj-fill-function' doc string for more.

;;; History:
;;

;;; Code:

(require 'lj-custom)

(defvar lj-fill-by-paragraph-fill-column 10000
  "*Value to be used for `fill-column' by `lj-fill-by-paragraph'.")

(defvar lj-fill-flush-empty-lines-flag t
  "*Non-nil means that `lj-fill-by-paragraph' will remove blank lines.")

(defvar lj-fill-inter-paragraph-newline-count 2
  "*How many newlines to use in between paragraphs.
Yuo probably want this to be at least 1.")

(defun lj-fill-by-paragraph ()
  "Fills your LiveJournal post while assuming you wrote text with auto fill.

Assumes that consecutive non-blank lines are paragraphs, unfills them,
and kills any extra blank lines. If your posts are predominately text,
with little to no markup, this is probably the behavior you will like.

This is like the default filling behavior of the old ljupdate code. If
you didn't like it then, you won't like it now. You may want to fiddle
with the values of `lj-fill-by-paragraph-fill-column',
`lj-fill-flush-empty-lines-flag', and/or
`lj-fill-inter-paragraph-newline-count' in order to produce the sort of
behavior you'd like this function to exhibit. Or, you may change the
value of `lj-fill-function' to a function more to your liking (e.g.
`ignore')."
  ;; Fill paragraphs
  (goto-char (point-min))
  (let ((fill-column lj-fill-by-paragraph-fill-column))
    (fill-paragraph nil)
    (while (zerop (forward-paragraph 1))
      (fill-paragraph nil)))
  ;; Kill blank lines
  (when lj-fill-flush-empty-lines-flag
    (flush-lines "^$" (point-min) (point-max)))
  ;; Restore paragraph separation
  (goto-char (point-min))
  (let ((newlines (make-string lj-fill-inter-paragraph-newline-count ?\n)))
    (while (search-forward "\n" nil t)
      (replace-match newlines))))

(defun lj-fill-by-shell-command ()
  "Filters your LiveJournal post through a shell command."
  (shell-command-on-region (point-min) (point-max)
                           lj-fill-by-shell-command-command t))


(provide 'lj-fill)
;;; lj-fill.el ends here

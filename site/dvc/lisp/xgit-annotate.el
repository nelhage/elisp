;;; xgit-annotate.el --- Git interface for dvc: mode for git-annotate style output

;; Copyright (C) 2007-2009 by all contributors

;; Author: Takuzo O'hara, <takuzo.ohara@gmail.com>

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

;; The git interface for dvc: a mode to handle git-annotate style output

;;; Code:
(require 'dvc-annotate)
(require 'rect)

(defvar xgit-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map dvc-keyvec-help 'describe-mode)
    (define-key map [return] 'xgit-annotate-show-rev)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map)
  "Keymap used in `xgit-annotate-mode'.")

(define-derived-mode xgit-annotate-mode fundamental-mode "xgit-annotate"
  "Major mode to display git annotate output.

Commands:
\\{xgit-annotate-mode-map}
"
  (dvc-annotate-display-autoscale t)
  (dvc-annotate-lines (point-max))
  (xgit-annotate-hide-revinfo)
  (toggle-read-only 1))

;; Matches to
;; e.g.
;; normal commit:
;;    "ee6e815b (Takuzo Ohara      2007-02-23 12:24:57 +0900   1) ..."
;; or initial commit:
;;    "^de398cf (Takuzo Ohara 2007-02-21 21:28:35 +0900 366) ..."
;; or not yet commited:
;;    "00000000 (Not Committed Yet 2007-02-24 15:31:42 +0900  37) ..."
(defconst xgit-annotate-info-regexp "^\\(\\(\\^?\\([[:xdigit:]]+\\)\\)[[:blank:]]+.*(\\(.*?\\)[[:blank:]]+\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\) \\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[[:blank:]]+\\)\\([0-9]+\\))")
(defun xgit-info-to-allbutlinenum ()                   (match-string-no-properties 1))
(defun xgit-info-to-rev         ()                   (match-string-no-properties 2))
(defun xgit-info-to-initrev     ()                   (match-string-no-properties 3))
(defun xgit-info-to-name        ()                   (match-string-no-properties 4))
(defun xgit-info-to-year        () (string-to-number (match-string-no-properties 5)))
(defun xgit-info-to-month       () (string-to-number (match-string-no-properties 6)))
(defun xgit-info-to-day         () (string-to-number (match-string-no-properties 7)))
(defun xgit-info-to-hour        () (string-to-number (match-string-no-properties 8)))
(defun xgit-info-to-min         () (string-to-number (match-string-no-properties 9)))
(defun xgit-info-to-sec         () (string-to-number (match-string-no-properties 10)))
(defun xgit-info-to-zone-hour   () (string-to-number (match-string-no-properties 11)))
(defun xgit-info-to-zone-min    () (string-to-number (match-string-no-properties 12)))
(defun xgit-info-to-linenum     () (string-to-number (match-string-no-properties 13)))

(defconst xgit-annotate-revision-regexp "^^?\\([[:xdigit:]]+\\)")

(defun xgit-annotate-get-rev ()
  "Returns git revision at point in annotate buffer."
  (if (< (point) (point-max))
      (save-excursion
        (beginning-of-line)
        (if (looking-at xgit-annotate-info-regexp)
            (xgit-info-to-rev)))))


(defun xgit-annotate-show-rev ()
  "Show the information at the point."
  (interactive)
  (let ((rev (xgit-annotate-get-rev)))
    (if (string-match xgit-annotate-revision-regexp rev)
        ;; initial version might result too large for git-show, so use
        ;; git-log.
        (xgit-log default-directory nil :rev (match-string-no-properties 1 rev))
        (xgit-show default-directory rev))
    (xgit-describe default-directory rev)))

(defun _xgit-annotate-hide-revinfo ()
  (let ((p_rev (xgit-annotate-get-rev))
        (width (- (match-end 12) (line-beginning-position))))
    (forward-line 1)
    ;; When revision of two subsequent lines are same:
    (if (string= p_rev (xgit-annotate-get-rev))
        (let ((start (line-beginning-position)))
          ;; forward until revision changes,
          (while (string= p_rev (xgit-annotate-get-rev))
            (forward-line 1))
          ;; point is at new revision so move back a line,
          (unless (= (point) (point-max))
            (previous-line 1))
          ;; match again to get position of right-bottom corner,
          (xgit-annotate-get-rev)
          ;; rectangular replace by white space, from start to end.
          (string-rectangle start (match-end 12) (make-string width ? ))))
    ))

(defun xgit-annotate-hide-revinfo ()
  "Hide revision information when it is same as previous line's info."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (_xgit-annotate-hide-revinfo))))

(defun xgit-annotate-time ()
  (when (< (point) (point-max))
    (beginning-of-line)
    (if (re-search-forward xgit-annotate-info-regexp nil t)
        (let* ((year  (xgit-info-to-year))
               (month (xgit-info-to-month))
               (day   (xgit-info-to-day))
               (hour  (xgit-info-to-hour))
               (min   (xgit-info-to-min))
               (sec   (xgit-info-to-sec))
               (zone-hour (xgit-info-to-zone-hour))
               (zone-min  (xgit-info-to-zone-min))
               (zone-sec  (* 60 (+ (* 60 zone-hour) zone-min))))
          (dvc-annotate-convert-time
           (encode-time sec min hour day month year zone-sec))
          ))))

(provide 'xgit-annotate)
;;; xgit-annotate.el ends here

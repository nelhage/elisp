;;; xhg-annotate.el ---

;; Copyright (C) 2009 Thierry Volpiatto.
;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Thierry Volpiatto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;; ==========

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `xhg-annotate-show-rev-number-log'
;;    Show xhg-log output corresponding to line at point in
;;  `xhg-annotate-show-prec-rev-number-log'
;;    Go to precedent line in xhg-annotate buffer and display
;;  `xhg-annotate-show-next-rev-number-log'
;;    Go to next line in xhg-annotate buffer and display
;;  `xhg-annotate'
;;    Run hg annotate and display xhg-log in other-window.
;;  `xhg-annotate-quit'
;;    Quit and restore precedent window config.

;; hg annotate:
;;
;; List changes in files, showing the revision id responsible for each line
;; This command is useful to discover who did a change or when a change took
;; place.
;; Without the -a option, annotate will avoid processing files it
;; detects as binary. With -a, annotate will generate an annotation
;; anyway, probably with undesirable results.

;; From current file under hg control, run xhg-annotate in one buffer
;; and xhg-log in the other buffer at the revision corresponding to current line
;; of current file.
;; once in the xhg-annotate buffer you can navigate to the different line
;; showing at each movement the xhg-log output corresponding to revision.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'derived)
(eval-when-compile (require 'cl))

;;;###autoload
(defvar xhg-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(shift down)] 'xhg-annotate-show-next-rev-number-log)
    (define-key map [(shift up)] 'xhg-annotate-show-prec-rev-number-log)
    (define-key map (kbd "<return>") 'xhg-annotate-show-rev-number-log)
    (define-key map [?q] 'xhg-annotate-quit)
    map)
  "Keymap used for xhg-annotate-mode commands.")

(define-derived-mode xhg-annotate-mode dvc-info-buffer-mode "xhg-annotate"
                     "Major mode to show revision number log.

Special commands:
\\{xhg-annotate-mode-map}")

(defvar xhg-annotate-current-buffer nil)

(defun xhg-annotate-get-rev-num-on-line ()
  "Extract revision number on line in xhg-annotate buffer."
  (let ((cur-line (buffer-substring (point-at-bol) (point-at-eol)))
        (rev-num))
    (when (string-match "^ *[0-9]*" cur-line)
      (setq rev-num (match-string 0 cur-line))
      (setq rev-num (replace-regexp-in-string " " "" rev-num)))))

;;;###autoload
(defun xhg-annotate-show-rev-number-log ()
  "Show xhg-log output corresponding to line at point in
xhg-annotate buffer."
  (interactive)
  (let ((rev-number (xhg-annotate-get-rev-num-on-line))
        (fname xhg-annotate-current-buffer))
    (save-excursion
      (xhg-log rev-number rev-number t fname)
      (other-window 1))))

;;;###autoload
(defun xhg-annotate-show-prec-rev-number-log ()
  "Go to precedent line in xhg-annotate buffer and display
corresponding xhg-log output."
  (interactive)
  (forward-line -1)
  (xhg-annotate-show-rev-number-log))

;;;###autoload
(defun xhg-annotate-show-next-rev-number-log ()
    "Go to next line in xhg-annotate buffer and display
corresponding xhg-log output."
  (interactive)
  (forward-line)
  (xhg-annotate-show-rev-number-log))

;;;###autoload
(defun xhg-annotate ()
  "Run hg annotate and display xhg-log in other-window."
  (interactive)
  (setq xhg-annotate-current-buffer (current-buffer))
  (let ((line-num (line-number-at-pos)))
    (dvc-run-dvc-display-as-info 'xhg (append '("annotate") (dvc-current-file-list)))
    (switch-to-buffer "*xhg-info*")
    (goto-line line-num)
    (xhg-annotate-mode)
    (xhg-annotate-show-rev-number-log)))

;;;###autoload
(defun xhg-annotate-quit ()
  "Quit and restore precedent window config."
  (interactive)
  (dvc-buffer-quit)
  (other-window 1)
  (dvc-buffer-quit)
  (switch-to-buffer xhg-annotate-current-buffer)
  (setq xhg-annotate-current-buffer nil)
  (delete-other-windows))

(provide 'xhg-annotate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhg-annotate.el ends here

;;; dvc-bug.el --- Reporting bugs to Xtla-el-dev list

;; Copyright (C) 2006-2007 by all contributors

;; This file is part of DVC.
;;
;; DVC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; DVC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'dvc-version)
(require 'dvc-register)

;;;###autoload
(defun dvc-submit-bug-report ()
  "Submit a bug report, with pertinent information to the dvc-dev list."
  (interactive)
  (require 'reporter)
  (delete-other-windows)
  ;; (dvc-version)
  (dvc-command-version)
  (reporter-submit-bug-report
   "dvc-dev@gna.org"
   (concat "Dvc " dvc-version)
   (append
    ;; non user variables
    '(emacs-version
      dvc-version
      dvc-command-version
      )
    ;; user variables
    (sort (apropos-internal (concat "^\\("
                                    (mapconcat (lambda (name)
                                                 (concat (regexp-quote (symbol-name name)) "-"))
                                               dvc-registered-backends
                                               "\\|")
                                    "\\)")
                            'user-variable-p)
          (lambda (v1 v2) (string-lessp (format "%s" v1) (format "%s" v2))))
    ;; see what the user had loaded
    (list 'features)
    )
   nil
   nil
   (concat
    "Please change the Subject header to a concise bug description or feature request.\n"
    "In this report, remember to cover the basics, that is, what you \n"
    "expected to happen and what in fact did happen.\n"
    "Please remove these instructions from your message."))
  ;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\n\n")
      (insert-buffer-substring backtrace)))

  (goto-char (point-min))
  (mail-position-on-field "Subject")
  (insert "[BUG/FEATURE] "))

;; For people used to Debian's reportbug
(defalias 'dvc-report-bug 'vc-submit-bug-report)
;; For people used to Gnus M-x gnus-bug RET
(defalias 'dvc-bug 'dvc-submit-bug-report)
;; (reporting bugs should be easy ;-)


(provide 'dvc-bug)

;; Local Variables:
;; End:

;;; dvc-bug.el ends here

;;; dvc-revlog.el --- View a single log entry in DVC

;; Copyright (C) 2005-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>

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

;;

;;; Code:

(require 'dvc-ui)

(defvar dvc-revlog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?g] 'dvc-generic-refresh)
    (define-key map dvc-keyvec-quit 'dvc-buffer-quit)
    map))


(define-derived-mode dvc-revlog-mode fundamental-mode
  "dvc-revlog"
  "Major mode to show a single log entry.

This mode is the DVC common denominator of the back-ends, and is
therefore pretty trivial, but each back-end will have to derive
it to something more specific.

Commands are:
\\{dvc-revlog-mode-map}"
  (setq dvc-buffer-current-active-dvc (dvc-current-active-dvc))

  (dvc-install-buffer-menu)
  (toggle-read-only 1))

(dvc-add-uniquify-directory-mode 'dvc-revlog-mode)

(defun dvc-revlog-show-revision (back-end source-buffer buffer-name)
  "Use the content of SOURCE-BUFFER to display a revlog.

Create a new buffer named from BUFFER-NAME."
  (let ((buffer (dvc-get-buffer-create
                 back-end 'revlog buffer-name)))
    (dvc-switch-to-buffer buffer)
    (insert-buffer-substring source-buffer)
    (funcall (dvc-function back-end "revlog-mode"))
    buffer))


(defun dvc-revlog-revision (rev-id)
  "Show the log for REV-ID.

Call `dvc-revlog-revision-in-buffer' to get the content, and display
it in revlog-mode."
  (with-temp-buffer
    (insert (dvc-revlog-revision-in-buffer rev-id))
    (dvc-revlog-show-revision (dvc-revision-get-dvc rev-id)
                              (current-buffer)
                              (dvc-revision-to-string rev-id))))

(defun dvc-revlog-revision-in-buffer (rev-id)
  "Get the log message for revision REV-ID.

Return the log message as a string.

REV-ID is as defined in docs/DVC-API. The behavior is similar to the
one of `dvc-revision-get-file-in-buffer', but for log entries instead
of file contents.

Currently, only 'revision type is supported."
  (dvc-trace "dd-ib=%S" default-directory)
  (dvc-trace "rev-id=%S" rev-id)
  (let ((type (dvc-revision-get-type rev-id)))
    (unless (eq type 'revision)
      (error "rev-id %S not supported by dvc-revision-revlog"
             type))
    (funcall (dvc-function (dvc-revision-get-dvc rev-id)
                           "dvc-revlog-get-revision")
             rev-id)))

(provide 'dvc-revlog)
;;; dvc-revlog.el ends here

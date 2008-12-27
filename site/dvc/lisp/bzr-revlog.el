;;; bzr-revlog.el --- Show a log entry for a bzr branch

;; Copyright (C) 2006 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Keywords:

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

(require 'dvc-core)
(require 'dvc-revlog)

(defun bzr-revlog-local (revno &optional path)
  "Quick and dirty prototype of function using dvc-revlog-mode."
  (interactive "MRevno: ")
  (let ((default-directory (or path default-directory)))
    (dvc-run-dvc-async 'bzr `("log"
                              "-r"
                              ,revno)
                       :finished
                       (dvc-capturing-lambda (output error status
                                                     arguments)
                         (dvc-switch-to-buffer
                          (dvc-revlog-show-revision 'bzr output
                                                    (capture revno)))))))


(defun bzr-revlog-get-revision (rev-id)
  (let ((data (car (dvc-revision-get-data rev-id))))
    (dvc-trace "dd=%S" default-directory)
    (dvc-trace "data=%S" data)
    (cond ((eq (car data) 'local)
           (let ((default-directory (nth 1 data)))
             (dvc-run-dvc-sync 'bzr
                               `("log" "--revision"
                                 ,(int-to-string (nth 2 data)))
                               :finished 'dvc-output-buffer-handler)))
          ((eq (car data) 'remote)
           (dvc-run-dvc-sync 'bzr
                             `("log" "--revision"
                               ,(concat "revno:"
                                        (int-to-string (nth 2 data))
                                        ":"
                                        (nth 1 data)))
                             :finished 'dvc-output-buffer-handler))
          (t (error (format "Revision ID %S not implemented" rev-id))))))


(provide 'bzr-revlog)
;;; bzr-revlog.el ends here

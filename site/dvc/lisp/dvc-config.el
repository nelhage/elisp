;;; dvc-config.el --- dvc configuration directory

;; Copyright (C) 2006 by all contributors

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
(require 'dvc-utils)
(require 'dvc-defs)

(defcustom dvc-config-directory "~/.dvc/"
  "*Directory in which the DVC config files will be stored."
  :type 'directory
  :group 'dvc)

(defun dvc-config-file-full-path (file &optional create-config-dir)
  "Return the full path for the config file FILE.
FILE will be stored in the `dvc-config-directory'.
If CREATE-CONFIG-DIR is non nil, ensure that the `dvc-config-directory'
does exist."
  (let ((full-name (dvc-uniquify-file-name
                    (concat dvc-config-directory file))))
    (unless (file-exists-p dvc-config-directory)
      (when create-config-dir
        (make-directory dvc-config-directory t)
        (message "The config files of DVC will be stored in %s!"
                 dvc-config-directory)
        (sit-for 5)))
    ;; return full-name
    full-name))

(provide 'dvc-config)

;; Local Variables:
;; End:

;;; dvc-config.el ends here

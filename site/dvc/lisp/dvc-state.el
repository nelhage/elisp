;;; dvc-state.el --- saving and loading state variables between Emacs sessions

;; Copyright (C) 2006-2008 by all contributors

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
;; `dvc-save-state' is for saving to a state file.
;; `dvc-load-state' is for loading from a state file.

;;; Code:
(require 'dvc-utils)
(require 'dvc-defs)
(require 'dvc-config)

(defgroup dvc-state nil
  "Saving DVC's state between Emacs sessions."
  :group 'dvc)

(defcustom dvc-state-file-name "state.el"
  "*File in which DVC saves state variables between Emacs sessions.
The file is stored in the `dvc-config-directory'"
  :type 'file
  :group 'dvc-state)

(defcustom dvc-state-variables-list '(dvc-tips-number)
  "*List of variables to store in the state file `dvc-state-file-name'."
  :type '(repeat (symbol))
  :group 'dvc-state)

;;;###autoload
(defun dvc-save-state (&optional vars state-file pp)
  "Save variables from VARS list to file STATE-FILE.
The default for VARS is `dvc-state-variables-list'
The default for STATE-FILE is `dvc-state-file-name'.
If PP is non-nil use `dvc-pp-to-string' to format object.

The file will contain a setq setting the vars during loading by
`dvc-load-state'."
  (let ((state-file (or state-file
                        (expand-file-name dvc-state-file-name
                                          dvc-config-directory)))
        (vars (or vars dvc-state-variables-list))
        v)
    (if (not (file-exists-p (file-name-directory state-file)))
        (make-directory (file-name-directory state-file) t))
    (save-excursion
      (set-buffer (get-buffer-create " *dvc-state*"))
      (erase-buffer)
      (insert ";; Generated file. Do not edit!!!\n(setq\n")
      (if pp
          (while vars
            (setq v (car vars) vars (cdr vars))
            (insert (format "%s\n'%s"
                            (symbol-name v)
                            (dvc-pp-to-string (symbol-value v)))))
        (while vars
          (setq v (car vars) vars (cdr vars))
          (insert (format "      %s '%S\n"
                          (symbol-name v)
                          (symbol-value v)))))
      (insert "      )")
      (write-region (point-min) (point-max) state-file))))

;;;###autoload
(defun dvc-load-state (&optional state-file)
  "Load STATE-FILE (default `dvc-state-file-name`), i.e. evaluate its content."
  (let ((state-file (or state-file
                        (expand-file-name dvc-state-file-name
                                          dvc-config-directory))))
    (if (file-exists-p state-file)
        (load state-file nil t t))))


(provide 'dvc-state)

;; Local Variables:
;; End:

;;; dvc-state.el ends here

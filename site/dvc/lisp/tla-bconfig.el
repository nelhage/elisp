;;; tla-bconfig.el --- mode for input file of GNU arch's build-config

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Masatake YAMATO <jet@gyve.org>
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

(eval-and-compile
  (require 'tla)
  (require 'easymenu))

(defvar tla-bconfig-font-lock-keywords
  '(("#.*$" . 'dvc-comment)
    ("\\(\\./[^ \n\t]*\\)[ \t]+\\(.*\\)"
     (1 'dvc-local-directory) (2 'tla-archive-name)))
  "Keywords in tla-bconfig mode.")

(defvar tla-bconfig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t"       'tla-bconfig-insert-contents)
    (define-key map " "        'tla-bconfig-insert-contents)
    (define-key map "."        'tla-bconfig-insert-contents-dot)
    ;;
    (define-key map "\C-c\t"   'tla-insert-location)
    (define-key map "\C-c "    'tla-insert-location)
    ;;
    (define-key map "\C-c/"    'tla-bconfig-insert-directory)
    (define-key map "\C-c."    'tla-bconfig-insert-directory)
    (define-key map "\C-c\C-c" 'tla-build-config)
    (define-key map "\C-c\C-v" 'tla-cat-config)
    map)
  "Keymap used in `tla-bconfig-mode'.")

(easy-menu-define tla-bconfig-mode-menu tla-bconfig-mode-map
  "`tla-bconfig-mode' menu"
  `("Build-Config"
    ["Insert Directory"  tla-bconfig-insert-directory t]
    ["Insert Name"       tla-insert-location      t]
    "--"
    ["Run cat-config"    tla-cat-config t]
    ["Run build-config"  tla-build-config t]))

(defun tla-bconfig-insert-directory ()
  "Read a directory relative from tla's tree root, and insert it."
  (interactive)
  (let* ((base-dir (tla-tree-root))
         (dir (dvc-read-directory-name "Directory: " base-dir)))
    (when dir
      (insert "./"
              (directory-file-name
               (substring (expand-file-name dir)
                          (length (expand-file-name base-dir))))))))

(defun tla-bconfig-insert-contents (n)
  "Insert a directory or tla name depending on the point position."
  (interactive "p")
  (cond

   ;; In comment: Insert self.
   ((nth 4 (parse-partial-sexp (point) (point-min)))
    (self-insert-command n))

   ;; Beginning of line: Insert a directory.
   ((bolp)
    (tla-bconfig-insert-directory))

   ;; filename + space + X
   ;; If X is still empty, insert a tla name at ?.
   ((save-excursion
      (beginning-of-line)
      (and (re-search-forward "\\(\\./[^ \t\n]*\\)[ \t]+\\(.*\\)"
                              (line-end-position)
                              t)
           (match-beginning 2)))
    (goto-char (match-beginning 2))
    (when (eq 0 (length (match-string 2)))
      (tla-insert-location)))

   ;; filename
   ;; Insert tab, then insert a tla name.
   ((save-excursion
      (beginning-of-line)
      (and (re-search-forward "\\(\\./[^ \t\n]*\\)"
                              (line-end-position)
                              t)
           (match-end 1)))
    (goto-char (match-end 1))
    (insert "\t")
    (tla-insert-location))

   ;; In other case insert self.
   (t (self-insert-command n))))

(defun tla-bconfig-insert-contents-dot (n)
  ""
  (interactive "p")
  (if (bolp)
      (tla-bconfig-insert-contents n)
    (self-insert-command n)))

(defvar tla-bconfig-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used in tla-bconfig mode.")

;;;###autoload
(defun tla-bconfig-mode ()
  "Major mode to edit GNU arch's build config files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tla-bconfig-mode-syntax-table)
  (use-local-map tla-bconfig-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(tla-bconfig-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "#")
  (setq major-mode 'tla-bconfig-mode
        mode-name "tla-bconfig")
  (run-hooks 'tla-bconfig-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arch$" . tla-bconfig-mode))

(provide 'tla-bconfig)

;; Local Variables:
;; End:
;; tla-bconfig.el ends here


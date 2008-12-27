;;; tla-autoconf.el --- Arch interface for emacs

;; Copyright (C) 2003-2005 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Autoconfiguration of Xtla, depending on the client (different
;; versions of tla and baz)
;;
;; Each autodected feature has a corresponding variable and a
;; corresponding function. The variable's name is
;; tla--autoconf-<feature> and serves *only* as a cache. The possible
;; values are 'yes 'no and nil (for "don't know").
;; The function's name is tla-<feature>, and is the one to use.


;;; History:
;;
;; Created on May 28, 2005 by Matthieu Moy.

;;; Code:
(eval-when-compile
  (require 'cl))


;; ----------------------------------------------------------------------------
;; Wether a command exists
;; ----------------------------------------------------------------------------
(defmacro tla--has-foo-command (cmd)
  "Create the autodetection function for the command CMDNAME.

Checks if the command CMDNAME exists (appear in the output of the
\"help\" command."
  (declare (debug (stringp)))
  (let ((var (intern (concat "tla--autoconf-has-" cmd "-command")))
        (fun (intern (concat "tla-has-"  cmd "-command"))))
    `(progn
       (defvar ,var nil
         ,(format "Whether tla|baz has a %s command.

Possible values are nil (don't know), 'yes, or 'no.  Don't use this
variable directly.  Use `tla-has-%s-command' instead." cmd cmd))

       (defun ,fun ()
         ,(format "Whether tla|baz has a %s command.

Returns 't or nil.

If `tla--autoconf-has-%s-command' is non-nil, use its value.
Otherwise, test if \"%s\" is listed by \"tla|baz help\", and memorize
the result in `tla--autoconf-has-%s-command'." cmd cmd cmd cmd)
         (interactive)
         (let ((answer
                (cond ((eq ,var 'yes) t)
                      ((eq ,var 'no) nil)
                      (t (tla--run-tla-sync
                          '("help")
                          :finished (lambda (output error status
                                                    arguments)
                                      (with-current-buffer output
                                        (goto-char (point-min))
                                        (search-forward (concat " " ,cmd " :")
                                                        nil t))))))))
           (when (interactive-p)
             (message (if answer "Yes" "No")))
           (setq ,var
                 (if answer 'yes 'no))
           answer)))))

(tla--has-foo-command "escape")         ; support for spaces in filename
(tla--has-foo-command "diff")
(tla--has-foo-command "file-diff")
(tla--has-foo-command "tree-id")
(tla--has-foo-command "status")
(tla--has-foo-command "switch")
(tla--has-foo-command "merge")
(tla--has-foo-command "resolved")
(tla--has-foo-command "lint")
(tla--has-foo-command "branch")
(tla--has-foo-command "add-id")

;; ----------------------------------------------------------------------------
;; Wether commands need or support an option
;; ----------------------------------------------------------------------------
(defmacro tla--foo-has-bar-option (cmdname cmd option helpstring)
  "Create the autodetection function for the command CMDNAME.

Checks if the command CMDNAME accepts the option OPTION. CMD may be a
lisp expression that returns the actual command to execute (usefull
for commands whose name is not the same for baz and tla. HELPSTRING is
the string to search for in the output of CMD --help."
  (declare (debug (stringp form stringp stringp)))
  (let ((var (intern (concat "tla--autoconf-" cmdname "-has-" option "-option")))
        (fun (intern (concat "tla-" cmdname "-has-" option "-option"))))
    `(progn
       (defvar ,var nil
         ,(format "Whether \"tla|baz %s\" needs the --%s option.

Possible values are nil (don't know), 'yes, or 'no.  Don't use this
variable directly.  Use `tla-%s-has-%s-option' instead." cmdname option
cmdname option))

       (defun ,fun ()
         ,(format "Whether \"tla|baz %s\" needs the --%s option.

Returns 't or nil.

If `tla--autoconf-%s-has-%s-option' is non-nil, use its value. Otherwise, test
if \"--%s\" is listed by \"tla %s --help\", and memorize the result in
`tla--autoconf-%s-has-%s-option'." cmdname option cmdname option option
cmdname cmdname option)
         (interactive)
         (let ((answer
                (cond ((eq ,var 'yes) t)
                      ((eq ,var 'no) nil)
                      (t (tla--run-tla-sync
                          (list ,cmd "--help")
                          :finished (lambda (output error status arguments)
                                      (with-current-buffer output
                                        (goto-char (point-min))
                                        (search-forward ,helpstring
                                                        nil t))))))))
           (when (interactive-p)
             (message (if answer "Yes" "No")))
           (setq ,var
                 (if answer 'yes 'no))
           answer)))))

(tla--foo-has-bar-option "tag" (if (tla-has-branch-command)
                                   "branch" "tag")
                         "setup" "  -S, --setup")
(tla--foo-has-bar-option "merge" (if (tla-has-merge-command)
                                     "merge" "star-merge")
                         "three-way" "  -t, --three-way")
(tla--foo-has-bar-option "merge" (if (tla-has-merge-command)
                                     "merge" "star-merge")
                         "show-ancestor" "  --show-ancestor")
(tla--foo-has-bar-option "switch" "switch" "show-ancestor"
                         "  --show-ancestor")
(tla--foo-has-bar-option "merge" (if (tla-has-merge-command)
                                     "merge" "star-merge")
                         "two-way" "  --two-way")
(tla--foo-has-bar-option "import" "import" "setup" " -S, --setup")
(tla--foo-has-bar-option "archives" "archives" "all-locations"
                         "  --all-locations")
(tla--foo-has-bar-option "inventory" "inventory" "no-recursion"
                         "  --no-recursion")
(tla--foo-has-bar-option "revisions" "revisions" "complete-log"
                         "  -l, --complete-log")
(tla--foo-has-bar-option "missing" "missing" "full" "  -f, --full")
(tla--foo-has-bar-option "archive-mirror" "archive-mirror" "all-mirrors"
                         "  -a, --all-mirrors")
(defalias 'tla-use-baz-archive-registration 'tla-archive-mirror-has-all-mirrors-option)

;; ----------------------------------------------------------------------------
;; Management of autoconf variables
;; ----------------------------------------------------------------------------
(defun tla-autoconf-reset ()
  "Forget the autodetected values about tla or baz capabilities.

Reset all variable whose name start with \"tla--autoconf-\" to nil."
  (interactive)
  (dolist (var (apropos-internal "^tla--autoconf-"))
    (set var nil)))

(defun tla-autoconf-show ()
  "Show the autodetected values about tla or baz capabilities.

Reset all variable whose name start with \"tla--autoconf-\" to nil."
  (interactive)
  (dvc-switch-to-buffer (get-buffer-create "*xtla-config*"))
  (erase-buffer)
  (dolist (var (apropos-internal "^tla--autoconf-"))
    (let ((value (eval var)))
      (insert (symbol-name var) ": "
              (cond ((eq value 'yes) "Yes")
                    ((eq value 'no) "No")
                    ((eq value nil) "Don't know")
                    (t (error "incorrect value")))
              "\n"))))

(defun tla-autoconf-compute ()
  "Autodetect values about tla or baz capabilities."
  (interactive)
  (dolist (var (apropos-internal "^tla--autoconf-"))
    (let* ((name (symbol-name var))
           (func-name (replace-regexp-in-string "^tla--autoconf-"
                                                "tla-" name))
           (fn (intern func-name))
           (value (funcall fn))))
    nil))

(defun tla-autoconf-show-compute ()
  "Autodetect and show values about tla or baz capabilities."
  (interactive)
  (tla-autoconf-compute)
  (tla-autoconf-show))


(provide 'tla-autoconf)

;;; tla-autoconf.el ends here

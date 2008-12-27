;;; dvc-register.el --- Registration of DVC back-ends

;; Copyright (C) 2005-2008 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from: Matthieu Moy <Matthieu.Moy@imag.fr>

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

;; DVC Back-end registration

(require 'dvc-defs)
(require 'dvc-utils)

(defvar dvc-registered-backends nil
  "List of registered back-ends.")

(defun dvc-intern-symbol-name (dvc postfix)
  "Intern a symbol for DVC, add POSTFIX to the name.
A '-' is put between DVC and the POSTFIX.

Example: (dvc-intern-symbol-name 'xhg \"tree-root\") => xhg-tree-root"
  (intern (concat (symbol-name dvc) "-" postfix)))

(defmacro dvc-register-dvc (dvc name)
  "Register DVC, NAME is displayed for user interaction.

It's a macro, so it can be called without loading dvc-unified. The
build system inserts a (eval-when-compile (require 'dvc-unified))
at the beginning of the autoload file, so, the macro is available in
the autoloads."
  ;; make sure dvc-back-end-wrappers is defined.
  (require 'dvc-unified)
  (let ((wrappers-defs
         (mapcar (lambda (wrapper)
                   (let* ((dvc-noquote (cadr dvc))
                          (name (nth 0 wrapper))
                          (symb (intern (concat (symbol-name
                                                 dvc-noquote)
                                                "-"
                                                name)))
                          (symb-dvc (intern (concat "dvc-"
                                                    name)))
                          (args (nth 1 wrapper))
                          (call-args (remove '&rest (remove '&optional args)))
                          (docstring (concat "Wrapper for dvc-" name
                                             ", for back-end "
                                             (symbol-name dvc-noquote)
                                             ".")))
                     `(defun ,symb ,args
                        ,docstring
                        (interactive)
                        (let ((dvc-temp-current-active-dvc ,dvc))
                          ,(if call-args
                               `(if (interactive-p)
                                    (call-interactively (quote ,symb-dvc))
                                  (funcall (quote ,symb-dvc) ,@call-args))
                             `(call-interactively (quote ,symb-dvc)))))))
                 dvc-back-end-wrappers
                 )))
    `(progn
       (defvar dvc-registered-backends nil)
       (add-to-list 'dvc-registered-backends ,dvc)
       (defvar ,(intern (concat (symbol-name (cadr dvc))
                                "-backend-name"))
         ,name
         ,(concat "Human friendly name used for the dvc backend '"
                  (symbol-name (cadr dvc))
                  ".\nThis variable was created by `dvc-register-dvc'"))
       ;; the hard thing is to make sure all back-ends define all
       ;; functions.
       ;; some dvc-register-dvc will be called before processing DVC
       ;; core's autoloads (_b_az, _b_zr, ...), some after (_x_hg,
       ;; _x_git, ...), since it's done in alphabetical order. here,
       ;; we make sure all functions are declared, and since
       ;; dvc-register-dvc is called for each back-end, we've got it.
       ,@wrappers-defs)))

(defvar dvc-backend-name "Unknown")

(defun dvc-function (dvc postfix &optional nodefault)
  "Return the function for DVC backend concatenated with POSTFIX.

To be used with `apply' or `funcall'. If NODEFAULT is nil and no
function is available for this backend, use dvc-<postfix>
instead.

POSTFIX is a string."
  (let ((res (dvc-intern-symbol-name dvc postfix)))
    (if (or nodefault (fboundp res)) res
      (let ((dvc-register-sym (intern (concat (symbol-name dvc) "-dvc"))))
        (unless (featurep dvc-register-sym)
          (dvc-trace "require %S" dvc-register-sym)
          (if (featurep 'xemacs)
              (require dvc-register-sym nil)
            (require dvc-register-sym nil t))))
      (let ((second-try (dvc-function dvc postfix t)))
        (if (fboundp second-try) second-try
          (let ((fall-back (dvc-intern-symbol-name 'dvc postfix)))
            (if (not fall-back) second-try
              (let ((result (dvc-intern-symbol-name 'dvc postfix)))
                (if (fboundp result) result
                  (error "No definition and no fallback for %s-\"%s\""
                         (symbol-name dvc) postfix))))))))))

(defun dvc-variable (dvc postfix &optional nodefault)
  "Get the value of a variable in a DVC backend.

If NODEFAULT is nil and no variable is available for this
backend, use dvc-<prefix> instead."
  (let ((res (dvc-intern-symbol-name dvc postfix)))
    (if (or nodefault (boundp res)) (eval res)
      (let ((dvc-register-sym (intern (concat (symbol-name dvc) "-dvc"))))
        (unless (featurep dvc-register-sym)
          (dvc-trace "require %S" dvc-register-sym)
          (if (featurep 'xemacs)
              (require dvc-register-sym nil)
            (require dvc-register-sym nil t))))
      (let ((second-try (dvc-variable dvc postfix t)))
        second-try))))

;;;###autoload
(defun dvc-apply (postfix &rest args)
  "Apply ARGS to the `dvc-current-active-dvc' concated with POSTFIX."
  ;; dvc-current-active-dvc does not prompt for the local tree
  (let ((current-dvc (dvc-current-active-dvc)))
    (if current-dvc
        ;; We bind dvc-temp-current-active-dvc here so functions that
        ;; create new buffers and then call dvc-current-active-dvc
        ;; get the right back-end.
        (let ((dvc-temp-current-active-dvc current-dvc))
          (apply 'apply (dvc-function current-dvc postfix) args))

      ;; no current dvc found; prompt for tree
      (let ((default-directory
              (dvc-read-directory-name "Local tree: ")))
        (if (dvc-current-active-dvc t)
            (apply 'dvc-apply postfix args)
          ;; user thinks this directory is a DVC directory; don't just
          ;; keep prompting.
          (error "%s is not a DVC managed directory" default-directory))))))

;;;###autoload
(defun dvc-call (postfix &rest args)
  "Call the function specified by concatenating `dvc-current-active-dvc' and
POSTFIX, with arguments ARGS."
  ;; The &rest argument turns ARGS into a list for us
  (dvc-apply postfix args))

(defvar dvc-current-active-dvc-cache (make-hash-table :test 'equal)
  "A cache that contains directories as keys and the DVC symbol as value.
That value is considered first in `dvc-current-active-dvc'.")

(defvar dvc-buffer-current-active-dvc nil
  "Tell DVC which back-end to use in some buffers.

Overrides the search for a control directory in `dvc-current-active-dvc'.")
(make-variable-buffer-local 'dvc-buffer-current-active-dvc)

(defvar dvc-temp-current-active-dvc nil
  "Tell DVC which back-end to use temporarily.

Overrides the search for a control directory in
`dvc-current-active-dvc'. This is meant to be set in a let statement.")

(defun dvc-current-active-dvc (&optional nocache)
  "Get the currently active dvc for the current `default-directory'.

Currently supported dvc's can be found in
`dvc-registered-backends'. If `dvc-prompt-active-dvc' is nil,
`dvc-select-priority' specifies the priority, if more than one
back-end is in use for `default-directory'.

If `dvc-prompt-active-dvc' is non-nil, `dvc-registered-backends'
specifies the list of back-ends to test for, and the user is
prompted when more than one is found. Note that
`dvc-registered-backends' defaults to all backends that DVC
supports; it may be customized to only those used.

The value found for each directory is cached in `dvc-current-active-dvc-cache'.

If NOCACHE is non-nil, ignore the cache for this call, but still
cache the result (useful for correcting an incorrect cache entry).

If either `dvc-temp-current-active-dvc' (a let-bound value)
or `dvc-buffer-current-active-dvc' (a buffer-local value) is non-nil,
then use that value instead of the cache or searching."
  (interactive "P")
  (or dvc-temp-current-active-dvc
      dvc-buffer-current-active-dvc
      (let (root
            (dvc (unless nocache
                   (gethash (dvc-uniquify-file-name default-directory)
                            dvc-current-active-dvc-cache))))
        (unless dvc
          (if dvc-prompt-active-dvc
              (let ((dvc-list dvc-registered-backends)
                    (options)
                    (tree-root-func))
                (while dvc-list
                  (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
                  (when (fboundp tree-root-func)
                    (let ((current-root (funcall tree-root-func nil t)))
                      (when current-root
                        ;; WORKAROUND: ido-completing-read requires
                        ;; strings, not symbols, in the options list.
                        (setq options (cons (list (symbol-name (car dvc-list)) current-root) options)))))
                  (setq dvc-list (cdr dvc-list)))
                (case (length options)
                  (0
                   ;; FIXME: In most situations we'd like to abort
                   ;; with a nice error message here, but in others
                   ;; (ie dvc-find-file-hook) we need to silently
                   ;; return nil if there is no back-end found. Need
                   ;; another arg.
                   (setq dvc nil))

                  (1
                   (setq dvc (intern (caar options))))

                  (t
                   ;; We should use (dvc-variable (car option)
                   ;; "backend-name") in the prompt and completion
                   ;; list, but we can't go from that name back to the
                   ;; dvc symbol; dvc-register-dvc needs to build an
                   ;; alist. On the other hand, users use the symbol
                   ;; name in setting `dvc-select-priority', so
                   ;; perhaps this is better.
                   (let ((selection
                          (dvc-completing-read
                           (concat "back-end ("
                                   (mapconcat (lambda (option) (car option)) options ", ")
                                   "): ")
                           options nil t)))
                     (setq dvc (intern selection))
                     (setq root (cadr (assoc dvc options)))))))

            ;; not prompting
            (let ((dvc-list (append dvc-select-priority dvc-registered-backends))
                  (tree-root-func))
              (setq root "/")
              (while dvc-list
                (setq tree-root-func (dvc-function (car dvc-list) "tree-root" t))
                (when (fboundp tree-root-func)
                  (let ((current-root (funcall tree-root-func nil t)))
                    (when (and current-root (> (length current-root) (length root)))
                      (setq root current-root)
                      (setq dvc (car dvc-list)))))
                (setq dvc-list (cdr dvc-list)))))

          (if dvc
              ;; cache the found dvc, for both default-directory and root,
              ;; since a previous call may have cached a different dvc for
              ;; the root.
              (puthash (dvc-uniquify-file-name default-directory)
                       dvc dvc-current-active-dvc-cache)

            (unless (string= root default-directory)
              (puthash (dvc-uniquify-file-name root)
                       dvc dvc-current-active-dvc-cache))

            (when (interactive-p)
              (message "DVC: using %s for %s" dvc default-directory))))
        dvc)))

(defun dvc-select-dvc (directory dvc)
  "Select the DVC to use for DIRECTORY.
The given value is stored in `dvc-current-active-dvc-cache'."
  (interactive (list (dvc-uniquify-file-name
                      (dvc-read-directory-name "Set dvc for path: " nil nil t))
                     (intern (dvc-completing-read
                              "dvc: "
                              (map t 'symbol-name
                                   (append '(None) dvc-registered-backends))))))
  (when (eq dvc 'None)
    (message "Removing %s from dvc-current-active-dvc-cache" directory)
    (setq dvc nil))
  (puthash directory dvc dvc-current-active-dvc-cache))

(defun dvc-clear-dvc-cache ()
  "Clear the dvc cache. Useful when changing to an alternate back-end."
  (interactive)
  (clrhash dvc-current-active-dvc-cache))

(provide 'dvc-register)
;;; dvc-register.el ends here

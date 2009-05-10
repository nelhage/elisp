;;; dvc-build.el --- compile-time helper.

;; Copyright (C) 2004-2008 by all contributors

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;;      Thien-Thi Nguyen <ttn@gnuvola.org>
;; Inspired from the work of Steve Youngs <steve@youngs.au.com>

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

;; This file provides various functions for $(ebatch); see Makefile.in.
;; It is neither compiled nor installed.

;;; Code:

(unless noninteractive
  (error "This file is not intended for interactive use (see Makefile.in)"))

;; Expect a small set of env vars to be set by caller.
(defvar srcdir (or (getenv "srcdir")
                   (error "Env var `srcdir' not set")))
(defvar otherdirs (or (getenv "otherdirs")
                      ;; We used to `error' as for `srcdir' here, but on some
                      ;; systems, if the value is "", `getenv' returns nil, so
                      ;; we can't be too strict.  Reported by Stephen Leake.
                      ""))

;; Take control of exit(3).
(fset 'bye-bye (symbol-function 'kill-emacs))
(defun kill-emacs (&optional arg)
  (when (and arg (not (equal 0 arg)))
    (bye-bye)))

;; Standard

(defun zonk-file (filename)
  (when (file-exists-p filename)
    (delete-file filename)))

(require 'cl)
(require 'loadhist)
(require 'bytecomp)

(defun f-set-difference (a b) (set-difference a b :test 'string=))
(defun f-intersection   (a b) (intersection   a b :test 'string=))

(defun srcdir/ (filename)
  (expand-file-name filename srcdir))

;; Increase the max-specpdl-size size to avoid an error on some platforms
(setq max-specpdl-size (max 1000 max-specpdl-size))

;; Munge `load-path': contrib at end, everything else in front.
(add-to-list 'load-path (srcdir/ "contrib") t)
(dolist (dir
         ;;+ (split-string otherdirs " " t)
         ;;  Three-arg `split-string' is supported as of Emacs 22 and XEmacs
         ;;  21.4.16.  We will switch to it eventually.  For now, this works:
         (delete "" (split-string otherdirs " ")))
  (add-to-list 'load-path dir))
(add-to-list 'load-path (unless (equal "." srcdir) srcdir))
(add-to-list 'load-path nil)

;; Avoid interference from Emacs' VC.
(setq vc-handled-backends nil)

;; Internal vars are named --foo.

;; Platform-specific filenames.
(defvar --autoloads-filename (if (featurep 'xemacs)
                                 "auto-autoloads.el"
                               "dvc-autoloads.el"))

(defvar --custom-autoloads-filename (if (featurep 'xemacs)
                                        "custom-load.el"
                                      "cus-load.el"))

;; List of files to compile.
(defvar --to-compile
  (f-set-difference
   ;; plus
   (append
    ;; generated files
    (unless (string= "." srcdir)
      (mapcar 'expand-file-name '("dvc-version.el"
                                  "dvc-site.el")))
    ;; contrib libraries
    (when (string= (file-name-directory (locate-library "ewoc"))
                   (srcdir/ "contrib/"))
      '("contrib/ewoc.el"))
    ;; $(srcdir)/*.el
    (directory-files srcdir nil "^[^=].*\\.el$"))
   ;; minus
   (append
    ;; static
    `("dvc-build.el"
      ,--autoloads-filename
      ,--custom-autoloads-filename
      ,(if (featurep 'xemacs)
           "dvc-emacs.el"
         "dvc-xemacs.el"))
    ;; dynamic: if invalid, use nil
    (unless (locate-library "tree-widget")
      '("tla-browse.el")))))

;; Warnings we care about.
(defvar --warnings '(unresolved callargs redefine))

;; Autoload forms for XEmacs.
(when (featurep 'xemacs)
  (autoload 'setenv (if (emacs-version>= 21 5) "process" "env") nil t)
  ;; DVC things
  (autoload 'replace-regexp-in-string "dvc-xemacs.el")
  (autoload 'line-number-at-pos       "dvc-xemacs.el")
  (autoload 'line-beginning-position  "dvc-xemacs.el")
  (autoload 'line-end-position        "dvc-xemacs.el")
  (autoload 'match-string-no-properties "dvc-xemacs.el")
  (autoload 'tla--run-tla-sync        "tla-core.el")
  (autoload 'dvc-switch-to-buffer     "dvc-buffers.el")
  (autoload 'dvc-trace                "dvc-utils.el")
  (autoload 'dvc-flash-line           "tla")
  (autoload 'tla-tree-root            "tla")
  (autoload 'tla--name-construct      "tla-core")
  (defalias 'dvc-cmenu-mouse-avoidance-point-position
    'mouse-avoidance-point-position)
  ;; External things
  (autoload 'debug                    "debug")
  (autoload 'tree-widget-action       "tree-widget")
  (autoload 'ad-add-advice            "advice")
  (autoload 'customize-group          "cus-edit" nil t)
  (autoload 'dired                    "dired" nil t)
  (autoload 'dired-other-window       "dired" nil t)
  (autoload 'dolist "cl-macs" nil nil 'macro)
  (autoload 'easy-mmode-define-keymap "easy-mmode")
  (autoload 'minibuffer-prompt-end    "completer")
  (autoload 'mouse-avoidance-point-position "avoid")
  (autoload 'read-passwd              "passwd")
  (autoload 'read-kbd-macro           "edmacro" nil t)
  (autoload 'regexp-opt               "regexp-opt")
  (autoload 'reporter-submit-bug-report "reporter")
  (autoload 'view-file-other-window   "view-less" nil t)
  (autoload 'view-mode                "view-less" nil t)
  (autoload 'with-electric-help       "ehelp")
  (autoload 'read-kbd-macro           "edmacro")
  (autoload 'pp-to-string             "pp"))

(unless (fboundp 'defadvice)
  (autoload 'defadvice "advice" nil nil 'macro))

(defalias 'facep 'ignore)               ; ???

(defun byte-compile-dest-file (source)
  "Convert an Emacs Lisp source file name to a compiled file name.
In addition, remove directory name part from SOURCE."
  (concat (file-name-nondirectory (file-name-sans-versions source)) "c"))

;; Fix some Emacs byte-compiler problems.
(unless (featurep 'xemacs)

  (when (and (= emacs-major-version 21)
             (>= emacs-minor-version 3)
             (condition-case code
                 (let ((byte-compile-error-on-warn t))
                   (byte-optimize-form (quote (pop x)) t)
                   nil)
               (error (string-match "called for effect"
                                    (error-message-string code)))))
    (defadvice byte-optimize-form-code-walker (around silence-warn-for-pop
                                                      (form for-effect)
                                                      activate)
      "Silence the warning \"...called for effect\" for the `pop' form.
It is effective only when the `pop' macro is defined by cl.el rather
than subr.el."
      (let (tmp)
        (if (and (eq (car-safe form) 'car)
                 for-effect
                 (setq tmp (get 'car 'side-effect-free))
                 (not byte-compile-delete-errors)
                 (not (eq tmp 'error-free))
                 (eq (car-safe (cadr form)) 'prog1)
                 (let ((var (cadr (cadr form)))
                       (last (nth 2 (cadr form))))
                   (and (symbolp var)
                        (null (nthcdr 3 (cadr form)))
                        (eq (car-safe last) 'setq)
                        (eq (cadr last) var)
                        (eq (car-safe (nth 2 last)) 'cdr)
                        (eq (cadr (nth 2 last)) var))))
            (progn
              (put 'car 'side-effect-free 'error-free)
              (unwind-protect
                  ad-do-it
                (put 'car 'side-effect-free tmp)))
          ad-do-it))))

  (when (byte-optimize-form '(and (> 0 1) foo) t)
    (defadvice byte-optimize-form-code-walker
      (around fix-bug-in-and/or-forms (form for-effect) activate)
      "Optimize the rest of the and/or forms.
It has been fixed in XEmacs before releasing 21.4 and also has been
fixed in Emacs after 21.3."
      (if (and for-effect (memq (car-safe form) '(and or)))
          (let ((fn (car form))
                (backwards (reverse (cdr form))))
            (while (and backwards
                        (null (setcar backwards
                                      (byte-optimize-form (car backwards) t))))
              (setq backwards (cdr backwards)))
            (if (and (cdr form) (null backwards))
                (byte-compile-log
                 "  all subforms of %s called for effect; deleted" form))
            (when backwards
              (setcdr backwards
                      (mapcar 'byte-optimize-form (cdr backwards))))
            (setq ad-return-value (cons fn (nreverse backwards))))
        ad-do-it))))

;; Work around for an incompatibility (XEmacs 21.4 vs. 21.5), see the
;; following threads:
;;
;; http://thread.gmane.org/gmane.emacs.gnus.general/56414
;; Subject: attachment problems found but not fixed
;;
;; http://thread.gmane.org/gmane.emacs.gnus.general/56459
;; Subject: Splitting mail -- XEmacs 21.4 vs 21.5
;;
;; http://thread.gmane.org/gmane.emacs.xemacs.beta/20519
;; Subject: XEmacs 21.5 and Gnus fancy splitting.
(when (and (featurep 'xemacs)
           (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
             (modify-syntax-entry ?= " " table)
             (with-temp-buffer
               (with-syntax-table table
                 (insert "foo=bar")
                 (goto-char (point-min))
                 (forward-sexp 1)
                 (eolp)))))
  ;; The original `with-syntax-table' uses `copy-syntax-table' which
  ;; doesn't seem to copy modified syntax entries in XEmacs 21.5.
  (defmacro with-syntax-table (syntab &rest body)
    "Evaluate BODY with the SYNTAB as the current syntax table."
    `(let ((stab (syntax-table)))
       (unwind-protect
           (progn
             ;;(set-syntax-table (copy-syntax-table ,syntab))
             (set-syntax-table ,syntab)
             ,@body)
         (set-syntax-table stab)))))

(defun missing-or-old-elc ()
  "Return the list of .el files newer than their .elc."
  (remove-if-not (lambda (file)
                   (let ((source (srcdir/ file))
                         (elc (byte-compile-dest-file file)))
                     (or (not (file-exists-p elc))
                         (file-newer-than-file-p source elc))))
                 --to-compile))

;; Teach make-autoload how to handle define-dvc-unified-command.
(require 'autoload)
(require 'dvc-unified)
(defadvice make-autoload (before handle-define-dvc-unified-command activate)
  (if (eq (car-safe (ad-get-arg 0)) 'define-dvc-unified-command)
      (ad-set-arg 0 (macroexpand (ad-get-arg 0)))))

;; Teach `make-autoload' how to handle `define-derived-mode'.
(unless (make-autoload '(define-derived-mode child parent name
                          "docstring" body)
                       "file")
  (defadvice make-autoload (around handle-define-derived-mode activate)
    "Handle `define-derived-mode'."
    (if (eq (car-safe (ad-get-arg 0)) 'define-derived-mode)
        (setq ad-return-value
              (list 'autoload
                    (list 'quote (nth 1 (ad-get-arg 0)))
                    (ad-get-arg 1)
                    (nth 4 (ad-get-arg 0))
                    t nil))
      ad-do-it))
  (put 'define-derived-mode 'doc-string-elt 3))

;; Update custom-autoloads and autoloads (merging them for GNU Emacs),
;; and compile everything that needs compiling.
(defun dvc-build-all ()
  ;; The default warnings don't look so bad to me!
  ;;(unless command-line-args-left
  ;;  (setq byte-compile-warnings --warnings))
  (setq command-line-args-left nil)

  (let ((fake-c-l-a-l (list srcdir))
        (changed (missing-or-old-elc)))

    ;; Make `--custom-autoloads-filename'.
    (when changed
      (load "cus-dep")
      (let ((cusload-base-file --custom-autoloads-filename)
            (command-line-args-left fake-c-l-a-l))
        (if (fboundp 'custom-make-dependencies)
            (custom-make-dependencies)
          (Custom-make-dependencies))
        (when (featurep 'xemacs)
          (message "Compiling %s..." --custom-autoloads-filename)
          (byte-compile-file --custom-autoloads-filename))))

    ;; Make `--autoloads-filename'.
    (unless (and (file-exists-p --autoloads-filename)
                 (null changed))
      (let ((generated-autoload-file (expand-file-name --autoloads-filename))
            (command-line-args-left fake-c-l-a-l)
            (make-backup-files nil)
            (autoload-package-name "dvc"))
        (if (featurep 'xemacs)
            (zonk-file generated-autoload-file)
          (with-temp-file generated-autoload-file
            (insert ?\014)))
        (batch-update-autoloads)))

    ;; Insert some preload forms into the autoload file.
    (with-temp-file --autoloads-filename
      (insert-file-contents --autoloads-filename)
      (let ((blurb ";;; DVC PRELOAD\n"))
        (unless (save-excursion
                  ;; The preload forms are not guaranteed to be at beginning
                  ;; of buffer; they might be prefixed by cus-load munging.
                  ;; So search for them.  (Previously, we used `looking-at'.)
                  (search-forward blurb nil t))
          (insert blurb)
          (dolist (form '((require 'dvc-core)
                          (eval-when-compile
                            (require 'dvc-unified)
                            (require 'dvc-utils))))
            (pp form (current-buffer))))))

    ;; Merge custom load and autoloads for GNU Emacs and compile the result.
    (let ((tail-blurb (concat "\n\n"
                              "(provide 'dvc-autoloads)\n\n"
                              ";;; Local Variables:\n"
                              ";;; version-control: never\n"
                              ";;; no-update-autoloads: t\n"
                              ";;; End:\n"
                              ";;; dvc-autoloads.el ends here\n")))
      (when (or (not (file-exists-p --autoloads-filename))
                changed)
        (unless (featurep 'xemacs)
          (message "Merging %s into %s ..."
                   --custom-autoloads-filename
                   --autoloads-filename)
          (with-temp-file --autoloads-filename
            (insert-file-contents --custom-autoloads-filename)
            (delete-file --custom-autoloads-filename)
            (search-forward ";;; Code:\n")
            (delete-region (point-min) (point))
            (insert ";;; dvc-autoloads.el\n\n"
                    ";;; Code:\n")
            (goto-char (point-max))
            ;; ??? What do we have against this innocent var? --ttn
            (when (search-backward "custom-versions-load-alist" nil t)
              (forward-line -1))
            (delete-region (point) (point-max))
            (insert-file-contents --autoloads-filename)
            (goto-char (point-max))
            (when (search-backward "\n(provide " nil t)
              (delete-region (1- (point)) (point-max)))
            (insert tail-blurb)))
        (message "Compiling %s..." --autoloads-filename)
        (byte-compile-file --autoloads-filename)
        (when (featurep 'xemacs)
          (message (concat "Creating dummy dvc-autoloads.el..."))
          (with-temp-file "dvc-autoloads.el"
            (insert tail-blurb)))))

    ;; Compile `--to-compile' files.
    (when changed
      (dolist (file --to-compile)
        (load (srcdir/ file) nil nil t))
      ;; We compute full fanout, not just root-set one-level-downstream.
      ;; In this way we err on the safe side.
      (let (todo)
        (while changed
          (nconc changed (f-set-difference
                          (f-intersection
                           (mapcar 'file-name-nondirectory
                                   (file-dependents
                                    (srcdir/ (car changed))))
                           --to-compile)
                          todo))
          (pushnew (pop changed) todo :test 'string=))
        (mapc 'zonk-file (mapcar 'byte-compile-dest-file todo))
        (mapc 'byte-compile-file (mapcar 'srcdir/ todo)))))

  ;; All done.  TODO: Summarize.
  (bye-bye))

;;; dvc-build.el ends here

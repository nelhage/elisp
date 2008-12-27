;;; dvc-utils.el --- Utility functions for DVC

;; Copyright (C) 2005 - 2008 by all contributors

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

;; This file provides some functions used in DVC, but not particularly
;; linked to revision control systems.


(require 'dvc-defs)
(require 'ewoc)

;; Load compatibility code
(if (featurep 'xemacs)
    (require 'dvc-xemacs)
  (require 'dvc-emacs))

;; Macros to generate correct code for different emacs variants
;; (progn ...) is here to have autoload generation actually insert the
;; code in the autoload file.
;;;###autoload
(progn
  (defmacro dvc-do-in-gnu-emacs (&rest body)
    "Execute BODY if in GNU/Emacs."
    (declare (indent defun) (debug (body)))
    (unless (featurep 'xemacs) `(progn ,@body))))

;;;###autoload
(progn
  (defmacro dvc-do-in-xemacs (&rest body)
    "Execute BODY if in XEmacs."
    (declare (indent defun) (debug (body)))
    (when (featurep 'xemacs) `(progn ,@body))))

(defconst dvc-mouse-2
  (if (featurep 'xemacs)
      [down-mouse-2]
    [mouse-2]))

(dvc-do-in-xemacs
  (unless (functionp 'clone-process)
    (defun clone-process (process &optional newname)
      "Create a twin copy of PROCESS.
If NEWNAME is nil, it defaults to PROCESS' name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.
If PROCESS is associated with a buffer, the new process will be associated
  with the current buffer instead.
Returns nil if PROCESS has already terminated."
      (setq newname (or newname (process-name process)))
      (if (string-match "<[0-9]+>\\'" newname)
          (setq newname (substring newname 0 (match-beginning 0))))
      (when (memq (process-status process) '(run stop open))
        (let* ((process-connection-type (process-tty-name process))
               (old-kwoq (process-kill-without-query process nil))
               (new-process
                (if (memq (process-status process) '(open))
                    (apply 'open-network-stream newname
                           (if (process-buffer process) (current-buffer)))
                  (apply 'start-process newname
                         (if (process-buffer process) (current-buffer))
                         (process-command process)))))
          (process-kill-without-query new-process old-kwoq)
          (process-kill-without-query process old-kwoq)
          (set-process-filter new-process (process-filter process))
          (set-process-sentinel new-process (process-sentinel process))
          new-process)))))

(defmacro dvc-funcall-if-exists (function &rest args)
  "Call FUNCTION with ARGS as parameters if it exists."
  (if (fboundp function)
      `(funcall ',function ,@args)))


(defun dvc-strip-final-newline (string)
  "Strip the final newline from STRING if there's one."
  (if (eq (aref string (- (length string) 1)) ?\n)
      (substring string 0 (- (length string) 1))
    string))


(defun dvc-add-to-list (list-var element &optional append)
  "Same behavior as GNU Emacs's `add-to-list', but also works on XEmacs.
LIST-VAR is a symbol representing the list to be modified.
ELEMENT is the element to be added to the list.
If APPEND is non-nil, add the item to the end of the list instead of the
front."
  (if (featurep 'xemacs)
      (if append
          (when (not (member element (eval list-var)))
            (set list-var (append (eval list-var) (list element))))
        (add-to-list list-var element))
    (add-to-list list-var element append)))

;; copied from Emacs22, only needed when omit-nulls is needed,
;; otherwise split-string can be used
(defun dvc-split-string (string &optional separators omit-nulls)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
which separates, but is not part of, the substrings.  If nil it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\n\\r\\v]+\", and
OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)').  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((keep-nulls (not (if separators omit-nulls t)))
        (rexp (or separators split-string-default-separators))
        (start 0)
        notfirst
        (list nil))
    (while (and (string-match rexp string
                              (if (and notfirst
                                       (= start (match-beginning 0))
                                       (< start (length string)))
                                  (1+ start) start))
                (< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
          (setq list
                (cons (substring string start (match-beginning 0))
                      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
        (setq list
              (cons (substring string start)
                    list)))
    (nreverse list)))

(eval-and-compile
  (unless (fboundp 'dired-delete-file)
    ;; NOTE: Cut-and-past from CVS Emacs
    ;;
    (defvar dired-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")
    (defun dired-make-relative (file &optional dir ignore)
      "Convert FILE (an absolute file name) to a name relative to DIR.
If this is impossible, return FILE unchanged.
DIR must be a directory name, not a file name."
      (or dir (setq dir default-directory))
      ;; This case comes into play if default-directory is set to
      ;; use ~.
      (if (and (> (length dir) 0) (= (aref dir 0) ?~))
          (setq dir (expand-file-name dir)))
      (if (string-match (concat "^" (regexp-quote dir)) file)
          (substring file (match-end 0))
        ;; (or no-error
        ;;     (error "%s: not in directory tree growing at %s" file dir))
        file))
    ;; Delete file, possibly delete a directory and all its files.
    ;; This function is useful outside of dired.  One could change it's name
    ;; to e.g. recursive-delete-file and put it somewhere else.
    (defun dired-delete-file (file &optional recursive) "\
Delete FILE or directory (possibly recursively if optional RECURSIVE is true.)
RECURSIVE determines what to do with a non-empty directory.  If RECURSIVE is:
Nil, do not delete.
`always', delete recursively without asking.
`top', ask for each directory at top level.
Anything else, ask for each sub-directory."
      (let (files)
        ;; This test is equivalent to
        ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
        ;; but more efficient
        (if (not (eq t (car (file-attributes file))))
            (delete-file file)
          (when (and recursive
                     (setq files
                           (directory-files file t dired-re-no-dot)) ; Not empty.
                     (or (eq recursive 'always)
                         (yes-or-no-p (format "Recursive delete of %s "
                                              (dired-make-relative file)))))
            (if (eq recursive 'top) (setq recursive 'always)) ; Don't ask again.
            (while files               ; Recursively delete (possibly asking).
              (dired-delete-file (car files) recursive)
              (setq files (cdr files))))
          (delete-directory file))))))

(defun dvc-sethome (dir)
  "Sets $HOME to DIR, safely.

`setenv' is not sufficient because `abbreviated-home-dir' would then
be incorrectly set, breaking a lot of Emacs function."
  (setenv "HOME" dir)
  (setq abbreviated-home-dir nil))

(defun dvc-read-directory-name (prompt &optional dir default-dirname
                                       mustmatch initial)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-DIRNAME if user exits with the same
non-empty string that was inserted by this function.
 (If DEFAULT-DIRNAME is omitted, the current buffer's directory is used,
  except that if INITIAL is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg MUSTMATCH non-nil means require existing directory's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL specifies text to start with.
DIR should be an absolute directory name.  It defaults to
the value of `default-directory'."
  (if (fboundp 'read-directory-name)
      (read-directory-name prompt dir default-dirname mustmatch initial)
    ;; The same as the definition of `read-directory-name'
    ;; in GNU Emacs in CVS.
    (unless dir
      (setq dir default-directory))
    (unless default-dirname
      (setq default-dirname
            (if initial (concat dir initial) dir)))
    (read-file-name prompt dir default-dirname mustmatch initial)))

(defun dvc-create-tarball-from-intermediate-directory (dir tgz-file-name)
  "Create a tarball with the content of DIR.
If DIR does not yet exist, wait until it does exist.
Then create the tarball TGZ-FILE-NAME and remove the contents of DIR."
  ;;create the archive: tar cfz ,,cset.tar.gz ,,cset
  (while (not (file-exists-p dir)) ;;somewhat dirty, but seems to work...
    (sit-for 0.01))
  ;;(message "Calling tar cfz %s -C %s %s" tgz-file-name (file-name-directory dir) (file-name-nondirectory dir))
  (call-process "tar" nil nil nil "cfz" tgz-file-name "-C" (file-name-directory dir) (file-name-nondirectory dir))
  (call-process "rm" nil nil nil "-rf" dir)
  (message "Created tarball %s" tgz-file-name))


(defvar dvc-digits (string-to-list "0123456789"))

(defun dvc-digit-char-p (character)
  "Returns non-nil if CHARACTER is a digit."
  (member character dvc-digits))

(defun dvc-position (item seq &optional comp-func)
  "Position of ITEM in list, or nil if not found.
Return 0 if ITEM is the first element of SEQ.
If an optional argument COMP-FUNC is given, COMP-FUNC
is used to compare ITEM with an item of SEQ; returning t
means the two items are the same."
  (let ((pos 0)
        (seq-int seq))
    (unless comp-func
      (setq comp-func 'eq))
    (while (and seq-int
                (not (funcall comp-func item (car seq-int))))
      (setq seq-int (cdr seq-int))
      (setq pos (1+ pos)))
    (when seq-int pos)))

(defun dvc-uniquify-file-name (path)
  "Return a unique string designating PATH.
If PATH is a directory,the returned contains one and exactly one trailing
slash.  If PATH is nil, then nil is returned."
  (and path
       (let ((expanded (file-truename
                        (expand-file-name
                         (if (file-directory-p path)
                             (file-name-as-directory path)
                           path)))))
         (if (featurep 'xemacs)
             (replace-regexp-in-string "/+$" "/" expanded)
           expanded))))

(defun dvc-add-uniquify-directory-mode (mode)
  "Add MODE to `uniquify-list-buffers-directory-modes'."
  (require 'uniquify)
  (when (boundp 'uniquify-list-buffers-directory-modes)
    (add-to-list 'uniquify-list-buffers-directory-modes mode)))

(defvar dvc-temp-directory "/tmp"
  "Temporary directory for some DVC operations.")

(defun dvc-make-temp-name (file)
  "Generate a temporary file name based on FILE.
The path for the file name can be set via `dvc-temp-directory'."
  (make-temp-name (concat (dvc-uniquify-file-name dvc-temp-directory) file)))

(defun dvc-buffer-content (buffer)
  "Return the content of BUFFER as a string.
Strips the final newline if there is one."
  (with-current-buffer buffer
    (buffer-substring-no-properties
     (point-min)
     (progn (goto-char (point-max))
            (if (eq (char-before) ?\n)
                (- (point) 1)
              (point))))))

;; this is no longer needed, because ewoc-create takes now the argument nosep:
;; (defun ewoc-create (pretty-printer &optional header footer nosep)
;; If you need that behaviour: set dvc-ewoc-create-needs-newline to t
(defvar dvc-ewoc-create-needs-newline nil)
(defun dvc-ewoc-create-api-select (pretty-printer)
  "Possibly insert a trailing newline after PRETTY-PRINTER call.
Work around `ewoc-create' interface change: oldest versions automatically
added a trailing newline, whereas newest versions do not."
  (if dvc-ewoc-create-needs-newline
      ;; if `ewoc-set-data' is defined, the pretty printer should insert a
      ;; trailing newline (new `ewoc-create' interface; there is no
      ;; `ewoc-version', therefore we test on `ewoc-set-data')
      `(lambda (elem) (,pretty-printer elem) (insert "\n"))
    pretty-printer))

;; ----------------------------------------------------------------------------
;; Face manipulators
;; ----------------------------------------------------------------------------
(defsubst dvc-face-add (str face &optional keymap menu help)
  "Add to string STR the face FACE.
Optionally, also add the text properties KEYMAP, MENU and HELP.

If KEYMAP is a symbol, (symbol-value KEYMAP) is used
as a keymap; and `substitute-command-keys' result
against (format \"\\{%s}\" (symbol-name keymap)) is appended to HELP.

If HELP is nil and if MENU is non nil, the MENU title is used as HELP."
  (if dvc-highlight
      (let* ((strcpy (copy-sequence str))
             (key-help (when (symbolp keymap)
                         (substitute-command-keys (format "\\{%s}" (symbol-name keymap)))))
             (prefix-help (if help help (when (and menu (stringp (cadr menu))) (cadr menu))))
             (long-help (if key-help
                            (if prefix-help (concat prefix-help "\n"
                                                    ;; Sigh. Font used on tooltips in GNU Emacs with Gtk+
                                                    ;; is a proportional.
                                                    ;; (make-string (length help) ?=) "\n"
                                                    "================" "\n"
                                                    key-help) key-help)
                          help))
             (keymap (if (symbolp keymap) (symbol-value keymap) keymap)))
        (add-text-properties 0 (length strcpy)
                             `(face ,face
;;; Even if we define a face in a buffer, it seems that
;;; font-lock mode just ignore it or remove the face property.
;;; I don't know the detail but in tla-inventory buffer,
;;; I cannot make both font-lock keywords and faces put by dvc-face-add
;;; highlight at once. When font-lock-face is defined, I can do.
;;; See "Special Properties" subsection in the emacs lisp reference manual.
;;; `font-lock-face' property is new in Emacs 21.4. However, I guess there is
;;; no wrong side effect if I define font-lock-face property here.
                                    font-lock-face ,face
                                    ,@(when keymap
                                        `(mouse-face highlight
                                                     keymap ,keymap
                                                     help-echo ,long-help))
                                    ,@(when menu
                                        `(dvc-cmenu ,menu))
                                    )
                             strcpy)
        strcpy)
    str))

(defun dvc-face-add-with-condition (condition text face1 face2)
  "If CONDITION then add TEXT the face FACE1, else add FACE2."
  (if condition
      (dvc-face-add text face1)
    (dvc-face-add text face2)))

(defun dvc-flash-line-on ()
  "Turn on highline mode or equivalent."
  (or (dvc-funcall-if-exists hl-line-mode)
      (dvc-funcall-if-exists highline-on)))

(defun dvc-flash-line-off ()
  "Turn off highline mode or equivalent."
  (or (dvc-funcall-if-exists hl-line-mode)
      (dvc-funcall-if-exists highline-off)))

(defun dvc-flash-line ()
  "Flash the current line."
  (let ((buffer (current-buffer)))
    (dvc-flash-line-on)
    (sit-for 1000)
    ;; Avoid to switching buffer by asynchronously running
    ;; processes.
    ;; TODO: This is adhoc solution. Something guard-mechanism to avoid
    ;; buffer switching may be needed.
    (set-buffer buffer)
    (dvc-flash-line-off)))

;; ----------------------------------------------------------------------------
;; Debugging facilities
;; ----------------------------------------------------------------------------
(defvar dvc-debug nil
  "*Indicate whether debugging messages should be printed by `dvc-trace'.")

;;;###autoload
(defun dvc-trace (&rest msg)
  "Display the trace message MSG.
Same as `message' if `dvc-debug' is non-nil.
Does nothing otherwise.  Please use it for your debug messages."
  (when dvc-debug
    (apply 'message (concat "dvc: " (car msg)) (cdr msg))))

(defun dvc-trace-current-line ()
  "Display the line the cursor is in."
  (dvc-trace "Current-line(%s)=%s[_]%s"
             (save-restriction (widen) (dvc-line-number-at-pos))
             (buffer-substring-no-properties
              (line-beginning-position)
              (point))
             (buffer-substring-no-properties
              (point)
              (line-end-position))))

(defmacro dvc-features-list ()
  "Topological sort of the dependancy graph. Root comes last.

It's a macro so that it remains available after (unload-feature ...)."
  (quote '(
           ;; DVC
           dvc-site
           dvc-version
           dvc-tips
           dvc-buffers
           dvc-core
           dvc-defs
           dvc-diff
           dvc-emacs
           dvc-lisp
           dvc-revlog
           dvc-revlist
           dvc-log
           dvc-register
           dvc-ui
           dvc-unified
           dvc-utils
           dvc-xemacs
           ;; xhg
           xhg-core
           xhg-dvc
           xhg-gnus
           xhg
           ;; tla
           tla-dvc
           tla-bconfig
           tla-browse
           tla-tests
           tla
           tla-core
           tla-autoconf
           tla-defs
           tla-gnus
           ;; baz
           baz-dvc
           baz
           ;; bzr
           bzr-core
           bzr-dvc
           bzr-revlist
           bzr-revision
           bzr
           ;; xgit
           xgit-annotate
           xgit-dvc
           xgit-gnus
           xgit-log
           xgit-revision
           xgit-core
           xgit
           )))

(defun dvc-unload ()
  "Unloads DVC.

run `unload-feature' for each DVC feature.

TODO: should also remove the hooks setup by DVC
\(`file-find-hook', ...)."
  (interactive)
  (dolist (feature (dvc-features-list))
    (when (featurep feature) (unload-feature feature t)))
  (when (featurep 'dvc-autoloads)
    (unload-feature 'dvc-autoloads t)))

;;;###autoload
(defun dvc-reload (&optional directory)
  "Reload DVC (usually for debugging purpose).

With prefix arg, prompts for the DIRECTORY in which DVC should be
loaded.  Useful to switch from one branch to the other.

If a Makefile is present in the directory where DVC is to be loaded,
run \"make\"."
  (interactive
   (list (when current-prefix-arg
           (let* ((other (dvc-read-directory-name
                          "Load DVC from: "))
                  (lispdir (concat (file-name-as-directory other)
                                   "lisp")))
             (if (file-directory-p lispdir)
                 lispdir
               other)))))
  (when directory
    (let ((current-path (file-name-directory (locate-library
                                              "dvc-core"))))
      (setq load-path
            (cons directory (remove current-path load-path)))))
  (let ((default-directory (file-name-directory (locate-library "dvc-core"))))
    (when (file-exists-p
           "Makefile")
      (shell-command "make")))
  (dvc-unload)
  (require 'dvc-autoloads))

(defun dvc-regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else.
Special characters are escaped to leave STRING in a suitable form for
Arch."
  (let ((quoted (regexp-quote string)))
    (replace-regexp-in-string
     "\\([{}()|]\\)"
     (concat "\\\\"                     ; leading slash
             "\\1")                     ; quoted character
     quoted)))

(defun dvc-pp-to-string (sexp)
  "Return sexp pretty printed by `pp-to-string'."
  (let ((print-readably t)
        print-level print-length)
    (pp-to-string sexp)))

(defvar dvc-buffer-refresh-function nil
  "Variable should be local to each buffer.
Function used to refresh the current buffer")
(make-variable-buffer-local 'dvc-buffer-refresh-function)

(defun dvc-read-project-tree-maybe (&optional prompt directory prefer-current)
  "Return a directory name which is the root of some project tree.
Either prompt from the user or use the current directory.
The behavior can be changed according to the value of
`dvc-read-project-tree-mode'.

PROMPT is used as a user prompt, and DIRECTORY is the starting point
of the project search.

When `dvc-read-project-tree-mode' is `unless-specified',
PREFER-CURRENT non-nil means use current `default-directory' if
it is a valid project tree."
  (let* ((root (dvc-tree-root (or directory default-directory) t))
         (default-directory (or root
                                directory
                                default-directory))
         (prompt (or prompt "Use directory: ")))
    (case dvc-read-project-tree-mode
      (always (dvc-tree-root (dvc-read-directory-name prompt)))

      (unless-specified
       (if (or directory (and prefer-current root))
           (if root
               root
             (error "%s directory is not a DVC managed directory" directory))
         (dvc-read-directory-name prompt)))

      (sometimes (or root
                     (dvc-tree-root (dvc-read-directory-name prompt))))

      (never (or root
                 (error "%s directory is not a DVC managed directory" directory)))

      (t (error "`%s': wrong value for dvc-read-project-tree-mode" dvc-read-project-tree-mode)))))

(defun dvc-generic-refresh ()
  "Call the function specified by `dvc-buffer-refresh-function'."
  (interactive)
  (let ((dvc-read-directory-mode 'never)
        (dvc-read-project-tree-mode 'never))
    (if dvc-buffer-refresh-function
        (let ((dvc-temp-current-active-dvc dvc-buffer-current-active-dvc))
          (funcall dvc-buffer-refresh-function))
      (message "I don't know how to refresh this buffer"))))

(defmacro dvc-make-move-fn (ewoc-direction function cookie
                                           &optional only-unmerged)
  "Create function to move up or down in `dvc-revlist-cookie'.

EWOC-DIRECTION is either `ewoc-next' or `ewoc-prev'.
FUNCTION is the name of the function to declare.
COOKIE is the ewoc to navigate in.
if ONLY-UNMERGED is non-nil, then, navigate only through revisions not
merged by another revision in the same list."
  (declare (indent 2) (debug (&define functionp name symbolp booleanp)))
  `(defun ,function ()
     (interactive)
     (let* ((elem (ewoc-locate ,cookie))
            (next (or (,ewoc-direction ,cookie elem) elem)))
       (while (and next
                   (if ,only-unmerged
                       (not (and (eq (car (ewoc-data next))
                                     'entry-patch)
                                 (eq (nth 4 (ewoc-data next))
                                     'nobody)))
                     (eq (car (ewoc-data next)) 'separator))
                   (,ewoc-direction ,cookie next))
         (setq next (,ewoc-direction ,cookie next)))
       (while (and next
                   (if ,only-unmerged
                       (not (and (eq (car (ewoc-data next))
                                     'entry-patch)
                                 (eq (nth 4 (ewoc-data next))
                                     'nobody)))
                     (eq (car (ewoc-data next)) 'separator)))
         (setq next (,(if (eq ewoc-direction 'ewoc-next)
                          'ewoc-prev
                        'ewoc-next) ,cookie next)))
       (when next (goto-char (ewoc-location next))))))

(defun dvc-ewoc-maybe-scroll (ewoc node)
  "If display of NODE goes off the bottom of the window, recenter."
  (let* ((next-node (ewoc-next ewoc node))
         (next-loc (if next-node
                       (ewoc-location next-node)
                     (ewoc-location (ewoc--footer ewoc)))))
    (if (> next-loc (window-end))
        ;; we tried scroll-up here, but it screws up sometimes
        (recenter))
  ))

(defmacro dvc-make-ewoc-next (function-name ewoc)
  "Declare a function FUNCTION-NAME to move to the next EWOC entry."
  (declare (indent 2) (debug (&define functionp function-name symbolp)))
  `(defun ,function-name (&optional filter no-ding)
     (interactive)
     "Move to the next ewoc entry.
If optional FILTER is non-nil, skip elements for which FILTER
returns non-nil. FILTER is called with one argument, the ewoc
element. If optional NO-DING, don't ding if there is no next."
     (let* ((current (ewoc-locate ,ewoc))
            (cur-location (ewoc-location current))
            (next (ewoc-next ,ewoc current)))
       (cond
        ((> cur-location (point))
         ;; not exactly at an element; move there
         (goto-char cur-location)
         (dvc-ewoc-maybe-scroll ,ewoc current))

        (next
         (if filter
             (progn
               (while (and next
                           (funcall filter next))
                 (setq next (ewoc-next ,ewoc next)))
               (if next
                   (goto-char (ewoc-location next))
                 (unless no-ding (ding))))
           (goto-char (ewoc-location next))
           (dvc-ewoc-maybe-scroll ,ewoc next)))

        (t
         ;; at last element
         (unless no-ding (ding)))))))

(defmacro dvc-make-ewoc-prev (function-name ewoc)
  "Declare a function FUNCTION-NAME to move to the previous EWOC entry."
  (declare (indent 2) (debug (&define functionp function-name symbolp)))
  `(defun ,function-name (&optional filter no-ding)
     "Move to the previous ewoc entry.
If optional FILTER is non-nil, skip elements for which FILTER
returns non-nil. FILTER is called with one argument, the ewoc
element. If optional NO-DING, don't ding if there is no next."
     (interactive)
     (let* ((current (ewoc-locate ,ewoc))
            (cur-location (ewoc-location current))
            (prev (ewoc-prev ,ewoc current)))
       (cond
        ((> (point) cur-location)
         (goto-char cur-location))

        (prev
         (if filter
             (progn
               (while (and prev
                           (funcall filter prev))
                 (setq prev (ewoc-prev ,ewoc prev)))
               (if prev
                   (goto-char (ewoc-location prev))
                 (unless no-ding (ding))))
           (goto-char (ewoc-location prev))))

        (t
         ;; at first element
         (unless no-ding (ding)))))))

(defun dvc-scroll-maybe (buffer up-or-down)
  "If BUFFER exists, show it, scroll and return non-nil.
Otherwise, return nil."
  (interactive)
  (when (buffer-live-p buffer)
    (let ((visible (dvc-buffer-visible-p buffer))
          (buf (current-buffer)))
      (pop-to-buffer buffer)
      (when visible
        (condition-case nil
            (funcall up-or-down 2)
          (error (message "Can't scroll anymore."))))
      (pop-to-buffer buf))))

(defun dvc-offer-choices (comment choices)
  "Present user with a choice of actions, labeled by COMMENT. CHOICES is a list of pairs
containing (symbol description)."
  ;; Could use "keyboard menu"; see elisp info 22.17.3 Menus and the Keyboard
  (let ((msg "use ")
        choice)
    (dolist (choice choices)
      (setq msg (concat msg
                        (key-description (car (where-is-internal (car choice))))
                        " (" (cadr choice) ") ")))
    (error (if comment
               (concat comment "; " msg)
             msg))))

(defun dvc-completing-read (&rest args)
  "Read a string in the minibuffer, with completion.
Set `dvc-completing-read-function' to determine which function to use.

See `completing-read' for a description of ARGS."
  ;; Initialize dvc-completing-read-function on the first invocation of dvc-completing-read
  ;; This allows to enable ido-mode after loading DVC
  (when (eq dvc-completing-read-function 'auto)
    (setq dvc-completing-read-function (if (and (boundp 'ido-mode) ido-mode)
                                           'ido-completing-read
                                         'completing-read)))
  (apply dvc-completing-read-function args))

(defun dvc-default-excluded-files ()
  "Return a list of strings (normally file names relative to tree
root) from the file \".dvc-exclude\" in `default-directory'.
Shell wildcards are converted to regexp, for use with
`dvc-match-excluded'."
  (if (file-readable-p ".dvc-exclude")
      (with-temp-buffer
        (insert-file-contents ".dvc-exclude")
        (let (result)
          (while (< (point) (point-max))
            (setq result (append result (list (wildcard-to-regexp (buffer-substring (point) (point-at-eol))))))
            (forward-line 1))
          result))))

(defun dvc-match-excluded (excluded-files file)
  "Non-nil if any element of EXCLUDED-FILES matches FILE,
according to `string-match'."
  (let (matched)
    (dolist (file-regexp excluded-files matched)
      (setq matched
            (or matched
                (string-match file-regexp file))))
    (not (null matched))))

(defun dvc-edit-exclude ()
  "Edit the file \".dvc-exclude\" in `default-directory'."
  (interactive)
  (find-file ".dvc-exclude"))

(defsubst dvc-xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun dvc-message-replace-header (header new-value &optional after force)
  "Remove HEADER and insert the NEW-VALUE.
If AFTER, insert after this header.  If FORCE, insert new field
even if NEW-VALUE is empty."
  ;; Similar to `nnheader-replace-header' but for message buffers.
  (require 'message)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header header))
    (when (or force (> (length new-value) 0))
      (if after
          (message-position-on-field header after)
        (message-position-on-field header))
      (insert new-value))))

(provide 'dvc-utils)
;;; dvc-utils.el ends here

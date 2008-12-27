;;; tla-core.el --- Core of xtla

;; Copyright (C) 2003-2004 by all contributors

;; Author: Stefan Reichoer, <stefan@xsteve.at>
;; Contributions from:
;;    Matthieu Moy <Matthieu.Moy@imag.fr>
;;    Masatake YAMATO <jet@gyve.org>
;;    Milan Zamazal <pdm@zamazal.org>
;;    Martin Pool <mbp@sourcefrog.net>
;;    Robert Widhopf-Fenk <hack@robf.de>
;;    Mark Triggs <mst@dishevelled.net>

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

;; This file provides the low-level functions used by xtla.el


;;; History:

;; This file was created to split out some commonly-used functionality.

;;; Code:
(eval-and-compile (require 'dvc-core))
(eval-and-compile (require 'dvc-utils))

(require 'tla-defs)
(require 'tla-autoconf)
(eval-and-compile (require 'dvc-lisp))

(require 'ewoc)

;; ----------------------------------------------------------------------------
;; Compatibility stuff
;; ----------------------------------------------------------------------------
(eval-when-compile
  (require 'cl)
  (if (featurep 'xemacs)
      (require 'dvc-xemacs)
    (require 'dvc-emacs)))

(require 'pp)

;;
;; Arch branch: baz, tla, ...
;;
(defun tla--executable ()
  "Return the Arch executable to use.
Can be either tla or baz."
  (cond ((eq tla-arch-branch 'tla)
         tla-executable)
        ((eq tla-arch-branch 'baz)
         baz-executable)))

(defun tla-arch-branch-name ()
  "Return the name of the branch of arch, as a string."
  (symbol-name tla-arch-branch))

(defun tla-arch-branch-name-caps ()
  "Return the name of the branch of arch, as a capitalized string."
  (capitalize (symbol-name tla-arch-branch)))




;;;###autoload
(defun tla-tree-root (&optional location no-error interactive)
  "Return the tree root for LOCATION, nil if not in a local tree.
Computation is done from withing Emacs, by looking at an {arch}
directory in a parent buffer of LOCATION.  This is therefore very
fast.

If LOCATION is nil, the tree root is returned, and it is
guaranteed to end in a \"/\" character.

If NO-ERROR is non-nil, don't raise an error if LOCATION is not an
arch managed tree (but return nil)."
  (interactive)
  (dvc-tree-root-helper "{arch}/=tagging-method" (or interactive
                                                     (interactive-p))
                        "%S is not in an arch-managed tree!"
                        location no-error))

(defun tla--last-visited-inventory-buffer ()
  "Return the last visited xtla's inventory buffer."
  (let ((inventories (remove nil (mapcar
                                  (lambda (elt)
                                    (when (buffer-live-p (cadr elt))
                                      elt))
                                  (cdr (assoc 'inventory dvc-buffers-tree)))))
        (bl (buffer-list)))
    (cadr (car (sort inventories (lambda (a b)
                                   (let ((aindex (dvc-position (cadr a) bl))
                                         (bindex (dvc-position (cadr b) bl)))
                                     (< aindex bindex))))))))

(defun tla-show-inventory-buffer ()
  "Switch to the last visited inventory buffer."
  (interactive)
  (dvc-switch-to-buffer (tla--last-visited-inventory-buffer)))

(defun tla-use-tla ()
  "From now, use tla."
  (interactive)
  (tla-autoconf-reset)
  (setq tla-arch-branch 'tla))

(defun tla-use-baz ()
  "From now, use baz."
  (interactive)
  (tla-autoconf-reset)
  (setq tla-arch-branch 'baz))

(defun tla--run-tla-async (arguments &rest keys)
  "Run tla asynchronously. See `dvc-run-dvc-async'"
  (if (and tla-arch-branch (not (eq tla-arch-branch 'none)))
      (apply 'dvc-run-dvc-async tla-arch-branch arguments keys)
    (error "No tla variant is installed on your system")))

(defun tla--run-tla-sync (arguments &rest keys)
  "Run tla synchronously. See `dvc-run-dvc-sync'"
  (if (and tla-arch-branch (not (eq tla-arch-branch 'none)))
      (apply 'dvc-run-dvc-sync tla-arch-branch arguments keys)
    (error "No tla variant is installed on your system")))

;; ----------------------------------------------------------------------------
;; Arch name manipulators
;; ======================
;;
;; Normally in xtla, a name, a revision specifier is represented as a
;; list like:
;;
;;    ("archive" "category" "branch" "version" "revision")
;;
;; Nil is permitted as the element. However the list length must be 5
;; like:
;;
;;    (nil "category" "branch" nil nil)
;;
;; In other hand, in tla command, the name must be represented as a
;; string like:
;;
;;    "archive/category--branch--version--revision"
;;
;; So we have to convert a name in different representation in many
;; cases.
;;
;; * tla--name-split-* is for converting from a string representation
;;   to a list representation. There are semi-qualified version and
;;   fully-qualified version.
;;
;;   - semi-qualified: "category--branch--version--revision".
;;     `tla--name-split-semi-qualified' expects a name string without
;;     archive component. The archive field of returned list is filled
;;     with nil.
;;
;;   - fully-qualified: "archive/category--branch--version--revision".
;;     `tla--name-split' expects a name string including archive.
;;
;; * tla--name-construct-* is for converting from a list
;;   representation to a string representation. The functions accept
;;   arguments two ways.
;;
;;   - normal passing: (tla--name-construct "archive" "category"...)
;;   - packed passing: (tla--name-construct '("archive" "category"...))
;;
;;   There are semi-qualified version and fully-qualified version.
;;   - semi-qualified: `tla--name-construct-semi-qualified' connects
;;     arguments with "--".
;;   - fully-qualified: `tla--name-construct" connects the first argument
;;     and the rest with "/". About the rest,
;;     `tla--name-construct-semi-qualified' is applied.
;;
;; * tla--name-{archive|category|branch|version|revision} is for
;;   extracting a component from a name. The both representations are
;;   acceptable.
;;
;; * tla--name-mask is for replace a component in the name list with nil.
;;
;; ----------------------------------------------------------------------------

;;
;; String representation -> List representation
;;
(defun tla--name-split-semi-qualified (name &optional archive)
  "Split \"--\" connected string NAME into 5 elements list.
The first element is always nil if ARCHIVE is not given.
If ARCHIVE is given, use it as the first.
Even if the elements in name are less than 5, the list is filled by nil
to make the length 5.

  ELISP> (tla--name-split-semi-qualified \"branch--category--version--revision\"
                                        \"archive\")
  (\"archive\" \"branch\" \"category\" \"version\" \"revision\")

  ELISP> (tla--name-split-semi-qualified
            \"branch--category--version--revision\")
  (nil \"branch\" \"category\" \"version\" \"revision\")

  ELISP> (tla--name-split-semi-qualified \"branch--category--version\")
  (nil \"branch\" \"category\" \"version\" nil)

  ELISP> (tla--name-split-semi-qualified
            \"branch--category--version\" \"archive\")
  (\"archive\" \"branch\" \"category\" \"version\" nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category\" \"archive\")
  (\"archive\" \"branch\" \"category\" nil nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category\" nil)
  (nil \"branch\" \"category\" nil nil)

  ELISP> (tla--name-split-semi-qualified \"branch--category--\" nil)
  (nil \"branch\" \"category\" \"\" nil)"
  (let ((list (tla--name-split-semi-qualified-internal name)))
    (while (> 4 (length list))
      (setq list (cons nil list)))
    (let ((result (cons archive (nreverse list))))
      (when (tla--is-version-string (nth 2 result))
        (setq result (list (nth 0 result)
                           (nth 1 result)
                           ""
                           (nth 2 result)
                           (nth 3 result))))
      result)))

(defun tla--is-version-string (string)
  "Non-nil if STRING is a candidate for a version name.
That is, if it contains only digits and dots.
The regexp here is less strict than the one of tla, but must verify
\(tla--is-version-string string) => string can't be a branch name."
  (and string (string-match "^[0-9\.]+$" string)))

(defun tla--name-split-semi-qualified-internal (name)
  "Helper function for `tla--name-split-semi-qualified'.
Splits a semi-qualified NAME."
  (if (string-match "^\\(.+\\)--\\(\\([^-]\\|-[^-]\\)*\\)" name)
      (cons (match-string 2 name)
            (tla--name-split-semi-qualified-internal
             (match-string 1 name)))
    (cons name nil)))

(defun tla--name-split (name)
  "Parse a fully qualified revision NAME, but possibly incomplete.
email@address.com--arch/cat--branch--ver ->
  (\"email@address.com--arch\" \"cat\" \"branch\" \"ver\" nil)
email@address.com--arch/cat ->
  (\"email@address.com--arch\" \"cat\" nil nil nil)
email@address.com--arch ->
  (\"email@address.com--arch\" nil nil nil nil)"
  (if (string-match "\\(.*\\)/\\(.*\\)" name)
      (tla--name-split-semi-qualified (match-string 2 name) (match-string 1 name))
    (if (string= name "")
        (list nil nil nil nil nil)
      (list name nil nil nil nil))))


;;
;; List representation -> string
;;
(defun tla--name-construct-semi-qualified (&rest comp)
  "Concatenate COMP with \"--\".
This function can accept strings or a list which contains strings.

    ELISP> (tla--name-construct-semi-qualified \"a\" \"b\" \"c\")
    \"a--b--c\"
    ELISP> (tla--name-construct-semi-qualified (list \"a\" \"b\" \"c\"))
    \"a--b--c\""
  (if (consp (car comp)) (setq comp (car comp)))
  (if (string= (cadr comp) "")
      ;; Unnamed branch.
      (concat (car comp) "--"
              (mapconcat 'identity (remove nil (cddr comp)) "--"))
    (mapconcat 'identity (remove nil comp) "--")))

(defun tla--name-construct (archive &optional
                                    category
                                    branch
                                    version
                                    revision)
  "Create the revision name ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION.
The arguments may be nil. If ARCHIVE is a revision name list like
 (archive category branch version revision), the list element is mapped
to arguments before creating the fully qualified revision name.

If the branch name is the empty string and the version is defined,
then, we have an unnamed branch. The full name is
archive/category--version."
  (when (consp archive)
    (setq category (tla--name-category archive)
          branch   (tla--name-branch archive)
          version  (tla--name-version archive)
          revision (tla--name-revision archive)
          ;; archive must be last
          archive  (tla--name-archive archive)))
  (let ((semi (tla--name-construct-semi-qualified
               category branch version revision)))
    (concat
     (and archive (not (string= archive ""))
          (concat archive (when category "/")))
     semi)))

(defun tla-revision-id-to-list (rev-id)
  (dvc-trace "rev-id=%S" rev-id)
  (unless (or (eq (car rev-id) 'tla)
              (eq (car rev-id) 'baz))
    (error "%S is not a tla/baz revision ID." rev-id))
  (let* ((data (dvc-revision-get-data rev-id))
         (type (dvc-revision-get-type rev-id)))
    (dvc-trace "data=%S" data)
    (dvc-trace "type=%S" type)
    (case type
      (revision (car data))
      (previous-revision (tla-revision-direct-ancestor
                          (nth 1 (car data)) (nth 1 data)))
      (otherwise (error "TODO: type of revision not implemented: %S" type)))))

;;
;; Get a component from a list or string.
;;
(defun tla--name-archive (target)
  "Get archive component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (car target))

(defun tla--name-category (target)
  "Get category component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (cadr target))

(defun tla--name-branch (target)
  "Get branch component from a TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (car (cddr target)))

(defun tla--name-version (target)
  "Get version component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (cadr (cddr target)))

(defun tla--name-revision (target)
  "Get revision component from TARGET.
Both representation of TARGET, a string and a list is acceptable."
  (when (stringp target)
    (setq target (tla--name-split target)))
  (car (cddr (cddr target))))

;;
;; Utilities
;; Mask a specified component in the name.
;;
(defun tla--name-mask (original do-construct-p
                                &optional
                                archive-mask
                                category-mask
                                branch-mask
                                version-mask
                                revision-mask)
  "Mask ORIGINAL, a tla revision name by masks; and return the masked value.

If DO-CONSTRUCT-P is given, the result is converted to a string by
`tla--name-construct'.

ARCHIVE-MASK, CATEGORY-MASK, BRANCH-MASK, VERSION-MASK and REVISION-MASK should
be either nil or t, and indicate whether that field should be masked.

If a mask value is nil, the associated element in ORIGINAL is set to nil.
Else If a mask value is a string, the associated element in ORIGINAL is set
to the string.
Else the associated element in ORIGINAL is not changed.

Examples:
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") nil t t t t nil)
 (\"a\" \"c\" \"b\" \"v\" nil)

 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") nil t t t nil nil)
 (\"a\" \"c\" \"b\" nil nil)

 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t t t t nil nil)
 \"a/c--b\"
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t nil nil nil nil t)
 \"r\"
 ELISP> (tla--name-mask '(\"a\" \"c\" \"b\" \"v\" \"r\") t nil nil nil t t)
 \"v--r\"
 ELISP>"
  (when (stringp original)
    (setq original (tla--name-split original)))
  (when (consp original)
    (let ((masked (list
                   (if archive-mask
                       (if (stringp archive-mask)
                           archive-mask
                         (tla--name-archive original)))
                   (if category-mask
                       (if (stringp category-mask)
                           category-mask
                         (tla--name-category original)))
                   (if branch-mask
                       (if (stringp branch-mask)
                           branch-mask
                         (tla--name-branch original)))
                   (if version-mask
                       (if (stringp version-mask)
                           version-mask
                         (tla--name-version original)))
                   (if revision-mask
                       (if (stringp revision-mask)
                           revision-mask
                         (tla--name-revision original))))))
      (if do-construct-p
          (tla--name-construct masked)
        masked))))

(defun tla--name-match (target mask)
  "Compare the fully qualified revision list TARGET with a MASK.
Each parameter is a list.  The elements of the both lists are compared
via a regexp match.  When the mask part of a component is nil, this
comparision is skipped.
Here are some examples:
\(tla--name-match
 '(\"xsteve@nit.at--public\" \"xtla\" \"main\" \"0.1\" \"patch-116\")
 '(nil \"xt.*\" \"main\" nil nil)) => t
\(tla--name-match
 '(\"xsteve@nit.at--public\" \"xtla\" \"main\" \"0.1\" \"patch-116\")
 '(nil \"xt.*\" \"devel\" nil nil)) => nil" ;"
  (let ((tl target)
        (ml mask)
        (t-part)
        (m-part)
        (matching t))
    (while tl
      (setq t-part (car tl))
      (setq m-part (car ml))
      (when m-part
        (setq matching (string-match m-part t-part)))
      (if matching
          (progn
            (setq tl (cdr tl))
            (setq ml (cdr ml)))
        (setq tl nil)))
    (if matching t nil)))


(defun tla--name-match-from-list (target match-list)
  "Match TARGET against a list of possible matches.
Every entry of MATCH-LIST is a list that contains a
match element and a possible result.
The target is matched against the elements in the match-list.
If a match is found return the corresponding result,
otherwise return nil."
  (let ((ml match-list)
        (match)
        (data)
        (result))
    (while (and (not result) ml)
      (setq match (caar ml))
      (setq data (car (cdar ml)))
      ;;(message "match: %s, data: %s" match data)
      (setq result (when (tla--name-match target match) data))
      (setq ml (cdr ml)))
    result))

;; example:
;;(setq tla-apply-patch-mapping
;;      '(((nil "atla" nil  nil nil) "~/work/tlaaaa")
;;        ((nil "xtla" nil  nil nil) "~/work/tla/xtla")))
;;(tla--name-match-from-list
;; '("xsteve@nit.at--public" "xtla" "main" "0.1" "patch-116") tla-apply-patch-mapping)

;; TODO: Use tla--archive-tree.
(defun tla--version-head (archive category branch version)
  "Return the newest revision for ARCHIVE/CATEGORY--BRANCH--VERSION."
  (tla--run-tla-sync (list "revisions"
                           (tla--name-construct
                            archive
                            category
                            branch
                            version))
                     :finished (lambda (output error status arguments)
                                 (with-current-buffer output
                                   (goto-char (point-max))
                                   (re-search-backward "^.")
                                   (buffer-substring-no-properties
                                    (point) (line-end-position))))))

;; ----------------------------------------------------------------------------
;; Archive tree manipulators
;; ----------------------------------------------------------------------------
(defvar tla--archive-tree-archives-complete nil
  "Non-nil when the list of archives is built.

In tla--archive-tree, the list of archives is built by running \"baz
archives\", but some items can be added also while adding categories,
branches, ... In this case, this variable remains nil so that \"baz
archives\" is ran next time, to get the full list of archives.")

(defvar tla--archive-tree nil
  "Arch archive/category/branch/version/revision are stored in assoc list:

 ((\"xsteve@nit.at--public\" \"http://arch.xsteve.at/2004\")
 [...]
  (\"mbp@sourcefrog.net--2004\"
   \"http://sourcefrog.net/arch/mbp@sourcefrog.net--2004\"
   (\"xtla\")
   (\"tilly\")
 [...]
   (\"dupes\"
    (\"mainline\"
     (\"0.1\")))
 [...]
   (\"archzoom\"))
  (\"mark@dishevelled.net--2003-mst\"
   \"http://members.iinet.net.au/~mtriggs/arch/\")
  (\"lord@emf.net--2004\"
   \"http://regexps.srparish.net/{archives}/lord@emf.net--2004\")
 [...]
  (\"Matthieu.Moy@imag.fr--public\"
   \"http://www-verimag.imag.fr/webdav/moy/public\"
   (\"xtla\"
    (\"main\"
     (\"0.1\"
      (\"patch-228\"
       \"Merged from Robert (patch8-9), Milan (patch21-22), Stefan (patch5-8)\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-06-03 20:13:11 GMT\")
      (\"patch-227\"
       \"Fix default-directory in tla--run-tla-sync, fix in dvc-diff-ediff\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-06-03 15:26:15 GMT\")
 [...]
      (\"patch-1\"
       \"typo\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\"
       \"2004-04-07 22:57:00 GMT\")
      (\"base-0\"
       \"tag of xsteve@nit.at--public/xtla--main--0.1--patch-5\"
       \"Matthieu Moy <Matthieu.Moy@imag.fr>\" \"2004-04-07 22:52:39 GMT\")))))
 [...]
   )

This list is initially empty, and is built/rebuilt on demand.")

;; Utilities
(defun tla--archive-tree-setcdr (parent value &optional rest)
  "In PARENT, update VALUE.
REST are the items that are already present."
  (let* ((current (cdr parent))
         (list-details (assoc value current)))
    (if (or (null current) (null list-details))
        ;; rest is '("summary" "creator" "date") when value is "patch-N"
        (setcdr parent (cons (cons value rest) current))
      (if (and list-details rest)
          ;; Field already there. update details.
          (setcdr list-details rest)))))

(defun tla--archive-tree-setcddr (parent value)
  "In PARENT, update VALUE."
  (let ((current (cddr parent)))
    (if (or (null current) (null (assoc value current)))
        (setcdr (cdr parent) (cons (cons value nil) current)))))

;; Archive
(defun tla--archive-tree-add-archive (archive locations &optional old)
  "Add ARCHIVE  at LOCATIONS to the archive tree.
If OLD is provided, it is an old archive tree from which some
information can be found (this is useful to keep the category/branch/version
info for existing archives)."
  (if (tla--archive-tree-get-archive archive)
      (let* ((a (tla--archive-tree-get-archive archive))
             (val (cdr a))
             (oldlocation (car val)))
        (setcar (cdr a) (or locations oldlocation)))
    (let ((oldinfo (tla--archive-tree-get-archive archive old))
          (newinfo (list archive locations)))
      (when oldinfo
        (setcdr (cdr newinfo) (cddr oldinfo))) ;; list of versions.
      (setq tla--archive-tree (cons newinfo
                                    tla--archive-tree)))))

(defun tla--archive-tree-get-archive (archive &optional archive-tree)
  "Get the value of ARCHIVE from ARCHIVE-TREE.
If ARCHIVE-TREE is not given, `tla--archive-tree' is used."
  (assoc archive (or archive-tree tla--archive-tree)))

;; Category
(defun tla--archive-tree-add-category (archive category)
  "Add a new category to ARCHIVE named CATEGORY."
  (tla--archive-tree-add-archive archive nil)
  (tla--archive-tree-setcddr
   (tla--archive-tree-get-archive archive)
   category))

(defun tla--archive-tree-get-category (archive category)
  "From ARCHIVE, get CATEGORY."
  (assoc category (cdr (cdr (tla--archive-tree-get-archive archive)))))

;; Branch
(defun tla--archive-tree-add-branch (archive category branch)
  "Add a new branch to ARCHIVE's CATEGORY named BRANCH."
  (tla--archive-tree-add-category archive category)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-category archive category)
   branch))

(defun tla--archive-tree-get-branch (archive category branch)
  "Get a branch from ARCHIVE's CATEGORY named BRANCH."
  (assoc branch (cdr (tla--archive-tree-get-category
                      archive category))))

;; Version
(defun tla--archive-tree-add-version (archive category branch version)
  "Add a new version to ARCHIVE CATEGORY BRANCH named VERSION."
  (tla--archive-tree-add-branch archive category branch)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-branch archive category branch )
   version))

(defun tla--archive-tree-get-version (archive category branch version)
  "Get a version from ARCHIVE CATEGORY BRANCH named VERSION."
  (assoc version (cdr (tla--archive-tree-get-branch
                       archive category branch))))

;; Revision
(defun tla--archive-tree-add-revision (archive category branch version revision
                                               &optional rev-struct)
  "Add a new revision to ARCHIVE CATEGORY BRANCH VERSION named REVISION."
  (tla--archive-tree-add-version archive category branch version)
  (tla--archive-tree-setcdr
   (tla--archive-tree-get-version archive category branch version)
   revision rev-struct))

(defun tla--archive-tree-get-revision (archive category branch version revision)
  "Get a revision from ARCHIVE CATEGORY BRANCH VERSION named REVISION."
  (assoc revision (cdr (tla--archive-tree-get-version
                        archive category branch version))))

(defun tla--archive-tree-get-revision-struct (archive category branch version revision)
  "Get a revision from ARCHIVE CATEGORY BRANCH VERSION named REVISION.

Return a structure `tla--revision'."
  (or (cdr (assoc revision (cdr (tla--archive-tree-get-version
                                 archive category branch version))))
      (progn
        (tla--archive-tree-build-revisions
         archive category branch version t)
        (cdr (assoc revision (cdr (tla--archive-tree-get-version
                                   archive category branch version)))))))

;; Archive tree builders
(defun tla--archive-tree-build (basename &optional use-cache ignore-error)
  "Generic version of tla--archive-tree-build-*.
BASENAME is used as a base for this tree.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (stringp basename)
    (setq basename (tla--name-split basename)))
  (let ((archive (tla--name-archive basename))
        (category (tla--name-category basename))
        (branch (tla--name-branch basename))
        (version (tla--name-version basename)))
    (cond
     (version
      (tla--archive-tree-build-revisions archive
                                         category
                                         branch
                                         version
                                         use-cache
                                         ignore-error))
     (branch
      (tla--archive-tree-build-versions archive
                                        category
                                        branch
                                        use-cache
                                        ignore-error))
     (category
      (tla--archive-tree-build-branches archive
                                        category
                                        use-cache
                                        ignore-error))
     (archive
      (tla--archive-tree-build-categories archive
                                          use-cache
                                          ignore-error))
     (t
      (tla--archive-tree-build-archives use-cache
                                        ignore-error)))))

(defun tla--archive-tree-build-archives (&optional use-cache ignore-error)
  "Builds the list of archives.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (when (or (not use-cache)
            (not tla--archive-tree)
            (not tla--archive-tree-archives-complete))
    (tla--run-tla-sync `("archives" ,(when
                                         (tla-archives-has-all-locations-option)
                                       "--all-locations"))
                       :finished 'dvc-null-handler
                       :error
                       (if ignore-error
                           'dvc-null-handler
                         'dvc-default-error-function))
    (setq tla--archive-tree-archives-complete t)
    (let ((old-archive-tree tla--archive-tree))
      (setq tla--archive-tree nil)
      (save-excursion
        (let (archive-name)
          (set-buffer dvc-last-process-buffer)
          (goto-char (point-min))
          (while (> (line-end-position) (line-beginning-position))
            (setq archive-name (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            (forward-line 1)
            (let (archive-locations)
              (while (looking-at "^    \\(.*\\)$")
                (push (match-string 1) archive-locations)
                (forward-line 1))
              (tla--archive-tree-add-archive archive-name
                                             ;;
                                             ;; Make master archive becoming the
                                             ;; first of list of the list.
                                             ;;
                                             (reverse archive-locations)
                                             old-archive-tree))))))))

(defun tla--archive-tree-build-categories (archive &optional
                                                   use-cache
                                                   ignore-error)
  "Build the list of categories for ARCHIVE in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (tla--archive-tree-build-archives t ignore-error)
  (when (or (not use-cache)
            (not (cddr (tla--archive-tree-get-archive archive))))
    (let ((basename archive))
      (message "building categories for `%s'..." basename)
      (tla--run-tla-sync (list "categories" basename)
                         :finished 'dvc-null-handler
                         :error
                         (if ignore-error
                             'dvc-null-handler
                           'dvc-default-error-function))
      (message "building categories for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer dvc-last-process-buffer
      (let (category)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq category (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-category archive category)
          )))))

(defun tla--archive-tree-build-branches (archive category
                                                 &optional
                                                 use-cache
                                                 ignore-error)
  "Build the list of branches for ARCHIVE/CATEGORY in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (tla--archive-tree-build-categories archive t ignore-error)
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-category archive category))))
    (let ((basename (tla--name-construct archive category)))
      (message "building branches for `%s'..." basename)
      (tla--run-tla-sync (list "branches" basename)
                         :finished 'dvc-null-handler
                         :error
                         (if ignore-error
                             'dvc-null-handler
                           'dvc-default-error-function))
      (message "building branches for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer dvc-last-process-buffer
      (let (branch)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq branch (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
          (tla--archive-tree-add-branch
           archive
           category
           (if (looking-at ".*--")
               (tla--name-branch (tla--name-split-semi-qualified
                                  branch))
             ;; unnamed branch
             ""))
          (forward-line 1))))))

(defun tla--archive-tree-build-versions (archive category branch
                                                 &optional
                                                 use-cache
                                                 ignore-error)
  "Build the version list in ARCHIVE/CATEGORY--BRANCH in `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors."
  (tla--archive-tree-build-branches archive category t ignore-error)
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-branch archive category
                                                    branch))))
    (let ((basename (tla--name-construct archive category branch)))
      (message "building versions for `%s'..." basename)
      (tla--run-tla-sync (list "versions" basename)
                         :finished 'dvc-null-handler
                         :error
                         (if ignore-error
                             'dvc-null-handler
                           'dvc-default-error-function))
      (message "building versions for `%s'...done" basename)
      (sit-for 0)
      (message nil))
    (with-current-buffer dvc-last-process-buffer
      (let (version)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq version (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-version
           archive
           category
           branch
           (tla--name-version (tla--name-split-semi-qualified version))))))))

(defun tla--read-field (field)
  "Read the contents of FIELD from a log buffer.
Must be called from a log file buffer.  Returns the content of the
field FIELD.  FIELD is just the name of the field, without trailing
\": \""
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" field ": ") nil t)
        (buffer-substring-no-properties
         (point) (progn
                   (re-search-forward "^[^ \t]")
                   (- (point) 2))) ;; back to the end of the last line
      ;; of the field.
      "")))

(defun tla--read-field-str (field log-as-string)
  "Read the contents of FIELD from a log buffer.

Returns the content of the field FIELD, extracted from the log
LOG-AS-STRING. FIELD is just the name of the field, without trailing
\": \""
  (with-temp-buffer
    (insert log-as-string)
    (tla--read-field field)))

(defun tla--read-complete-log-string (&optional buffer)
  "Read the output of \"baz .. --complete-log\", starting at \"N chars\".

Return the log as a string."
  (with-current-buffer (or buffer (current-buffer))
    (dvc-funcall-if-exists set-buffer-multibyte nil)
    (let ((chars (string-to-number
                  (buffer-substring-no-properties
                   (point)
                   (search-forward " ")))))
      (forward-line 1)
      (let ((result (buffer-substring-no-properties
                     (point)
                     (progn (forward-char chars)
                            (point)))))
        result))))

(defun tla--skip-complete-log (&optional buffer)
  "Skip a log in the output of \"baz .. --complete-log\", starting at \"N chars\".

Same as `tla--read-complete-log-string', but don't return anything and
is faster."
  (with-current-buffer (or buffer (current-buffer))
    (dvc-funcall-if-exists set-buffer-multibyte nil)
    (let ((chars (string-to-number
                  (buffer-substring-no-properties
                   (point)
                   (search-forward " ")))))
      (forward-line 1)
      (forward-char chars))))

(defun tla--read-complete-log-struct (&optional buffer)
  "Read the output of \"baz .. --complete-log\", starting at \"N chars\".

Return the log as a string."
  (tla--parse-log-file (tla--read-complete-log-string buffer)))

(defun tla--parse-log-file (log-as-string)
  "Parses a log file and return a structure `tla--revision'."
  (let ((rev-struct (make-tla--revision))
        archive)
    (with-temp-buffer
      (insert log-as-string)
      (goto-char (point-min))
      (while (re-search-forward "^\\([A-Za-z0-9_-]*\\): ?" nil t)
        (let ((header (match-string-no-properties 1))
              (begin (point)))
          (forward-line 1)
          (while (looking-at "^[\t ]")
            (forward-line 1))
          (let ((value (buffer-substring-no-properties
                        begin (- (point) 1))))
            (cond ((string= header "Summary")
                   (setf (tla--revision-summary rev-struct)
                         value))
                  ((string= header "Creator")
                   (setf (tla--revision-creator rev-struct)
                         value))
                  ((string= header "Standard-date")
                   (setf (tla--revision-date rev-struct)
                         value))
                  ((string= header "New-patches")
                   (setf (tla--revision-merges rev-struct)
                         (split-string value)))
                  ((string= header "Revision")
                   (setf (tla--revision-revision rev-struct)
                         (tla--name-split-semi-qualified value)))
                  ((string= header "Archive")
                   (setq archive value))
                  ))))
      (forward-line 1)
      (setf (tla--revision-body rev-struct)
            (buffer-substring-no-properties (point)
                                            (point-max)))
      (setf (car (tla--revision-revision rev-struct))
            archive)
      (setf (tla--revision-merges rev-struct)
            (remove (tla--name-construct (tla--revision-revision rev-struct))
                    (tla--revision-merges rev-struct))))
    (setf (tla--revision-log rev-struct) log-as-string)
    rev-struct))

(defun tla--archive-tree-build-revisions (archive category branch version
                                                  &optional
                                                  use-cache
                                                  ignore-error
                                                  need-complete-info
                                                  callback)
  "Build the revision list in ARCHIVE/CATEGORY--BRANCH--VERSION.
Updates `tla--archive-tree'.
If USE-CACHE is non-nil, load details from the cache where possible.
If IGNORE-ERROR is non-nil, don't throw errors.

If CALLBACK is non-nil, run the process asynchronously and call
callback afterwards."
  (tla--archive-tree-build-versions archive category branch t ignore-error)
  (when (or (not use-cache)
            (not (cdr (tla--archive-tree-get-version archive category branch
                                                     version)))
            (and need-complete-info
                 (not (cdar (cdr (tla--archive-tree-get-version
                                  archive category branch version))))))
    (let ((details (or dvc-revisions-shows-summary
                       dvc-revisions-shows-date
                       dvc-revisions-shows-creator))
          (basename (tla--name-construct
                     archive category branch version)))
      (message "building revisions for `%s'..." basename)
      (funcall
       (if callback 'tla--run-tla-async 'tla--run-tla-sync)
       `("revisions"
         ,@(when details
             (if (tla-revisions-has-complete-log-option)
                 '("--complete-log")
               '("--summary" "--date" "--creator")))
         ,basename)
       :error (if ignore-error
                  'dvc-null-handler
                'dvc-default-error-function)
       :finished
       (dvc-capturing-lambda (output errors status arguments)
         (message "building revisions for `%s'...done" (capture basename))
         (sit-for 0)
         (message nil)
         (with-current-buffer output
           (let (revision date creator summary rev-struct)
             (goto-char (point-min))
             (while (> (line-end-position) (line-beginning-position))
               (setq revision (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
               (while (string-match ".*password: $" revision)
                 (forward-line 1)
                 (setq revision (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
               (forward-line 1)
               (if (capture details)
                   (if (tla-revisions-has-complete-log-option)
                       (setq rev-struct (tla--read-complete-log-struct))
                     (skip-chars-forward " ")
                     (setq date (buffer-substring-no-properties (point)
                                                                (line-end-position)))
                     (forward-line 1)
                     (skip-chars-forward " ")
                     (setq creator (buffer-substring-no-properties (point)
                                                                   (line-end-position)))
                     (forward-line 1)
                     (skip-chars-forward " ")
                     (setq summary (buffer-substring-no-properties
                                    (point)
                                    (progn (re-search-forward "^\\([^ \t]\\|$\\)")
                                           (previous-line 1)
                                           (end-of-line)
                                           (point))))
                     (forward-line 1)
                     (setq rev-struct (make-tla--revision
                                       :creator creator
                                       :summary summary
                                       :date date
                                       :revision
                                       (list
                                        (capture archive)
                                        (capture category)
                                        (capture branch)
                                        (capture version)
                                        revision))))
                 (setq rev-struct nil))
               (tla--archive-tree-add-revision
                (capture archive)
                (capture category)
                (capture branch)
                (capture version)
                revision
                rev-struct))))
         (when (capture callback) (funcall (capture callback))))))))


(defun tla--revisions-tree-contains-details
  (archive category branch version)
  "Whether VERSION has already been listed full details.
Details include summary lines, dates, and creator in the archive tree."
  (let ((vtree (tla--archive-tree-get-version archive category branch
                                              version)))
    (and (cdr vtree)            ;; revision list is here
         (cadr (cadr vtree))))) ;; summary line also

;; ----------------------------------------------------------------------------
;; Revlib tree manipulators
;; ----------------------------------------------------------------------------
(defvar tla--revlib-tree nil
  "Same as `tla--archive-tree', but for revision library.

Does not contain details for revisions, since they would be redundant
with the archive tree.")

(defun tla--revlib-tree-get-archive (archive &optional archive-tree)
  "Get ARCHIVE from ARCHIVE-TREE.
If ARCHIVE-TREE is not given, `tla--revlib-tree' is used instead."
  (assoc archive (or archive-tree tla--revlib-tree)))

(defun tla--revlib-tree-build-archives (&optional use-cache ignore-error)
  "Build the list of archives in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for archives are updated."
  (when (or (not use-cache)
            (not tla--revlib-tree))
    (tla--run-tla-sync '("library-archives")
                       :finished 'dvc-null-handler
                       :error
                       (if ignore-error
                           'dvc-null-handler
                         'dvc-default-error-function))
    (let ((old-revlib-tree tla--revlib-tree) )
      (setq tla--revlib-tree nil)
      (save-excursion
        (let ((archive-name)
              (tmp tla--archive-tree)
              (tla--archive-tree tla--revlib-tree)
              result)
          (set-buffer dvc-last-process-buffer)
          (goto-char (point-min))
          (while (> (line-end-position) (line-beginning-position))
            (setq result t)
            (setq archive-name (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            (forward-line 1)
            (tla--archive-tree-add-archive archive-name
                                           nil
                                           old-revlib-tree))
          (setq tla--revlib-tree tla--archive-tree
                tla--archive-tree tmp)
          result)))))

(defun tla--revlib-tree-get-category (archive category)
  "Get a category from ARCHIVE named CATEGORY."
  (assoc category (cdr (cdr (tla--revlib-tree-get-archive archive)))))

(defun tla--revlib-tree-build-categories (archive &optional
                                                  use-cache
                                                  ignore-error)
  "Builds the list of categories for an ARCHIVE in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for categories are updated."
  (when (or (not use-cache)
            (not (cddr (tla--revlib-tree-get-archive archive))))
    (tla--run-tla-sync (list "library-categories" archive)
                       :finished 'dvc-null-handler
                       :error
                       (if ignore-error
                           'dvc-null-handler
                         'dvc-default-error-function))
    (with-current-buffer dvc-last-process-buffer
      (let (category
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq category (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-category archive category))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-branch (archive category branch)
  "From ARCHIVE/CATEGORY, get BRANCH."
  (assoc branch (cdr (tla--revlib-tree-get-category
                      archive category))))

(defun tla--revlib-tree-build-branches (archive category
                                                &optional
                                                use-cache
                                                ignore-error)
  "Build the list of branches for ARCHIVE/CATEGORY in `tla--revlib-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for branches are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-category archive category))))
    (tla--run-tla-sync (list "library-branches"
                             (tla--name-construct archive category))
                       :finished 'dvc-null-handler
                       :error
                       (if ignore-error
                           'dvc-null-handler
                         'dvc-default-error-function))
    (with-current-buffer dvc-last-process-buffer
      (let (branch
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq branch (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-branch
           archive
           category
           (tla--name-branch (tla--name-split-semi-qualified branch))))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-version (archive category branch version)
  "Get ARCHIVE/CATEGORY--BRANCH--VERSION from the revlib tree."
  (assoc version (cdr (tla--revlib-tree-get-branch
                       archive category branch))))

(defun tla--revlib-tree-build-versions (archive category branch
                                                &optional
                                                use-cache
                                                ignore-error)
  "Build the versions list in ARCHIVE/CATEGORY/BRANCH in `tla--archive-tree'.
If USE-CACHE is non-nil, load from the cache where possible.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for versions are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-branch archive category
                                                   branch))))
    (tla--run-tla-sync (list "library-versions"
                             (tla--name-construct
                              archive category branch))
                       :finished 'dvc-null-handler
                       :error
                       (if ignore-error
                           'dvc-null-handler
                         'dvc-default-error-function))
    (with-current-buffer dvc-last-process-buffer
      (let (version
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq version (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-version
           archive
           category
           branch
           (tla--name-version (tla--name-split-semi-qualified version))))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result))))

(defun tla--revlib-tree-get-revision (archive category branch version revision)
  "Get ARCHIVE/CATEGORY--BRANCH--VERSION--REVISION from the revlib tree."
  (assoc revision (cdr (tla--revlib-tree-get-version
                        archive category branch version))))

(defun tla--revlib-tree-build-revisions (archive category branch version
                                                 &optional
                                                 use-cache
                                                 ignore-error)

  "Build the revision list of ARCHIVE/CATEGORY--BRANCH--VERSION.
Updates `tla--revlib-tree'.
If IGNORE-ERROR is non-nil, error is not reported.
Return non-nil if the tree entry for revisions are updated."
  (when (or (not use-cache)
            (not (cdr (tla--revlib-tree-get-version archive category branch
                                                    version))))
    (tla--run-tla-sync (list "library-revisions"
                             (tla--name-construct
                              archive category branch version))
                       :finished 'dvc-null-handler
                       :error (if ignore-error
                                  'dvc-null-handler
                                'dvc-default-error-function))
    (with-current-buffer dvc-last-process-buffer
      (let (revision
            (tmp tla--archive-tree)
            (tla--archive-tree tla--revlib-tree)
            result)
        (goto-char (point-min))
        (while (> (line-end-position) (line-beginning-position))
          (setq result t)
          (setq revision (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
          (forward-line 1)
          (tla--archive-tree-add-revision
           archive
           category
           branch
           version
           revision))
        (setq tla--revlib-tree tla--archive-tree
              tla--archive-tree tmp)
        result
        ))))

;; ----------------------------------------------------------------------------
;; Name reading engine
;; ----------------------------------------------------------------------------
;;Currently only able to read a full revision starting from nothing.
(defun tla-name-read-refresh-cache ()
  "Function to be called from the minibuffer while reading a name."
  (interactive)
  (tla--archive-tree-build
   (tla--name-construct
    (butlast (delete nil (tla--name-split (minibuffer-contents))))))
  (setq tla--archive-tree nil))

(defvar tla--name-read-arguments "This value should not be refereed."
  "Used to suppress warnings from the byte code compiler.
This variable is a just placeholder introduced to suppress the
warnings from byte code compiler.  Variable `tla--name-read-arguments'
should be bound in `let'.  Variable `tla--name-read-arguments' is used
for passing information from `tla-name-read' to functions called internally
from `tla-name-read'.  Use function `tla--name-read-arguments' to get the
information")

(defun tla--name-read-arguments (key)
  "Get `tla-name-read' context information associated to KEY.
`tla-name-read' calls some functions to read a tla name.
In the functions, the arguments passed to `tla-name-read'(context information)
are needed to know.  However, `tla-name-read' cannot pass the context
information directly to the functions because the functions are something to do
with Emacs's completion mechanism; and the mechanism specifies the number
of arguments of the functions.  So the context information is passed via
a local variable, `tla--name-read-arguments', defined in let.

Symbol `archive', `category', `branch', `version', or `revision' are
acceptable as KEY."
  (cdr (assoc key tla--name-read-arguments)))


(defun tla--name-read-complete (string predicate what)
  "Completion function for name reading.

Displays STRING and prompts for something satisfying PREDICATE.

This function uses the free variables archive, category, branch,
version, and revision.  If one of these variables is non-nil, it means
the corresponding value must be read from keyboard.

REMINDER: this function may be called several times, with different
values for WHAT:

 - nil : The function must return the longest prefix
 - t : The function must return the list of completions
 - 'lambda : The function must return t if the completion correspond
   to an exact match, nil otherwise.  (so that Emacs can distinguish
   between \"sole completion\" and \"complete, but not unique\"."
  (if (and (eq what 'lambda)
           (string-match "/\\(.*--\\)?$" string))
      ;; The caller just want to know whether this is a full
      ;; completion. This can not be the case with such suffix.
      nil
    (let* ((empty-branch nil)
           (use-cache (not current-prefix-arg))
           (splited (tla--name-split string))
           (archive-loc  (tla--name-archive  splited))
           (category-loc (tla--name-category splited))
           (branch-loc   (tla--name-branch   splited))
           (version-loc  (tla--name-version  splited))
           (revision-loc (tla--name-revision splited))
           (suffix (cond
                    ((and (tla--name-read-arguments 'category)
                          (not category-loc) "/"))
                    ((and (tla--name-read-arguments 'branch)
                          (not branch-loc)   "--"))
                    ((and (tla--name-read-arguments 'version)
                          (not version-loc)  "--"))
                    ((and (tla--name-read-arguments 'revision)
                          (not revision-loc) "--"))
                    (t nil)))
           (maybep (cond
                    ((eq 'maybe (tla--name-read-arguments 'category))
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'branch))
                          archive-loc category-loc)
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'version))
                          archive-loc category-loc branch-loc)
                     t)
                    ((and (eq 'maybe (tla--name-read-arguments 'revision))
                          archive-loc category-loc branch-loc version-loc)
                     t)
                    (t nil)))
           (completions
            (cond
             ;; If the user started to write a revision ...
             (revision-loc
              ;; ... and if the user is supposed to be prompted a
              ;; revision
              (when (tla--name-read-arguments 'revision)
                (let ((dvc-revisions-shows-summary nil)
                      (dvc-revisions-shows-date nil)
                      (dvc-revisions-shows-creator nil))
                  (tla--archive-tree-build-revisions
                   archive-loc category-loc branch-loc version-loc use-cache t))
                (cdr (tla--archive-tree-get-version
                      archive-loc category-loc branch-loc version-loc))))
             (version-loc
              (when (tla--name-read-arguments 'version)
                (tla--archive-tree-build-versions
                 archive-loc category-loc branch-loc use-cache t)
                (cdr (tla--archive-tree-get-branch
                      archive-loc category-loc branch-loc))))
             ;; If the user started a branch ...
             (branch-loc
              ;; And a branch is needed
              (when (tla--name-read-arguments 'branch)
                (tla--archive-tree-build-branches
                 archive-loc category-loc use-cache t)
                (let ((result (cdr (tla--archive-tree-get-category
                                    archive-loc category-loc))))
                  (when (and (string= branch-loc "")
                             (tla--name-read-arguments 'version)
                             (let ((empty-br-exists nil))
                               (dolist (branch
                                        (cdr (tla--archive-tree-get-category
                                              archive-loc category-loc)))
                                 (when (string= (car branch) "")
                                   (setq empty-br-exists t)))
                               empty-br-exists))
                    (tla--archive-tree-build-versions
                     archive-loc category-loc "")
                    (setq empty-branch (tla--archive-tree-get-branch
                                        archive-loc category-loc ""))
                    (when empty-branch
                      ;; Remove the "" branch to avoid the ----
                      ;; completion.
                      (let ((tmp result))
                        (setq result nil)
                        (while tmp
                          (when (not (string= (caar tmp) ""))
                            (setq result (cons (car tmp) result)))
                          (setq tmp (cdr tmp))))))
                  result)))
             (category-loc
              (when (tla--name-read-arguments 'category)
                (tla--archive-tree-build-categories archive-loc use-cache t)
                (cddr (tla--archive-tree-get-archive archive-loc))))
             (t
              (when (tla--name-read-arguments 'archive)
                (tla--archive-tree-build-archives use-cache t)
                tla--archive-tree)))))
      (let* ((base (mapcar (lambda (x)
                             (tla--name-construct
                              (delete
                               nil
                               (list
                                (when category-loc archive-loc)
                                (when branch-loc category-loc)
                                (when version-loc branch-loc)
                                (when revision-loc version-loc)
                                (car x)))))
                           completions))
             (sans-suffix
              (and maybep suffix))
             (empty-branch-versions
              (and empty-branch
                   (mapcar (lambda (x)
                             (tla--name-construct
                              archive-loc category-loc "" (car x)))
                           (cdr empty-branch))))
             (completions (funcall 'all-completions
                                   string
                                   (nconc (mapcar
                                           (lambda (x)
                                             (list (concat x suffix)))
                                           base)
                                          (when sans-suffix
                                            (mapcar
                                             (lambda (x) (list x))
                                             base))
                                          (when empty-branch
                                            (mapcar
                                             (lambda (x) (list x))
                                             empty-branch-versions)))
                                   predicate)))
        (let ((result
               (cond ((eq what t)
                      ;; We just want the list of completions
                      completions)
                     ((eq (length completions) 1)
                      ;; There's only one completion
                      (if (eq what 'lambda)
                          (string= (car completions) string)
                        (cond ((string= (car completions) string) t)
                              (t (car completions)))))
                     ;; there are several possible completions
                     (t (if (eq what 'lambda)
                            ;; complete, but not unique ?
                            (member string completions)
                          (try-completion string (mapcar 'list
                                                         completions)))))))
          ;; (dvc-trace "string=%s predicate=%S what=%s ==> result=%S\ncompletions=%S"
          ;;            string predicate what result completions)
          result)))))

(defconst tla-part-of-name-regex "\\([^/ \t\n-]\\|-[^-]\\)+")

;;;###autoload
(defun tla-make-name-regexp (level slash-mandatory exact)
  "Make a regexp for an Arch name (archive, category, ...).

LEVEL can be 0 (archive), 1 (category), 2 (branch), 3 (version)
or 4 (revision).

If SLASH-MANDATORY is non-nil, the '/' after the archive name is
mandatory. (allows to distinguish between Arch archives and emails.

If EXACT is non-nil, match exactly LEVEL."
  (let ((qmark (if exact "" "?")))
    (concat
     "\\([^/@ \t\n]+" "@" "[^/ \t\n]+" ;; email
     "\\(--"
     "[^/ \t\n]+\\)?" ;; suffix (not mandatory)
     (when (>= level 1)
       (concat
        "/\\("                 ;; Separator archive/category
        tla-part-of-name-regex ;; category
        (when (>= level 2)
          (concat
           "\\("
           "--"
           tla-part-of-name-regex ;; branch
           (when (>= level 3)
             (concat
              "\\("
              "--"
              "[0-9]+[.0-9]*" ;; version
              (when (>= level 4)
                (concat
                 "\\("
                 "--"
                 "\\(base\\|patch\\|version\\|versionfix\\)-[0-9]+" ;; patch
                 "\\)" qmark))
              "\\)" qmark))
           "\\)" qmark))
        "\\)" qmark))
     "\\)" ;; end of group
     (when (and slash-mandatory (< level 1))
       "/")
     "\\( \\|\n\\|:\\)")))

(defun tla-get-name-at-point ()
  "Provides a default value for tla-name-read.
It first looks, if a name is found near point.
If this does not succeed, use the revision at point, when in tla-changelog-mode."
  (interactive)
  (let ((name))
    (save-excursion
      (if (re-search-backward "[ \t\n]" (point-min) t)
          (goto-char (1+ (point)))
        (beginning-of-line))
      (when (looking-at (tla-make-name-regexp 4 nil nil))
        (setq name (match-string 1))))
    (unless name
      (when (eq major-mode 'tla-changelog-mode)
        (setq name (tla-changelog-revision-at-point))))
    name))

;; Test cases
;; (tla-name-read "enter category: " "Matthieu.Moy@imag.fr--public" 'prompt)
;; (tla-name-read "branch: " "lord@emf.net--2004" 'prompt 'prompt)
;; (tla-name-read "revision: " 'prompt 'prompt 'prompt 'prompt 'prompt)
;; (tla-name-read "revision or version: " 'prompt 'prompt 'prompt 'prompt 'maybe)
;; (tla-name-read "revision or version: " "jet@gyve.org--xtla" "xtla" "jet" 'prompt 'maybe)
;;
(defvar tla--name-read-history nil)     ; TODO: multiple history list?
(defvar tla--name-read-debug nil
  "If non-nil, `condition-case' in `tla-name-read' is made disabled.")
(defun tla-name-read (&optional prompt archive category
                                branch version revision)
  "Read a name.
To get help on the user interface of `tla-name-read', please type
M-x tla-name-read-help RET.

Function reading an archive location from keyboard.
Read name is expressed in a list built by `tla--name-split'.

First argument PROMPT is the prompt the user will get. Next arguments
ARCHIVE CATEGORY BRANCH VERSION and REVISION are either the default
value, or a request for a value. They can take four values:

 - A string means the default value, and will be used as an initial
   input.

 - The symbol 'prompt means the value will be prompted from the user.
   The user will HAVE to give this value.

 - The symbol 'maybe means the value will be prompted, but is optional
   for the user.

 - nil means the value won't be prompted.

They should appear in the same order as above.

Example:
- Read a category in archive \"Matthieu.Moy@imag.fr--public\":
 (tla-name-read \"enter category: \" \"Matthieu.Moy@imag.fr--public\" 'prompt)
- Read a revision, anywhere:
 (tla-name-read \"revision: \" 'prompt 'prompt 'prompt 'prompt 'prompt)
- Read either a revision or a version:
 (tla-name-read \"revision: \" 'prompt 'prompt 'prompt 'prompt 'maybe)

While prompting, a menu \"Xtla\" is added to the menubar. The
following commands are available:

\\{tla--name-read-minibuf-map}"

  ;; use the defaults found under point if no defaults have been provided
  (let ((l (tla-get-name-at-point)))
    (when l
      (setq l (tla--name-split l))
      (if (and archive  (symbolp archive))  (setq archive  (or (nth 0 l) archive)))
      (if (and category (symbolp category)) (setq category (or (nth 1 l) category)))
      (if (and branch   (symbolp branch))   (setq branch   (or (nth 2 l) branch)))
      (if (and version  (symbolp version))  (setq version  (or (nth 3 l) version)))
      (if (and revision (symbolp revision)) (setq revision (or (nth 4 l) revision)))))

  (let ((tla--name-read-arguments `((archive  . ,archive)
                                    (category . ,category)
                                    (branch   . ,branch)
                                    (version  . ,version)
                                    (revision . ,revision))))
    (if tla--name-read-debug
        (tla--name-read-internal prompt archive category branch version revision)
      (condition-case reason
          (tla--name-read-internal prompt archive category branch version revision)
        ((quit error)
         (run-hooks 'tla-name-read-error-hook)
         (signal (car reason) (cdr reason)))))))

(defun tla--name-read-internal (prompt archive category branch version revision)
  "See `tla-name-read'."
  (run-hooks 'tla-name-read-init-hook)

  (let* ((minibuffer-local-completion-map tla--name-read-minibuf-map)
         (result (tla--name-construct
                  (delete
                   'maybe
                   (delete 'prompt (list archive category
                                         branch version revision)))))
         (first-try t)
         not-finished too-long last-empty)
    ;; Without in some case 'maybe is ignored by tla--prompt-not-finished
    ;; and never the control flow enters the while loop.
    ;; We need C language's do-while loop.
    (while (or first-try
               not-finished
               too-long
               last-empty)
      (unless first-try
        (unless (eq this-command 'choose-completion)
          (ding)
          (message (cond (not-finished "%s%s [incomplete input: %s]")
                         (too-long "%s%s [too long input for: %s]")
                         (last-empty (concat "%s%s [empty " last-empty
                                             " name]"))
                         (t (error
                             (concat "case not managed."
                                     " Please submit a bug report"))))
                   prompt result
                   (tla--name-read-required-input archive
                                                  category
                                                  branch
                                                  version
                                                  revision))
          (sit-for 2)
          (message nil)))

      (setq result (dvc-completing-read
                    (or prompt "Location: ")
                    'tla--name-read-complete
                    nil nil result
                    'tla--name-read-history)
            first-try nil)
      (setq not-finished (tla--prompt-not-finished
                          result archive category branch
                          version revision))
      (setq too-long (tla--prompt-too-long
                      result archive category branch
                      version revision))
      (setq last-empty (tla--prompt-last-empty result)))

    (when result
      (setq result (tla--name-split result)))
    (run-hook-with-args 'tla-name-read-final-hook result)
    result))

(defun tla--prompt-not-finished (result archive category branch
                                        version revision)
  "Check whether user input is complete.
True if RESULT (a string) is not sufficient when the user is
prompted for ARCHIVE CATEGORY BRANCH VERSION REVISION."
  (let ((res-split (tla--name-split result)))
    (or (and (eq archive 'prompt)                 ;; archive required
             (not (tla--name-archive res-split))) ;; but not provided
        (and (eq category 'prompt)
             (not (tla--name-category res-split)))
        (and (eq branch 'prompt)
             (not (tla--name-branch res-split)))
        (and (eq version 'prompt)
             (not (tla--name-version res-split)))
        (and (eq revision 'prompt)
             (not (tla--name-revision res-split))))))

(defun tla--prompt-too-long (result archive category branch
                                    version revision)
  "Check whether the user has entered too many elements.
True if RESULT (a string) contains too many elements when the user
is prompted for ARCHIVE CATEGORY BRANCH VERSION REVISION.

For example, will return true if the user entered
foo@bar--2004/xtla--main while prompted only for a category."
  (let ((res-split (tla--name-split result)))
    (or (and (not revision)                  ;; revision not needed
             (tla--name-revision res-split)) ;; but provided
        (and (not version)
             (tla--name-version res-split))
        (and (not branch)
             (tla--name-branch res-split))
        (and (not category)
             (tla--name-category res-split))
        (and (not archive)
             (tla--name-archive res-split)))))

(defun tla--prompt-last-empty (result)
  "Check whether the last field is empty.
Non-nil if RESULT (a string) is terminated by \"--\" or \"/\". This
means the user entered a delimiter but not the element after.

When non-nil, the returned value is a string giving the name of the
item that is currently empty. (eg: archive, category, ...)"
  (let ((res-split (tla--name-split result)))
    (cond ((equal (tla--name-archive  res-split) "") "archive" )
          ((equal (tla--name-category res-split) "") "category")
          ((and (equal (tla--name-branch res-split) "")
                (not (tla--name-version res-split))) "branch"  )
          ((equal (tla--name-version  res-split) "") "version" )
          ((equal (tla--name-revision res-split) "") "revision")
          (t nil))))


(defun tla--name-read-required-input (archive
                                      category
                                      branch
                                      version
                                      revision)
  "Return string which represents the elements to be readin `tla-name-read'.
If ARCHIVE, CATEGORY, BRANCH, VERSION or REVISION are equal to 'maybe, the
corresponding element will be optionally read.
If any of these are non-nil (but not 'maybe), the corresponding element will be
required.
If any of these are nil, the correpsonding element is not required."
  (concat
   (cond ((eq archive 'maybe) "[A]")
         (archive "A")
         (t ""))
   (cond ((eq category 'maybe) "[/C]")
         (category "/C")
         (t ""))
   (cond ((eq branch 'maybe) "[--B]")
         (branch "--B")
         (t ""))
   (cond ((eq version 'maybe) "[--V]")
         (version "--V")
         (t ""))
   (cond ((eq revision 'maybe) "[--R]")
         (revision "--R")
         (t ""))))



(defun tla--location-type (location)
  "Return the type of LOCATION."
  (cond
   ((string-match "^ftp://" location) 'ftp)
   ((string-match "^sftp://" location) 'sftp)
   ((string-match "^http://" location) 'http)
   (t 'local)))

(defun tla--archive-type (archive)
  "Return the type of ARCHIVE."
  (cond
   ((string-match "SOURCE$" archive) 'source)
   ;; archive-MIRROR, archive-MIRROR-2 should be treated as mirror
   ((string-match ".+-MIRROR" archive) 'mirror)
   (t 'normal)))

;; (tla--archive-name-source "a")
;; (tla--archive-name-source "a-SOURCE")
;; (tla--archive-name-source "a-MIRROR")
(defun tla--archive-name-source (archive &optional existence-check)
  "Make source archive name from ARCHIVE.
If EXISTENCE-CHECK is non-nil, check whether the made source archive name
already exists or not; return nil if it doesn't exists.
Example:
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla\")
\"jet@gyve.org--xtla-SOURCE\"
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla-MIRROR\")
\"jet@gyve.org--xtla\"
ELISP> (tla--archive-name-source \"jet@gyve.org--xtla-SOURCE\")
nil"
  (let* ((type (tla--archive-type archive))
         (source (cond
                  ((eq 'normal type)
                   (concat archive "-SOURCE"))
                  ((eq 'mirror type)
                   (string-match "\\(.*\\)-MIRROR$" archive)
                   (match-string 1 archive))
                  (t nil))))
    (if existence-check
        (progn
          (tla--archive-tree-build-archives t)
          (when (and source (tla--archive-tree-get-archive source))
            source))
      source)))

;; (tla--archive-name-mirror "a")
;; (tla--archive-name-mirror "a-SOURCE")
;; (tla--archive-name-mirror "a-MIRROR")
(defun tla--archive-name-mirror (archive &optional existence-check)
  "Make mirror archive name from ARCHIVE.
If EXISTENCE-CHECK is non-nil, check whether the made mirror archive name
already exists or not; return nil if it doesn't exists.
Example:
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla\")
\"jet@gyve.org--xtla-MIRROR\"
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla-SOURCE\")
\"jet@gyve.org--xtla\"
ELISP> (tla--archive-name-mirror \"jet@gyve.org--xtla-MIRROR\")
nil"
  (let* ((type (tla--archive-type archive))
         (mirror (cond
                  ((eq 'normal type)
                   (concat archive "-MIRROR"))
                  ((eq 'source type)
                   (string-match "\\(.*\\)-SOURCE" archive)
                   (match-string 1 archive))
                  (t nil))))
    (if existence-check
        (progn
          (tla--archive-tree-build-archives t)
          (when (and mirror (tla--archive-tree-get-archive mirror))
            mirror))
      mirror)))

(defun tla-revision-direct-ancestor (&optional revision num)
  "Compute the direct ancestor of REVISION.
REVISION must be provided as a list, and a list is returned.
If revision is nil, return the ancestor of the last revision
of the local tree."
  (interactive
   (list (tla-name-read "Compute direct ancestor of: "
                        'prompt 'prompt 'prompt 'prompt 'prompt)))
  (let ((ancestor
         (tla--run-tla-sync (list "ancestry-graph" "--immediate"
                                  (and revision
                                       (tla--name-construct revision)))
                            :finished (lambda (output error status arguments)
                                        (tla--name-split
                                         (dvc-buffer-content
                                          output))))))
    (when (interactive-p)
      (message "Ancestor of: %s\n         is: %s"
               (tla--name-construct ancestor)
               (tla--name-construct revision)))
    (if (or (eq num 1) (eq num nil)) ancestor
      (tla-revision-direct-ancestor ancestor (- num 1)))))

;; Copied from ediff-mouse-event-p. I prefer keeping this duplication
;; to avoid one more dependancy on ediff.el (whose interface may
;; change one day ...)
(defsubst tla--mouse-event-p (event)
  "Return true if EVENT is a mouse-related event."
  (if (featurep 'xemacs)
      (dvc-do-in-xemacs (button-event-p event))
    (dvc-do-in-gnu-emacs
      (string-match "mouse" (format "%S" (event-basic-type event))))))

(defun tla-escape (string &optional unescape message)
  "Return the pika escaped value of STRING.
If pika escaping is not supported by tla, return STRING.
If UNESCAPE is non-nil, returns the unescaped version of string.
If MESSAGE is non-nil or if run interactively, also display the value
as a message."
  (interactive "sString to escape: ")
  (let ((res (if (and (string-match (if unescape "\\\\"
                                      "[^a-zA-Z._+,{}-]") string)
                      (tla-has-escape-command))
                 ;; We need to do the (un)escaping
                 (tla--run-tla-sync
                  (list "escape" (when unescape "--unescaped") string)
                  :finished (lambda (output error status arguments)
                              (dvc-buffer-content output)))
               string)))
    (when (or (interactive-p) message)
      (message res))
    res))

(defun tla-unescape (string)
  "Run \"tla escape --unescaped\" on STRING.

Return STRING if \"tla escape\" is not available."
  (interactive "sString to unescape: ")
  (when string (tla-escape string t (interactive-p))))

;; ----------------------------------------------------------------------------
;; Saving and loading state variables
;; ----------------------------------------------------------------------------


;; (setq tla--archive-tree nil)
;; (setq tla--revlib-tree nil)
(provide 'tla-core)
;;; tla-core.el ends here

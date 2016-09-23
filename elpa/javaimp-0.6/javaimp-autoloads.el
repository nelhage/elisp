;;; javaimp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "javaimp" "javaimp.el" (22418 23239 0 0))
;;; Generated autoloads from javaimp.el

(autoload 'javaimp-maven-visit-project "javaimp" "\
Loads a project and its submodules.  PATH should point to a
directory containing pom.xml.

Calls `mvn help:effective-pom' on the pom.xml in the PATH, reads
project structure from the output and records which files belong
to which modules and other module information.

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' maybe called inside any
module file.

\(fn PATH)" t nil)

(autoload 'javaimp-add-import "javaimp" "\
Imports classname in the current file.  Interactively,
asks for a class to import, adds import statement and calls
`javaimp-organize-imports'.  Import statements are not
duplicated.  Completion alternatives are constructed based on
this module's dependencies' classes, JDK classes and top-level
classes in the current module.

\(fn CLASSNAME)" t nil)

(autoload 'javaimp-organize-imports "javaimp" "\
Groups import statements according to the value of
`javaimp-import-group-alist' (which see) and prints resulting
groups leaving one blank line between groups.

If the file already contains some import statements, this command
rewrites them, starting with the same place.  Else, if the the
file contains package directive, this command inserts one blank
line below and then imports.  Otherwise, imports are inserted at
the beginning of buffer.

Classes within a single group are ordered in a lexicographic
order.  Imports not matched by any regexp in `javaimp-import-group-alist'
are assigned a default order defined by
`javaimp-import-default-order'.

NEW-IMPORTS is a list of additional imports; each element should
be of the form (CLASS . TYPE), where CLASS is a string and TYPE
is `'ordinary' or `'static'.  Interactively, NEW-IMPORTS is nil.

\(fn &rest NEW-IMPORTS)" t nil)

;;;***

;;;### (autoloads nil nil ("javaimp-pkg.el" "javaimp-tests.el") (22418
;;;;;;  23239 595192 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; javaimp-autoloads.el ends here

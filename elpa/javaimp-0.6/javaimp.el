;;; javaimp.el --- Add and reorder Java import statements in Maven projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015, 2016  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.6
;; Keywords: java, maven, programming

;;; Commentary:

;; Allows to manage Java import statements in Maven projects.
;;
;;   Quick start:
;;
;; - customize `javaimp-import-group-alist'
;; - call `javaimp-maven-visit-project', giving it the top-level project
;; directory where pom.xml resides
;;
;; Then in a Java buffer visiting a file under that project or one of its
;; submodules call `javaimp-organize-imports' or `javaimp-add-import'.
;;
;; This module does not add all needed imports automatically!  It only helps
;; you to quickly add imports when stepping through compilation errors.
;;
;;   Some details:
;;
;; If Maven failed, you can see its output in the buffer named by
;; `javaimp-debug-buf-name' (default is "*javaimp-debug*").
;;
;; Contents of jar files and Maven project structures (pom.xml) are cached,
;; so usually only the first command should take a considerable amount of
;; time to complete.  If a module's pom.xml or any of its parents' pom.xml
;; (within visited tree) was modified after information was loaded, `mvn
;; dependency:build-classpath' is re-run on the current module.  If a jar
;; file was changed, its contents are re-read.
;;
;; Currently inner classes are filtered out from completion alternatives.
;; You can always import top-level class and use qualified name.
;;
;;
;;   Example of initialization:
;;
;; (require 'javaimp)
;;
;; (add-to-list 'javaimp-import-group-alist
;;   '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
;;
;; (setq javaimp-additional-source-dirs '("generated-sources/thrift"))
;;
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ci" 'javaimp-add-import)
;; 	    (local-set-key "\C-co" 'javaimp-organize-imports)))
;;
;;
;; TODO:
;;
;; - use functions `cygwin-convert-file-name-from-windows' and
;; `cygwin-convert-file-name-to-windows' when they are available instead of
;; calling `cygpath'.  See https://cygwin.com/ml/cygwin/2013-03/msg00228.html
;;
;; - save/restore state, on restore check if a root exists and delete it if
;; not
;;
;; - `javaimp-add-import': without prefix arg narrow alternatives by local name;
;; with prefix arg include all classes in alternatives
;;
;; - :type for defcustom

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'xml)


;; User options

(defgroup javaimp ()
  "Add and reorder Java import statements in Maven projects"
  :group 'c)

(defcustom javaimp-import-group-alist '(("\\`javax?\\." . 10))
  "Specifies how to group classes and how to order resulting
groups in the imports list.

Each element should be of the form `(CLASSNAME-REGEXP . ORDER)'
where `CLASSNAME-REGEXP' is a regexp matching the fully qualified
class name.  Lowest-order groups are placed earlier.

The order of classes which were not matched is defined by
`javaimp-import-default-order'.")

(defcustom javaimp-import-default-order 50
  "Defines the order of classes which were not matched by
`javaimp-import-group-alist'")

(defcustom javaimp-java-home (getenv "JAVA_HOME")
  "Path to the JDK.  Directory jre/lib underneath this path is
searched for JDK libraries.  By default, it is initialized from
the JAVA_HOME environment variable.")

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory is a relative path from ${project.build.directory} project
property value.

Typically you would check documentation for a Maven plugin, look
at the parameter's default value there and add it to this list.

E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence
of the leading slash.

Custom values set in plugin configuration in pom.xml are not
supported yet.")

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  Customize it if the program is not
on `exec-path'.")

(defcustom javaimp-cygpath-program
  (if (eq system-type 'cygwin) "cygpath")
  "Path to the `cygpath' program (Cygwin only).  Customize it if
the program is not on `exec-path'.")

(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program used to read contents of jar files.
Customize it if the program is not on `exec-path'.")

(defcustom javaimp-include-current-module-classes t
  "If non-nil, current module's classes are included into
completion alternatives.  `javaimp-add-import' will find all java
files in the current project and add their fully-qualified names
to the completion alternatives list.")


;; Variables and constants

(defvar javaimp-project-forest nil
  "Visited projects")

(defvar javaimp-cached-jars nil
  "Alist of cached jars.  Each element is of the form (FILE
  . CACHED-JAR).")

(defconst javaimp-debug-buf-name "*javaimp-debug*")

;; Structs

(cl-defstruct javaimp-node
  parent children contents)

(cl-defstruct javaimp-module
  id parent-id
  file
  final-name
  packaging
  source-dir test-source-dir build-dir
  modules
  dep-jars
  load-ts)

(cl-defstruct javaimp-id
  group artifact version)

(cl-defstruct javaimp-cached-jar
  file read-ts classes)


;; Utilities

(defun javaimp--xml-children (xml-tree child-name)
  "Returns list of children of XML-TREE filtered by CHILD-NAME"
  (seq-filter (lambda (child)
		(and (consp child)
		     (eq (car child) child-name)))
	      (cddr xml-tree)))

(defun javaimp--xml-child (name el)
  "Returns a child of EL named by symbol NAME"
  (assq name (cddr el)))

(defun javaimp--xml-first-child (el)
  "Returns a first child of EL"
  (car (cddr el)))

(defun javaimp--get-file-ts (file)
  (nth 5 (file-attributes file)))

(defun javaimp--get-jdk-jars ()
  (and javaimp-java-home
       (file-accessible-directory-p javaimp-java-home)
       (let ((lib-dir
	      (concat (file-name-as-directory javaimp-java-home)
		      (file-name-as-directory "jre")
		      (file-name-as-directory "lib"))))
	 (directory-files lib-dir t "\\.jar\\'"))))

(defun javaimp-cygpath-convert-maybe (path &optional mode is-really-path)
  "On Cygwin, converts PATH using cygpath according to MODE and
IS-REALLY-PATH.  If MODE is `unix' (the default), adds -u switch.
If MODE is `windows', adds -m switch.  If `is-really-path' is
non-nil, adds `-p' switch.  On other systems, PATH is returned
unchanged."
  (if (eq system-type 'cygwin)
      (progn
	(unless mode (setq mode 'unix))
	(let (args)
	  (push (cond ((eq mode 'unix) "-u")
		      ((eq mode 'windows) "-m")
		      (t (error "Invalid mode: %s" mode)))
		args)
	  (and is-really-path (push "-p" args))
	  (push path args)
	  (car (apply #'process-lines javaimp-cygpath-program args))))
    path))


;; Project loading

;;;###autoload
(defun javaimp-maven-visit-project (path)
  "Loads a project and its submodules.  PATH should point to a
directory containing pom.xml.

Calls `mvn help:effective-pom' on the pom.xml in the PATH, reads
project structure from the output and records which files belong
to which modules and other module information.

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' maybe called inside any
module file."
  (interactive "DVisit maven project in directory: ")
  (let ((file (expand-file-name
	       (concat (file-name-as-directory path) "pom.xml"))))
    (unless (file-readable-p file)
      (error "Cannot read file: %s" file))
    ;; delete previous loaded tree, if any
    (setq javaimp-project-forest
	  (seq-remove (lambda (tree)
			(equal (javaimp-module-file (javaimp-node-contents tree))
			       file))
		      javaimp-project-forest))
    (message "Loading file %s..." file)
    (let* ((xml-tree
	    (javaimp--maven-call file "help:effective-pom"
				 #'javaimp--maven-xml-effective-pom-handler))
	   (projects (javaimp--maven-xml-extract-projects xml-tree))
	   (modules (mapcar #'javaimp--maven-xml-parse-project projects))
	   ;; first module is always root
	   (tree (javaimp--maven-build-tree (car modules) nil modules file)))
      (if tree
	  (push tree javaimp-project-forest)))
    (message "Loaded tree for %s" file)))


;; Maven XML routines

(defun javaimp--maven-xml-effective-pom-handler ()
  (let ((start
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\?xml\\|<projects?")
	     (match-beginning 0))))
	(end
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\(projects?\\)")
	     ;; corresponding close tag is the end of parse region
	     (search-forward (concat "</" (match-string 1) ">"))
	     (match-end 0)))))
    (xml-parse-region start end)))

(defun javaimp--maven-xml-extract-projects (xml-tree)
  "Analyzes result of `mvn help:effective-pom' and returns list
of <project> elements"
  (let ((project (assq 'project xml-tree))
	(projects (assq 'projects xml-tree)))
    (cond (project
	   (list project))
	  (projects
	   (javaimp--xml-children projects 'project))
	  (t
	   (error "Neither <project> nor <projects> was found in pom")))))

(defun javaimp--maven-xml-parse-project (project)
  (let ((build-elt (javaimp--xml-child 'build project)))
    (make-javaimp-module
     :id (javaimp--maven-xml-extract-id project)
     :parent-id (javaimp--maven-xml-extract-id (javaimp--xml-child 'parent project))
     ;; <project> element does not contain pom file path (we set :file slot later)
     :file nil
     :final-name (javaimp--xml-first-child
		  (javaimp--xml-child 'finalName build-elt))
     :packaging (javaimp--xml-first-child
		 (javaimp--xml-child 'packaging project))
     :source-dir (file-name-as-directory
		  (javaimp-cygpath-convert-maybe
		   (javaimp--xml-first-child
		    (javaimp--xml-child 'sourceDirectory build-elt))))
     :test-source-dir (file-name-as-directory
		       (javaimp-cygpath-convert-maybe
			(javaimp--xml-first-child
			 (javaimp--xml-child 'testSourceDirectory build-elt))))
     :build-dir (file-name-as-directory
		 (javaimp-cygpath-convert-maybe
		  (javaimp--xml-first-child (javaimp--xml-child 'directory build-elt))))
     :modules (mapcar (lambda (module-elt)
			(javaimp--xml-first-child module-elt))
		      (javaimp--xml-children (javaimp--xml-child 'modules project) 'module))
     :dep-jars nil		      ; dep-jars is initialized lazily on demand
     :load-ts (current-time))))

(defun javaimp--maven-xml-extract-id (elt)
  (make-javaimp-id
   :group (javaimp--xml-first-child (javaimp--xml-child 'groupId elt))
   :artifact (javaimp--xml-first-child (javaimp--xml-child 'artifactId elt))
   :version (javaimp--xml-first-child (javaimp--xml-child 'version elt))))

(defun javaimp--maven-xml-file-matches (file id parent-id)
  (let* ((xml-tree (with-temp-buffer
		     (insert-file-contents file)
		     (xml-parse-region (point-min) (point-max))))
	 (project-elt (assq 'project xml-tree))
	 (tested-id (javaimp--maven-xml-extract-id project-elt))
	 (tested-parent-id (javaimp--maven-xml-extract-id (assq 'parent project-elt))))
    ;; seems that the only mandatory component in tested ids is artifact, while
    ;; group and version may be inherited and thus not presented in pom.xml
    (let ((test (if (or (null (javaimp-id-group tested-id))
			(null (javaimp-id-version tested-id))
			(null (javaimp-id-group tested-parent-id))
			(null (javaimp-id-version tested-parent-id)))
		    (progn
		      (message "File %s contains incomplete id, using lax match" file)
		      (lambda (first second)
			(equal (javaimp-id-artifact first) (javaimp-id-artifact second))))
		  #'equal)))
      (and (funcall test tested-id id)
	   (funcall test tested-parent-id parent-id)))))


;; Maven routines

(defun javaimp--maven-call (pom-file target handler)
  "Runs Maven target TARGET on POM-FILE, then calls HANDLER in
the temporary buffer and returns its result"
  (message "Calling \"mvn %s\" on pom: %s" target pom-file)
  (with-temp-buffer
    (let* ((pom-file (javaimp-cygpath-convert-maybe pom-file))
	   (status
	    ;; TODO check  in Maven output on Gnu/Linux
	    (let ((coding-system-for-read
		   (if (eq system-type 'cygwin) 'utf-8-dos)))
	      (process-file javaimp-mvn-program nil t nil "-f" pom-file target)))
	   (buf (current-buffer)))
      (with-current-buffer (get-buffer-create javaimp-debug-buf-name)
	(erase-buffer)
	(insert-buffer-substring buf))
      (or (and (numberp status) (= status 0))
	  (error "Maven target \"%s\" failed with status \"%s\"" target status))
      (goto-char (point-min))
      (funcall handler))))

(defun javaimp--maven-build-tree (this parent-node all file)
  (message "Building tree for module: %s" (javaimp-module-id this))
  (let ((children
	 ;; reliable way to find children is to look for modules with "this" as
	 ;; the parent
	 (seq-filter (lambda (m) (equal (javaimp-module-parent-id m)
					(javaimp-module-id this)))
			      all)))
    (if (and (null children)
	     (equal (javaimp-module-packaging this) "pom"))
	(progn (message "Skipping empty aggregate module: %s" (javaimp-module-id this))
	       nil)
      ;; filepath was not set before, but now we know it
      (setf (javaimp-module-file this) file)
      ;; node
      (let* ((this-node (make-javaimp-node
			:parent parent-node
			:children nil
			:contents this))
	     ;; recursively build child nodes
	     (child-nodes
	      (mapcar (lambda (child)
			(let ((child-file
			       ;; !! this is hack
			       (javaimp--maven-get-submodule-file
				child file (javaimp-module-modules this))))
			  (javaimp--maven-build-tree
			   child this-node all child-file)))
		      children)))
	(setf (javaimp-node-children this-node) child-nodes)
	this-node))))

(defun javaimp--maven-get-submodule-file (submodule parent-file rel-paths-from-parent)
  ;; Seems that the only reliable way to match a module parsed from <project>
  ;; element with module relative path taken from <modules> is to visit pom and
  ;; check that id and parent-id matches
  (let* ((parent-dir (file-name-directory parent-file))
	 (files (mapcar (lambda (rel-path)
			  (concat parent-dir
				  (file-name-as-directory rel-path)
				  "pom.xml"))
			rel-paths-from-parent)))
    (or (seq-find
	 (lambda (file)
	   (javaimp--maven-xml-file-matches
	    file (javaimp-module-id submodule) (javaimp-module-parent-id submodule)))
	 files)
	(error "Cannot find file for module: %s" (javaimp-module-id submodule)))))


;;; Loading dep-jars

(defun javaimp--maven-update-module-maybe (node)
  (let (need-update)
    (let ((module (javaimp-node-contents node)))
      (or (javaimp-module-dep-jars module)
	  (progn (message "Loading dependencies: %s" (javaimp-module-id module))
		 (setq need-update t))))
    ;; check if any pom up to the top has changed
    (let ((tmp node))
      (while (and tmp
		  (not need-update))
	(let ((module (javaimp-node-contents tmp)))
	  (if (> (float-time (javaimp--get-file-ts (javaimp-module-file module)))
		 (float-time (javaimp-module-load-ts module)))
	      (progn
		(message "Reloading (%s pom changed)" (javaimp-module-id module))
		(setq need-update t))))
	(setq tmp (javaimp-node-parent tmp))))
    (when need-update
      (let* ((module (javaimp-node-contents node))
	     (new-dep-jars (javaimp--maven-fetch-dep-jars module))
	     (new-load-ts (current-time)))
	(setf (javaimp-module-dep-jars module) new-dep-jars)
	(setf (javaimp-module-load-ts module) new-load-ts)))))

(defun javaimp--maven-fetch-dep-jars (module)
  (let* ((path (javaimp--maven-call (javaimp-module-file module)
				    "dependency:build-classpath"
				    #'javaimp--maven-build-classpath-handler))
	 (converted-path (javaimp-cygpath-convert-maybe path 'unix t))
	 (path-separator-regex (concat "[" path-separator "\n" "]+")))
    (split-string converted-path path-separator-regex t)))

(defun javaimp--maven-build-classpath-handler ()
  (goto-char (point-min))
  (search-forward "Dependencies classpath:")
  (forward-line 1)
  (thing-at-point 'line))


;; Working with jar classes

(defun javaimp--get-jar-classes (file)
  (let ((cached (cdr (assoc file javaimp-cached-jars))))
    (cond ((null cached)
	   ;; create, load & put into cache
	   (setq cached
		 (make-javaimp-cached-jar
		  :file file
		  :read-ts (javaimp--get-file-ts file)
		  :classes (javaimp--fetch-jar-classes file)))
	   (push (cons file cached) javaimp-cached-jars))
	  ((> (float-time (javaimp--get-file-ts (javaimp-cached-jar-file cached)))
	      (float-time (javaimp-cached-jar-read-ts cached)))
	   ;; reload
	   (setf (javaimp-cached-jar-classes cached) (javaimp--fetch-jar-classes file))
	   ;; update read-ts
	   (setf (javaimp-cached-jar-read-ts cached) (current-time))))
    ;; return from cached
    (javaimp-cached-jar-classes cached)))

(defun javaimp--fetch-jar-classes (file)
  (message "Reading classes in file: %s" file)
  (with-temp-buffer
    (let ((coding-system-for-read (and (eq system-type 'cygwin) 'utf-8-dos)))
      ;; on cygwin, "jar" is a windows program, so file path needs to be
      ;; converted appropriately.
      (process-file javaimp-jar-program nil t nil
		    ;; `jar' accepts commands/options as a single string
		    "tf" (javaimp-cygpath-convert-maybe file 'windows))
      (goto-char (point-min))
      (while (search-forward "/" nil t)
	(replace-match "."))
      (goto-char (point-min))
      (let (result)
	(while (re-search-forward "\\(^[[:alnum:]._]+\\)\\.class$" nil t)
	  (push (match-string 1) result))
	result))))


;; Tree search routines

(defun javaimp--find-node (predicate)
  (javaimp--find-node-in-forest javaimp-project-forest predicate))

(defun javaimp--select-nodes (predicate)
  (javaimp--select-nodes-from-forest javaimp-project-forest predicate))

(defun javaimp--find-node-in-forest (forest predicate)
  (catch 'found
    (dolist (tree forest)
      (javaimp--find-node-in-tree tree predicate))))

(defun javaimp--find-node-in-tree (tree predicate)
  (if tree
      (progn (if (funcall predicate (javaimp-node-contents tree))
		 (throw 'found tree))
	     (dolist (child (javaimp-node-children tree))
	       (javaimp--find-node-in-tree child predicate)))))

(defun javaimp--select-nodes-from-forest (forest predicate)
  (apply #'seq-concatenate 'list
	 (mapcar (lambda (tree)
		   (javaimp--select-nodes-from-tree tree predicate))
		 forest)))

(defun javaimp--select-nodes-from-tree (tree predicate)
  (if tree
      (append (if (funcall predicate (javaimp-node-contents tree))
		  (list tree))
	      (apply #'seq-concatenate 'list
		     (mapcar (lambda (child)
			       (javaimp--select-nodes-from-tree child predicate))
			     (javaimp-node-children tree))))))


;; Some API functions

;; do not expose tree structure, return only modules

(defun javaimp-find-module (predicate)
  (let ((node (javaimp--find-node predicate)))
    (and node
	 (javaimp-node-contents node))))

(defun javaimp-select-modules (predicate)
  (mapcar #'javaimp-node-contents
	  (javaimp--select-nodes predicate)))


;;; Adding imports

;;;###autoload
(defun javaimp-add-import (classname)
  "Imports classname in the current file.  Interactively,
asks for a class to import, adds import statement and calls
`javaimp-organize-imports'.  Import statements are not
duplicated.  Completion alternatives are constructed based on
this module's dependencies' classes, JDK classes and top-level
classes in the current module."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((file (expand-file-name
		   (or buffer-file-name
		       (error "Buffer is not visiting a file!"))))
	    (node (or (javaimp--find-node
		       (lambda (m)
			 (or (string-prefix-p (javaimp-module-source-dir m) file)
			     (string-prefix-p (javaimp-module-test-source-dir m) file))))
		      (error "Cannot find module by file: %s" file))))
       (javaimp--maven-update-module-maybe node)
       (let ((module (javaimp-node-contents node)))
	 (list (completing-read
		"Import: "
		(append
		 ;; we're not caching full list of classes coming from module
		 ;; dependencies because jars may change and we need to reload
		 ;; them
		 (let ((jars (append (javaimp-module-dep-jars module)
				     (javaimp--get-jdk-jars))))
		   (apply #'seq-concatenate 'list
			  (mapcar #'javaimp--get-jar-classes jars)))
		 (and javaimp-include-current-module-classes
		      (javaimp--get-module-classes module)))
		nil t nil nil (symbol-name (symbol-at-point))))))))
  (javaimp-organize-imports (cons classname 'ordinary)))

(defun javaimp--get-module-classes (module)
  "Returns list of top-level classes in current module"
  (append
   (let ((build-dir (javaimp-module-build-dir module)))
     ;; additional source dirs
     (and (seq-mapcat
	   (lambda (rel-dir)
	     (let ((dir (concat build-dir (file-name-as-directory rel-dir))))
	       (and (file-accessible-directory-p dir)
		    (javaimp--get-directory-classes dir nil))))
	   javaimp-additional-source-dirs)))
   ;; source dir
   (let ((dir (javaimp-module-source-dir module)))
     (and (file-accessible-directory-p dir)
	  (javaimp--get-directory-classes dir nil)))
   ;; test source dir
   (let ((dir (javaimp-module-test-source-dir module)))
     (and (file-accessible-directory-p dir)
	  (javaimp--get-directory-classes dir nil)))))

(defun javaimp--get-directory-classes (dir prefix)
  (append
   ;; .java files in current directory
   (mapcar (lambda (file)
	     (concat prefix (file-name-sans-extension (car file))))
	   (seq-filter (lambda (file) (null (cadr file))) ;only files
		       (directory-files-and-attributes dir nil "\\.java\\'" t)))
   ;; descend into subdirectories
   (apply #'seq-concatenate 'list
	  (mapcar (lambda (subdir)
		    (let ((name (car subdir)))
		      (javaimp--get-directory-classes
		       (concat dir (file-name-as-directory name)) (concat prefix name "."))))
		  (seq-filter (lambda (file)
				(and (eq (cadr file) t) ;only directories
				     (null (member (car file) '("." "..")))))
			      (directory-files-and-attributes dir nil nil t))))))


;; Organizing imports

;;;###autoload
(defun javaimp-organize-imports (&rest new-imports)
  "Groups import statements according to the value of
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
is `'ordinary' or `'static'.  Interactively, NEW-IMPORTS is nil."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (goto-char (point-min))
    (let* ((old-data (javaimp--parse-imports))
	   (first (car old-data))
	   (last (cadr old-data))
	   (all-imports (append new-imports (cddr old-data))))
      (if all-imports
	  (progn
	    ;; delete old imports, if any
	    (if first
		(progn
		  (goto-char last)
		  (forward-line)
		  (delete-region first (point))))
	    (javaimp--prepare-for-insertion first)
	    (setq all-imports
		  (cl-delete-duplicates
                   all-imports
                   :test (lambda (first second)
                           (equal (car first) (car second)))))
	    ;; assign order
	    (let ((with-order
		   (mapcar
		    (lambda (import)
		      (let ((order (or (assoc-default (car import)
						      javaimp-import-group-alist
						      'string-match)
				       javaimp-import-default-order)))
			(cons import order)))
		    all-imports)))
	      (setq with-order
		    (sort with-order
			  (lambda (first second)
			    ;; sort by order, name
			    (if (= (cdr first) (cdr second))
				(string< (caar first) (caar second))
			      (< (cdr first) (cdr second))))))
	      (javaimp--insert-imports with-order)))
      (message "Nothing to organize!")))))

(defun javaimp--parse-imports ()
  (let (first last list)
    (while (re-search-forward "^\\s-*import\\s-+\\(static\\s-+\\)?\\([._[:word:]]+\\)" nil t)
      (push (cons (match-string 2) (if (match-string 1) 'static 'ordinary)) list)
      (setq last (line-beginning-position))
      (or first (setq first last)))
    (cons first (cons last list))))

(defun javaimp--prepare-for-insertion (start)
  (cond (start
	 ;; if there were any imports, we start inserting at the same place
	 (goto-char start))
	((re-search-forward "^\\s-*package\\s-" nil t)
	 ;; if there's a package directive, insert one blank line below and
	 ;; leave point after it
	 (end-of-line)
	 (if (eobp)
	     (insert ?\n)
	   (forward-line))
	 ;; then insert one blank line and we're done
	 (insert ?\n))
	(t
	 ;; otherwise, just go to bob
	 (goto-char (point-min)))))

(defun javaimp--insert-imports (imports)
  (let ((static (seq-filter (lambda (elt)
			      (eq (cdar elt) 'static))
			    imports))
	(ordinary (seq-filter (lambda (elt)
				(eq (cdar elt) 'ordinary))
			      imports)))
    (javaimp--insert-import-group "import static %s;" static)
    (and static ordinary (insert ?\n))
    (javaimp--insert-import-group "import %s;" ordinary)))

(defun javaimp--insert-import-group (pattern imports)
  (let (last-order)
    (dolist (import imports)
      ;; if adjacent imports have different order value, insert a newline
      ;; between them
      (let ((order (cdr import)))
	(and last-order
	     (/= order last-order)
	     (insert ?\n))
	(insert (format pattern (caar import)) ?\n)
	(setq last-order order)))))

(provide 'javaimp)

;;; javaimp.el ends here

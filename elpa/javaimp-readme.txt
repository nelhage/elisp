Allows to manage Java import statements in Maven projects.

  Quick start:

- customize `javaimp-import-group-alist'
- call `javaimp-maven-visit-project', giving it the top-level project
directory where pom.xml resides

Then in a Java buffer visiting a file under that project or one of its
submodules call `javaimp-organize-imports' or `javaimp-add-import'.

This module does not add all needed imports automatically!  It only helps
you to quickly add imports when stepping through compilation errors.

  Some details:

If Maven failed, you can see its output in the buffer named by
`javaimp-debug-buf-name' (default is "*javaimp-debug*").

Contents of jar files and Maven project structures (pom.xml) are cached,
so usually only the first command should take a considerable amount of
time to complete.  If a module's pom.xml or any of its parents' pom.xml
(within visited tree) was modified after information was loaded, `mvn
dependency:build-classpath' is re-run on the current module.  If a jar
file was changed, its contents are re-read.

Currently inner classes are filtered out from completion alternatives.
You can always import top-level class and use qualified name.


  Example of initialization:

(require 'javaimp)

(add-to-list 'javaimp-import-group-alist
  '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))

(setq javaimp-additional-source-dirs '("generated-sources/thrift"))

(add-hook 'java-mode-hook
	  (lambda ()
	    (local-set-key "\C-ci" 'javaimp-add-import)
	    (local-set-key "\C-co" 'javaimp-organize-imports)))


TODO:

- use functions `cygwin-convert-file-name-from-windows' and
`cygwin-convert-file-name-to-windows' when they are available instead of
calling `cygpath'.  See https://cygwin.com/ml/cygwin/2013-03/msg00228.html

- save/restore state, on restore check if a root exists and delete it if
not

- `javaimp-add-import': without prefix arg narrow alternatives by local name;
with prefix arg include all classes in alternatives

- :type for defcustom

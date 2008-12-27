

PACKAGE_VERSION = 0
PACKAGE_BUGREPORT = dvc-dev@gna.org

##############################################################################
# location of required programms
prefix = /usr/local
RM   = @RM@
ETAGS = etags
MKDIR_P = /bin/mkdir -p
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

top_srcdir = ..
srcdir = .

datarootdir= ${prefix}/share
lispdir = $(prefix)/share/emacs/site-lisp/dvc

EMACS_PROG = emacs

FLAGS = --no-site-file

# Other settings

OTHERDIRS = 

ebatch = srcdir=$(srcdir) otherdirs="$(OTHERDIRS)" \
 $(EMACS_PROG) -batch -q $(FLAGS) -l $(srcdir)/dvc-build.el -f

alldeps = dvc-version.el dvc-site.el

all: $(alldeps)
	$(ebatch) dvc-build-all

all-verbose: $(alldeps)
	$(ebatch) dvc-build-all verbose

# We install foo.el only if there is also foo.elc.
install: all
	$(MKDIR_P) -m 0755 "$(lispdir)"
	@dlist='$(srcdir) $(srcdir)/contrib' ;		\
	test '$(srcdir)' = '.' || dlist=". $$dlist" ;	\
	for elc in *.elc ; do				\
	  el=`echo $$elc | sed 's/.$$//'` ; orig= ;	\
	  for d in $$dlist ; do				\
	    if [ -r "$$d/$$el" ] ; then			\
	      orig="$$d/$$el" ; break ; fi ; done ;	\
	  test "$$orig" || continue ;			\
	  echo Installing $$el ;			\
	  $(INSTALL_DATA) "$$orig" "$(lispdir)" ;	\
	  echo Installing $$elc ;			\
	  $(INSTALL_DATA) $$elc "$(lispdir)" ;		\
	done

clean:
	rm -f *.elc dvc-site.el \
		dvc-autoloads.el auto-autoloads.el custom-load.el

Makefile: $(srcdir)/Makefile.in ../config.status
	cd ..; ./config.status

distclean: clean
	rm -f Makefile

maintainer-clean:
	rm -f dvc-version.el

TAGS: $(SRCS)
	@if test "x$(ETAGS)" = "x" ; then                 \
           echo "Sorry, no \`etags' program available." ; \
         else                                             \
           $(ETAGS) */*.el                              ; \
         fi

##############################################################################
autoloads:
	$(ebatch) dvc-build-autoloads $(srcdir)


##############################################################################
dvc-version.el: ../config.status
	@echo Creating $@
	@( echo ';;; $@ (generated file -- do not edit!)' ; \
	   echo '(defconst dvc-version "$(PACKAGE_VERSION)"' ; \
	   echo '  "Version of DVC loaded.' ; \
	   echo 'Please send bug reports to <$(PACKAGE_BUGREPORT)>.")' ; \
	   echo "(provide 'dvc-version)" ) \
	 > $@

dvc-site.el: ../config.status $(srcdir)/dvc-site.el.in
	(cd .. ; ./config.status lisp/$@)

.PHONY: all all-verbose install \
	clean distclean maintainer-clean

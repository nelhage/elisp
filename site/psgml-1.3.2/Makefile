# Generated automatically from Makefile.in by configure.
# Makefile.in 
#
# Makefile for the GNU Emacs lisp library, PSGML

prefix = /usr/local
datadir = ${prefix}/share
lispdir = $(datadir)/emacs/site-lisp
srcdir = .

EMACS = emacs
XEMACS = xemacs

FLAGS = -batch -q -no-site-file -l ./psgml-maint.el
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
SHELL = /bin/sh

compile:
	$(EMACS) $(FLAGS) -f psgml-compile-files

all: 
	rm -f *.elc ; $(EMACS) $(FLAGS) -f psgml-compile-files


install: compile installdirs 
	$(SHELL) mkinstalldirs $(lispdir)
	for p in *.elc; do \
	  echo " $(INSTALL_DATA) $$p $(lispdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(lispdir)/$$p; \
	done
	$(INSTALL_DATA) iso88591.map $(lispdir)

installdirs:
	$(srcdir)/mkinstalldirs $(psgmldir)

install-info:
	$(INSTALL_DATA) psgml.info $(infodir)
	$(INSTALL_DATA) psgml-api.info $(infodir)
	if $(SHELL) -c 'install-info --version' \
		>/dev/null 2>&1; then \
		install-info --info-dir=$(infodir) psgml.info; \
		install-info --info-dir=$(infodir) psgml-api.info; \
	else true; fi


xemacs:
	$(MAKE) EMACS=xemacs all

install-xemacs:
	$(MAKE) EMACS=xemacs install


# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

# Makefile.in ends here

# Generated automatically from Makefile.in by configure.
## This is a -*- makefile -*-

# What the Emacs binary is called on your system
EMACS = emacs
EMACSFLAGS = 

# Prefix for constructing installation directory paths
prefix = /usr/local
exec_prefix = ${prefix}

# Shared directory for read-only data files
datadir = $(prefix)/share

# Where to put the .el and .elc files
lispdir = $(datadir)/emacs/site-lisp

# Where to put the Info files
infodir = ${prefix}/info
INFOFILES = mailcrypt.info mailcrypt.info-1 mailcrypt.info-2

# Where to put the helper scripts
bindir = $(prefix)/bin

# Installation command
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = /usr/sbin/install-info

# Various auxiliary programs
MAKEINFO=makeinfo
DVIPS=dvips
TEXI2DVI=texi2dvi
TEXI2HTML=texi2html
TAR=tar

srcdir = .

SOURCES =  \
	mailcrypt.el mc-toplev.el mc-pgp.el mc-remail.el \
	mc-pgp5.el mc-gpg.el expect.el mc-setversion.el

OBJECTS =  \
	mailcrypt.elc mc-toplev.elc mc-pgp.elc mc-remail.elc \
	mc-pgp5.elc mc-gpg.elc expect.elc mc-setversion.elc

GPG_TESTCASES = $(foreach f,CS.s1v CS.s3v E.e1r E.e3 ES.e1r.s1v ES.e1r.s3v \
		  ES.e3.s1v S.s1v S.s3v CS.s2v CS.s4 E.e2r E.e4 ES.e1r.s2v \
		  ES.e1r.s4 ES.e4.s1v S.s2v S.s4 SE,tests/gpg-testcases/$(f))

TESTFILES_GPG = tests/Makefile.in					      \
	tests/make_gpg_testcases.py tests/test-gpg.el tests/gpg-keys/Makefile \
	tests/gpg-keys/fix-trust.py tests/gpg-keys/make-rings.py	      \
	tests/gpg-keys/rings.txt $(GPG_TESTCASES)
TESTFILES_REMAILER = tests/remailer/Makefile.in tests/remailer/rings.txt \
	tests/remailer/rlist.txt tests/remailer/test-remailer.el	 \
	tests/remailer/unwind.py

DISTFILES = $(SOURCES) ANNOUNCE COPYING ChangeLog ChangeLog.1 INSTALL	     \
	LCD-entry Makefile.in NEWS ONEWS README configure configure.in	     \
	install-sh load-path.hack mailcrypt.texi mkinstalldirs texi2html.ext \
	FSF-timer.el $(INFOFILES) mailcrypt.dvi README.gpg $(TESTFILES_GPG)  \
	$(TESTFILES_REMAILER)

SHELL = /bin/sh
#.PHONY: all clean dist distclean dvi html info install \
#	installdirs ps uninstall
.SUFFIXES:
.SUFFIXES: .elc .el

.el.elc:
	$(EMACS) -batch $(EMACSFLAGS) -l $(srcdir)/load-path.hack \
	  -f batch-byte-compile $<

all: $(OBJECTS)

check: $(OBJECTS)
	$(MAKE) -C tests check

install: all installdirs install-info $(bindir)
	for f in $(SOURCES); do \
	  $(INSTALL_DATA) $(srcdir)/$$f $(lispdir); \
	done;
	for f in $(OBJECTS); do \
	  $(INSTALL_DATA) $$f $(lispdir); \
	done;

# Make sure all installation directories actually exist
# by making them if necessary.
installdirs: mkinstalldirs
	$(srcdir)/mkinstalldirs $(lispdir) $(infodir) $(bindir)

# If your install-info doesn't know how to look inside the .info file for the
# menu entry to add, try adding
#   --entry="* Mailcrypt: (mailcrypt).       An Emacs/PGP interface"
# to the arguments.

install-info: info
	cd $(srcdir) && for file in $(INFOFILES); do\
	  $(INSTALL_DATA) $$file $(infodir)/$$file; \
	done
	-$(INSTALL_INFO) --info-dir=$(infodir) mailcrypt.info

uninstall:
	-cd $(lispdir) && rm -f $(SOURCES) $(OBJECTS)
	-cd $(infodir) && rm -f $(INFOFILES)
	$(INSTALL_INFO) --remove --info-dir=$(infodir) mailcrypt.info

info: $(INFOFILES)

$(INFOFILES): mailcrypt.texi
	$(MAKEINFO) $(srcdir)/mailcrypt.texi

timer.el: FSF-timer.el
	cp FSF-timer.el timer.el

dvi: mailcrypt.dvi

mailcrypt.dvi: mailcrypt.texi
	$(TEXI2DVI) $(srcdir)/mailcrypt.texi

ps: mailcrypt.ps

mailcrypt.ps: mailcrypt.dvi
	$(DVIPS) -f $(srcdir)/mailcrypt.dvi >$@

html: mailcrypt_toc.html

mailcrypt_toc.html: mailcrypt.texi
	$(TEXI2HTML) -split_chapter $(srcdir)/mailcrypt.texi

TAGS: $(SOURCES)
	cd $(srcdir) && etags $(SOURCES)

clean:
	rm -f $(OBJECTS)
	-rm -f *.aux *.cp *.cps *.fn *.ky *.log *.pg *.toc *.tp *.vr
	-rm -f *.html
	rm -f TAGS

distclean: clean
	-rm -f *~ *.tar.gz
	-rm -f Makefile config.status config.cache config.log timer.el
	-rm -f *.info* *.dvi

${srcdir}/configure: configure.in
	cd ${srcdir} && autoconf

Makefile: Makefile.in config.status
	./config.status

config.status: ${srcdir}/configure
	./config.status --recheck

dist: $(DISTFILES)
	version=`perl -ne 'print $$1 if /defconst mc-version \"(.*)\"/' \
		 mailcrypt.el`; \
	distname=mailcrypt-$$version; \
	rm -rf $$distname; \
	mkdir $$distname; \
	mkdir $$distname/tests; mkdir $$distname/tests/gpg-keys; \
	mkdir $$distname/tests/gpg-testcases; \
	mkdir $$distname/tests/remailer; \
	for file in $(DISTFILES); do \
	  ln $$file $$distname/$$file; \
	done; \
	$(TAR) -chz -f $$distname.tar.gz $$distname; \
	rm -rf $$distname

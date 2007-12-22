

infodir = ${prefix}/info
prefix = /usr/local
srcdir = .
subdir = texinfo
top_srcdir = ..


INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

RM   = /bin/rm -f
LN_S = ln -s

MAKEINFO = makeinfo
EMACSINFO = emacs -no-site-file -no-init-file -batch -q -l infohack.el -f batch-makeinfo

PACKAGEDIR = /usr/lib/xemacs/site-packages
SYMLINKS = no
LINKPATH = 

all: info bbdb.dvi

install-pkg: uninstall-pkg info
	@if test "x$(SYMLINKS)" = "xno" ; then                         \
          for i in `ls bbdb.info* ` ; do                               \
            $(INSTALL_DATA) $$i $(PACKAGEDIR)/info/ ;                  \
          done ;                                                       \
        else                                                           \
          if test "x$(LINKPATH)" = "x" ; then                          \
            for i in `ls bbdb.info* ` ; do                             \
              $(LN_S) `pwd`/$$i $(PACKAGEDIR)/info/$$i ;               \
            done ;                                                     \
          else                                                         \
            for i in `ls bbdb.info* ` ; do                             \
              $(LN_S) $(LINKPATH)/texinfo/$$i $(PACKAGEDIR)/info/$$i ; \
            done ;                                                     \
          fi ;                                                         \
        fi

uninstall-pkg:
	-$(RM) $(PACKAGEDIR)/info/bbdb.info*

info: bbdb.info

bbdb.info: bbdb.texinfo
	@if test "x$(MAKEINFO)" = "x" ; then                 \
           $(EMACSINFO) bbdb.texinfo ;                       \
         else                                                \
           $(MAKEINFO) bbdb.texinfo ;                        \
         fi

bbdb.dvi: bbdb.texinfo
	@if test "x$(TEXI2DVI)" = "x" ; then                 \
           echo "Sorry, no \`texi2dvi' program available." ; \
         else                                                \
           $(TEXI2DVI) bbdb.texinfo ;                        \
         fi

bbdb.pdf: bbdb.texinfo
	@if test "x$(TEXI2DVI)" = "x" ; then                 \
           echo "Sorry, no \`texi2dvi' program available." ; \
         else                                                \
           $(TEXI2DVI) -p bbdb.texinfo ;                     \
         fi

clean:
	-$(RM) *.aux *.cp *.cps *.dvi *.pdf *.fn *.fns *.ky *.log *.pg \
	  *.toc *.tp *.vr *.vrs

# Backward compatibility
reallyclean: distclean

distclean: clean
	-$(RM) bbdb.info*

cvsclean: distclean
	-$(RM) Makefile

Makefile: $(srcdir)/Makefile.in ../config.status
	cd .. \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

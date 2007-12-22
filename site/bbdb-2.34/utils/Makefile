

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

RM   = /bin/rm -f
LN_S = ln -s

PACKAGEDIR = /usr/lib/xemacs/site-packages
SYMLINKS = no
LINKPATH = 

install-pkg: uninstall-pkg
	@if test "x$(SYMLINKS)" = "xno" ; then                       \
          mkdir -p -m 0755 $(PACKAGEDIR)/etc/bbdb/utils ;            \
          for i in `ls -d * | egrep -v '(Makefile|CVS)'` ; do        \
            $(INSTALL_DATA) $$i $(PACKAGEDIR)/etc/bbdb/utils/ ;      \
          done ;                                                     \
        else                                                         \
          if test "x$(LINKPATH)" = "x" ; then                        \
            $(LN_S) `pwd` $(PACKAGEDIR)/etc/bbdb/utils ;             \
          else                                                       \
            $(LN_S) $(LINKPATH)/utils $(PACKAGEDIR)/etc/bbdb/utils ; \
          fi ;                                                       \
        fi

uninstall-pkg:
	-$(RM) -r $(PACKAGEDIR)/etc/bbdb/utils

clean:

distclean:

# Backward compatibility:
reallyclean: distclean

cvsclean: distclean
	-$(RM) Makefile

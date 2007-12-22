

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
	@if test "x$(SYMLINKS)" = "xno" ; then                   \
          mkdir -p -m 0755 $(PACKAGEDIR)/etc/bbdb/tex ;          \
          for i in `ls *.tex` ; do                               \
            $(INSTALL_DATA) $$i $(PACKAGEDIR)/etc/bbdb/tex/ ;    \
          done ;                                                 \
        else                                                     \
          if test "x$(LINKPATH)" = "x" ; then                    \
            $(LN_S) `pwd` $(PACKAGEDIR)/etc/bbdb/tex ;           \
          else                                                   \
            $(LN_S) $(LINKPATH)/tex $(PACKAGEDIR)/etc/bbdb/tex ; \
          fi ;                                                   \
        fi

uninstall-pkg:
	-$(RM) -r $(PACKAGEDIR)/etc/bbdb/tex

clean:

distclean: clean

# Backward compatibility:
reallyclean: distclean

cvsclean: distclean
	-$(RM) Makefile

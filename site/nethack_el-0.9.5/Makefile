# path to your emacs
EMACS = emacs

.PHONY: clean patch dist

all: nethack.elc nethack-api.elc nethack-cmd.elc nethack-tiles.elc slashem-tiles.elc nethack-keys.elc

%.elc: %.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $<

clean:
	$(RM) *.elc

#
# Make a datestamped distribution in the current directory
#
# ../nethack must be your CVS version of the winlisp nethack sources
# ../nethack-$(NH_VER).tgz must exist
#

# nethack-el version
NETHACK_EL_VER=0.9.5

# nethack source version
NH_VER=3.4.3
NH_VER_NODOTS=343
SLASHEM_VER=0.0.7E7
SLASHEM_VER_NODOTS=007e7

PATCHFILE=enh-$(NH_VER_NODOTS).patch
SLASHEM_PATCHFILE=enh-$(SLASHEM_VER_NODOTS).patch

DISTFILES=AUTHORS BUGS COPYING ChangeLog INSTALL Makefile README TODO	\
	$(PATCHFILE) $(SLASHEM_PATCHFILE) mkpatch nethack-api.el	\
	nethack-cmd.el nethack-example.el				\
	nethack-tiles.el slashem-tiles.el nethack-keys-dvorak.el nethack-keys.el		\
	nethack.el nethack-compat.el
DISTDIR=nethack_el-$(NETHACK_EL_VER)

PATCH_OK=.patch-ok
SLASHEM_PATCH_OK=.se-patch-ok

$(PATCH_OK): test-patch ;

$(SLASHEM_PATCH_OK): slashem-test-patch ;

dist: clean all $(PATCH_OK) $(SLASHEM_PATCH_OK)
	@echo
	@echo Things look in order... building the distribution tarball.
	mkdir ./$(DISTDIR)
	cp $(DISTFILES) ./$(DISTDIR)
	tar -cf /dev/stdout ./$(DISTDIR) | gzip > ./$(DISTDIR).tar.gz
	rm -rf ./$(DISTDIR)

# Generate patch.
# Apply the generated patch from inside the nethack dir like this: 
# src/nethack-x.y.z patch -p 1 < PATH/TO/enh-xyz.patch
patch:
	@echo
	@echo Creating $(PATCHFILE)
	sh ./mkpatch nethack-$(NH_VER_NODOTS).tgz nethack-$(NH_VER) nethack > $(PATCHFILE)
	rm -f $(PATCH_OK)

slashem-patch:
	@echo
	@echo Creating $(SLASHEM_PATCHFILE)
	sh ./mkpatch se$(SLASHEM_VER_NODOTS).tar.gz slashem-$(SLASHEM_VER) slashem > $(SLASHEM_PATCHFILE) 
	rm -f $(SLASHEM_PATCH_OK)


# Make sure nothing obvious was missed when hacking the source and
# creating the patch
test-patch: $(PATCHFILE)
	@echo
	@echo Testing $< for sanity
	cd ..; tar -xzf nethack-$(NH_VER_NODOTS).tgz
	cd ../nethack-$(NH_VER) ; patch -p1 < ../nethack-el/$(PATCHFILE)
	cd ../nethack-$(NH_VER)/sys/unix; sh setup.sh
	$(MAKE) -C ../nethack-$(NH_VER)
	cd ../nethack-$(NH_VER)/src; HACKDIR=. ./nethack | grep api-init
	rm -r ../nethack-$(NH_VER)
	touch $(PATCH_OK)

slashem-test-patch: $(SLASHEM_PATCHFILE)
	@echo
	@echo Testing $< for sanity
	cd ..; tar -xzf se$(SLASHEM_VER_NODOTS).tar.gz
	cd ../slashem-$(SLASHEM_VER) ; patch -p1 < ../nethack-el/$(SLASHEM_PATCHFILE)
	cd ../slashem-$(SLASHEM_VER)/sys/unix; sh setup.sh
	$(MAKE) -C ../slashem-$(SLASHEM_VER)
	cd ../slashem-$(SLASHEM_VER)/src; HACKDIR=. ./slashem | grep api-init
	rm -r ../slashem-$(SLASHEM_VER)
	touch $(SLASHEM_PATCH_OK)

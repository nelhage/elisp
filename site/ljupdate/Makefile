### Makefile --- byte-compile ljupdate and its dependencies

## Configuration

# choose one:
EMACS=emacs -q --no-site-file
# or
#EMACS=xemacs -vanilla

## Building

SOURCE=lj-acct.el lj-compat.el lj-compose.el lj-custom.el lj-edit.el \
	lj-fill.el lj-login.el lj-pcomplete.el lj-protocol.el lj-util.el
TARGET=$(patsubst %.el,%.elc, $(SOURCE) ljupdate.el)
TARBALL=ljupdate.tar.gz

compile: $(TARGET)

ljupdate.el: ljupdate.in $(SOURCE)
	rm -f ljupdate.elc
	cat ljupdate.in | sed -e "s/##revision##/`svnversion .`/" > ljupdate.el
	@$(EMACS) -batch -l lj-maint.el -f lj-generate-autoloads ljupdate.el .

## Distribution

DISTFILES=$(patsubst %,ljupdate/%,$(SOURCE) lj-maint.el ljupdate.in \
	ljupdate.el README COPYING Makefile)

dist: $(TARBALL)

$(TARBALL): $(SOURCE) lj-maint.el ljupdate.in ljupdate.el README \
	COPYING Makefile
	tar czvf $(TARBALL) -C .. $(DISTFILES)

pub: $(TARBALL)
	darcs push -va
	scp $(TARBALL) rakim:/web/edward.oconnor.cx/html/code/ljupdate

## Support for downloading required libraries

THIRD_PARTY=http-cookies.el http-get.el http-post.el
fetch: $(THIRD_PARTY)

SAVANNAH_VIEWCVS=http://cvs.savannah.gnu.org/viewvc
HTTP_EMACS_SITE=$(SAVANNAH_VIEWCVS)/*checkout*/http-emacs/http-emacs

$(THIRD_PARTY):
	curl -O $(HTTP_EMACS_SITE)/$*.el

## Cleaning

pretty:
	@rm -f *~

clean:
	@rm -f $(TARGET)

distclean: clean
	@rm -f $(THIRD_PARTY) ljupdate.el $(TARBALL)

## Workhorse

.el.elc:
	@$(EMACS) -batch -l lj-maint.el -f batch-byte-compile $*.el \
		|| (echo "Perhaps you should specifcy LOAD_PATH to make?" \
		"(e.g. \"gmake LOAD_PATH=~/elisp\".)" \
		&& echo "Please see README for compilation instructions." \
		&& exit 1)

### Makefile ends here

#
# RCS makefile,v 1.1.1.1 1995/08/07 08:43:10 mohsen Exp
#

# The name of the file
MANUAL=main
INFOFILE= bbdb-filters.info

TEXPARTS = 

EPSFIGS = 

TGRINDS = 

EOEBASE		= /usr/public/eoe/lisp/public/bbdbPlus
EOEINFO		= /usr/public/eoe/info

# The name of your DVI to PS filter
DVIPS=dvips -f

# The name of your GNU Emacs
EMACS=	xemacs

LATEXINFO= /usr/public/tex/latexinfo1.7

###
###  SHOUL NOT HAVE TO TOUCH ANYTHING BELOW HERE
###
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES:	.lpr .ps .tty .xdvi .dvi .tex  .ptex .eps .fig .c

.fig.eps:
	fig2dev -L ps $< > $@

.c.tex: 
	tgrind -f $< > $@


# DEFAULT TARGET
#all:	$(INFOFILE) $(MANUAL).ps
all:	fast.ps

$(INFOFILE):	$(MANUAL).tex $(TEXPARTS)
	rm -f makeinfo.el
	sed -e 	"s+MANUAL+$(MANUAL)+" \
	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/makeinfo.tmplt > makeinfo.el
	$(EMACS) -batch -q -l makeinfo.el
	#cp $(INFOFILE) /usr/public/eoe/info

$(MANUAL).tex:	$(MANUAL).texinfo $(TEXPARTS)
	sed -e 	"s+INFOFILE+$(INFOFILE)+" $(MANUAL).texinfo | expand > $(MANUAL).tex
	-rm -f maketex.el
	sed -e 	"s+MANUAL+$(MANUAL)+" \
	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/maketex.tmplt > maketex.el
	$(EMACS) -batch -q -l maketex.el

$(MANUAL).hyperlatex:	$(MANUAL).texinfo $(TEXPARTS)
	sed -e 	"s+INFOFILE+$(INFOFILE)+" $(MANUAL).texinfo | expand > $(MANUAL).hyperlatex
	-rm -f makehyperlatex.el
	sed -e 	"s+MANUAL+$(MANUAL)+" \
	    -e	"s+LATEXINFO+$(LATEXINFO)+" $(LATEXINFO)/local/makehyperlatex.tmplt > makehyperlatex.el
	$(EMACS) -batch -q -l makehyperlatex.el

$(MANUAL).dvi:	$(MANUAL).tex $(EPSFIGS) $(TGRINDS)
	latex2dvi $(MANUAL).tex

$(MANUAL).bbl:	
	latex $(MANUAL)
	-bibtex $(MANUAL)
	latex $(MANUAL)

$(MANUAL).xdvi:	$(MANUAL).dvi
	xdvi $(MANUAL).dvi &

$(MANUAL).ps:	$(MANUAL).dvi
	$(DVIPS) $(MANUAL) > $(MANUAL).ps

$(MANUAL).lpr:	$(MANUAL).ps
	lpr $(MANUAL).ps

info:   $(INFOFILE)
	-echo Built $(INFOFILE)

$(MANUAL).html: $(MANUAL)/$(MANUAL).html
	-echo Building $(MANUAL)/$(MANUAL).html

$(MANUAL)/$(MANUAL).html: $(MANUAL).dvi $(MANUAL).htmlTex
	/usr/public/src/Sol-2/networking/www/latex2html-95.1/latex2html $(MANUAL).tex

EMACSBASE	= /opt/public/networking/www/hyperlatex-1.3/emacs

html:	 $(MANUAL).hyperlatex # $(MANUAL).dvi
	-mkdir bbdbFilters
	$(EMACS) -batch -no-init-file -no-site-file \
	-l $(EMACSBASE)/hyperlatex1.el -funcall batch-hyperlatex-format $(MANUAL).hyperlatex
	echo latex \'\\def\\makegifs{}\\input{$(MANUAL).hyperlatex}\' > dolatex.sh
	#sh dolatex.sh ; /bin/rm dolatex.sh
	#sh $(MANUAL).makegif 

install: $(INFOFILE)
	cp $(INFOFILE) $(EOEINFO)/$(INFOFILE)
	
#
# Fast Processing
#

fast.tex:	$(MANUAL).texinfo $(TEXPARTS)
	sed -e "s+INFOFILE+$(INFOFILE)+" -e "s+mbinput+input+" $(MANUAL).texinfo | expand > fast.tex

fast.dvi:	fast.tex $(EPSFIGS) $(TGRINDS)
	latex fast.tex

fast.xdvi:	fast.dvi
	xdvi fast.dvi &

fast.ps:	fast.dvi
	$(DVIPS) fast > fast.ps

fast.xps:	fast.ps
	pageview fast.ps &

fast.lpr:	fast.ps
	lpr fast.ps


# TeX Figures for when dvi files are needed. Just an example
#XX.tex YY.tex: XX.fig YY.fig 
#	transfig -m 1.00 -L eepic -M fig.make XX.fig YY.fig
#	make -f fig.make

# Encapsulated PostScript figures -- Done by the Suffix rules
#XX.eps: XX.fig 
#	fig2dev -L ps -m 1.0 $< > $@

# Src Code
#cot-calling.tex: cot-calling.c 
#	tgrind -f $< > $@


shar::
	split $(MANUAL).tex $(MANUAL)-

clean:	
	rm -f $(MANUAL).log $(MANUAL).blg makeinfo.el maketex.el *~ #~

veryclean: clean
	rm -f $(MANUAL).ps $(MANUAL).dvi $(MANUAL).dlog $(MANUAL).info

realclean: veryclean
	rm -f $(MANUAL).aux $(MANUAL).bbl $(MANUAL).blg $(MANUAL).cp \
	$(MANUAL).toc $(MANUAL).cps $(MANUAL).lot $(MANUAL).lof fig.make \
	$(MANUAL).auxO $(MANUAL).fn $(MANUAL).fns \
	transfig.tex $(MANUAL).tex $(INFOFILE) \
	$(MANUAL).hyperlatex makehyperlatex.el dolatex.sh \
	fast.aux fast.dvi fast.log fast.ps fast.tex fast.toc fast.cp fast.fn \
	$(EPSFIGS) $(TGRINDS)


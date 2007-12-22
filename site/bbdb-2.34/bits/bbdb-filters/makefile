#  This file is part of the BBDB Filters Package. BBDB Filters Package is a
#  collection of input and output filters for BBDB.
# 
#  Copyright (C) 1995 Neda Communications, Inc.
# 	Prepared by Mohsen Banan (mohsen@neda.com)
# 
#  This library is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Library General Public License as
#  published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version.  This library is
#  distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
#  License for more details.  You should have received a copy of the GNU
#  Library General Public License along with this library; if not, write
#  to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
#  USA.
# 
#

# Makefile for the Insidious Big Brother Database -- Input and Output Filters
#
# RCS makefile,v 1.2 1995/08/08 01:20:32 mohsen Exp
#

EOEBASE	= /usr/public/eoe/lisp/public/bbdbPlus
EOEINFO	= /usr/public/eoe/info
EMACS	= xemacs


# You shouldn't need to change anything after this point.

SRCS = bbdb-ccmail.el bbdb-eudora.el bbdb-export.el bbdb-hp200lx.el bbdb-ph.el bbdb-passwd.el

SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .elc .el 

.el.elc: 
	$(EMACS) -batch -q -f batch-byte-compile $(@:.elc=.el)

default:
	@echo Targets: install clean shar	

install: 
	cp $(SRCS) $(EOEBASE)
	cd doc; make EOEBASE=$(EOEBASE) EOEINFO=$(EOEINFO) install

clean:
	-/bin/rm *.elc package.shar
	cd doc; make EOEBASE=$(EOEBASE) EOEINFO=$(EOEINFO) clean


shar: $(SRCS) makefile
	shar -o package.shar $(SRCS) makefile COPYING.LIB README \
	doc doc/main.texinfo doc/lgpl.tex doc/makefile \
	doc/formatted  doc/formatted/bbdb-filters.info 


FORFTPING	= /h8/var/ftp/pub/eoe/bbdbPlus/bbdb-filters-0.2.tar 

tar: $(SRCS) makefile
	tar cvf $(FORFTPING) .




#!/usr/local/bin/perl
#
# Author: Christopher Kline <ckline@media.mit.edu>
#
# $Id: bbdb-unlazy-lock.pl,v 1.1 1997/10/06 00:56:14 simmonmt Exp $
#
# $Log: bbdb-unlazy-lock.pl,v $
# Revision 1.1  1997/10/06 00:56:14  simmonmt
# Initial revision
#
#
# Lazy-lock-mode has (had) a nasty habit of munging .bbdb files if you visited them
# with it on.  This script removes the lazy-lock mung
#

while( <STDIN> ) {
    s/#\(("[^"]*")( \d+ \d+ (nil)*(\(lazy-lock t\))*)*\)/$1/gi;
    print;
}

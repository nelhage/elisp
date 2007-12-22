dnl aclocal.m4 --- More autoconf macros for BBDB

dnl Author:        Didier Verna <didier@xemacs.org>
dnl Maintainer:    Didier Verna <didier@xemacs.org>
dnl Created:       Tue Nov 14 18:28:52 2000
dnl Last Revision: Tue Jan  2 16:53:50 2001

dnl Copyright (C) 2000-2001 Didier Verna

dnl BBDB is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU Library General Public License as published
dnl by the Free Software Foundation; either version 2 of the License, or (at
dnl your option) any later version.

dnl BBDB is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU Library General Public License for more details.

dnl You should have received a copy of the GNU Library General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


dnl BBDB_PRE_INIT
dnl
dnl Find BBDB version number and put it in the m4 macro BBDB_VERSION
dnl
dnl I fought really hard, but finally I got it accepted to make autoconf load
dnl aclocal *before* AC_INIT is called. This is important for me, in
dnl situations like this precise one, where I want to dynamically compute the
dnl version number to pass to AC_INIT.
dnl There's one minor glitch however, the AC_DEFUN mechanism is not available
dnl before AC_INIT is called because of diversions. So this macro is defined
dnl only in terms of "m4 sugar".
m4_define([BBDB_PRE_INIT],
[ m4_define([_BBDB_VERSION],
             m4_esyscmd([sed -n 's/^(defconst bbdb-version "\(.*\)")/\1/p' lisp/bbdb.el]))
  m4_define([BBDB_VERSION],
             m4_substr(_BBDB_VERSION, 0, decr(len(_BBDB_VERSION))))
  m4_undefine([_BBDB_VERSION])
])

dnl BBDB_ARG_SUBST(VAR, OPTION, VAL, DESC[, DEFAULT[, ACTION]])
dnl
dnl Substitute the autoconf variable VAR to a value specified by the user
dnl option --with-OPTION[=VAL] (described by DESC), or with a DEFAULT value.
dnl If an additional ACTION is given, it is executed at the top of the
dnl ACTION-IF-FOUND part of AC_ARG_WITH.
dnl #### WARNING: pay attention to the quoting of ACTION if given !!!!!
AC_DEFUN([BBDB_ARG_SUBST],
[
  AC_SUBST([$1])
  AC_ARG_WITH([$2],
    AC_HELP_STRING([--with-][$2]ifelse($3, [], [], [=$3]),
      [$4]ifelse($5, [], [], [ [[[$5]]]])),
    [
      ifelse($6, [], [], $6)
      $1="${withval}"
    ],
    ifelse($5, [], [], [$1="$5"]))
])

dnl BBDB_COLON_TO_SPACE(VAR)
dnl
dnl Transforms a (possibly) colon separated list VAR into a space separated
dnl one. VAR needs not be quoted.
AC_DEFUN([BBDB_COLON_TO_SPACE],
[ case "$$1" in *:*)
    $1="`echo $$1 | sed -e 's/:/ /g'`";;
  esac ])

dnl BBDB_PROG_GNU_TAR
dnl
dnl Find a (g)tar program and make sure it is GNU one. A failure is not fatal
dnl since tar is needed for non critical targets only.
AC_DEFUN([BBDB_PROG_GNU_TAR],
  [ AC_CHECK_PROGS(TAR, gtar tar)
    if test "x${TAR}" = "xtar" ; then
      AC_MSG_CHECKING([that tar is GNU tar])
      ${TAR} --version > /dev/null 2>&1 || TAR=
      if test "x${TAR}" = "x" ; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    if test "x${TAR}" = "x" ; then
      AC_MSG_WARN([*** No GNU tar program found.])
      AC_MSG_WARN([*** Some targets will be unavailable.])
    fi ])

dnl BBDB_PROG_COMPRESS
dnl
dnl Find a gzip / compress compression program. A failure is not fatal, only
dnl tarballs won't be compressed.
AC_DEFUN([BBDB_PROG_COMPRESS],
  [ AC_CHECK_PROGS(COMPRESS, gzip compress)
    AC_SUBST(COMPEXT)
    if test "x${COMPRESS}" = "x" ; then
      AC_MSG_WARN([*** No compression program found.])
      AC_MSG_WARN([*** Tarballs will not be compressed.])
      COMPEXT=
    elif test "x${COMPRESS}" = "xgzip" ; then
      COMPRESS="gzip --verbose --best"
      COMPEXT=gz
    else
      COMPEXT=Z
    fi ])

dnl BBDB_PROG_MAKEINFO
dnl
dnl Find a makeinfo program. A failure is not fatal, only info files won't be
dnl built.
AC_DEFUN([BBDB_PROG_MAKEINFO],
  [ AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo)
    if test "x${MAKEINFO}" = "x" ; then
      AC_MSG_WARN([*** No makeinfo program found.])
      AC_MSG_WARN([*** Info files will not be built.])
    fi ])

dnl BBDB_PROG_TEXI2DVI
dnl
dnl Find a texi2dvi program. A failure is not fatal, only dvi and pdf files
dnl won't be built.
AC_DEFUN([BBDB_PROG_TEXI2DVI],
  [ AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi)
    if test "x${TEXI2DVI}" = "x" ; then
      AC_MSG_WARN([*** No texi2dvi program found.])
      AC_MSG_WARN([*** DVI and PDF files will not be built.])
    fi ])

dnl BBDB_PROG_ETAGS
dnl
dnl Find an etags program. A failure is not fatal, only TAGS file won't be
dnl built.
AC_DEFUN([BBDB_PROG_ETAGS],
  [ AC_CHECK_PROG(ETAGS, etags, etags)
    if test "x${ETAGS}" = "x" ; then
      AC_MSG_WARN([*** No etags program found.])
      AC_MSG_WARN([*** Tags file will not be built.])
    fi ])

dnl BBDB_PROG_EMACS
dnl
dnl Choose an Emacs flavor according to the --with-emacs user option, or try
dnl emacs and xemacs.
dnl We use EMACS_PROG instead of EMACS to avoid colliding with Emacs' own
dnl internal environment.
AC_DEFUN([BBDB_PROG_EMACS],
  [ AC_ARG_WITH([emacs],
     AC_HELP_STRING([--with-emacs=PROG],
        [choose which flavor of Emacs to use]),
      [ EMACS_PROG="${withval}" ],
      [ AC_CHECK_PROGS(EMACS_PROG, emacs xemacs) ])
    if test "x${EMACS_PROG}" = "x" ; then
      dnl This is critical enough to generate an error and not a warning...
      AC_MSG_ERROR([*** No Emacs program found.])
    fi
    dnl If I were pedantic, I'd check that the user-specified executable is
    dnl actually working. I might do that someday.
    AC_SUBST(EMACS_PROG) ])

dnl aclocal.m4 ends here

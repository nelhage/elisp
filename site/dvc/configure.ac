# configure.ac --- configuration setup for DVC

# Copyright (C) 2004-2007 by all contributors
# Author: Robert Widhopf-Fenk <hack@robf.de>

# DVC is free software; you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.

# DVC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.

# You should have received a copy of the GNU Library General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Process this file with autoconf to produce a new configure script

# Find a program. A failure is not fatal, just issue a warning
AC_DEFUN([DVC_PROG_WARN],
  [ AC_SUBST([$1])
    AC_ARG_WITH([$2],
      [AS_HELP_STRING([--with-][$2]=PROG, [$3])],
      [ [$1]=${withval} ],
      [
        AC_CHECK_PROG([$1], [$2], [$2])
        if test "x${$1}" = "x" ; then
          AC_MSG_WARN([*** No $2 program found.])
        fi
      ])
  ])

# Find a program. A failure is not fatal.
AC_DEFUN([DVC_PROG],
  [ AC_SUBST([$1])
    AC_ARG_WITH([$2],
      [AS_HELP_STRING([--with-][$2]=PROG, [$3])],
      [ [$1]=${withval} ],
      [
        AC_CHECK_PROG([$1], [$2], [$2])
      ])
  ])


##############################################################################
AC_INIT([DVC], [0], [dvc-dev@gna.org])

AC_COPYRIGHT([Copyright (C) 2004-2007 Robert Widhopf-Fenk <hack@robf.de> and the DVC team])

AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_FILES([Makefile lisp/Makefile texinfo/Makefile dvc-load.el lisp/dvc-site.el])

# Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_MKDIR_P

# External programs checking:

# Choose an Emacs flavor according to the --with-emacs user option, or
# try "emacs" and "xemacs".  We use EMACS_PROG instead of EMACS to
# avoid colliding with Emacs' own internal environment.
AC_ARG_WITH([emacs],
  [AS_HELP_STRING([--with-emacs=PROG], [choose which flavor of Emacs to use])],
  [ EMACS_PROG="${withval}" ],
  [ AC_CHECK_PROGS(EMACS_PROG, emacs xemacs) ])
if test "x${EMACS_PROG}" = "x" ; then
  AC_MSG_ERROR([*** No Emacs program found.])
fi

AC_MSG_CHECKING([emacs-type of ${EMACS_PROG}])
if ${EMACS_PROG} --no-site-file --batch --eval \
      '(kill-emacs (if (featurep (quote xemacs)) 0 1))'
then EMACS_FLAVOR=xemacs ; FLAGS='-no-site-file -no-autoloads'
else EMACS_FLAVOR=emacs  ; FLAGS=--no-site-file
fi
AC_MSG_RESULT([${EMACS_FLAVOR}])

# Copied from gnus aclocal.m4
AC_ARG_WITH([lispdir],
  [AS_HELP_STRING([--with-lispdir=DIR], [where to install lisp files])],
  [lispdir=${withval}])
AC_MSG_CHECKING([where .elc files should go])
if test -z "$lispdir"; then
  theprefix=$prefix
  if test "x$theprefix" = "xNONE"; then
    theprefix=$ac_default_prefix
  fi
  datadir="\$(prefix)/share"
  if test "$EMACS_FLAVOR" = "xemacs"; then
    datadir="\$(prefix)/lib"
    lispdir="${datadir}/${EMACS_FLAVOR}/site-packages/lisp/dvc"
    if test ! -d "${lispdir}"; then
      if test -d "${theprefix}/share/${EMACS_FLAVOR}/site-lisp"; then
        lispdir="\$(prefix)/lib/${EMACS_FLAVOR}/site-packages/lisp/dvc"
      fi
    fi
  else
    lispdir="${datadir}/${EMACS_FLAVOR}/site-lisp/dvc"
  fi
fi
AC_MSG_RESULT([$lispdir])
AC_SUBST([lispdir])

AC_SUBST([EMACS_PROG])
AC_SUBST([FLAGS])

# Copied from gnus aclocal.m4 (AC_PATH_INFO_DIR)
AC_MSG_CHECKING([where the TeXinfo docs should go])
dnl Set default value.  This must be an absolute path.
if test "$infodir" = "\${prefix}/info"; then
  if test "$EMACS_FLAVOR" = "xemacs"; then
    info_dir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/info"
  else
    info_dir="\$(prefix)/share/info"
  fi
else
  info_dir=$infodir
fi
AC_MSG_RESULT([$info_dir])
AC_SUBST([info_dir])

DVC_PROG([TLA], [tla], [tla program to use])
DVC_PROG([BAZ], [baz], [baz program to use])

AC_MSG_CHECKING([arch branch to use])
AC_ARG_WITH([arch],
  [AS_HELP_STRING([--with-arch=BRANCH],
    [which arch branch (one of: tla, baz, none) to use by default
     (default is none)])],
  [ARCH_BRANCH=$withval],
  [ARCH_BRANCH=none])
case $ARCH_BRANCH in
  tla|baz|none) blurb= ;;
  *) blurb=" (ignored invalid --with-arch=$ARCH_BRANCH)"
     ARCH_BRANCH=none ;;
esac
AC_MSG_RESULT([$ARCH_BRANCH$blurb])
AC_SUBST([ARCH_BRANCH])

DVC_PROG_WARN([DIFF], [diff], [diff program to use])
DVC_PROG_WARN([PATCH], [patch], [patch program to use])

AC_ARG_WITH([other-dirs],
  [AS_HELP_STRING([--with-other-dirs=DIRS],
                  [push DIRS (list of space- or colon-separated paths)
                   onto `load-path' during build])],
  [OTHERDIRS=`echo "$withval" | sed 'y/:/ /'`])
AC_SUBST([OTHERDIRS])

# tree widget
AC_MSG_CHECKING([whether tree-widget is in the load-path])
if ${EMACS_PROG} ${FLAGS} --batch --eval \
      '(kill-emacs
        (if (locate-library "tree-widget" nil
              (append command-line-args-left load-path))
            0 1))' \
      $OTHERDIRS
  then HAS_TREE_WIDGET=yes
  else HAS_TREE_WIDGET=no
fi
AC_MSG_RESULT([${HAS_TREE_WIDGET}])
if test "x${HAS_TREE_WIDGET}" = "xno" ; then
  AC_MSG_WARN([*** tree-widget.el not found in 'load-path.])
  AC_MSG_WARN([*** tla-browse.el won't be available unless you install it.])
  AC_MSG_WARN([*** See http://sourceforge.net/projects/emhacks/])
  AC_MSG_WARN([*** Provide the path to tree-widget with --with-other-dirs])
  AC_MSG_WARN([*** if tree-widget.el is already present on your system])
fi

AC_OUTPUT

# configure.ac ends here

## guile-sdl.m4 --- some -*-autoconf-*- macros for Guile-SDL

# Copyright (C) 2007, 2013 Thien-Thi Nguyen
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA  02110-1301  USA

##----------------------------------------------------------------------------

# GUILE_SDL_OPTLIB --- Handle --disable-FOO for optional libraries
#
# $1 is a component, one of: mixer, ttf.
# $2 is a C-language function name.
#
# Arrange for --disable-$1 support.
# If not disabled, check libSDL_$1 for function $2.
# If found, do two things w/ var HAVE_$1 (all upcased):
# - create an AM_CONDITIONAL;
# - set its value to 1 and AC_SUBST it.
#
AC_DEFUN([GUILE_SDL_OPTLIB],[
m4_pushdef([ENABLE],  [enable_]$1)
m4_pushdef([ENABLED], [test $[]ENABLE = yes])
m4_pushdef([COND],    [AS_TR_CPP([HAVE_]$1)])

AC_ARG_ENABLE([$1],[AC_HELP_STRING([--disable-$1],
[omit bindings for SDL_$1 (default=enabled)])],:,[ENABLE=yes])
AS_IF([ENABLED],[
  dnl Use ":" to avoid prepending to $LIBS.
  AC_CHECK_LIB([SDL_$1], $2, :, ENABLE=no)
])

AM_CONDITIONAL(COND, ENABLED)

AS_IF([ENABLED],[
  COND=1
  AC_SUBST(COND)
])

m4_popdef([COND])
m4_popdef([ENABLED])
m4_popdef([ENABLE])
])dnl GUILE_SDL_OPTLIB

##----------------------------------------------------------------------------

# Override libtool's attempt to support C++, Fortran, etc.
AC_DEFUN([_LT_AC_TAGCONFIG],[:])

##----------------------------------------------------------------------------
## guile-sdl.m4 ends here

dnl -*- Autoconf -*-

AC_INIT(guile-2d, 0.1.0)
AC_CONFIG_SRCDIR(2d)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

AC_CONFIG_FILES([Makefile doc/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

PKG_CHECK_MODULES([SDL], [sdl <= 1.3 sdl >= 1.2])
PKG_CHECK_MODULES([FTGL], [ftgl >= 2.1])
# Why doesn't freeimage have a pkg-config file?
#PKG_CHECK_MODULES([freeimage], [freeimage >= 3.0])

dnl Bindings for optional libs.
GUILE_SDL_OPTLIB([mixer],[Mix_OpenAudio])
GUILE_SDL_OPTLIB([ttf],[TTF_Init])

AC_OUTPUT

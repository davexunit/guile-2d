include guile.am

moddir=$(prefix)/share/guile/site/2.0
godir=$(libdir)/guile/2.0/ccache

ACLOCAL_AMFLAGS = -I build-aux

SOURCES = $(wildcard 2d/*.scm  2d/*/*.scm)

EXTRA_DIST += env.in \
	build-aux/guile-sdl.m4

SUBDIRS = doc

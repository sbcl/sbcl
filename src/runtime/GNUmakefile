# makefile for the C-level run-time support for SBCL

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

all: sbcl sbcl.nm
.PHONY: all

# defaults which might be overridden or modified by values in the
# Config file
#
# FIXME: The -fno-strength-reduce flag comes from before the fork
# from CMU CL. It's presumably to work around some optimizer bug in gcc,
# but the fork was a long time ago, and the optimizer could easily
# have been fixed since then. Try doing without it.
# CFLAGS =  -g -Wall -O2 -fno-strength-reduce -DGENCGC
CFLAGS =  -g -Wall -O3 -DGENCGC
ASFLAGS = -g -DGENCGC
DEPEND_FLAGS =
CPPFLAGS = -I.

# The Config file is the preferred place for tweaking options which
# are appropriate for particular setups (OS, CPU, whatever). Make a
# Config-foo file for setup foo, then arrange for Config to be a
# symlink to Config-foo.
include Config

# Some of these things might be Config-dependent in future versions,
# but they're not right now, i.e., they happen to be the same for
# all supported systems.
CPP = cpp
GC_SRC = gencgc.c
LD = ld
LINKFLAGS = -g
NM = nm -gp

SRCS =	alloc.c backtrace.c breakpoint.c coreparse.c \
	dynbind.c globals.c interr.c interrupt.c \
	monitor.c parse.c print.c purify.c \
	regnames.c run-program.c runtime.c save.c search.c \
	time.c util.c validate.c vars.c wrap.c \
	${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

sbcl.nm: sbcl
	$(NM) sbcl | grep -v " F \| U " > ,$@
	mv -f ,$@ $@

sbcl: ${OBJS} 
	$(CC) ${LINKFLAGS} ${OS_LINK_FLAGS} -o $@ ${OBJS} ${OS_LIBS} -lm

.PHONY: clean all
clean:
	rm -f depend *.o sbcl sbcl.nm core *.tmp ; true

depend: ${SRCS} sbcl.h
	$(CC) -MM -E ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $? > depend.tmp
	mv -f depend.tmp depend

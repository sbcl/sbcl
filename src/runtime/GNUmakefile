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

# defaults which might be overridden by values in the Config file
CC = gcc
DEPEND_FLAGS =

# The Config file is the preferred place for tweaking options which
# are appropriate for particular setups (OS, CPU, whatever). Make
# a Config-foo file for setup foo, then set Config to be a symlink
# to Config-foo.
include Config

SRCS =	alloc.c backtrace.c breakpoint.c coreparse.c \
	dynbind.c globals.c interr.c interrupt.c \
	monitor.c parse.c print.c purify.c \
	regnames.c run-program.c runtime.c save.c search.c \
	time.c validate.c vars.c \
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

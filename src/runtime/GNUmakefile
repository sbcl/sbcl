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
CFLAGS =  -g -Wall -O3
ASFLAGS = $(CFLAGS)
DEPEND_FLAGS =
CPPFLAGS = -I.

# Some of these things might be Config-dependent in future versions,
# but they're the same on most systems right now.  If you need to
# override one of these, do it in Config
CPP = cpp
LD = ld
LINKFLAGS = -g
NM = nm -gp
DEPEND_FLAGS=-M

# The Config file is the preferred place for tweaking options which
# are appropriate for particular setups (OS, CPU, whatever). Make a
# Config-foo file for setup foo, then arrange for Config to be a
# symlink to Config-foo.
include Config


C_SRCS =alloc.c backtrace.c breakpoint.c coreparse.c \
	dynbind.c gc-common.c globals.c interr.c interrupt.c \
	monitor.c parse.c print.c purify.c \
	regnames.c run-program.c runtime.c save.c search.c \
	thread.c time.c util.c validate.c vars.c wrap.c 

SRCS=	$(C_SRCS) ${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

sbcl.nm: sbcl
	$(NM) sbcl | grep -v " F \| U " > ,$@
	mv -f ,$@ $@

sbcl: ${OBJS} 
	$(CC) ${LINKFLAGS} ${OS_LINK_FLAGS} -o $@ ${OBJS} ${OS_LIBS} -lm


.PHONY: clean all
clean:
	-rm -f depend *.o sbcl sbcl.nm core *.tmp

TAGS: $(SRCS)
	etags $(SRCS)

sbcl.h: genesis/*.h
	echo '#include "genesis/config.h"' >sbcl.h
	echo '#include "genesis/constants.h"' >>sbcl.h

depend: ${C_SRCS} sbcl.h
	$(CC) ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS}  ${C_SRCS} > depend.tmp
	mv -f depend.tmp depend

# By including this file, we cause GNU make to automatically "make depend"
# if it can't find it or it is out of date.
include depend

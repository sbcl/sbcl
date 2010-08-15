# -*- makefile -*- for the C-level run-time support for SBCL

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

.PHONY: all clean TAGS tags targets

all: targets
TARGET=sbcl

# Defaults which might be overridden or modified by values in the
# Config file. Most of them are same on most systems right now.
# If you need to override one of these, do it in Config.
LD = ld
LINKFLAGS = -g
NM = nm -gp
DEPEND_FLAGS = -MM
GREP = grep

include ../../output/prefix.def

CFLAGS = -g -Wall -Wsign-compare -O3
ASFLAGS = $(CFLAGS)
CPPFLAGS = -I. -DSBCL_PREFIX=\"$(SBCL_PREFIX)\"

# Give make access to the target Lisp features.
include genesis/Makefile.features

# The Config file is the preferred place for tweaking options which
# are appropriate for particular setups (OS, ARCH, whatever). Make a
# Config-foo file for setup foo, then arrange for Config to be a
# symlink to Config-foo.
# Commonly used variables in Config are: ARCH_SRC, ASSEM_SRC, GC_SRC,
# OS_SRC, OS_LIBS, OS_OBJS, OS_CLEAN_FILES
include Config

COMMON_SRC = alloc.c backtrace.c breakpoint.c coreparse.c \
	dynbind.c funcall.c gc-common.c globals.c interr.c interrupt.c \
	largefile.c monitor.c os-common.c parse.c print.c purify.c \
	pthread-futex.c pthread-lutex.c \
	regnames.c run-program.c runtime.c save.c search.c \
	thread.c time.c util.c validate.c vars.c wrap.c	

C_SRC = $(COMMON_SRC) ${ARCH_SRC} ${OS_SRC} ${GC_SRC}

SRCS = $(C_SRC) ${ASSEM_SRC}

OBJS = $(C_SRC:.c=.o) $(ASSEM_SRC:.S=.o) ${OS_OBJS}

LIBS = ${OS_LIBS} -lm

targets: $(TARGET) sbcl.nm

$(TARGET): $(OBJS)
	$(CC) ${LINKFLAGS} -o $@ $^ $(LIBS)

sbcl.nm: $(TARGET)
	$(NM) $(TARGET) | $(GREP) -v " [FUw] " > ,$@
	mv -f ,$@ $@

sbcl.h: $(wildcard genesis/*.h)
	echo '#include "genesis/config.h"' >sbcl.h
	echo '#include "genesis/constants.h"' >>sbcl.h

TAGS tags: $(SRCS)
	etags $(SRCS)

clean:
	-rm -f *.[do] $(TARGET) sbcl.nm sbcl.h core *.tmp $(OS_CLEAN_FILES)

%.d: %.c sbcl.h
	@$(CC) $(DEPEND_FLAGS) $(CPPFLAGS) $< > $@.tmp; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.tmp > $@; \
	rm -f $@.tmp

%.d: %.S sbcl.h
	@$(CC) $(DEPEND_FLAGS) $(CPPFLAGS) $< > $@.tmp; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.tmp > $@; \
	rm -f $@.tmp

# By including those files, we cause GNU make to automatically re-make
# all dependencies of the .c file if necessary.
ifneq ($(MAKECMDGOALS),clean)
-include $(C_SRC:.c=.d) $(ASSEM_SRC:.S=.d)
endif

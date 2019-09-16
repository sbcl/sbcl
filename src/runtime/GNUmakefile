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

all: targets tags
TARGET=sbcl

# Allow using paxctl(8) or similar programs to adjust PaX permissions
# of src/runtime/sbcl during the build.
SBCL_PAXCTL ?= :

# Defaults which might be overridden or modified by values in the
# Config file. Most of them are same on most systems right now.
# If you need to override one of these, do it in Config.
LINKFLAGS += -g
NM = nm -gp
DEPEND_FLAGS = -MM
GREP = grep
LD = ld

# By default, don't make and use a library, just use the object files.
LIBSBCL = $(OBJS)
USE_LIBSBCL = $(OBJS)
__LDFLAGS__ =

include ../../output/prefix.def

CFLAGS += -g -Wall -Wundef -Wsign-compare -Wpointer-arith -O3
ASFLAGS += $(CFLAGS)
CPPFLAGS += -I. -DSBCL_PREFIX=\"$(SBCL_PREFIX)\"

# Give make access to the target Lisp features.
include genesis/Makefile.features

# The Config file is the preferred place for tweaking options which
# are appropriate for particular setups (OS, ARCH, whatever). Make a
# Config-foo file for setup foo, then arrange for Config to be a
# symlink to Config-foo.
# Commonly used variables in Config are: ARCH_SRC, ASSEM_SRC, GC_SRC,
# OS_SRC, OS_LIBS, OS_CLEAN_FILES
DISABLE_PIE=yes
include Config

# Disable PIE when possible
# If dynamic core is enabled, we should not care whether we get a PIE or not,
# and would prefer the default for the installed C toolchain,
# however, when building the 32-bit x86 code without the disabling of PIE,
# I got a linker error that was totally unhelpful in that it did not indicate
# an offending source file:
#  /usr/bin/ld: BFD (GNU Binutils for Debian) 2.31.1 assertion fail ../../bfd/elflink.c:14480
# Process of elimination concluded that it's from ldso-stubs.
# I'm disinclined to find the problem with the asm code in that file.
# More pertinently, if sb-dynamic-core is enabled, the stubs shouldn't be needed.
# But we link them them because of, I guess, "historical reasons".
ifeq ($(DISABLE_PIE),yes)
ifneq ($(shell $(CC) -dumpspecs 2>/dev/null | grep -e '[^f]no-pie'),)
CFLAGS += -fno-pie
LINKFLAGS += -no-pie
LDFLAGS += -no-pie
__LDFLAGS__ += -no-pie
endif
ifneq ($(shell $(CC) -dumpspecs 2>/dev/null | grep -e '[^f]nopie'),)
CFLAGS += -fno-pie
LINKFLAGS += -nopie
LDFLAGS += -nopie
__LDFLAGS__ += -nopie
endif
endif

COMMON_SRC = alloc.c backtrace.c breakpoint.c coalesce.c coreparse.c    \
	dynbind.c funcall.c gc-common.c globals.c hopscotch.c           \
	interr.c interrupt.c largefile.c main.c                         \
	monitor.c murmur_hash.c os-common.c parse.c print.c             \
	purify.c pthread-futex.c regnames.c run-program.c runtime.c     \
	safepoint.c save.c sc-offset.c search.c thread.c time.c         \
	validate.c var-io.c vars.c wrap.c

C_SRC = $(COMMON_SRC) ${ARCH_SRC} ${OS_SRC} ${GC_SRC}

SRCS = $(C_SRC) ${ASSEM_SRC}

OBJS = $(C_SRC:.c=.o) $(ASSEM_SRC:.S=.o)

LIBS = ${OS_LIBS} -lm

targets: $(TARGET) $(OBJTARGET) sbcl.nm sbcl.mk

$(TARGET): $(LIBSBCL)
	$(CC) ${LINKFLAGS} -o $@ $(USE_LIBSBCL) $(LIBS)
	$(SBCL_PAXCTL) $@

# Enable compiling gencgc with even more assertions and/or
# data collection, with COMPILING_TESTS. Not really used yet.
gc-unit-tests.o: CFLAGS=-DCOMPILING_TESTS
unit-tests: gc-unit-tests.o libsbcl.a
	cc -g -no-pie -o $@ $^ -ldl -lpthread -lm

# ld -r -o sbcl.o works on Linux, but not on other platforms.
# On macOS, it fails to keep debug sections.
# On mingw64, it leads to an executable that cannot be executed.
sbcl.o: $(OBJS)
	$(LD) $(__LDFLAGS__) -r -o $@ $^

libsbcl.a: $(OBJS)
	rm -f $@ ; ar rcs $@ $^

PIC_OBJS = $(subst .o,.pic.o,$(filter-out ldso-stubs.o,$(OBJS)))
libsbcl.so: $(PIC_OBJS)
	cc -shared -o $@ $^ -ldl -lpthread -lm
# for this to work, you must have with-gcc-tls in your build features already.
# can't define it here because then it conflicts if you have it in both places.
%.pic.o: %.c
	$(CC) -fPIC -c $(filter-out -fno-pie,$(CFLAGS)) $< -o $@
%.pic.o: %.S # (-fPIC doesn't affect hand-written assembly source)
	$(CC) -DPOSITION_INDEPENDENT_ASM -c $(CFLAGS) $< -o $@
testmain: testmain.c libsbcl.so -ldl

SHRINKWRAP_DEPS = ../../output/sbcl.core ../../tools-for-build/editcore.lisp
shrinkwrap-sbcl.s shrinkwrap-sbcl-core.o: $(SHRINKWRAP_DEPS)
	../../run-sbcl.sh --script ../../tools-for-build/editcore.lisp split \
		../../output/sbcl.core shrinkwrap-sbcl.s
pie-shrinkwrap-sbcl.s pie-shrinkwrap-sbcl-core.o: $(SHRINKWRAP_DEPS)
	../../run-sbcl.sh --script ../../tools-for-build/editcore.lisp split --pie \
		../../output/sbcl.core pie-shrinkwrap-sbcl.s
comma := , # "Commas ... cannot appear in the text of an argument as written"
shrinkwrap-sbcl: shrinkwrap-sbcl.s shrinkwrap-sbcl-core.o $(LIBSBCL)
	$(CC) $(filter-out -Wl$(comma)--export-dynamic, $(LINKFLAGS)) \
 $(CFLAGS) -o $@ $(filter-out ldso-stubs.o, $^) $(LIBS)
pie-shrinkwrap-sbcl: pie-shrinkwrap-sbcl.s pie-shrinkwrap-sbcl-core.o $(PIC_OBJS)
	$(CC) -pie -o $@ $^ $(LIBS)
semiwrap-sbcl: shrinkwrap-sbcl.s $(LIBSBCL)
	$(CC) $(LINKFLAGS) $(CFLAGS) -o $@ $^ $(LIBS)

sbcl.mk: Config
	( echo 'CC=$(CC)' ; \
	  echo 'LD=$(LD)' ; \
	  echo 'CFLAGS=$(CFLAGS)' ; \
	  echo 'ASFLAGS=$(ASFLAGS)' ; \
	  echo 'LINKFLAGS=$(LINKFLAGS)' ; \
	  echo 'LDFLAGS=$(LDFLAGS)' ; \
	  echo '__LDFLAGS__=$(__LDFLAGS__)' ; \
	  echo 'LIBS=$(LIBS)' ; \
	  if [ -n '$(LISP_FEATURE_SB_LINKABLE_RUNTIME)' ] ; then \
	    echo 'LIBSBCL=$(LIBSBCL)' ; \
	    echo 'USE_LIBSBCL=$(USE_LIBSBCL)' ; \
	  fi ; \
	  : ) > $@

sbcl.nm: $(TARGET)
	$(NM) $(TARGET) | $(GREP) -v " [FUw] " > ,$@
	mv -f ,$@ $@

sbcl.h: $(wildcard genesis/*.h)
	echo '#include "genesis/config.h"' >sbcl.h
	echo '#include "genesis/constants.h"' >>sbcl.h

# || true because we don't want the build to break if etags isn't there.
# ...but it's still nice to have it done by default.
HEADERS=$(wildcard *.h genesis/*.h)
TAGS tags: $(SRCS) $(HEADERS)
	@etags $(SRCS) $(HEADERS) || true

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

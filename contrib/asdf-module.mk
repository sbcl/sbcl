# We need to extend flags to the C compiler and the linker
# here. sb-posix, sb-grovel, and sb-bsd-sockets depends upon these
# being set on x86_64. Setting these in their Makefiles is not
# adequate since, while we're building contrib, they can be compiled
# directly via ASDF from a non-C-aware module which has these tricky
# ones as dependencies.

UNAME:=$(shell uname -s)
DEST=$(SBCL_PWD)/obj/sbcl-home/contrib/
FASL=$(DEST)/$(SYSTEM).fasl
ASD=$(DEST)/$(SYSTEM).asd

ifeq (SunOS,$(UNAME))
  # _XOPEN_SOURCE tells your compiler to include definitions for some
  # *extra* functions that are defined in the X/Open and POSIX standards.
  # The numbers refer to different versions of the standard:
  # 500 - X/Open 5, incorporating POSIX 1995
  # 600 - X/Open 6, incorporating POSIX 2004
  # 700 - X/Open 7, incorporating POSIX 2008
  # OpenCSW GCC 5.5.0 and Oracle Studio compiler 12.5 both throw
  # "Compiler or options invalid for pre-UNIX 03 X/Open applications and
  #  pre-2001 POSIX applications" with -D_XOPEN_SOURCE=500.
  EXTRA_CFLAGS+=-D_XOPEN_SOURCE=600 -D__EXTENSIONS__
  # OpenCSW GCC 5.5.0 cannot compile foo.c successfully.
  CC:=/usr/bin/cc
  PATH:=/usr/xpg4/bin:${PATH}
endif
ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
  # SBCL can't read cygwin symlinks, and cygwin likes to symlink
  # gcc.  To further complicate things, SBCL can't handle cygwin
  # paths, either.
  CC:=$(shell cygpath -m $(shell readlink -fn $(shell which $(CC))))
endif
ifeq (Linux,$(UNAME))
  EXTRA_CFLAGS+=-D_GNU_SOURCE
endif

export CC SBCL EXTRA_CFLAGS

all: $(FASL) $(ASD)

$(FASL)::
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(setf (sb-ext:readtable-base-char-preference *readtable*) :both)' \
		--load ../asdf-stub.lisp \
		--eval '(asdf::build-asdf-contrib "$(SYSTEM)")'

$(ASD)::
	echo "(defsystem :$(SYSTEM) :class require-system)" > $@

test: $(FASL) $(ASD)
	$(SBCL) --load ../asdf-stub.lisp \
		--eval '(asdf::test-asdf-contrib "$(SYSTEM)")'

# KLUDGE: There seems to be no portable way to tell tar to not to
# preserve owner, so chown after installing for the current user.
install:
	cp $(FASL) $(ASD) "$(BUILD_ROOT)$(INSTALL_DIR)"

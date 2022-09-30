# We need to extend flags to the C compiler and the linker
# here. sb-posix, sb-grovel, and sb-bsd-sockets depends upon these
# being set on x86_64. Setting these in their Makefiles is not
# adequate since, while we're building contrib, they can be compiled
# directly via ASDF from a non-C-aware module which has these tricky
# ones as dependencies.

UNAME:=$(shell uname -s)
# no trailing slash on DEST. Don't want a "//" in FASL and ASD
DEST=$(SBCL_TOP)/obj/sbcl-home/contrib
FASL=$(DEST)/$(SYSTEM).fasl
ASD=$(DEST)/$(SYSTEM).asd

ifeq (SunOS,$(UNAME))
  EXTRA_CFLAGS+=-D_XOPEN_SOURCE=500 -D__EXTENSIONS__
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

all: $(FASL)

$(FASL):: # this produces $(ASD) as a side-effect
	$(SBCL)	--load ../make-contrib.lisp "$(SYSTEM)" $(MODULE_REQUIRES)

install:
	cp $(FASL) $(ASD) "$(BUILD_ROOT)$(INSTALL_DIR)"

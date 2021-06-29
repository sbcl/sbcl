# We need to extend flags to the C compiler and the linker
# here. sb-posix, sb-grovel, and sb-bsd-sockets depends upon these
# being set on x86_64. Setting these in their Makefiles is not
# adequate since, while we're building contrib, they can be compiled
# directly via ASDF from a non-C-aware module which has these tricky
# ones as dependencies.

UNAME:=$(shell uname -s)
DEST=$(SBCL_TOP)/obj/sbcl-home/contrib/
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

all: $(FASL) $(ASD)

$(FASL)::
	$(SBCL) --eval '(setf (sb-ext:readtable-base-char-preference *readtable*) :both)' \
		--eval '(declaim (muffle-conditions (and compiler-note (not sb-c::unknown-typep-note))))' \
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

CC=gcc

# We need to extend flags to the C compiler and the linker
# here. sb-posix, sb-grovel, and sb-bsd-sockets depends upon these
# being set on x86_64. Setting these in their Makefiles is not
# adequate since, while we're building contrib, they can be compiled
# directly via ASDF from a non-C-aware module which has these tricky
# ones as dependencies.

UNAME:=$(shell uname -s)

ifeq (SunOS,$(UNAME))
  EXTRA_CFLAGS=-D_XOPEN_SOURCE=500 -D__EXTENSIONS__
  PATH:=/usr/xpg4/bin:${PATH}
endif
ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
  EXTRA_CFLAGS=-mno-cygwin
  # GCC 4.x doesn't accept -mno-cygwin.
  CC:=gcc-3
  # SBCL can't read cygwin symlinks, and cygwin likes to symlink
  # gcc.  To further complicate things, SBCL can't handle cygwin
  # paths, either.
  CC:=$(shell cygpath -m $(shell readlink -fn $(shell which $(CC))))
endif

export CC SBCL EXTRA_CFLAGS EXTRA_LDFLAGS

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(defvar *system* "$(SYSTEM)")' --load ../asdf-stub.lisp --eval '(quit)'

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM))" \
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

# KLUDGE: There seems to be no portable way to tell tar to not to
# preserve owner, so chown after installing for the current user.
install: $(EXTRA_INSTALL_TARGETS)
	tar cf - . | ( cd "$(BUILD_ROOT)$(INSTALL_DIR)" && tar xpvf - )
	find "$(BUILD_ROOT)$(INSTALL_DIR)" -exec chown `id -u`:`id -g` {} \;

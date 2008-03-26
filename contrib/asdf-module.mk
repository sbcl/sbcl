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
endif
ifeq (CYGWIN,$(findstring CYGWIN,$(UNAME)))
  EXTRA_CFLAGS=-mno-cygwin
endif

export CC SBCL EXTRA_CFLAGS EXTRA_LDFLAGS

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(defvar *system* "$(SYSTEM)")' --load ../asdf-stub.lisp --eval '(quit)'

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM))" \
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

# KLUDGE / FIXME: Perhaps each module should have it's own list of
# files to install? At any rate, this is a portable (we hope) way of
# installing all the files needed -- as long as all the files are in
# the first level directory...
install: $(EXTRA_INSTALL_TARGETS)
	cp -p $(SYSTEM).asd *.lisp *.fasl "$(BUILD_ROOT)$(INSTALL_DIR)"
	find "$(BUILD_ROOT)$(INSTALL_DIR)" -type f -exec chown `id -u`:`id -g` {} \;

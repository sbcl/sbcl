CC=gcc

# We need to extend flags to the C compiler and the linker
# here. sb-posix, sb-grovel, and sb-bsd-sockets depends upon these
# being set on x86_64. Setting these in their Makefiles is not
# adequate since, while we're building contrib, they can be compiled
# directly via ASDF from a non-C-aware module which has these tricky
# ones as dependencies.

UNAME:=$(shell uname -m)
ifeq (x86_64,$(UNAME))
    export EXTRA_LDFLAGS=-m32 -shared
    export EXTRA_CFLAGS+=-m32
endif

export CC SBCL EXTRA_CFLAGS EXTRA_LDFLAGS

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(defvar *system* "$(SYSTEM)")' --load ../asdf-stub.lisp --eval '(quit)'

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM))" \
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install: $(EXTRA_INSTALL_TARGETS)
	tar cf - . | ( cd $(BUILD_ROOT)$(INSTALL_DIR) && tar xpvf - )
	( cd  $(BUILD_ROOT)$(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )

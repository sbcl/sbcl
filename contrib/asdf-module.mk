CC=gcc

# Need to set CFLAGS and LDFLAGS here. sb-posix, sb-grovel, and
# sb-bsd-sockets depends upon these being set on x86_64. Setting these
# in their Makefile's is not adequate since their asd files are
# invoked when loaded from other modules which don't require these
# environmental values in their Makefile's.

UNAME:=$(shell uname -m)
export CFLAGS=-fPIC
ifeq (solaris,$(UNAME))
  export LDFLAGS=-shared -lresolv -lsocket -lnsl
else
  ifeq (Darwin,$(UNAME))
    export LDFLAGS=-bundle
  else
    ifeq (x86_64,$(UNAME))
      export LDFLAGS=-m32 -shared
      export CFLAGS+= -m32
    else
      export LDFLAGS=-shared
    endif
  endif
endif

export CC SBCL CFLAGS LDFLAGS

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(load "../asdf/asdf")' \
	  --eval "(setf asdf::*central-registry* '((MERGE-PATHNAMES \"systems/\" (TRUENAME (SB-EXT:POSIX-GETENV \"SBCL_HOME\")))))" \
	  --eval "(push :sb-building-contrib *features*)" \
	  --eval "(asdf:operate 'asdf:load-op :$(SYSTEM))" \
	  --eval "(progn (when (probe-file \"$(SYSTEM).fasl\") (error \"fasl file exists\")) (with-open-file (s \"$(SYSTEM).lisp\" :direction :output :if-exists :error) (print (quote (require :asdf)) s) (print (quote (require :$(SYSTEM))) s)) (compile-file \"$(SYSTEM).lisp\") (delete-file \"$(SYSTEM).lisp\"))" \
	  --eval "(quit)"

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM))" \
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install: $(EXTRA_INSTALL_TARGETS)
	tar cf - . | ( cd $(BUILD_ROOT)$(INSTALL_DIR) && tar xpvf - )
	( cd  $(BUILD_ROOT)$(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
